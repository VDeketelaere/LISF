!-----------------------BEGIN NOTICE -- DO NOT EDIT-----------------------
! NASA Goddard Space Flight Center
! Land Information System Framework (LISF)
! Version 7.5
!
! Copyright (c) 2024 United States Government as represented by the
! Administrator of the National Aeronautics and Space Administration.
! All Rights Reserved.
!-------------------------END NOTICE -- DO NOT EDIT-----------------------
#include "LDT_misc.h"
!BOP
! !ROUTINE: read_agera5_elev
!  \label{read_agera5_elev}
!
! !REVISION HISTORY:
!
!  Dec 2025: Initial implementation based on MERRA2
!
! !INTERFACE:
subroutine read_agera5_elev( n, findex, agera5elev, elevdiff )

! !USES:
  use LDT_coreMod,       only : LDT_rc, LDT_domain
  use LDT_metforcingMod, only : LDT_forc
  use agera5_forcingMod, only : agera5_struc
  use LDT_logMod,        only : LDT_logunit, LDT_verify, &
                                LDT_endrun
  use LDT_fileIOMod,     only : LDT_transform_paramgrid
#if (defined USE_NETCDF3 || defined USE_NETCDF4) 
  use netcdf
#endif

  implicit none

! !ARGUMENTS: 
  integer, intent(in) :: n 
  integer, intent(in) :: findex
!- Terrain height will be set to run domain:
  real, intent(inout) :: agera5elev(LDT_rc%lnc(n),LDT_rc%lnr(n),1)
  real, intent(inout) :: elevdiff(LDT_rc%met_nc(findex), LDT_rc%met_nr(findex))

! !DESCRIPTION:
!
!  Opens, reads, and interpolates AgERA5 model elevation to the LDT
!  grid. The data will be used to perform any topographical 
!  adjustments to the forcing. AgERA5 uses actual elevation in meters,
!  not geopotential height.
!
!  The arguments are: 
!  \begin{description}
!  \item[n]
!   index of the nest
!  \item[findex]
!   index of the forcing dataset selected
!  \end{description}
! 
!EOP
   logical :: file_exists
   integer :: ftn_const
   integer :: i,c,r,k,iret

   integer :: elevId
   real    :: read_elev(agera5_struc(n)%nc, agera5_struc(n)%nr,1)

   ! Grid transform fields:
   integer   :: inpts, outpts
   real      :: elev1d(agera5_struc(n)%nc*agera5_struc(n)%nr)
   logical*1 :: lb(agera5_struc(n)%nc*agera5_struc(n)%nr)
   real      :: elev_regrid(LDT_rc%lnc(n)*LDT_rc%lnr(n))
   logical*1 :: lb_regrid(LDT_rc%lnc(n)*LDT_rc%lnr(n))

! _____________________________________________________________________________

#if (defined USE_NETCDF3) 
  write(LDT_logunit,*) "[ERR] AgERA5 terrain height reader requires NetCDF4"
  call LDT_endrun()
#endif

   agera5elev = LDT_rc%udef
   elevdiff = LDT_rc%udef

   ! Check if AgERA5 grid is selected but downscaling options, 
   !  like bilinear, is incorrectly selected.
   if( agera5_struc(n)%gridDesc(9)  == LDT_rc%gridDesc(n,9) .and. &
       agera5_struc(n)%gridDesc(10) == LDT_rc%gridDesc(n,10).and. &
       LDT_rc%gridDesc(n,1) == 0 .and. &
       LDT_rc%met_gridtransform_parms(findex) .ne. "neighbor" ) then
      write(LDT_logunit,*) "[WARN] The AgERA5 grid was selected for the"
      write(LDT_logunit,*) "  LDT run domain; however, 'bilinear', 'budget-bilinear',"
      write(LDT_logunit,*) "  or some other unknown option was selected to spatially"
      write(LDT_logunit,*) "  downscale the grid, which will cause errors during runtime."
      write(LDT_logunit,*) "Program stopping ..."
      call LDT_endrun()
   endif

   inquire(file = trim(agera5_struc(n)%agera5hgt_file), exist=file_exists)
   if(.not. file_exists) then
      write(LDT_logunit,*) "[ERR] The AgERA5 terrain height file ",&
            trim(agera5_struc(n)%agera5hgt_file)," is not found."
      write(LDT_logunit,*) "Program stopping ..."
      call LDT_endrun
   endif

! -------------------------------------------------------------------
! Open and Read-in Forcing Terrain Hght File - Bring to LIS run domain
! -------------------------------------------------------------------
   write(LDT_logunit,*) "[INFO] Reading the AgERA5 terrain height file: ", &
        trim(agera5_struc(n)%agera5hgt_file)

#if (defined USE_NETCDF4) 

   ! Open the AgERA5 height file to read in the elevation field:
   call LDT_verify(nf90_open(path=trim(agera5_struc(n)%agera5hgt_file), &
            mode=NF90_NOWRITE, ncid=ftn_const), &
           'nf90_open failed in read_agera5_elev')

   call LDT_verify(nf90_inq_varid(ftn_const,'elev',elevId), &
           'nf90_inq_varid failed for elev in read_agera5_elev')

   ! Reading in elevation field (already in meters):
   call LDT_verify(nf90_get_var(ftn_const, elevId, read_elev), &
           'nf90_get_var failed for elev in read_agera5_elev')

   ! Initialize arrays for grid transformation:
   inpts  = agera5_struc(n)%nc * agera5_struc(n)%nr
   outpts = LDT_rc%lnr(n)*LDT_rc%lnc(n)
   lb     = .true.
   lb_regrid = .true.
   elev1d = -9999.0

   ! Convert 2D to 1D array for the interplation call:
   do r = 1, agera5_struc(n)%nr
      do c = 1, agera5_struc(n)%nc
         k= c+(r-1)*agera5_struc(n)%nc
         elev1d(k) = read_elev(c,r,1)
         if ( elev1d(k) == -9999.0 ) then
           elev1d(k)  = LDT_rc%udef
           lb(k) = .false.
         endif
      enddo
   enddo

   ! Interp elevation field to output field:
   call LDT_transform_paramgrid(n, LDT_rc%met_gridtransform_parms(findex), &
            agera5_struc(n)%gridDesc(:), inpts, 1, elev1d, lb, &
            outpts, elev_regrid, lb_regrid )

   ! Convert 1D to 2D elevation (already in meters):
   i = 0
   do r = 1, LDT_rc%lnr(n)
      do c = 1, LDT_rc%lnc(n)
         i = i + 1
         agera5elev(c,r,1) = elev_regrid(i)
      end do
   end do

   call LDT_verify(nf90_close(ftn_const), &
           'failed to close terrain height file in read_agera5_elev')

#endif
end subroutine read_agera5_elev
