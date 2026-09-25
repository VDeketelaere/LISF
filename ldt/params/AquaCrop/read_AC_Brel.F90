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
!
! !ROUTINE: read_AC_Brel
! \label{read_AC_Brel}
!
! !REVISION HISTORY:
!  12 Aug 2026; Vince Deketelaere; Initial implementation
!
! !INTERFACE:
subroutine read_AC_Brel(n, array)

  ! !USES:
  use AquaCrop_parmsMod
  use ESMF
  use LDT_coreMod,       only : LDT_rc, LDT_domain
  use LDT_logMod,        only : LDT_logunit, LDT_verify, LDT_endrun
  use LDT_paramDataMod,  only : LDT_LSMparam_struc
  use, intrinsic :: ieee_arithmetic, only : ieee_is_nan
#if(defined USE_NETCDF3 || defined USE_NETCDF4)
  use netcdf
#endif

  implicit none

  ! !ARGUMENTS:
  integer, intent(in)    :: n
  real,    intent(inout) :: array(LDT_rc%lnc(n),LDT_rc%lnr(n))

  ! !DESCRIPTION:
  !  This subroutine retrieves the spatially varying relative biomass
  !  (Brel = 1 - SF) from a netCDF file, on a regular lat/lon grid,
  !  and regrids it onto the LIS run domain. Grid cells outside the
  !  landmask are set to undefined.
  !
  !  The arguments are:
  !  \begin{description}
  !  \item[n]
  !   index of the nest
  !  \item[array]
  !   output field with the retrieved Brel values
  !  \end{description}
  !
  !EOP

  integer   :: ncid, latid, lonid, varid
  logical   :: file_exists
  integer   :: c, r, i, iret
  integer   :: ncols, nrows
  real      :: xllcorner, yllcorner
  real      :: cellxsize, cellysize
  real,     allocatable :: read_lat(:), read_lon(:)
  real,     allocatable :: read_input(:,:)
  integer   :: mi                        ! Total number of input param grid array points
  integer   :: mo                        ! Total number of output LIS grid array points
  real      :: param_gridDesc(20)
  real,     allocatable :: gi1(:)        ! input parameter 1d grid
  logical*1,allocatable :: li1(:)        ! input logical mask (to match gi)
  real      :: go1(LDT_rc%lnc(n)*LDT_rc%lnr(n))  ! output lis 1d grid
  logical*1 :: lo1(LDT_rc%lnc(n)*LDT_rc%lnr(n))  ! output logical mask (to match go)
  real      :: undef_value

  !- Grid transform arrays:
  integer, allocatable     :: n11(:)     ! Map array for aggregating methods
  integer, allocatable     :: n113(:)    ! Map array for nearest neighbor interp
  integer, allocatable     :: n111(:)    ! Map array for bilinear interp
  integer, allocatable     :: n121(:)
  integer, allocatable     :: n211(:)
  integer, allocatable     :: n221(:)
  real, allocatable        :: w111(:),w121(:)
  real, allocatable        :: w211(:),w221(:)

  external :: upscaleByAveraging_input
  external :: upscaleByAveraging
  external :: neighbor_interp_input
  external :: neighbor_interp
  external :: bilinear_interp_input
  external :: bilinear_interp

  ! ______________________________________________________________________

  array = LDT_rc%udef
  undef_value = -9999.0

  inquire(file=trim(AquaCrop_struc(n)%brelfile), exist=file_exists)
  if(.not. file_exists) then
     write(LDT_logunit,*) "[ERR] AquaCrop Brel map ",&
          trim(AquaCrop_struc(n)%brelfile)," not found."
     write(LDT_logunit,*) "Program stopping ..."
     call LDT_endrun
  endif

  select case ( trim(AquaCrop_struc(n)%brel_gridtransform) )
  case( "none", "neighbor", "average", "bilinear" )
  case default
     write(LDT_logunit,*) "[ERR] The spatial transform option selected for the AquaCrop"
     write(LDT_logunit,*) "     Brel file is not recognized nor recommended."
     write(LDT_logunit,*) "     Please select: "
     write(LDT_logunit,*) "  ==  none, neighbor, average, bilinear "
     write(LDT_logunit,*) "Program stopping ..."
     call LDT_endrun
  end select

#if(defined USE_NETCDF3 || defined USE_NETCDF4)

  write(LDT_logunit,*) "[INFO] Reading AquaCrop Brel file: ",&
       trim(AquaCrop_struc(n)%brelfile)

  call LDT_verify(nf90_open(path=trim(AquaCrop_struc(n)%brelfile),&
       mode=nf90_nowrite,ncid=ncid),&
       'nf90_open failed for AquaCrop Brel file in read_AC_Brel')

  call LDT_verify(nf90_inq_dimid(ncid,"lon",lonid),&
       'nf90_inq_dimid for lon failed in read_AC_Brel')
  call LDT_verify(nf90_inquire_dimension(ncid,lonid,len=ncols),&
       'nf90_inquire_dimension for lon failed in read_AC_Brel')

  call LDT_verify(nf90_inq_dimid(ncid,"lat",latid),&
       'nf90_inq_dimid for lat failed in read_AC_Brel')
  call LDT_verify(nf90_inquire_dimension(ncid,latid,len=nrows),&
       'nf90_inquire_dimension for lat failed in read_AC_Brel')

  allocate( read_lat(nrows), read_lon(ncols) )
  allocate( read_input(ncols,nrows) )

  call LDT_verify(nf90_inq_varid(ncid,"lat",varid),&
       'nf90_inq_varid for lat failed in read_AC_Brel')
  call LDT_verify(nf90_get_var(ncid,varid,read_lat),&
       'nf90_get_var for lat failed in read_AC_Brel')

  call LDT_verify(nf90_inq_varid(ncid,"lon",varid),&
       'nf90_inq_varid for lon failed in read_AC_Brel')
  call LDT_verify(nf90_get_var(ncid,varid,read_lon),&
       'nf90_get_var for lon failed in read_AC_Brel')

  call LDT_verify(nf90_inq_varid(ncid,"brel_meanfilled",varid),&
       'nf90_inq_varid for Brel failed in read_AC_Brel')
  call LDT_verify(nf90_get_var(ncid,varid,read_input),&
       'nf90_get_var for Brel failed in read_AC_Brel')

  call LDT_verify(nf90_close(ncid))

  cellxsize = read_lon(2) - read_lon(1)
  xllcorner = read_lon(1) - 0.5*cellxsize

  !- Force the input grid to be south-to-north for the regridding routines
  !  below; flip the data array if the netCDF file is stored north-to-south.
  if( read_lat(1) > read_lat(nrows) ) then
     cellysize = read_lat(1) - read_lat(2)
     yllcorner = read_lat(nrows) - 0.5*cellysize
     read_input(:,:) = read_input(:,nrows:1:-1)
  else
     cellysize = read_lat(2) - read_lat(1)
     yllcorner = read_lat(1) - 0.5*cellysize
  endif

  deallocate( read_lat, read_lon )

#else
  write(LDT_logunit,*) "[ERR] AquaCrop Brel reader requires netCDF support."
  write(LDT_logunit,*) "Program stopping ..."
  call LDT_endrun
#endif

  ! -------------------------------------------------------------------
  !     AGGREGATING FINE-SCALE GRID TO COARSER LIS OUTPUT GRID
  ! -------------------------------------------------------------------

  param_gridDesc(:)  = 0.
  param_gridDesc(1)  = 0.    ! Latlon
  param_gridDesc(2)  = float(ncols)
  param_gridDesc(3)  = float(nrows)
  param_gridDesc(4)  = yllcorner
  param_gridDesc(5)  = xllcorner
  param_gridDesc(6)  = 128
  param_gridDesc(7)  = yllcorner + (nrows-1)*cellysize
  param_gridDesc(8)  = xllcorner + (ncols-1)*cellxsize
  param_gridDesc(9)  = cellxsize
  param_gridDesc(10) = cellysize
  param_gridDesc(20) = 64

  mi = ncols * nrows
  mo = LDT_rc%lnc(n)*LDT_rc%lnr(n)
  allocate( gi1(mi), li1(mi) )
  gi1 = LDT_rc%udef
  li1 = .false.
  lo1 = .false.

  !- Assign 2-D array to 1-D for aggregation routines.
  !  NaN fill values are converted to LDT_rc%udef first: the build traps
  !  floating-point exceptions, so comparing a NaN directly raises
  !  "floating invalid" rather than returning .false.
  i = 0
  do r = 1, nrows
     do c = 1, ncols;  i = i + 1
        if( ieee_is_nan(read_input(c,r)) ) then
           gi1(i) = LDT_rc%udef
        else
           gi1(i) = read_input(c,r)
           if( (gi1(i) .ne. LDT_rc%udef) .and. (gi1(i) .ne. undef_value) ) then
              li1(i) = .true.
           endif
        endif
     enddo
  enddo
  deallocate( read_input )

  !- Select grid spatial transform option:
  select case ( trim(AquaCrop_struc(n)%brel_gridtransform) )

     !- Aggregate by calculating average of each output gridcell:
  case ( "average" )
     allocate( n11(mi) )
     write(LDT_logunit,*)"[INFO] Regridding: Applying average to Brel parameter"
     call upscaleByAveraging_input( param_gridDesc, LDT_rc%gridDesc(n,:), mi, mo, n11 )
     call upscaleByAveraging( mi, mo, LDT_rc%udef, n11,li1, gi1, lo1(:), go1(:) )
     deallocate( n11 )

     !- Select neighboring point:
  case ( "neighbor" )
     allocate( n113(mo) )
     write(LDT_logunit,*)"[INFO] Regridding: Applying nearest neighbor to Brel parameter"
     call neighbor_interp_input( n, param_gridDesc, n113 )
     call neighbor_interp( LDT_rc%gridDesc(n,:), li1, gi1, lo1(:), go1(:), mi, mo, &
          LDT_domain(n)%lat, LDT_domain(n)%lon, n113, LDT_rc%udef, iret )
     deallocate( n113 )

     !- Bilinear interpolation:
  case ( "bilinear" )
     allocate( n111(mo), n121(mo), n211(mo), n221(mo) )
     allocate( w111(mo), w121(mo), w211(mo), w221(mo) )
     write(LDT_logunit,*)"[INFO] Regridding: Applying bilinear interp to Brel parameter"
     call bilinear_interp_input( n, param_gridDesc, &
          n111, n121, n211, n221, w111, w121, w211, w221)
     call bilinear_interp(LDT_rc%gridDesc(n,:), li1, gi1, lo1(:), go1(:), &
          mi, mo, LDT_domain(n)%lat, LDT_domain(n)%lon, &
          w111, w121, w211, w221, n111, n121, n211, n221,&
          LDT_rc%udef, iret)
     deallocate( n111, n121, n211, n221, w111, w121, w211, w221 )

     !- When no transform is performed (must be same grid as LDT grid!):
  case ( "none" )
     write(LDT_logunit,*) "[INFO] No aggregation applied for Brel parameter file ... "
     go1(:) = gi1(:)

  case default
     write(LDT_logunit,*) &
          "[ERR] This spatial transformation option ("&
          //trim(AquaCrop_struc(n)%brel_gridtransform)//") "
     write(LDT_logunit,*) "  is not currently supported."
     write(LDT_logunit,*) "Program stopping ...."
     call LDT_endrun
  end select

  !- Convert 1D to 2D grid output arrays, masking out non-land points:
  i = 0
  do r = 1, LDT_rc%lnr(n)
     do c = 1, LDT_rc%lnc(n)
        i = i + 1
        if( (go1(i) < 0.)&
             .or.(LDT_LSMparam_struc(n)%landmask%value(c,r,1).eq.0) ) then
           array(c,r) = LDT_rc%udef
        else
           array(c,r) = go1(i)
        endif
     enddo
  enddo
  deallocate( gi1, li1 )

  write(LDT_logunit,*) "[INFO] Done reading AquaCrop Brel file."

end subroutine read_AC_Brel