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
! !ROUTINE: read_agera5
! \label{read_agera5}
!
! !REVISION HISTORY:
! Dec 2025: Initial implementation for LDT
!
! !INTERFACE:
subroutine read_agera5(n, findex, order, filename, ferror)
! !USES:
  use LDT_coreMod,       only : LDT_rc, LDT_domain
  use LDT_logMod,        only : LDT_logunit, LDT_verify, LDT_endrun
  use LDT_metforcingMod, only : LDT_forc
  use agera5_forcingMod, only : agera5_struc

#if (defined USE_NETCDF3 || defined USE_NETCDF4)
  use netcdf
#endif

  implicit none
! !ARGUMENTS:
  integer, intent(in)          :: n
  integer, intent(in)          :: findex
  integer, intent(in)          :: order
  character(len=*), intent(in) :: filename
  integer, intent(out)         :: ferror
!
! !DESCRIPTION:
!  Reads and interpolates AgERA5 forcing data for LDT.
!
!  AgERA5 FORCING VARIABLES (daily values):
!  1. Precipitation_Flux (Rainf) [mm/day]
!  2. ReferenceET_PenmanMonteith_FAO56 (ETo) [mm/day]
!  3. Temperature_Air_2m_Min_24h (Tmin) [K]
!  4. Temperature_Air_2m_Max_24h (Tmax) [K]
!
!EOP

  integer :: ftn
  logical :: file_exists
  integer :: tminId, tmaxId, rainfId, etoId
  integer :: iret

  real    :: tmin(agera5_struc(n)%nc, agera5_struc(n)%nr)
  real    :: tmax(agera5_struc(n)%nc, agera5_struc(n)%nr)
  real    :: rainf(agera5_struc(n)%nc, agera5_struc(n)%nr)
  real    :: eto(agera5_struc(n)%nc, agera5_struc(n)%nr)

#if (defined USE_NETCDF3)
  write(LDT_logunit,*) "[ERR] AgERA5 reader requires NetCDF4"
  call LDT_endrun()
#endif

#if (defined USE_NETCDF4)

  ferror = 0

  inquire(file=filename, exist=file_exists)
  if (.not. file_exists) then
     write(LDT_logunit,*) "[ERR] AgERA5 file not found: ", trim(filename)
     ferror = 1
     return
  endif

  write(LDT_logunit,*) "[INFO] Reading AgERA5 file: ", trim(filename)

  ! Open NetCDF file
  iret = nf90_open(path=trim(filename), mode=NF90_NOWRITE, ncid=ftn)
  if (iret .ne. 0) then
     write(LDT_logunit,*) "[ERR] Failed to open AgERA5 file: ", trim(filename)
     ferror = 1
     return
  endif

  ! Get variable IDs and read data
  iret = nf90_inq_varid(ftn, 'Temperature_Air_2m_Min_24h', tminId)
  call LDT_verify(iret, 'nf90_inq_varid failed for Temperature_Air_2m_Min_24h')

  iret = nf90_inq_varid(ftn, 'Temperature_Air_2m_Max_24h', tmaxId)
  call LDT_verify(iret, 'nf90_inq_varid failed for Temperature_Air_2m_Max_24h')

  iret = nf90_inq_varid(ftn, 'Precipitation_Flux', rainfId)
  call LDT_verify(iret, 'nf90_inq_varid failed for Precipitation_Flux')

  iret = nf90_inq_varid(ftn, 'ReferenceET_PenmanMonteith_FAO56', etoId)
  call LDT_verify(iret, 'nf90_inq_varid failed for ReferenceET_PenmanMonteith_FAO56')

  ! Read data
  iret = nf90_get_var(ftn, tminId, tmin)
  call LDT_verify(iret, 'nf90_get_var failed for tmin')

  iret = nf90_get_var(ftn, tmaxId, tmax)
  call LDT_verify(iret, 'nf90_get_var failed for tmax')

  iret = nf90_get_var(ftn, rainfId, rainf)
  call LDT_verify(iret, 'nf90_get_var failed for rainf')

  iret = nf90_get_var(ftn, etoId, eto)
  call LDT_verify(iret, 'nf90_get_var failed for eto')

  ! Close NetCDF file
  iret = nf90_close(ftn)
  call LDT_verify(iret, 'nf90_close failed')

  ! Interpolate each variable to the LDT grid
  ! Store in order-specific array (1=ageraforc1, 2=ageraforc2)
  if (order == 1) then
     call interp_agera5_var(n, findex, 1, rainf, .true., agera5_struc(n)%ageraforc1)
     call interp_agera5_var(n, findex, 2, eto, .true., agera5_struc(n)%ageraforc1)
     call interp_agera5_var(n, findex, 3, tmin, .false., agera5_struc(n)%ageraforc1)
     call interp_agera5_var(n, findex, 4, tmax, .false., agera5_struc(n)%ageraforc1)
  else
     call interp_agera5_var(n, findex, 1, rainf, .true., agera5_struc(n)%ageraforc2)
     call interp_agera5_var(n, findex, 2, eto, .true., agera5_struc(n)%ageraforc2)
     call interp_agera5_var(n, findex, 3, tmin, .false., agera5_struc(n)%ageraforc2)
     call interp_agera5_var(n, findex, 4, tmax, .false., agera5_struc(n)%ageraforc2)
  endif

#endif

end subroutine read_agera5

!BOP
! !ROUTINE: interp_agera5_var
! \label{interp_agera5_var}
!
! !INTERFACE:
subroutine interp_agera5_var(n, findex, var_index, varfield, convert_rate, ageraforc)
! !USES:
  use LDT_coreMod,       only : LDT_rc, LDT_domain
  use LDT_logMod,        only : LDT_logunit, LDT_endrun
  use agera5_forcingMod, only : agera5_struc

  implicit none
! !ARGUMENTS:
  integer, intent(in)    :: n
  integer, intent(in)    :: findex
  integer, intent(in)    :: var_index
  real, intent(in)       :: varfield(agera5_struc(n)%nc, agera5_struc(n)%nr)
  logical, intent(in)    :: convert_rate
  real, intent(inout)    :: ageraforc(agera5_struc(n)%nvars, 1, LDT_rc%lnc(n)*LDT_rc%lnr(n))
!
! !DESCRIPTION:
!  Spatially interpolates an AgERA5 field to the LDT grid.
!
!EOP

  integer :: c, r, t, iret
  logical*1 :: lb(agera5_struc(n)%nc * agera5_struc(n)%nr)
  logical*1 :: lo(LDT_rc%lnc(n)*LDT_rc%lnr(n))
  real    :: f(agera5_struc(n)%nc * agera5_struc(n)%nr)
  real    :: varfield_out(LDT_rc%lnc(n)*LDT_rc%lnr(n))
  real    :: conversion_factor

  ! Determine conversion factor
  if (convert_rate) then
     conversion_factor = 1.0 / 86400.0  ! mm/day to mm/s
  else
     conversion_factor = 1.0
  endif

  ! Flatten 2D field into 1D array
  ! NOTE: AgERA5 NetCDF has lat from North to South (90 to -90)
  ! LDT expects South to North (-90 to 90), so we flip the row index
  lb = .false.
  f = LDT_rc%udef

  do r = 1, agera5_struc(n)%nr
     do c = 1, agera5_struc(n)%nc
        ! Flip row index: r=1 (north) becomes nr (south in LDT grid)
        t = c + (agera5_struc(n)%nr - r) * agera5_struc(n)%nc
        f(t) = varfield(c, r)
        if (f(t) .ne. LDT_rc%udef .and. f(t) .gt. -9000.0) then
           lb(t) = .true.
        endif
     enddo
  enddo

  ! Initialize output
  lo = .false.
  varfield_out = LDT_rc%udef

  ! Perform spatial interpolation
  select case (trim(LDT_rc%met_gridtransform(findex)))

  case ("bilinear")
     call bilinear_interp(LDT_rc%gridDesc(n,:), lb, f, lo, varfield_out, &
          agera5_struc(n)%mi, LDT_rc%lnc(n)*LDT_rc%lnr(n), &
          LDT_domain(n)%lat, LDT_domain(n)%lon, &
          agera5_struc(n)%w111, agera5_struc(n)%w121, &
          agera5_struc(n)%w211, agera5_struc(n)%w221, &
          agera5_struc(n)%n111, agera5_struc(n)%n121, &
          agera5_struc(n)%n211, agera5_struc(n)%n221, &
          LDT_rc%udef, iret)

  case ("budget-bilinear")
     call conserv_interp(LDT_rc%gridDesc(n,:), lb, f, lo, varfield_out, &
          agera5_struc(n)%mi, LDT_rc%lnc(n)*LDT_rc%lnr(n), &
          LDT_domain(n)%lat, LDT_domain(n)%lon, &
          agera5_struc(n)%w112, agera5_struc(n)%w122, &
          agera5_struc(n)%w212, agera5_struc(n)%w222, &
          agera5_struc(n)%n112, agera5_struc(n)%n122, &
          agera5_struc(n)%n212, agera5_struc(n)%n222, &
          LDT_rc%udef, iret)

  case ("neighbor")
     call neighbor_interp(LDT_rc%gridDesc(n,:), lb, f, lo, varfield_out, &
          agera5_struc(n)%mi, LDT_rc%lnc(n)*LDT_rc%lnr(n), &
          LDT_domain(n)%lat, LDT_domain(n)%lon, &
          agera5_struc(n)%n113, LDT_rc%udef, iret)

  case default
     write(LDT_logunit,*) "[ERR] Invalid interpolation option: ", &
          trim(LDT_rc%met_gridtransform(findex))
     call LDT_endrun()

  end select

  ! Store in 3D array with correct indexing
  ! ageraforc dimensions: (nvars, 1, lnc*lnr)
  do t = 1, LDT_rc%lnc(n)*LDT_rc%lnr(n)
     ageraforc(var_index, 1, t) = varfield_out(t) * conversion_factor
  enddo

end subroutine interp_agera5_var
