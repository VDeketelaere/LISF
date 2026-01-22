!-----------------------BEGIN NOTICE -- DO NOT EDIT-----------------------
! NASA Goddard Space Flight Center
! Land Information System Framework (LISF)
! Version 7.5
!
! Copyright (c) 2024 United States Government as represented by the
! Administrator of the National Aeronautics and Space Administration.
! All Rights Reserved.
!-------------------------END NOTICE -- DO NOT EDIT-----------------------
#include "LIS_misc.h"
!BOP
!
! !ROUTINE: read_agera5
! \label{read_agera5}
!
! !REVISION HISTORY:
! Dec 2025: Initial implementation
!
! !INTERFACE:
subroutine read_agera5(n, kk, findex, order, filename, ferror)
! !USES:
  use LIS_coreMod, only : LIS_rc, LIS_domain, LIS_masterproc
  use LIS_logMod
  use LIS_metforcingMod
  use agera5_forcingMod, only : agera5_struc

#if (defined USE_NETCDF3 || defined USE_NETCDF4)
  use netcdf
#endif

  implicit none
! !ARGUMENTS:
  integer, intent(in)          :: n
  integer, intent(in)          :: kk
  integer, intent(in)          :: findex
  integer, intent(in)          :: order
  character(len=*), intent(in) :: filename
  integer, intent(out)         :: ferror
!
! !DESCRIPTION:
!  Reads and interpolates AgERA5 forcing.
!
!  AgERA5 FORCING VARIABLES (daily values):
!  1. Temperature_Air_2m_Min_24h (Tmin) [K]
!  2. Temperature_Air_2m_Max_24h (Tmax) [K]
!  3. Precipitation_Flux (Rainf) [mm/day]
!  4. Evapotranspiration_Reference_Crop (ETo) [mm/day]
!
!EOP

  integer :: ftn, month
  logical :: file_exists
  integer :: tminId, tmaxId, rainfId, etoId
  integer :: nc_index, nr_index
  integer :: c, r, t, iret

  real    :: tmin(agera5_struc(n)%ncold, agera5_struc(n)%nrold)
  real    :: tmax(agera5_struc(n)%ncold, agera5_struc(n)%nrold)
  real    :: rainf(agera5_struc(n)%ncold, agera5_struc(n)%nrold)
  real    :: eto(agera5_struc(n)%ncold, agera5_struc(n)%nrold)

#if (defined USE_NETCDF3)
  write(LIS_logunit,*) "[ERR] AgERA5 reader requires NetCDF4"
  call LIS_endrun()
#endif

#if (defined USE_NETCDF4)

  ferror = 0
  nr_index = agera5_struc(n)%nrold
  nc_index = agera5_struc(n)%ncold
  month = LIS_rc%mo

  inquire(file=filename, exist=file_exists)
  if (.not. file_exists) then
     write(LIS_logunit,*) "[ERR] AgERA5 file not found: ", trim(filename)
     ferror = 1
     return
  endif

  ! Open NetCDF file
  iret = nf90_open(path=trim(filename), mode=NF90_NOWRITE, ncid=ftn)
  if (iret .ne. 0) then
     write(LIS_logunit,*) "[ERR] Failed to open AgERA5 file: ", trim(filename)
     ferror = 1
     return
  endif

  ! Get variable IDs
  iret = nf90_inq_varid(ftn, 'Temperature_Air_2m_Min_24h', tminId)
  call LIS_verify(iret, 'nf90_inq_varid failed for Temperature_Air_2m_Min_24h in read_agera5')

  iret = nf90_inq_varid(ftn, 'Temperature_Air_2m_Max_24h', tmaxId)
  call LIS_verify(iret, 'nf90_inq_varid failed for Temperature_Air_2m_Max_24h in read_agera5')

  iret = nf90_inq_varid(ftn, 'Precipitation_Flux', rainfId)
  call LIS_verify(iret, 'nf90_inq_varid failed for Precipitation_Flux in read_agera5')

  iret = nf90_inq_varid(ftn, 'ReferenceET_PenmanMonteith_FAO56', etoId)
  call LIS_verify(iret, 'nf90_inq_varid failed for Evapotranspiration_Reference_Crop in read_agera5')

  ! Read data from NetCDF
  iret = nf90_get_var(ftn, tminId, tmin)
  call LIS_verify(iret, 'nf90_get_var failed for tmin in read_agera5')

  iret = nf90_get_var(ftn, tmaxId, tmax)
  call LIS_verify(iret, 'nf90_get_var failed for tmax in read_agera5')

  iret = nf90_get_var(ftn, rainfId, rainf)
  call LIS_verify(iret, 'nf90_get_var failed for rainf in read_agera5')

  iret = nf90_get_var(ftn, etoId, eto)
  call LIS_verify(iret, 'nf90_get_var failed for eto in read_agera5')

  ! Close NetCDF file
  iret = nf90_close(ftn)
  call LIS_verify(iret, 'nf90_close failed in read_agera5')

  ! Interpolate each variable to the LIS grid
  ! Store in order-specific array (1=ageraforc1, 2=ageraforc2)
  if (order == 1) then
     ! Variable 1: Tmin
     call interp_agera5_var(n, kk, findex, 3, tmin, .false., &
          agera5_struc(n)%ageraforc1(kk,:,:,:))

     ! Variable 2: Tmax
     call interp_agera5_var(n, kk, findex, 4, tmax, .false., &
          agera5_struc(n)%ageraforc1(kk,:,:,:))

     ! Variable 3: Rainf (convert from mm/day to mm/s)
     call interp_agera5_var(n, kk, findex, 1, rainf, .true., &
          agera5_struc(n)%ageraforc1(kk,:,:,:))

     ! Variable 4: ETo (convert from mm/day to mm/s)
     call interp_agera5_var(n, kk, findex, 2, eto, .true., &
          agera5_struc(n)%ageraforc1(kk,:,:,:))
  else
     ! Variable 1: Tmin
     call interp_agera5_var(n, kk, findex, 3, tmin, .false., &
          agera5_struc(n)%ageraforc2(kk,:,:,:))

     ! Variable 2: Tmax
     call interp_agera5_var(n, kk, findex, 4, tmax, .false., &
          agera5_struc(n)%ageraforc2(kk,:,:,:))

     ! Variable 3: Rainf (convert from mm/day to mm/s)
     call interp_agera5_var(n, kk, findex, 1, rainf, .true., &
          agera5_struc(n)%ageraforc2(kk,:,:,:))

     ! Variable 4: ETo (convert from mm/day to mm/s)
     call interp_agera5_var(n, kk, findex, 2, eto, .true., &
          agera5_struc(n)%ageraforc2(kk,:,:,:))
  endif

#endif

end subroutine read_agera5

!BOP
! !ROUTINE: interp_agera5_var
! \label{interp_agera5_var}
!
! !INTERFACE:
subroutine interp_agera5_var(n, kk, findex, var_index, varfield, convert_rate, ageraforc)
! !USES:
  use LIS_coreMod
  use LIS_logMod
  use agera5_forcingMod, only: agera5_struc

  implicit none
! !ARGUMENTS:
  integer, intent(in)    :: n
  integer, intent(in)    :: kk
  integer, intent(in)    :: findex
  integer, intent(in)    :: var_index
  real, intent(in)       :: varfield(agera5_struc(n)%ncold, agera5_struc(n)%nrold)
  logical, intent(in)    :: convert_rate
  ! 3D array: (nvars, 1, ngrid)
  real, intent(inout)    :: ageraforc(LIS_rc%met_nf(findex), 1, LIS_rc%lnc(n)*LIS_rc%lnr(n))
!
! !DESCRIPTION:
!  This subroutine spatially interpolates a AgERA5 field to the
!  LIS grid.
!
!EOP

  integer :: c, r, t, iret
  logical*1 :: lb(agera5_struc(n)%ncold * agera5_struc(n)%nrold)
  logical*1 :: lo(LIS_rc%lnc(n)*LIS_rc%lnr(n))                      ! FIXED: Use full grid size
  real    :: f(agera5_struc(n)%ncold * agera5_struc(n)%nrold)
  real    :: varfield_out(LIS_rc%lnc(n)*LIS_rc%lnr(n))            ! FIXED: Use full grid size
  real    :: conversion_factor

  ! Determine conversion factor
  if (convert_rate) then
     conversion_factor = 1.0 / 86400.0  ! mm/day to mm/s
  else
     conversion_factor = 1.0
  endif

  ! Flatten 2D field into 1D array
  lb = .false.
  f = LIS_rc%udef

  do r = 1, agera5_struc(n)%nrold
     do c = 1, agera5_struc(n)%ncold
        ! Flip row index: r=1 (north) becomes nrold (south in LIS grid)
        t = c + (agera5_struc(n)%nrold - r) * agera5_struc(n)%ncold
        f(t) = varfield(c, r)
        if (f(t) .ne. LIS_rc%udef .and. f(t) .gt. -9000.0) then
           lb(t) = .true.
        endif
     enddo
  enddo

  write(LIS_logunit,*) "[DEBUG] interp_agera5_var: var_index=", var_index
  write(LIS_logunit,*) "[DEBUG] interp_agera5_var: count of valid input (lb=true)=", count(lb)
  write(LIS_logunit,*) "[DEBUG] interp_agera5_var: f(1)=", f(1)
  write(LIS_logunit,*) "[DEBUG] interp_agera5_var: minval(f, lb)=", minval(f, lb)
  write(LIS_logunit,*) "[DEBUG] interp_agera5_var: maxval(f, lb)=", maxval(f, lb)

  select case (trim(LIS_rc%met_interp(findex)))

  case ("bilinear")
     write(LIS_logunit,*) "[DEBUG] interp: n111(1)=", agera5_struc(n)%n111(1)
     write(LIS_logunit,*) "[DEBUG] interp: n121(1)=", agera5_struc(n)%n121(1)
     write(LIS_logunit,*) "[DEBUG] interp: n211(1)=", agera5_struc(n)%n211(1)
     write(LIS_logunit,*) "[DEBUG] interp: n221(1)=", agera5_struc(n)%n221(1)
     write(LIS_logunit,*) "[DEBUG] interp: lb at n111(1)=", lb(agera5_struc(n)%n111(1))
     write(LIS_logunit,*) "[DEBUG] interp: f at n111(1)=", f(agera5_struc(n)%n111(1))
     write(LIS_logunit,*) "[DEBUG] interp: lb at n121(1)=", lb(agera5_struc(n)%n121(1))
     write(LIS_logunit,*) "[DEBUG] interp: f at n121(1)=", f(agera5_struc(n)%n121(1))
     write(LIS_logunit,*) "[DEBUG] interp: lb at n211(1)=", lb(agera5_struc(n)%n211(1))
     write(LIS_logunit,*) "[DEBUG] interp: f at n211(1)=", f(agera5_struc(n)%n211(1))
     write(LIS_logunit,*) "[DEBUG] interp: lb at n221(1)=", lb(agera5_struc(n)%n221(1))
     write(LIS_logunit,*) "[DEBUG] interp: f at n221(1)=", f(agera5_struc(n)%n221(1))
     write(LIS_logunit,*) "[DEBUG] interp: size(LIS_domain%lat)=", size(LIS_domain(n)%lat)
     write(LIS_logunit,*) "[DEBUG] interp: size(lo)=", size(lo)
     write(LIS_logunit,*) "[DEBUG] interp: size(varfield_out)=", size(varfield_out)
     
     call bilinear_interp(LIS_rc%gridDesc(n,:), lb, f, lo, varfield_out, &
          agera5_struc(n)%mi, LIS_rc%lnc(n)*LIS_rc%lnr(n), &    ! FIXED: Use full grid size
          LIS_domain(n)%lat, LIS_domain(n)%lon, &
          agera5_struc(n)%w111, agera5_struc(n)%w121, &
          agera5_struc(n)%w211, agera5_struc(n)%w221, &
          agera5_struc(n)%n111, agera5_struc(n)%n121, &
          agera5_struc(n)%n211, agera5_struc(n)%n221, &
          LIS_rc%udef, iret)

  case ("budget-bilinear")
     call conserv_interp(LIS_rc%gridDesc(n,:), lb, f, lo, varfield_out, &
          agera5_struc(n)%mi, LIS_rc%lnc(n)*LIS_rc%lnr(n), &    ! FIXED: Use full grid size
          LIS_domain(n)%lat, LIS_domain(n)%lon, &
          agera5_struc(n)%w112, agera5_struc(n)%w122, &
          agera5_struc(n)%w212, agera5_struc(n)%w222, &
          agera5_struc(n)%n112, agera5_struc(n)%n122, &
          agera5_struc(n)%n212, agera5_struc(n)%n222, &
          LIS_rc%udef, iret)

  case ("neighbor")
     call neighbor_interp(LIS_rc%gridDesc(n,:), lb, f, lo, varfield_out, &
          agera5_struc(n)%mi, LIS_rc%lnc(n)*LIS_rc%lnr(n), &    ! FIXED: Use full grid size
          LIS_domain(n)%lat, LIS_domain(n)%lon, &
          agera5_struc(n)%n113, LIS_rc%udef, iret)

  case default
     write(LIS_logunit,*) "[ERR] Invalid interpolation option: ", &
          trim(LIS_rc%met_interp(findex))
     call LIS_endrun()

  end select

  write(LIS_logunit,*) "[DEBUG] interp_agera5_var: count of valid output (lo=true)=", count(lo)
  write(LIS_logunit,*) "[DEBUG] interp_agera5_var: varfield_out(1)=", varfield_out(1)
  if (count(lo) > 0) then
     write(LIS_logunit,*) "[DEBUG] interp_agera5_var: minval(varfield_out, lo)=", minval(varfield_out, lo)
     write(LIS_logunit,*) "[DEBUG] interp_agera5_var: maxval(varfield_out, lo)=", maxval(varfield_out, lo)
  endif

  ! Store in 3D array with correct indexing
  ! ageraforc dimensions: (nvars, 1, lnc*lnr)
  do t = 1, LIS_rc%lnc(n)*LIS_rc%lnr(n)
    ageraforc(var_index, 1, t) = varfield_out(t) * conversion_factor
  enddo

end subroutine interp_agera5_var
