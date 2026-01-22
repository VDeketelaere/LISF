!-----------------------BEGIN NOTICE -- DO NOT EDIT-----------------------
! NASA Goddard Space Flight Center
! Land Information System Framework (LISF)
! Version 7.5
!
! Copyright (c) 2024 United States Government as represented by the
! Administrator of the National Aeronautics and Space Administration.
! All Rights Reserved.
!-------------------------END NOTICE -- DO NOT EDIT-----------------------
!BOP
!
! !ROUTINE: get_agera5
! \label{get_agera5}
!
! !REVISION HISTORY:
! Dec 2025: Initial implementation
!
! !INTERFACE:
subroutine get_agera5(n, findex)
  use LDT_coreMod
  use LDT_metforcingMod
  use LDT_timeMgrMod
  use LDT_logMod
  use agera5_forcingMod

  implicit none

  integer, intent(in) :: n
  integer, intent(in) :: findex
  
  integer :: c, r, ferror
  integer :: order
  integer :: yr_use, mo_use, da_use, hr_use
  real*8  :: time1, time2, timenow
  real    :: gmt1, gmt2
  character(len=255) :: filename

  if ( LDT_rc%tscount(n).eq.1 .or. LDT_rc%rstflag(n).eq.1 .or. &
       agera5_struc(n)%reset_flag ) then
     agera5_struc(n)%findtime1 = 0
     agera5_struc(n)%findtime2 = 0
     agera5_struc(n)%reset_flag = .false.
  endif

  ! Determine required observed data times
  yr_use = LDT_rc%yr
  mo_use = LDT_rc%mo
  da_use = LDT_rc%da
  hr_use = 0  ! AgERA5 is daily data starting at 00:00

  call LDT_tick(timenow, LDT_rc%doy, LDT_rc%gmt, &
       LDT_rc%yr, LDT_rc%mo, LDT_rc%da, &
       LDT_rc%hr, LDT_rc%mn, LDT_rc%ss, 0.0)

  if ( timenow >= agera5_struc(n)%agera5time2 ) then
     agera5_struc(n)%findtime2 = 1
     agera5_struc(n)%findtime1 = 0

     ! Get bookend 1: Current day
     call LDT_tick(time1, LDT_rc%doy, gmt1, &
          yr_use, mo_use, da_use, hr_use, 0, 0, 0.0)

     ! Get bookend 2: Next day
     call LDT_tick(time2, LDT_rc%doy, gmt2, &
          yr_use, mo_use, da_use, hr_use, 0, 0, 86400.0)

     write(LDT_logunit,*) '[INFO] Getting AgERA5 data'
     write(LDT_logunit,*) '[INFO] AgERA5 bookend 1: ', yr_use, mo_use, da_use

     ! Read bookend 1 (current day)
     call agera5file(n, findex, agera5_struc(n)%agera5dir, &
          yr_use, mo_use, da_use, filename)
     write(LDT_logunit,*) '[INFO] Reading AgERA5 file: ', trim(filename)

     order = 1
     call read_agera5(n, findex, order, filename, &
          agera5_struc(n)%ageraforc1, ferror)

     if ( ferror == 1 ) then
        write(LDT_logunit,*) '[ERR] Error reading AgERA5 file1: ', trim(filename)
        call LDT_endrun()
     endif

     ! Update to next day for bookend 2
     call LDT_tick(time2, LDT_rc%doy, gmt2, &
          yr_use, mo_use, da_use, hr_use, 0, 0, 86400.0)

     write(LDT_logunit,*) '[INFO] AgERA5 bookend 2: ', yr_use, mo_use, da_use

     call agera5file(n, findex, agera5_struc(n)%agera5dir, &
          yr_use, mo_use, da_use, filename)
     write(LDT_logunit,*) '[INFO] Reading AgERA5 file: ', trim(filename)

     order = 2
     call read_agera5(n, findex, order, filename, &
          agera5_struc(n)%ageraforc2, ferror)

     if ( ferror == 1 ) then
        write(LDT_logunit,*) '[ERR] Error reading AgERA5 file2: ', trim(filename)
        call LDT_endrun()
     endif

     agera5_struc(n)%agera5time1 = time1
     agera5_struc(n)%agera5time2 = time2
  endif  ! End of data read conditional

  ! *** KEY FIX: Assign AgERA5 forcing fields to LDT metdata placeholders ***
  ! Note: ageraforc1/2 are now 3D: (nvars, 1, ngrid)
  do r=1,LDT_rc%lnr(n)
     do c=1,LDT_rc%lnc(n)
        if (LDT_domain(n)%gindex(c,r).ne.-1) then
           ! *** FIXED: Access 3D array with all 3 indices ***
           ! Dimension 2 (time) = 1 for daily data
           LDT_forc(n,findex)%metdata1(:,LDT_domain(n)%gindex(c,r)) = &
                agera5_struc(n)%ageraforc1(:, 1, c+(r-1)*LDT_rc%lnc(n))
                
           LDT_forc(n,findex)%metdata2(:,LDT_domain(n)%gindex(c,r)) = &
                agera5_struc(n)%ageraforc2(:, 1, c+(r-1)*LDT_rc%lnc(n))
        endif
     enddo
  enddo

end subroutine get_agera5

!BOP
! !ROUTINE: agera5file
! \label{agera5file}
!
! !INTERFACE:
subroutine agera5file(n, findex, agera5dir, yr, mo, da, filename)

! !USES:
  use LDT_coreMod
  use LDT_logMod

  implicit none
! !ARGUMENTS:
  integer                       :: n 
  integer                       :: findex
  character(len=*), intent(in)  :: agera5dir
  integer, intent(in)           :: yr,mo,da
  character(len=*), intent(out) :: filename

! !DESCRIPTION:
!   This subroutine puts together AgERA5 file names for
!   daily netcdf files. The file structure is:
!   <agera5dir>/YYYY/AgERA5_YYYYMMDD.nc
!
!  The arguments are:
!  \begin{description}
!  \item[agera5dir]
!    Name of the AgERA5 directory
!  \item[yr]
!    year
!  \item[mo]
!   month
!  \item[da]
!   day of month
!  \item[filename]
!   name of the time-stamped AgERA5 file
!  \end{description}
!
!EOP

  character*4  :: fyr
  character*2  :: fmo
  character*2  :: fda

  write(unit=fyr, fmt='(i4.4)') yr
  write(unit=fmo, fmt='(i2.2)') mo
  write(unit=fda, fmt='(i2.2)') da

  filename = trim(agera5dir)//'/'//fyr//'/AgERA5_'//fyr//fmo//fda//'.nc'

end subroutine agera5file
