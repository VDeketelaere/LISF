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
! !USES:
  use LIS_coreMod
  use LIS_timeMgrMod
  use LIS_logMod
  use agera5_forcingMod

  implicit none

! !ARGUMENTS:
  integer, intent(in) :: n
  integer, intent(in) :: findex
  
! *** ADD THESE LOCAL VARIABLE DECLARATIONS ***
  integer :: c, r, ferror, kk
  integer :: yr1, mo1, da1, hr1, mn1, ss1, doy1
  integer :: yr2, mo2, da2, hr2, mn2, ss2, doy2
  real*8  :: time1, time2, timenow
  real    :: gmt1, gmt2
  real    :: ts1, ts2
  character(len=255) :: filename
  integer :: order

! ... rest of your code ...

  ! Initialize
  gmt1 = 0.0
  gmt2 = 0.0
  ts1 = 0.0
  ts2 = 86400.0  ! Next day

  ! First call initialization
  if (agera5_struc(n)%agera5time1 == 0.0 .and. &
      agera5_struc(n)%agera5time2 == 0.0) then
     write(LIS_logunit,*) '[INFO] get_agera5: First call, initializing times'
     agera5_struc(n)%findtime1 = 1
     agera5_struc(n)%findtime2 = 1
  endif

  if ( LIS_rc%rstflag(n).eq.1 .or. agera5_struc(n)%reset_flag ) then
    agera5_struc(n)%findtime1 = 0
    agera5_struc(n)%findtime2 = 0
    agera5_struc(n)%reset_flag = .false.
  endif

  ! Current time
  yr1 = LIS_rc%yr
  mo1 = LIS_rc%mo
  da1 = LIS_rc%da
  hr1 = 0
  mn1 = 0
  ss1 = 0

  call LIS_tick(timenow, doy1, gmt1, yr1, mo1, da1, hr1, mn1, ss1, 0.0)

  ! Check if we need to read new data
  if ( timenow >= agera5_struc(n)%agera5time2 ) then
     agera5_struc(n)%findtime2 = 1
     agera5_struc(n)%findtime1 = 0

     write(LIS_logunit,*) '[INFO] Getting AgERA5 data'
     write(LIS_logunit,*) '[INFO] AgERA5 bookend 1: ', yr1, mo1, da1

     ! Time for bookend 1 (current day)
     call LIS_tick(time1, doy1, gmt1, yr1, mo1, da1, hr1, mn1, ss1, ts1)

     ! Time for bookend 2 (next day)
     yr2 = yr1
     mo2 = mo1
     da2 = da1
     hr2 = hr1
     mn2 = mn1
     ss2 = ss1
     call LIS_tick(time2, doy2, gmt2, yr2, mo2, da2, hr2, mn2, ss2, ts2)

     write(LIS_logunit,*) '[INFO] AgERA5 bookend 2: ', yr2, mo2, da2

     kk = 1
     
     ! Read bookend 1
     call agera5file(n, kk, findex, agera5_struc(n)%agera5dir, &
          yr1, mo1, da1, filename)
     write(LIS_logunit,*) '[INFO] Reading AgERA5 file: ', trim(filename)

     order = 1
     call read_agera5(n, kk, findex, order, filename, ferror)

     if ( ferror == 1 ) then
        write(LIS_logunit,*) '[ERR] Error reading AgERA5 file1: ', trim(filename)
        call LIS_endrun()
     endif

     ! Read bookend 2
     call agera5file(n, kk, findex, agera5_struc(n)%agera5dir, &
          yr2, mo2, da2, filename)
     write(LIS_logunit,*) '[INFO] Reading AgERA5 file: ', trim(filename)

     order = 2
     call read_agera5(n, kk, findex, order, filename, ferror)

     if ( ferror == 1 ) then
        write(LIS_logunit,*) '[ERR] Error reading AgERA5 file2: ', trim(filename)
        call LIS_endrun()
     endif

     agera5_struc(n)%agera5time1 = time1
     agera5_struc(n)%agera5time2 = time2
  endif

      do r=1,LIS_rc%lnr(n)
        do c=1,LIS_rc%lnc(n)
           if (LIS_domain(n)%gindex(c,r).ne.-1) then

              if ( order == 1 ) then
                   ! Daily AgERA5: Mapping the single daily record (index 1) 
                   ! into the time placeholders.
                   agera5_struc(n)%metdata1(:,:,LIS_domain(n)%gindex(c,r)) = &
                         agera5_struc(n)%ageraforc1(:,:,1,&  ! Use 1 instead of hr_int
                         (c+(r-1)*LIS_rc%lnc(n)))
                   agera5_struc(n)%metdata2(:,:,LIS_domain(n)%gindex(c,r)) = &
                         agera5_struc(n)%ageraforc1(:,:,1,&  ! Use 1 instead of hr_int
                         (c+(r-1)*LIS_rc%lnc(n)))
              else
                   agera5_struc(n)%metdata1(:,:,LIS_domain(n)%gindex(c,r)) = &
                         agera5_struc(n)%ageraforc1(:,:,1,& 
                         (c+(r-1)*LIS_rc%lnc(n)))
                   agera5_struc(n)%metdata2(:,:,LIS_domain(n)%gindex(c,r)) = &
                         agera5_struc(n)%ageraforc2(:,:,1,& 
                         (c+(r-1)*LIS_rc%lnc(n)))
              endif

            endif
         enddo
      enddo

      ! Assign the daily AgERA5 times:
      agera5_struc(n)%agera5time2 = time2
      agera5_struc(n)%agera5time1 = time1

end subroutine get_agera5

!BOP
! !ROUTINE: agera5file
! \label{agera5file}
!
! !INTERFACE:
subroutine agera5file(n, kk, findex, agera5dir, yr, mo, da, filename)

  implicit none

! !ARGUMENTS:
  integer, intent(in)           :: n
  integer, intent(in)           :: kk
  integer, intent(in)           :: findex
  character(len=*), intent(in)  :: agera5dir
  integer, intent(in)           :: yr, mo, da
  character(len=*), intent(out) :: filename

!
! !DESCRIPTION:
!  This subroutine constructs the AgERA5 filename for a given date.
!  AgERA5 files are organized as: <dir>/YYYY/AgERA5_YYYYMMDD.nc
!
!EOP

  character*4  :: cyear
  character*2  :: cmonth, cday
  character*8  :: cdate

  write(unit=cyear, fmt='(i4.4)') yr
  write(unit=cmonth, fmt='(i2.2)') mo
  write(unit=cday, fmt='(i2.2)') da
  write(unit=cdate, fmt='(i4.4,i2.2,i2.2)') yr, mo, da

  filename = trim(agera5dir) // cyear // '/AgERA5_' // cdate // '.nc'

end subroutine agera5file
