subroutine get_agera5(n, findex)
  use LDT_coreMod
  use LDT_timeMgrMod
  use LDT_logMod
  use LDT_constantsMod, only : LDT_CONST_PATH_LEN
  use LDT_metforcingMod
  use agera5_forcingMod
  implicit none

  integer, intent(in) :: n, findex
  integer :: yr1, mo1, da1, yr2, mo2, da2
  integer :: ferror, try, kk, order
  character(len=LDT_CONST_PATH_LEN) :: fname
  real*8 :: time1, time2, timenow
  real :: gmt1, gmt2
  integer :: movetime

  ! Initialize
  agera5_struc(n)%findtime1 = 0
  agera5_struc(n)%findtime2 = 0
  movetime = 0

  ! Current time
  yr1 = LDT_rc%yr
  mo1 = LDT_rc%mo
  da1 = LDT_rc%da
  call LDT_tick(timenow, _, gmt1, yr1, mo1, da1, LDT_rc%hr, LDT_rc%mn, 0, 0)

  ! Determine bookends (previous day and next day)
  if (timenow >= agera5_struc(n)%agera5time2) then
     call LDT_tick(time1, _, gmt1, yr1, mo1, da1, 0, 0, 0, -86400) ! previous day
     yr2 = LDT_rc%yr
     mo2 = LDT_rc%mo
     da2 = LDT_rc%da
     call LDT_tick(time2, _, gmt2, yr2, mo2, da2, 0, 0, 0, 0)      ! current day
     movetime = 1
     agera5_struc(n)%findtime2 = 1
  endif

  if (LDT_rc%tscount(n) == 1 .or. LDT_rc%rstflag(n) == 1) then
     agera5_struc(n)%findtime1 = 1
     agera5_struc(n)%findtime2 = 1
     movetime = 0
     LDT_rc%rstflag(n) = 0
  endif

  ! Move bookend data
  if (movetime == 1) then
     agera5_struc(n)%agera5time1 = agera5_struc(n)%agera5time2
     do f = 1, LDT_rc%met_nf(findex)
        do c = 1, LDT_rc%ngrid(n)
           agera5_struc(n)%metdata1(f,c) = agera5_struc(n)%metdata2(f,c)
        enddo
     enddo
  endif

  ! Read previous day
  if (agera5_struc(n)%findtime1 == 1) then
     ferror = 0; try = 0
     do
        if (ferror /= 0) exit
        try = try + 1
        kk = 1; order = 1
        call agera5files(n, kk, findex, agera5_struc(n)%agera5dir, yr1, mo1, da1, fname)
        write(LDT_logunit,*) '[INFO] getting file.. ', trim(fname)
        call read_agera5(n, kk, order, yr1, mo1, da1, findex, fname, ferror)
        if (ferror >= 1) agera5_struc(n)%agera5time1 = time1
        call LDT_tick(time1, _, gmt1, yr1, mo1, da1, 0, 0, 0, -86400)
        if (try > 11) then
           write(LDT_logunit,*) '[ERR] AgERA5 data gap exceeds 10 days on file 1'
           call LDT_endrun()
        endif
     enddo
  endif

  ! Read current day
  if (agera5_struc(n)%findtime2 == 1) then
     ferror = 0; try = 0
     do
        if (ferror /= 0) exit
        try = try + 1
        kk = 1; order = 2
        call agera5files(n, kk, findex, agera5_struc(n)%agera5dir, yr2, mo2, da2, fname)
        write(LDT_logunit,*) '[INFO] getting file.. ', trim(fname)
        call read_agera5(n, kk, order, yr2, mo2, da2, findex, fname, ferror)
        if (ferror >= 1) agera5_struc(n)%agera5time2 = time2
        call LDT_tick(time2, _, gmt2, yr2, mo2, da2, 0, 0, 0, -86400)
        if (try > 11) then
           write(LDT_logunit,*) '[ERR] AgERA5 data gap exceeds 10 days on file 2'
           call LDT_endrun()
        endif
     enddo
  endif
end subroutine get_agera5
