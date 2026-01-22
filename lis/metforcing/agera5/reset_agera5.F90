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
! !MODULE: reset_agera5
! \label{reset_agera5}
! 
! !REVISION HISTORY: 
! Dec 2025: Initial implementation
! 
! !INTERFACE:
subroutine reset_agera5
  use LIS_coreMod,  only : LIS_rc
  use LIS_timeMgrMod, only : LIS_date2time
  use agera5_forcingMod

  implicit none
  integer :: n 
  
  do n=1,LIS_rc%nnest
     agera5_struc(n)%startFlag = .true. 
     agera5_struc(n)%dayFlag = .true. 
     agera5_struc(n)%agera5time1 = 3000.0
     agera5_struc(n)%agera5time2 = 0.0
     agera5_struc(n)%reset_flag = .true.
  enddo
  
end subroutine reset_agera5
