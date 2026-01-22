!-------------------------------------------------------------------------
! NASA Goddard Space Flight Center
! Land Information System Framework (LISF)
! Version 7.5
!
! Copyright (c) 2024 United States Government as represented by the
! Administrator of the National Aeronautics and Space Administration.
! All Rights Reserved.
!-------------------------------------------------------------------------
!BOP
! !ROUTINE: timeinterp_agera5
! \label{timeinterp_agera5}
!
! !REVISION HISTORY:
! Dec 2025: Initial implementation for AgERA5 with AquaCrop
!
! !INTERFACE:
subroutine timeinterp_agera5(n, findex)
! !USES:
  use LIS_logMod
  use agera5_forcingMod
  implicit none
! !ARGUMENTS:
  integer, intent(in) :: n
  integer, intent(in) :: findex
! !DESCRIPTION:
!  For AgERA5 with AquaCrop, temporal interpolation is not needed.
!  The daily forcing data is read directly from agera5_struc%metdata1
!  by the AC72_f2t routine.
!
!  This routine is a placeholder to satisfy the LIS forcing interface.
!
!EOP
  ! Nothing to do - AC72_f2t reads directly from agera5_struc%metdata1
  return
end subroutine timeinterp_agera5