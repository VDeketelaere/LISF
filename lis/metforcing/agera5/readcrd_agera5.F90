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
! !ROUTINE: readcrd_agera5
! \label{readcrd_agera5}
!
! !REVISION HISTORY:
! Dec 2025: Initial implementation
!
! !INTERFACE:    
subroutine readcrd_agera5()
! !USES:
  use ESMF
  use LIS_coreMod, only : LIS_rc, LIS_config
  use LIS_logMod
  use agera5_forcingMod, only : agera5_struc
!
! !DESCRIPTION:
!
!  This routine reads the options specific to AgERA5 forcing
!  from the LIS configuration file. 
!  
!EOP
  implicit none

  integer :: n, rc

  call ESMF_ConfigFindLabel(LIS_config,"AgERA5 forcing directory:",rc=rc)
  do n=1,LIS_rc%nnest
     call ESMF_ConfigGetAttribute(LIS_config,agera5_struc(n)%agera5dir,&
          rc=rc)
     call LIS_verify(rc,&
          'AgERA5 forcing directory: not defined')
  enddo

  do n=1,LIS_rc%nnest
     write(LIS_logunit,*) 'Using AgERA5 forcing'
     write(LIS_logunit,*) 'AgERA5 forcing directory: ',&
           trim(agera5_struc(n)%agera5DIR)

     agera5_struc(n)%agera5time1 = 3000.0
     agera5_struc(n)%agera5time2 = 0.0

  enddo
end subroutine readcrd_agera5
