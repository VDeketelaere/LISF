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
! !ROUTINE: finalize_agera5
! \label{finalize_agera5}
!
! !REVISION HISTORY: 
! Dec 2025: Initial implementation
! 
! !INTERFACE:

subroutine finalize_agera5(findex)

! !USES:
  use LDT_coreMod,       only : LDT_rc
  use agera5_forcingMod, only : agera5_struc
!
! !DESCRIPTION:
!  Routine to cleanup AgERA5 forcing related memory allocations.   
! 
!EOP
  implicit none

  integer :: findex
  integer :: n

  do n=1,LDT_rc%nnest
    select case( LDT_rc%met_gridtransform(findex) )

     case( "bilinear" )
       deallocate(agera5_struc(n)%n111)
       deallocate(agera5_struc(n)%n121)
       deallocate(agera5_struc(n)%n211)
       deallocate(agera5_struc(n)%n221)
       deallocate(agera5_struc(n)%w111)
       deallocate(agera5_struc(n)%w121)
       deallocate(agera5_struc(n)%w211)
       deallocate(agera5_struc(n)%w221)

     case( "budget-bilinear" )
       deallocate(agera5_struc(n)%n111)
       deallocate(agera5_struc(n)%n121)
       deallocate(agera5_struc(n)%n211)
       deallocate(agera5_struc(n)%n221)
       deallocate(agera5_struc(n)%w111)
       deallocate(agera5_struc(n)%w121)
       deallocate(agera5_struc(n)%w211)
       deallocate(agera5_struc(n)%w221)

     case( "conservative" )
       deallocate(agera5_struc(n)%n112)
       deallocate(agera5_struc(n)%n122)
       deallocate(agera5_struc(n)%n212)
       deallocate(agera5_struc(n)%n222)
       deallocate(agera5_struc(n)%w112)
       deallocate(agera5_struc(n)%w122)
       deallocate(agera5_struc(n)%w212)
       deallocate(agera5_struc(n)%w222)

     case( "neighbor" )
       deallocate(agera5_struc(n)%n113)
    end select

 enddo
 deallocate(agera5_struc)

end subroutine finalize_agera5
