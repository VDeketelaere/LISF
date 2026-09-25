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
! !MODULE: agera5_forcingMod
! \label{agera5_forcingMod}
!
! !DESCRIPTION:
!  This module contains variables and data structures that are used
!  for the implementation of the AgERA5 forcing data.
!  The data is available at daily temporal resolution at 0.1 degree
!  global spatial resolution.
!
!  The implementation in LDT has the derived data type {\tt agera5\_struc}
!  that includes the variables to specify the runtime options, and
!  the calculation of weights for spatial interpolation.
!
!  AgERA5 daily variables (all in one file per day):
!  1. Temperature-2m-Min (Tmin) [K]
!  2. Temperature-2m-Max (Tmax) [K]
!  3. Precipitation (Rainf) [mm/day]
!  4. Evapotranspiration-Flux-Mean (ETo) [mm/day]
!
! !REVISION HISTORY:
! Dec 2025: Generated based on MERRA2 template
!
! !USES:
module agera5_forcingMod

  use LDT_constantsMod, only : LDT_CONST_PATH_LEN

  implicit none

  PRIVATE
!-----------------------------------------------------------------------------
! !PUBLIC MEMBER FUNCTIONS:
!-----------------------------------------------------------------------------
  public :: init_agera5      !defines the native resolution of the input data
!-----------------------------------------------------------------------------
! !PUBLIC TYPES:
!-----------------------------------------------------------------------------
  public :: agera5_struc

!EOP
  type, public ::  agera5_type_dec
     real         :: ts
     integer      :: nc, nr
     integer      :: nvars
     character(len=LDT_CONST_PATH_LEN) :: agera5dir   !AgERA5 Forcing Directory
     real*8       :: agera5time1, agera5time2, ringtime
     logical      :: reset_flag

     integer                :: mi
     real                   :: gridDesc(20)
     integer, allocatable   :: n111(:)
     integer, allocatable   :: n121(:)
     integer, allocatable   :: n211(:)
     integer, allocatable   :: n221(:)
     real, allocatable      :: w111(:),w121(:)
     real, allocatable      :: w211(:),w221(:)

     integer, allocatable   :: n112(:,:)
     integer, allocatable   :: n122(:,:)
     integer, allocatable   :: n212(:,:)
     integer, allocatable   :: n222(:,:)
     real, allocatable      :: w112(:,:),w122(:,:)
     real, allocatable      :: w212(:,:),w222(:,:)
     integer, allocatable   :: n113(:)
     integer                :: findtime1, findtime2
     logical                :: startFlag, dayFlag
     real, allocatable      :: ageraforc1(:,:,:), ageraforc2(:,:,:)

     character(len=LDT_CONST_PATH_LEN) :: agera5hgt_file

  end type agera5_type_dec

  type(agera5_type_dec), allocatable :: agera5_struc(:)

contains

!BOP
!
! !ROUTINE: init_agera5
! \label{init_agera5}
!
! !REVISION HISTORY:
! Dec 2025: Initial implementation
!
! !INTERFACE:
  subroutine init_agera5(findex)
    use LDT_coreMod
    use LDT_timeMgrMod
    use LDT_logMod
    
    implicit none
    integer, intent(in) :: findex
    integer :: n

    allocate(agera5_struc(LDT_rc%nnest))
    
    ! Read configuration
    call readcrd_agera5()
    
    do n=1, LDT_rc%nnest
      agera5_struc(n)%ts = 86400  ! Daily timestep
      call LDT_update_timestep(LDT_rc, n, agera5_struc(n)%ts)
      
      ! AgERA5 native grid: 0.1 degree global
      agera5_struc(n)%nc = 3601
      agera5_struc(n)%nr = 1801
      agera5_struc(n)%nvars = 4  ! *** SET NUMBER OF VARIABLES ***
      
      ! Grid description
      agera5_struc(n)%gridDesc(1) = 0           ! Lat/lon
      agera5_struc(n)%gridDesc(2) = 3601        ! ncols
      agera5_struc(n)%gridDesc(3) = 1801        ! nrows
      agera5_struc(n)%gridDesc(4) = -90.00      ! LL lat
      agera5_struc(n)%gridDesc(5) = -180.00   ! LL lon
      agera5_struc(n)%gridDesc(6) = 128
      agera5_struc(n)%gridDesc(7) = 90.00       ! UR lat
      agera5_struc(n)%gridDesc(8) = 180.00      ! UR lon
      agera5_struc(n)%gridDesc(9) = 0.1         ! dlon
      agera5_struc(n)%gridDesc(10) = 0.1        ! dlat
      agera5_struc(n)%gridDesc(20) = 0
      
      agera5_struc(n)%mi = agera5_struc(n)%nc * agera5_struc(n)%nr
      
      ! Set up interpolation (bilinear/conservative/neighbor)
      select case( trim(LDT_rc%met_gridtransform(findex)) )
        case( "bilinear" )
          allocate(agera5_struc(n)%n111(LDT_rc%lnc(n)*LDT_rc%lnr(n)))
          allocate(agera5_struc(n)%n121(LDT_rc%lnc(n)*LDT_rc%lnr(n)))
          allocate(agera5_struc(n)%n211(LDT_rc%lnc(n)*LDT_rc%lnr(n)))
          allocate(agera5_struc(n)%n221(LDT_rc%lnc(n)*LDT_rc%lnr(n)))
          allocate(agera5_struc(n)%w111(LDT_rc%lnc(n)*LDT_rc%lnr(n)))
          allocate(agera5_struc(n)%w121(LDT_rc%lnc(n)*LDT_rc%lnr(n)))
          allocate(agera5_struc(n)%w211(LDT_rc%lnc(n)*LDT_rc%lnr(n)))
          allocate(agera5_struc(n)%w221(LDT_rc%lnc(n)*LDT_rc%lnr(n)))
          
          call bilinear_interp_input(n, agera5_struc(n)%gridDesc(:), &
              agera5_struc(n)%n111, agera5_struc(n)%n121, &
              agera5_struc(n)%n211, agera5_struc(n)%n221, &
              agera5_struc(n)%w111, agera5_struc(n)%w121, &
              agera5_struc(n)%w211, agera5_struc(n)%w221)
              
        case( "budget-bilinear" )
          ! Same as bilinear for weights
          allocate(agera5_struc(n)%n111(LDT_rc%lnc(n)*LDT_rc%lnr(n)))
          allocate(agera5_struc(n)%n121(LDT_rc%lnc(n)*LDT_rc%lnr(n)))
          allocate(agera5_struc(n)%n211(LDT_rc%lnc(n)*LDT_rc%lnr(n)))
          allocate(agera5_struc(n)%n221(LDT_rc%lnc(n)*LDT_rc%lnr(n)))
          allocate(agera5_struc(n)%w111(LDT_rc%lnc(n)*LDT_rc%lnr(n)))
          allocate(agera5_struc(n)%w121(LDT_rc%lnc(n)*LDT_rc%lnr(n)))
          allocate(agera5_struc(n)%w211(LDT_rc%lnc(n)*LDT_rc%lnr(n)))
          allocate(agera5_struc(n)%w221(LDT_rc%lnc(n)*LDT_rc%lnr(n)))
          
          call bilinear_interp_input(n, agera5_struc(n)%gridDesc(:), &
              agera5_struc(n)%n111, agera5_struc(n)%n121, &
              agera5_struc(n)%n211, agera5_struc(n)%n221, &
              agera5_struc(n)%w111, agera5_struc(n)%w121, &
              agera5_struc(n)%w211, agera5_struc(n)%w221)
              
        case( "conservative" )
          allocate(agera5_struc(n)%n112(LDT_rc%lnc(n)*LDT_rc%lnr(n),25))
          allocate(agera5_struc(n)%n122(LDT_rc%lnc(n)*LDT_rc%lnr(n),25))
          allocate(agera5_struc(n)%n212(LDT_rc%lnc(n)*LDT_rc%lnr(n),25))
          allocate(agera5_struc(n)%n222(LDT_rc%lnc(n)*LDT_rc%lnr(n),25))
          allocate(agera5_struc(n)%w112(LDT_rc%lnc(n)*LDT_rc%lnr(n),25))
          allocate(agera5_struc(n)%w122(LDT_rc%lnc(n)*LDT_rc%lnr(n),25))
          allocate(agera5_struc(n)%w212(LDT_rc%lnc(n)*LDT_rc%lnr(n),25))
          allocate(agera5_struc(n)%w222(LDT_rc%lnc(n)*LDT_rc%lnr(n),25))
          
          call conserv_interp_input(n, agera5_struc(n)%gridDesc(:), &
              agera5_struc(n)%n112, agera5_struc(n)%n122, &
              agera5_struc(n)%n212, agera5_struc(n)%n222, &
              agera5_struc(n)%w112, agera5_struc(n)%w122, &
              agera5_struc(n)%w212, agera5_struc(n)%w222)
              
        case( "neighbor" )
          allocate(agera5_struc(n)%n113(LDT_rc%lnc(n)*LDT_rc%lnr(n)))
          call neighbor_interp_input(n, agera5_struc(n)%gridDesc(:), &
              agera5_struc(n)%n113)
              
        case default
          write(LDT_logunit,*) '[ERR] Interpolation option not supported: ', &
              trim(LDT_rc%met_gridtransform(findex))
          call LDT_endrun()
      end select
      
      ! *** KEY FIX: Allocate 3D forcing arrays ***
      ! Dimensions: (nvars, ntimes, ngrid)
      ! For daily data: (4, 1, lnc*lnr)
      if (trim(LDT_rc%runmode) == "Metforce processing" .or. &
          trim(LDT_rc%runmode) == "Metforce temporal downscaling") then
        
        allocate(agera5_struc(n)%ageraforc1( &
            agera5_struc(n)%nvars, &          ! 4 variables
            1, &                               ! 1 time slice (daily)
            LDT_rc%lnc(n)*LDT_rc%lnr(n)))     ! spatial points
            
        allocate(agera5_struc(n)%ageraforc2( &
            agera5_struc(n)%nvars, &
            1, &
            LDT_rc%lnc(n)*LDT_rc%lnr(n)))
        
        ! Initialize to undefined
        agera5_struc(n)%ageraforc1 = LDT_rc%udef
        agera5_struc(n)%ageraforc2 = LDT_rc%udef
      endif
      
      ! Initialize timing
      agera5_struc(n)%agera5time1 = 3000.0
      agera5_struc(n)%agera5time2 = 0.0
      agera5_struc(n)%startFlag = .true.
      agera5_struc(n)%dayFlag = .true.
      
    enddo
    
    write(LDT_logunit,*) "[INFO] AgERA5 forcing initialized"
    
  end subroutine init_agera5
end module agera5_forcingMod
