!-----------------------BEGIN NOTICE -- DO NOT EDIT-----------------------
! NASA Goddard Space Flight Center
! Land Information System Framework (LISF)
! Version 7.5
!
! Copyright (c) 2024 United States Government as represented by the
! Administrator of the National Aeronautics and Space Administration.
! All Rights Reserved.
!-------------------------END NOTICE -- DO NOT EDIT-----------------------
module agera5_forcingMod
!BOP
! !MODULE: agera5_forcingMod
!
! !USES:
  use LIS_constantsMod, only : LIS_CONST_PATH_LEN
  implicit none

  PRIVATE
!-----------------------------------------------------------------------------
! !PUBLIC MEMBER FUNCTIONS:
!-----------------------------------------------------------------------------
  public :: init_agera5      !defines the native resolution of
                             !the input data
!-----------------------------------------------------------------------------
! !PUBLIC TYPES:
!-----------------------------------------------------------------------------
  public :: agera5_struc

!EOP
  type, public ::  agera5_type_dec
     real         :: ts
     integer      :: ncold, nrold
     character(len=LIS_CONST_PATH_LEN) :: agera5dir   !agera5 Forcing Directory
     real*8       :: agera5time1,agera5time2
     logical      :: reset_flag

     integer                :: mi
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
     real, allocatable      :: ageraforc1(:,:,:,:), ageraforc2(:,:,:,:)

     integer            :: nvars

     real*8             :: ringtime
     
     integer            :: nIter, st_iterid,en_iterid

     real, allocatable :: metdata1(:,:,:) 
     real, allocatable :: metdata2(:,:,:) 

     integer                 :: usescalef
     !integer                 :: usepcpsampling
     !integer                 :: pcpscal_cmo
     !integer                 :: use2mwind
     character(len=LIS_CONST_PATH_LEN) :: scaleffile
     integer                 :: nbins
     real, allocatable       :: refxrange(:,:,:,:)
     real, allocatable       :: refcdf(:,:,:,:)
     !real, allocatable       :: refmean(:,:,:)
     !real, allocatable       :: refmean_ip(:)
     !real, allocatable       :: refstdev(:,:,:)
     !real, allocatable       :: refstdev_ip(:)
     real, allocatable       :: ageraxrange(:,:,:,:)
     real, allocatable       :: ageracdf(:,:,:,:)
     !integer, allocatable    :: rseed(:,:)
  end type agera5_type_dec

  type(agera5_type_dec), allocatable :: agera5_struc(:)

contains

!BOP
!
! !ROUTINE: init_agera5
! \label{init_agera5}
!
! !INTERFACE:
  subroutine init_agera5(findex)

! !USES:
    use LIS_coreMod
    use LIS_timeMgrMod
    use LIS_logMod
    !use LIS_spatialDownscalingMod, only : LIS_init_pcpclimo_native
    use LIS_forecastMod

    implicit none
! !AGRUMENTS:
    integer, intent(in) :: findex
!
!EOP
    real :: gridDesci(LIS_rc%nnest,50)
    integer :: updoy, yr1,mo1,da1,hr1,mn1,ss1
    real :: upgmt
    integer :: n

    allocate(agera5_struc(LIS_rc%nnest))

    do n=1,LIS_rc%nnest
       agera5_struc(n)%ncold = 3601
       agera5_struc(n)%nrold = 1801
    enddo

    call readcrd_agera5()
    LIS_rc%met_nf(findex) = 4

    agera5_struc%reset_flag = .false.

    do n=1, LIS_rc%nnest
       agera5_struc(n)%ts = 86400  !check
       call LIS_update_timestep(LIS_rc, n, agera5_struc(n)%ts)
    enddo

    gridDesci = 0

    do n=1,LIS_rc%nnest
       gridDesci(n,1) = 0
       gridDesci(n,2) = agera5_struc(n)%ncold
       gridDesci(n,3) = agera5_struc(n)%nrold
       gridDesci(n,4) = -90.00
       gridDesci(n,5) = -180.00
       gridDesci(n,6) = 128
       gridDesci(n,7) = 90.00
       gridDesci(n,8) = 180.00
       gridDesci(n,9) = 0.1
       gridDesci(n,10) = 0.1
       gridDesci(n,20) = 0

       agera5_struc(n)%mi = agera5_struc(n)%ncold*agera5_struc(n)%nrold

       ! Setting up weights for Interpolation
       if(trim(LIS_rc%met_interp(findex)).eq."bilinear") then
          allocate(agera5_struc(n)%n111(LIS_rc%lnc(n)*LIS_rc%lnr(n)))
          allocate(agera5_struc(n)%n121(LIS_rc%lnc(n)*LIS_rc%lnr(n)))
          allocate(agera5_struc(n)%n211(LIS_rc%lnc(n)*LIS_rc%lnr(n)))
          allocate(agera5_struc(n)%n221(LIS_rc%lnc(n)*LIS_rc%lnr(n)))
          allocate(agera5_struc(n)%w111(LIS_rc%lnc(n)*LIS_rc%lnr(n)))
          allocate(agera5_struc(n)%w121(LIS_rc%lnc(n)*LIS_rc%lnr(n)))
          allocate(agera5_struc(n)%w211(LIS_rc%lnc(n)*LIS_rc%lnr(n)))
          allocate(agera5_struc(n)%w221(LIS_rc%lnc(n)*LIS_rc%lnr(n)))
          call bilinear_interp_input(n, gridDesci(n,:),&
               agera5_struc(n)%n111,agera5_struc(n)%n121,&
               agera5_struc(n)%n211,agera5_struc(n)%n221,&
               agera5_struc(n)%w111,agera5_struc(n)%w121,&
               agera5_struc(n)%w211,agera5_struc(n)%w221)

       elseif(trim(LIS_rc%met_interp(findex)).eq."budget-bilinear") then
          allocate(agera5_struc(n)%n111(LIS_rc%lnc(n)*LIS_rc%lnr(n)))
          allocate(agera5_struc(n)%n121(LIS_rc%lnc(n)*LIS_rc%lnr(n)))
          allocate(agera5_struc(n)%n211(LIS_rc%lnc(n)*LIS_rc%lnr(n)))
          allocate(agera5_struc(n)%n221(LIS_rc%lnc(n)*LIS_rc%lnr(n)))
          allocate(agera5_struc(n)%w111(LIS_rc%lnc(n)*LIS_rc%lnr(n)))
          allocate(agera5_struc(n)%w121(LIS_rc%lnc(n)*LIS_rc%lnr(n)))
          allocate(agera5_struc(n)%w211(LIS_rc%lnc(n)*LIS_rc%lnr(n)))
          allocate(agera5_struc(n)%w221(LIS_rc%lnc(n)*LIS_rc%lnr(n)))
          call bilinear_interp_input(n, gridDesci(n,:),&
               agera5_struc(n)%n111,agera5_struc(n)%n121,&
               agera5_struc(n)%n211,agera5_struc(n)%n221,&
               agera5_struc(n)%w111,agera5_struc(n)%w121,&
               agera5_struc(n)%w211,agera5_struc(n)%w221)

          allocate(agera5_struc(n)%n112(LIS_rc%lnc(n)*LIS_rc%lnr(n),25))
          allocate(agera5_struc(n)%n122(LIS_rc%lnc(n)*LIS_rc%lnr(n),25))
          allocate(agera5_struc(n)%n212(LIS_rc%lnc(n)*LIS_rc%lnr(n),25))
          allocate(agera5_struc(n)%n222(LIS_rc%lnc(n)*LIS_rc%lnr(n),25))
          allocate(agera5_struc(n)%w112(LIS_rc%lnc(n)*LIS_rc%lnr(n),25))
          allocate(agera5_struc(n)%w122(LIS_rc%lnc(n)*LIS_rc%lnr(n),25))
          allocate(agera5_struc(n)%w212(LIS_rc%lnc(n)*LIS_rc%lnr(n),25))
          allocate(agera5_struc(n)%w222(LIS_rc%lnc(n)*LIS_rc%lnr(n),25))
          call conserv_interp_input(n, gridDesci(n,:),&
               agera5_struc(n)%n112,agera5_struc(n)%n122,&
               agera5_struc(n)%n212,agera5_struc(n)%n222,&
               agera5_struc(n)%w112,agera5_struc(n)%w122,&
               agera5_struc(n)%w212,agera5_struc(n)%w222)

       elseif(trim(LIS_rc%met_interp(findex)).eq."neighbor") then
          allocate(agera5_struc(n)%n113(LIS_rc%lnc(n)*LIS_rc%lnr(n)))
          call neighbor_interp_input(n, gridDesci(n,:),&
               agera5_struc(n)%n113)

       else
          write(LIS_logunit,*) '[ERR] Interpolation option '// &
               trim(LIS_rc%met_interp(findex))//&
               ' for agera5 forcing is not supported'
          call LIS_endrun()
       endif

       call LIS_registerAlarm("agera5 forcing alarm",&
            86400.0,86400.0)
       agera5_struc(n)%startFlag = .true.
       agera5_struc(n)%dayFlag = .true.

       agera5_struc(n)%nvars = 4

       ! Forecast mode:
       if(LIS_rc%forecastMode.eq.1) then 
          
          if(mod(LIS_rc%nensem(n),&
               LIS_forecast_struc(1)%niterations).ne.0) then 
             write(LIS_logunit,*) '[ERR] The number of ensembles must be a multiple'
             write(LIS_logunit,*) '[ERR] of the number of iterations '
             write(LIS_logunit,*) '[ERR] nensem = ',LIS_rc%nensem(n)
             write(LIS_logunit,*) '[ERR] niter = ',LIS_forecast_struc(1)%niterations
             call LIS_endrun()
          endif

          allocate(agera5_struc(n)%ageraforc1(&
               LIS_forecast_struc(1)%niterations,&
               agera5_struc(n)%nvars, 1, &
               LIS_rc%lnc(n)*LIS_rc%lnr(n)))
          allocate(agera5_struc(n)%ageraforc2(&
               LIS_forecast_struc(1)%niterations,&
               agera5_struc(n)%nvars, 1, &
               LIS_rc%lnc(n)*LIS_rc%lnr(n)))

          agera5_struc(n)%st_iterid = LIS_forecast_struc(1)%st_iterId
          agera5_struc(n)%en_iterId = LIS_forecast_struc(1)%niterations
          agera5_struc(n)%nIter = LIS_forecast_struc(1)%niterations
          
          allocate(agera5_struc(n)%metdata1(LIS_forecast_struc(1)%niterations,&
               LIS_rc%met_nf(findex),&
               LIS_rc%ngrid(n)))
          allocate(agera5_struc(n)%metdata2(LIS_forecast_struc(1)%niterations,&
               LIS_rc%met_nf(findex),&
               LIS_rc%ngrid(n)))
          
       ! Regular retrospective or non-forecast mode:
       else
          allocate(agera5_struc(n)%ageraforc1(1,&
               agera5_struc(n)%nvars, 1, &
               LIS_rc%lnc(n)*LIS_rc%lnr(n)))
          allocate(agera5_struc(n)%ageraforc2(1,&
               agera5_struc(n)%nvars, 1, &
               LIS_rc%lnc(n)*LIS_rc%lnr(n)))
          agera5_struc(n)%st_iterid = 1
          agera5_struc(n)%en_iterId = 1
          agera5_struc(n)%nIter = 1
          
          allocate(agera5_struc(n)%metdata1(1,LIS_rc%met_nf(findex),&
               LIS_rc%ngrid(n)))
          allocate(agera5_struc(n)%metdata2(1,LIS_rc%met_nf(findex),&
               LIS_rc%ngrid(n)))
          
       endif

       agera5_struc(n)%metdata1 = 0
       agera5_struc(n)%metdata2 = 0

       agera5_struc(n)%ageraforc1 = LIS_rc%udef
       agera5_struc(n)%ageraforc2 = LIS_rc%udef

       if ( LIS_rc%met_ecor(findex) == "lapse-rate" .or. &
            LIS_rc%met_ecor(findex) == "lapse-rate and slope-aspect" .or. &
            LIS_rc%met_ecor(findex) == "micromet" ) then
          call read_agera5_elev(n,findex)
       endif

    enddo   ! End nest loop

  end subroutine init_agera5
end module agera5_forcingMod
