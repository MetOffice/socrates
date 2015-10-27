! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
!  Subroutine to apply a path-length scaling to the continuum.
!
! Method:
!   The scaling function is calculated. This is multpiled by a
!   suitable "amount" of continuum incorporating a broadening
!   density.
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: Radiance Core
!
!- ---------------------------------------------------------------------
SUBROUTINE rescale_continuum(n_profile, n_layer, i_continuum            &
     , p, t, i_top                                                      &
     , density, molar_density_water, molar_density_frn                  &
     , water_frac                                                       &
     , amount_continuum                                                 &
     , i_fnc                                                            &
     , p_reference, t_reference, scale_parameter                        &
     , nd_profile, nd_layer                                             &
     , nd_scale_variable                                                &
     )


  USE realtype_rd, ONLY: RealK
  USE rad_pcf
  USE rad_ccf, ONLY: n2_mass_frac
  USE vectlib_mod, ONLY : rtor_v
  USE yomhook, ONLY: lhook, dr_hook
  USE parkind1, ONLY: jprb, jpim

  IMPLICIT NONE


! Sizes of dummy arrays.
  INTEGER, INTENT(IN) ::                                                &
      nd_profile                                                        &
!       Size allocated for atmospheric profiles
    , nd_layer                                                          &
!       Size allocated for atmospheric layers
    , nd_scale_variable
!       Size allocated for scaling variables


! Dummy arguments.
  INTEGER, INTENT(IN) ::                                                &
      n_profile                                                         &
!       Number of profiles
    , n_layer                                                           &
!       Number of layers
    , i_continuum                                                       &
!       Continuum type
    , i_fnc                                                             &
!       Scaling function
    , i_top
!       Top `index' of arrays
  REAL (RealK), INTENT(IN) ::                                           &
      water_frac(nd_profile, nd_layer)                                  &
!       Mass fraction of water
    , p(nd_profile, nd_layer)                                           &
!       Pressure
    , t(nd_profile, nd_layer)                                           &
!       Temperature
    , density(nd_profile, nd_layer)                                     &
!       Overall density
    , molar_density_water(nd_profile, nd_layer)                         &
!       Molar density of water vapour
    , molar_density_frn(nd_profile, nd_layer)                           &
!       Molar density of foreign species
    , p_reference                                                       &
!       Reference pressure
    , t_reference                                                       &
!       Reference pressure
    , scale_parameter(nd_scale_variable)
!       Scaling paramters
  REAL (RealK), INTENT(OUT) ::                                          &
      amount_continuum(nd_profile, nd_layer)
!       Amount of continuum

! Local variables.
  INTEGER ::                                                            &
      l                                                                 &
!       Loop variable
    , i
!       Loop variable
  REAL (RealK) :: pwk_in(n_profile,n_layer-i_top+1)  ! Workspace
  REAL (RealK) :: pwk(n_profile,n_layer-i_top+1)  ! Workspace
  REAL (RealK) :: twk_in(n_profile,n_layer-i_top+1)  ! Workspace
  REAL (RealK) :: twk(n_profile,n_layer-i_top+1)  ! Workspace
  REAL (RealK) :: sp1(n_profile,n_layer-i_top+1)  ! Workspace
  REAL (RealK) :: sp2(n_profile,n_layer-i_top+1)  ! Workspace
  INTEGER :: n_input      ! No. of inputs for rtor_v function

  INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
  INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
  REAL(KIND=jprb)               :: zhook_handle

  CHARACTER(LEN=*), PARAMETER :: RoutineName='RESCALE_CONTINUUM'


  IF (lhook) CALL dr_hook(RoutineName,zhook_in,zhook_handle)

  DO i=1, n_layer-i_top+1
    DO l=1, n_profile
      sp1(l,i)=scale_parameter(1)
      sp2(l,i)=scale_parameter(2)
    END DO
  END DO
  n_input=(n_layer-i_top+1)*n_profile
  DO i=   1, n_layer-i_top+1
    DO l=1, n_profile
      pwk_in(l,i)=p(l, i_top+i-1)/p_reference
    END DO
  END DO

  CALL rtor_v(n_input,pwk_in,sp1,pwk)

  IF (i_fnc == ip_scale_power_law) THEN

    DO i=   1, n_layer-i_top+1
      DO l=1, n_profile
        twk_in(l,i)=t(l, i_top+i-1)/t_reference
      END DO
    END DO

    CALL rtor_v(n_input,twk_in,sp2,twk)

    DO i=i_top, n_layer
      DO l=1, n_profile
        amount_continuum(l, i)=pwk(l,i-i_top+1)*twk(l,i-i_top+1)
      END DO
    END DO

  ELSE IF(i_fnc == ip_scale_power_quad) THEN

    DO i=i_top, n_layer
      DO l=1, n_profile
        amount_continuum(l, i)                                          &
           =pwk(l,i-i_top+1)                                            &
           *(1.0e+00+scale_parameter(2)*(t(l, i)                        &
           /t_reference-1.0e+00)                                        &
           +scale_parameter(3)*(t(l, i)                                 &
           /t_reference-1.0e+00)**2)
      END DO
    END DO
  END IF

  IF (i_continuum == ip_self_continuum) THEN
    DO i=1, n_layer
      DO l=1, n_profile
        amount_continuum(l, i)=amount_continuum(l, i)                   &
          *molar_density_water(l, i)*water_frac(l, i)
      END DO
    END DO
  ELSE IF (i_continuum == ip_frn_continuum) THEN
    DO i=1, n_layer
      DO l=1, n_profile
        amount_continuum(l, i)=amount_continuum(l, i)                   &
          *molar_density_frn(l, i)*water_frac(l, i)
      END DO
    END DO
  ELSE IF (i_continuum == ip_n2_continuum) THEN
    DO i=1, n_layer
      DO l=1, n_profile
        amount_continuum(l, i)=amount_continuum(l, i)                   &
          *n2_mass_frac*density(l, i)
      END DO
    END DO
  END IF


  IF (lhook) CALL dr_hook(RoutineName,zhook_out,zhook_handle)

END SUBROUTINE rescale_continuum
