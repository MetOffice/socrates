! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
!  Subroutine to calculate densities.
!
! Method:
!   This routine calculates the density of air and the molar
!   densities of the broadening species for the self and foreign-
!   broadened continua using the gas law including the effect
!   of water vapour.
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: Radiance Core
!
!- ---------------------------------------------------------------------
SUBROUTINE calculate_density(n_profile, n_layer, l_continuum            &
     , water_frac, p, t, i_top                                          &
     , density, molar_density_water, molar_density_frn                  &
     , nd_profile, nd_layer                                             &
     )


  USE realtype_rd, ONLY: RealK
  USE rad_ccf, ONLY: mol_weight_air, r, repsilon, c_virtual
  USE yomhook, ONLY: lhook, dr_hook
  USE parkind1, ONLY: jprb, jpim

  IMPLICIT NONE


! Sizes of dummy arrays.
  INTEGER, INTENT(IN) ::                                                &
      nd_profile                                                        &
!       Maximum number of profiles
    , nd_layer
!       Maximum number of layers


! Dummy arguments.
  INTEGER, INTENT(IN) ::                                                &
      n_profile                                                         &
!       Number of profiles
    , n_layer                                                           &
!       Number of layers
    , i_top
!       Top vertical `index'
  LOGICAL                                                               &
      l_continuum
!       Continuum flag
  REAL (RealK), INTENT(IN) ::                                           &
      water_frac(nd_profile, nd_layer)                                  &
!       Mass fraction of water
    , p(nd_profile, nd_layer)                                           &
!       Pressure
    , t(nd_profile, nd_layer)
!       Temperature
  REAL (RealK), INTENT(OUT) ::                                          &
      density(nd_profile, nd_layer)                                     &
!       Air density
    , molar_density_water(nd_profile, nd_layer)                         &
!       Molar density of water
    , molar_density_frn(nd_profile, nd_layer)
!       Molar density of foreign species

! Local variables.
  INTEGER                                                               &
      l                                                                 &
!       Loop variable
    , i
!       Loop variable

  INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
  INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
  REAL(KIND=jprb)               :: zhook_handle

  CHARACTER(LEN=*), PARAMETER :: RoutineName='CALCULATE_DENSITY'


  IF (lhook) CALL dr_hook(RoutineName,zhook_in,zhook_handle)

! Find the air density first.
  DO i=i_top, n_layer
    DO l=1, n_profile
       density(l, i)=p(l, i)/(r*t(l, i)                                 &
         *(1.0e+00_RealK+c_virtual*water_frac(l, i)))
    END DO
  END DO

  IF (l_continuum) THEN
    DO i=i_top, n_layer
      DO l=1, n_profile
        molar_density_frn(l, i)=density(l, i)                           &
          *(1.0e+00_RealK-water_frac(l, i))/mol_weight_air
        molar_density_water(l, i)=density(l, i)                         &
          *water_frac(l, i)/(repsilon*mol_weight_air)
      END DO
    END DO
  END IF


  IF (lhook) CALL dr_hook(RoutineName,zhook_out,zhook_handle)

END SUBROUTINE calculate_density
