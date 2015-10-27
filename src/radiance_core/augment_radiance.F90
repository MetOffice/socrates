! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
!  Subroutine to increment a radiances or fluxes.
!
! Method:
!       The arrays holding the summed fluxes or radiances are
!       incremented by a weighted sum of the variables suffixed
!       with _INCR. Arguments specify which arrays are to be
!       incremented.
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: Radiance Core
!
!- ---------------------------------------------------------------------
SUBROUTINE augment_radiance(n_profile, n_layer                          &
    , i_angular_integration, i_sph_mode                                 &
    , n_viewing_level, n_direction                                      &
    , isolir, l_clear                                                   &
    , l_initial, weight_incr                                            &
    , l_blue_flux_surf, weight_blue                                     &
!                 Actual radiances
    , flux_direct, flux_down, flux_up                                   &
    , flux_direct_blue_surf                                             &
    , flux_down_blue_surf, flux_up_blue_surf                            &
    , i_direct, radiance, photolysis                                    &
    , flux_direct_clear, flux_down_clear, flux_up_clear                 &
!                 Increments to radiances
    , flux_direct_incr, flux_total_incr                                 &
    , i_direct_incr, radiance_incr, photolysis_incr                     &
    , flux_direct_incr_clear, flux_total_incr_clear                     &
!                 Dimensions
    , nd_flux_profile, nd_radiance_profile, nd_j_profile                &
    , nd_layer, nd_viewing_level, nd_direction                          &
    )


  USE realtype_rd, ONLY: RealK
  USE rad_pcf
  USE yomhook, ONLY: lhook, dr_hook
  USE parkind1, ONLY: jprb, jpim

  IMPLICIT NONE


! Sizes of dummy arrays.
  INTEGER, INTENT(IN) ::                                                &
      nd_flux_profile                                                   &
!       Size allocated for points where fluxes are calculated
    , nd_radiance_profile                                               &
!       Size allocated for points where radiances are calculated
    , nd_j_profile                                                      &
!       Size allocated for points where photolysis is calculated
    , nd_layer                                                          &
!       Size allocated for layers
    , nd_viewing_level                                                  &
!       Size allocated for levels where radiances are calculated
    , nd_direction
!       Size allocated for viewing directions


! Dummy arguments.
  INTEGER, INTENT(IN) ::                                                &
      n_profile                                                         &
!       Number of profiles
    , n_layer                                                           &
!       Number of layers
    , n_viewing_level                                                   &
!       Number of levels where the radiance is calculated
    , n_direction
!       Number of viewing directions
  INTEGER, INTENT(IN) ::                                                &
      isolir                                                            &
!       Spectral region
    , i_sph_mode                                                        &
!       Mode in which spherical harmonics are used
    , i_angular_integration
!       Treatment of angular integration
  LOGICAL, INTENT(IN) ::                                                &
      l_clear                                                           &
!       Clear fluxes calculated
    , l_initial
!       Logical to perform initialization instead of incrementing

  REAL (RealK), INTENT(IN) ::                                           &
      weight_incr
!       Weight to apply to incrementing fluxes

!                 Increments to Fluxes
  REAL (RealK), INTENT(IN) ::                                           &
      flux_direct_incr(nd_flux_profile, 0: nd_layer)                    &
!       Increment to direct flux
    , flux_total_incr(nd_flux_profile, 2*nd_layer+2)                    &
!       Increment to total flux
    , flux_direct_incr_clear(nd_flux_profile, 0: nd_layer)              &
!       Increment to clear direct flux
    , flux_total_incr_clear(nd_flux_profile, 2*nd_layer+2)
!       Increment to clear total flux
!                 Increments to Radiances
  REAL (RealK), INTENT(IN) ::                                           &
      i_direct_incr(nd_radiance_profile, 0: nd_layer)                   &
!       Increments to the solar irradiance
    , radiance_incr(nd_radiance_profile, nd_viewing_level               &
        , nd_direction)
!       Increments to the radiance
!                 Increments to Rates of photolysis
  REAL (RealK), INTENT(IN) ::                                           &
      photolysis_incr(nd_j_profile, nd_viewing_level)
!       Increments to the rates of photolysis

!                 Total Fluxes
  REAL (RealK), INTENT(INOUT) ::                                        &
      flux_direct(nd_flux_profile, 0: nd_layer)                         &
!       Direct flux
    , flux_down(nd_flux_profile, 0: nd_layer)                           &
!       Total downward flux
    , flux_up(nd_flux_profile, 0: nd_layer)                             &
!       Upward flux
    , flux_direct_clear(nd_flux_profile, 0: nd_layer)                   &
!       Clear direct flux
    , flux_down_clear(nd_flux_profile, 0: nd_layer)                     &
!       Clear total downward flux
    , flux_up_clear(nd_flux_profile, 0: nd_layer)
!       Clear upward flux
!                 Total Radiances
  REAL (RealK), INTENT(INOUT) ::                                        &
      i_direct(nd_radiance_profile, 0: nd_layer)                        &
!       Solar irradiance
    , radiance(nd_radiance_profile, nd_viewing_level                    &
        , nd_direction)
!       Radiance
!                   Rates of photolysis
  REAL (RealK), INTENT(INOUT) ::                                        &
      photolysis(nd_j_profile, nd_viewing_level)
!       Rates of photolysis

!                    Special Diagnostics:
  LOGICAL, INTENT(IN) ::                                                &
      l_blue_flux_surf
!       Flag to calculate blue fluxes at the surface
  REAL (RealK), INTENT(IN) ::                                           &
      weight_blue
!       Weights for blue fluxes in this band
  REAL (RealK), INTENT(INOUT) ::                                        &
      flux_direct_blue_surf(nd_flux_profile)                            &
!       Direct blue flux at the surface
    , flux_down_blue_surf(nd_flux_profile)                              &
!       Total downward blue flux at the surface
    , flux_up_blue_surf(nd_flux_profile)
!       Upward blue flux at the surface


! Local arguments.
  INTEGER                                                               &
      i                                                                 &
!       Loop variable
    , l                                                                 &
!       Loop variable
    , k
!       Loop variable

  INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
  INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
  REAL(KIND=jprb)               :: zhook_handle

  CHARACTER(LEN=*), PARAMETER :: RoutineName='AUGMENT_RADIANCE'


  IF (lhook) CALL dr_hook(RoutineName,zhook_in,zhook_handle)

  IF (.NOT.l_initial) THEN

!   Most commonly, this routine will be called to increment
!   rather than to initialize fluxes.

    IF ( (i_angular_integration == ip_two_stream).OR.                   &
         (i_angular_integration == ip_ir_gauss).OR.                     &
       ( (i_angular_integration == ip_spherical_harmonic).AND.          &
         (i_sph_mode == ip_sph_mode_flux) ) ) THEN

!     Increment the actual fluxes.
      IF (isolir == ip_solar) THEN
        DO i=0, n_layer
          DO l=1, n_profile
            flux_direct(l, i)=flux_direct(l, i)                         &
              +weight_incr*flux_direct_incr(l, i)
          END DO
        END DO
        IF (l_blue_flux_surf) THEN
          DO l=1, n_profile
            flux_up_blue_surf(l)=flux_up_blue_surf(l)                   &
              +weight_blue*flux_total_incr(l, 2*n_layer+1)
            flux_down_blue_surf(l)=flux_down_blue_surf(l)               &
              +weight_blue*flux_total_incr(l, 2*n_layer+2)
          END DO
          IF (isolir == ip_solar) THEN
            DO l=1, n_profile
              flux_direct_blue_surf(l)=flux_direct_blue_surf(l)         &
                +weight_blue*flux_direct_incr(l, n_layer)
            END DO
          END IF
        END IF
      END IF
      DO i=0, n_layer
        DO l=1, n_profile
          flux_up(l, i)=flux_up(l, i)                                   &
            +weight_incr*flux_total_incr(l, 2*i+1)
          flux_down(l, i)=flux_down(l, i)                               &
            +weight_incr*flux_total_incr(l, 2*i+2)
        END DO
      END DO

      IF (l_clear) THEN
        IF (isolir == ip_solar) THEN
          DO i=0, n_layer
            DO l=1, n_profile
              flux_direct_clear(l, i)=flux_direct_clear(l, i)           &
                +weight_incr*flux_direct_incr_clear(l, i)
            END DO
          END DO
        END IF
        DO i=0, n_layer
          DO l=1, n_profile
            flux_up_clear(l, i)=flux_up_clear(l, i)                     &
              +weight_incr*flux_total_incr_clear(l, 2*i+1)
            flux_down_clear(l, i)=flux_down_clear(l, i)                 &
              +weight_incr*flux_total_incr_clear(l, 2*i+2)
          END DO
        END DO
      END IF

    ELSE IF ( (i_angular_integration == ip_spherical_harmonic).AND.     &
              (i_sph_mode == ip_sph_mode_rad) ) THEN


      DO k=1, n_direction
        DO i=1, n_viewing_level
          DO l=1, n_profile
            radiance(l, i, k)=radiance(l, i, k)                         &
              +weight_incr*radiance_incr(l, i, k)
          END DO
        END DO
      END DO

      IF (isolir == ip_solar) THEN
        DO i=0, n_layer
          DO l=1, n_profile
            i_direct(l, i)=i_direct(l, i)                               &
              +weight_incr*i_direct_incr(l, i)
          END DO
        END DO
      END IF

    ELSE IF ( (i_angular_integration == ip_spherical_harmonic).AND.     &
              (i_sph_mode == ip_sph_mode_j) ) THEN

      DO i=1, n_viewing_level
        DO l=1, n_profile
          photolysis(l, i)=photolysis(l, i)                             &
            +weight_incr*photolysis_incr(l, i)
        END DO
      END DO

    END IF

  ELSE

!   Initialization of the radiance field takes place here.

    IF ( (i_angular_integration == ip_two_stream).OR.                   &
         (i_angular_integration == ip_ir_gauss).OR.                     &
       ( (i_angular_integration == ip_spherical_harmonic).AND.          &
          (i_sph_mode == ip_sph_mode_flux) ) ) THEN

!     Increment the actual fluxes.
      IF (isolir == ip_solar) THEN
        DO i=0, n_layer
          DO l=1, n_profile
            flux_direct(l, i)=weight_incr*flux_direct_incr(l, i)
          END DO
        END DO
        IF (l_blue_flux_surf) THEN
          DO l=1, n_profile
            flux_up_blue_surf(l)                                        &
              =weight_blue*flux_total_incr(l, 2*n_layer+1)
            flux_down_blue_surf(l)                                      &
              =weight_blue*flux_total_incr(l, 2*n_layer+2)
          END DO
          IF (isolir == ip_solar) THEN
            DO l=1, n_profile
              flux_direct_blue_surf(l)                                  &
                =weight_blue*flux_direct_incr(l, n_layer)
            END DO
          END IF
        END IF
      END IF
      DO i=0, n_layer
        DO l=1, n_profile
          flux_up(l, i)=weight_incr*flux_total_incr(l, 2*i+1)
          flux_down(l, i)=weight_incr*flux_total_incr(l, 2*i+2)
        END DO
      END DO

      IF (l_clear) THEN
        IF (isolir == ip_solar) THEN
          DO i=0, n_layer
            DO l=1, n_profile
              flux_direct_clear(l, i)                                   &
                =weight_incr*flux_direct_incr_clear(l, i)
            END DO
          END DO
        END IF
        DO i=0, n_layer
          DO l=1, n_profile
            flux_up_clear(l, i)                                         &
              =weight_incr*flux_total_incr_clear(l, 2*i+1)
            flux_down_clear(l, i)                                       &
              =weight_incr*flux_total_incr_clear(l, 2*i+2)
          END DO
        END DO
      END IF

    ELSE IF ( (i_angular_integration == ip_spherical_harmonic).AND.     &
              (i_sph_mode == ip_sph_mode_rad) ) THEN

!     Increment the radiances on levels where they are calculated.
      DO k=1, n_direction
        DO i=1, n_viewing_level
          DO l=1, n_profile
            radiance(l, i, k)=weight_incr*radiance_incr(l, i, k)
          END DO
        END DO
      END DO

      IF (isolir == ip_solar) THEN
        DO i=0, n_layer
          DO l=1, n_profile
            i_direct(l, i)=weight_incr*i_direct_incr(l, i)
          END DO
        END DO
      END IF

    ELSE IF ( (i_angular_integration == ip_spherical_harmonic).AND.     &
              (i_sph_mode == ip_sph_mode_j) ) THEN

      DO i=1, n_viewing_level
        DO l=1, n_profile
          photolysis(l, i)=weight_incr*photolysis_incr(l, i)
        END DO
      END DO

    END IF

  END IF


  IF (lhook) CALL dr_hook(RoutineName,zhook_out,zhook_handle)

END SUBROUTINE augment_radiance
