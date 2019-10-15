! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Subroutine to increment radiances or fluxes.
!
! Method:
!   The arrays holding the summed fluxes or radiances are
!   incremented by a weighted sum of the variables suffixed
!   with _INCR. Arguments specify which arrays are to be
!   incremented.
!
!- ---------------------------------------------------------------------
SUBROUTINE augment_radiance(control, radout, i_band                     &
    , n_profile, n_layer, n_viewing_level, n_direction                  &
    , l_clear, l_initial, l_initial_band                                &
    , weight_incr, weight_blue                                          &
!                 Actual radiances
    , i_direct                                                          &
!                 Increments to radiances
    , flux_direct_incr, flux_total_incr                                 &
    , i_direct_incr, radiance_incr, photolysis_incr                     &
    , flux_direct_incr_clear, flux_total_incr_clear                     &
    , sph, contrib_funci_incr, contrib_funcf_incr                       &
!                 Dimensions
    , nd_flux_profile, nd_radiance_profile, nd_j_profile                &
    , nd_layer, nd_viewing_level, nd_direction                          &
    )


  USE realtype_rd, ONLY: RealK
  USE def_control, ONLY: StrCtrl
  USE def_out,     ONLY: StrOut
  USE def_spherical_geometry, ONLY: StrSphGeo
  USE rad_pcf
  USE yomhook, ONLY: lhook, dr_hook
  USE parkind1, ONLY: jprb, jpim

  IMPLICIT NONE


! Control options:
  TYPE(StrCtrl), INTENT(IN)    :: control

! Output fields:
  TYPE(StrOut),  INTENT(INOUT) :: radout

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
      i_band
!       Band being considered
  INTEGER, INTENT(IN) ::                                                &
      n_profile                                                         &
!       Number of profiles
    , n_layer                                                           &
!       Number of layers
    , n_viewing_level                                                   &
!       Number of levels where the radiance is calculated
    , n_direction
!       Number of viewing directions
  LOGICAL, INTENT(IN) ::                                                &
      l_clear                                                           &
!       Clear fluxes calculated
    , l_initial                                                         &
!       Logical to perform initialization instead of incrementing
    , l_initial_band
!       Logical to perform initialisation for band-by-band diagnostics
  
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

  TYPE(StrSphGeo), INTENT(IN) :: sph
!   Spherical geometry fields

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
  REAL (RealK), INTENT(IN) ::                                           &
      contrib_funci_incr(nd_flux_profile, nd_layer)
!       Contribution function (intensity)
  REAL (RealK), INTENT(IN) ::                                           &
      contrib_funcf_incr(nd_flux_profile, nd_layer)
!       Contribution function (flux)

!                 Total Radiances
  REAL (RealK), INTENT(INOUT) ::                                        &
      i_direct(nd_radiance_profile, 0: nd_layer)
!       Solar irradiance

!                    Special Diagnostics:
  REAL (RealK), INTENT(IN) ::                                           &
      weight_blue
!       Weights for blue fluxes in this band


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

    IF ( (control%i_angular_integration == ip_two_stream).OR. &
         (control%i_angular_integration == ip_ir_gauss).OR. &
       ( (control%i_angular_integration == ip_spherical_harmonic).AND. &
         (control%i_sph_mode == ip_sph_mode_flux) ) ) THEN

!     Increment the actual fluxes.
      IF (control%isolir == ip_solar) THEN
        IF (control%l_spherical_solar) THEN
          DO i=0, n_layer+1
            DO l=1, n_profile
              radout%flux_direct_sph(l, i, control%map_channel(i_band)) &
                = radout%flux_direct_sph(l, i, control%map_channel(i_band)) &
                + weight_incr*sph%allsky%flux_direct(l, i)
            END DO
          END DO
          DO i=1, n_layer
            DO l=1, n_profile
              radout%flux_direct_div(l, i, control%map_channel(i_band)) &
                = radout%flux_direct_div(l, i, control%map_channel(i_band)) &
                + weight_incr*sph%allsky%flux_direct_div(l, i)
            END DO
          END DO
          IF (control%l_blue_flux_surf) THEN
            DO l=1, n_profile
              radout%flux_direct_blue_surf(l) &
                = radout%flux_direct_blue_surf(l) &
                + weight_blue*sph%allsky%flux_direct(l, n_layer+1)
            END DO
          END IF
        ELSE
          DO i=0, n_layer
            DO l=1, n_profile
              radout%flux_direct(l, i, control%map_channel(i_band)) &
                = radout%flux_direct(l, i, control%map_channel(i_band)) &
                + weight_incr*flux_direct_incr(l, i)
            END DO
          END DO
          IF (control%l_blue_flux_surf) THEN
            DO l=1, n_profile
              radout%flux_direct_blue_surf(l) &
                = radout%flux_direct_blue_surf(l) &
                + weight_blue*flux_direct_incr(l, n_layer)
            END DO
          END IF
        END IF
        IF (control%l_blue_flux_surf) THEN
          DO l=1, n_profile
            radout%flux_up_blue_surf(l) &
              = radout%flux_up_blue_surf(l) &
              + weight_blue*flux_total_incr(l, 2*n_layer+1)
            radout%flux_down_blue_surf(l) &
              = radout%flux_down_blue_surf(l) &
              + weight_blue*flux_total_incr(l, 2*n_layer+2)
          END DO
        END IF
      END IF
      DO i=0, n_layer
        DO l=1, n_profile
          radout%flux_up(l, i, control%map_channel(i_band)) &
            = radout%flux_up(l, i, control%map_channel(i_band)) &
            + weight_incr*flux_total_incr(l, 2*i+1)
          radout%flux_down(l, i, control%map_channel(i_band)) &
            = radout%flux_down(l, i, control%map_channel(i_band)) &
            + weight_incr*flux_total_incr(l, 2*i+2)
        END DO
      END DO

      IF (l_clear) THEN
        IF (control%isolir == ip_solar) THEN
          IF (control%l_spherical_solar) THEN
            DO i=0, n_layer+1
              DO l=1, n_profile
                radout%flux_direct_clear_sph(l,i,control%map_channel(i_band)) &
                 =radout%flux_direct_clear_sph(l,i,control%map_channel(i_band))&
                 +weight_incr*sph%clear%flux_direct(l, i)
              END DO
            END DO
            DO i=1, n_layer
              DO l=1, n_profile
                radout%flux_direct_clear_div(l,i,control%map_channel(i_band)) &
                 =radout%flux_direct_clear_div(l,i,control%map_channel(i_band))&
                 +weight_incr*sph%clear%flux_direct_div(l, i)
              END DO
            END DO
          ELSE
            DO i=0, n_layer
              DO l=1, n_profile
                radout%flux_direct_clear(l, i, control%map_channel(i_band)) &
                  = radout%flux_direct_clear(l,i, control%map_channel(i_band)) &
                  + weight_incr*flux_direct_incr_clear(l, i)
              END DO
            END DO
          END IF
        END IF
        DO i=0, n_layer
          DO l=1, n_profile
            radout%flux_up_clear(l, i, control%map_channel(i_band)) &
              = radout%flux_up_clear(l, i, control%map_channel(i_band)) &
              + weight_incr*flux_total_incr_clear(l, 2*i+1)
            radout%flux_down_clear(l, i, control%map_channel(i_band)) &
              = radout%flux_down_clear(l, i, control%map_channel(i_band)) &
              + weight_incr*flux_total_incr_clear(l, 2*i+2)
          END DO
        END DO
      END IF

      IF (control%l_contrib_func) THEN
        DO i=1, n_layer
          DO l=1, n_profile
            radout%contrib_funci(l, i, control%map_channel(i_band)) &
              = radout%contrib_funci(l, i, control%map_channel(i_band)) &
              + weight_incr*contrib_funci_incr(l, i)
            radout%contrib_funcf(l, i, control%map_channel(i_band)) &
              = radout%contrib_funcf(l, i, control%map_channel(i_band)) &
              + weight_incr*contrib_funcf_incr(l, i)
          END DO
        END DO
      END IF

    ELSE IF ( (control%i_angular_integration == ip_spherical_harmonic).AND. &
              (control%i_sph_mode == ip_sph_mode_rad) ) THEN


      DO k=1, n_direction
        DO i=1, n_viewing_level
          DO l=1, n_profile
            radout%radiance(l, i, k, control%map_channel(i_band)) &
              = radout%radiance(l, i, k, control%map_channel(i_band)) &
              + weight_incr*radiance_incr(l, i, k)
          END DO
        END DO
      END DO

      IF (control%isolir == ip_solar) THEN
        DO i=0, n_layer
          DO l=1, n_profile
            i_direct(l, i)=i_direct(l, i) &
              +weight_incr*i_direct_incr(l, i)
          END DO
        END DO
      END IF

    ELSE IF ( (control%i_angular_integration == ip_spherical_harmonic).AND. &
              (control%i_sph_mode == ip_sph_mode_j) ) THEN

      DO i=1, n_viewing_level
        DO l=1, n_profile
          radout%photolysis(l, i, control%map_channel(i_band)) &
            = radout%photolysis(l, i, control%map_channel(i_band)) &
            + weight_incr*photolysis_incr(l, i)
        END DO
      END DO

    END IF

  ELSE

!   Initialization of the radiance field takes place here.

    IF ( (control%i_angular_integration == ip_two_stream).OR. &
         (control%i_angular_integration == ip_ir_gauss).OR. &
       ( (control%i_angular_integration == ip_spherical_harmonic).AND. &
          (control%i_sph_mode == ip_sph_mode_flux) ) ) THEN

!     Increment the actual fluxes.
      IF (control%isolir == ip_solar) THEN
        IF (control%l_spherical_solar) THEN
          DO i=0, n_layer+1
            DO l=1, n_profile
              radout%flux_direct_sph(l, i, control%map_channel(i_band)) &
                = weight_incr*sph%allsky%flux_direct(l, i)
            END DO
          END DO
          DO i=1, n_layer
            DO l=1, n_profile
              radout%flux_direct_div(l, i, control%map_channel(i_band)) &
                = weight_incr*sph%allsky%flux_direct_div(l, i)
            END DO
          END DO
          IF (control%l_blue_flux_surf) THEN
            DO l=1, n_profile
              radout%flux_direct_blue_surf(l) &
                = weight_blue*sph%allsky%flux_direct(l, n_layer+1)
            END DO
          END IF
        ELSE
          DO i=0, n_layer
            DO l=1, n_profile
              radout%flux_direct(l, i, control%map_channel(i_band)) &
                = weight_incr*flux_direct_incr(l, i)
            END DO
          END DO
          IF (control%l_blue_flux_surf) THEN
            DO l=1, n_profile
              radout%flux_direct_blue_surf(l) &
                =weight_blue*flux_direct_incr(l, n_layer)
            END DO
          END IF
        END IF
        IF (control%l_blue_flux_surf) THEN
          DO l=1, n_profile
            radout%flux_up_blue_surf(l) &
              =weight_blue*flux_total_incr(l, 2*n_layer+1)
            radout%flux_down_blue_surf(l) &
              =weight_blue*flux_total_incr(l, 2*n_layer+2)
          END DO
        END IF
      END IF
      DO i=0, n_layer
        DO l=1, n_profile
          radout%flux_up(l, i, control%map_channel(i_band)) &
            = weight_incr*flux_total_incr(l, 2*i+1)
          radout%flux_down(l, i, control%map_channel(i_band)) &
            = weight_incr*flux_total_incr(l, 2*i+2)
        END DO
      END DO

      IF (l_clear) THEN
        IF (control%isolir == ip_solar) THEN
          IF (control%l_spherical_solar) THEN
            DO i=0, n_layer+1
              DO l=1, n_profile
                radout%flux_direct_clear_sph(l,i, control%map_channel(i_band)) &
                  = weight_incr*sph%clear%flux_direct(l, i)
              END DO
            END DO
            DO i=1, n_layer
              DO l=1, n_profile
                radout%flux_direct_clear_div(l,i, control%map_channel(i_band)) &
                  = weight_incr*sph%clear%flux_direct_div(l, i)
              END DO
            END DO
          ELSE
            DO i=0, n_layer
              DO l=1, n_profile
                radout%flux_direct_clear(l, i, control%map_channel(i_band)) &
                  = weight_incr*flux_direct_incr_clear(l, i)
              END DO
            END DO
          END IF
        END IF
        DO i=0, n_layer
          DO l=1, n_profile
            radout%flux_up_clear(l, i, control%map_channel(i_band)) &
              = weight_incr*flux_total_incr_clear(l, 2*i+1)
            radout%flux_down_clear(l, i, control%map_channel(i_band)) &
              = weight_incr*flux_total_incr_clear(l, 2*i+2)
          END DO
        END DO
      ELSE
        IF (control%isolir == ip_solar) THEN
          IF (control%l_spherical_solar) THEN
            DO i=0, n_layer+1
              DO l=1, n_profile
                radout%flux_direct_clear_sph(l,i, control%map_channel(i_band)) &
                  = 0.0_RealK
              END DO
            END DO
            DO i=1, n_layer
              DO l=1, n_profile
                radout%flux_direct_clear_div(l,i, control%map_channel(i_band)) &
                  = 0.0_RealK
              END DO
            END DO
          ELSE
            DO i=0, n_layer
              DO l=1, n_profile
                radout%flux_direct_clear(l, i, control%map_channel(i_band)) &
                  = 0.0_RealK
              END DO
            END DO
          END IF
        END IF
        DO i=0, n_layer
          DO l=1, n_profile
            radout%flux_up_clear(l, i, control%map_channel(i_band)) &
              = 0.0_RealK
            radout%flux_down_clear(l, i, control%map_channel(i_band)) &
              = 0.0_RealK
          END DO
        END DO
      END IF

      IF (control%l_contrib_func) THEN
        DO i=1, n_layer
          DO l=1, n_profile
            radout%contrib_funci(l, i, control%map_channel(i_band)) &
              = weight_incr*contrib_funci_incr(l, i)
            radout%contrib_funcf(l, i, control%map_channel(i_band)) &
              = weight_incr*contrib_funcf_incr(l, i)
          END DO
        END DO
      END IF

    ELSE IF ( (control%i_angular_integration == ip_spherical_harmonic).AND. &
              (control%i_sph_mode == ip_sph_mode_rad) ) THEN

!     Increment the radiances on levels where they are calculated.
      DO k=1, n_direction
        DO i=1, n_viewing_level
          DO l=1, n_profile
            radout%radiance(l, i, k, control%map_channel(i_band)) &
              = weight_incr*radiance_incr(l, i, k)
          END DO
        END DO
      END DO

      IF (control%isolir == ip_solar) THEN
        DO i=0, n_layer
          DO l=1, n_profile
            i_direct(l, i)=weight_incr*i_direct_incr(l, i)
          END DO
        END DO
      END IF

    ELSE IF ( (control%i_angular_integration == ip_spherical_harmonic).AND. &
              (control%i_sph_mode == ip_sph_mode_j) ) THEN

      DO i=1, n_viewing_level
        DO l=1, n_profile
          radout%photolysis(l, i, control%map_channel(i_band)) &
            = weight_incr*photolysis_incr(l, i)
        END DO
      END DO

    END IF

  END IF


  IF (l_initial_band) THEN

!   Initialise the band-by-band fluxes
    IF (control%l_flux_direct_band) THEN
      DO i=0, n_layer
        DO l=1, n_profile
          radout%flux_direct_band(l, i, i_band) &
            = weight_incr*flux_direct_incr(l, i)
        END DO
      END DO
    END IF
    IF (control%l_flux_direct_div_band .AND. &
        control%l_spherical_solar) THEN
      DO i=1, n_layer
        DO l=1, n_profile
          radout%flux_direct_div_band(l, i, i_band) &
            = weight_incr*sph%allsky%flux_direct_div(l, i)
        END DO
      END DO
    END IF
    IF (control%l_flux_direct_sph_band .AND. &
        control%l_spherical_solar) THEN
      DO i=0, n_layer+1
        DO l=1, n_profile
          radout%flux_direct_sph_band(l, i, i_band) &
            = weight_incr*sph%allsky%flux_direct(l, i)
        END DO
      END DO
    END IF
    IF (control%l_flux_down_band) THEN
      DO i=0, n_layer
        DO l=1, n_profile
          radout%flux_down_band(l, i, i_band) &
            = weight_incr*flux_total_incr(l, 2*i+2)
        END DO
      END DO
    END IF
    IF (control%l_flux_up_band) THEN
      DO i=0, n_layer
        DO l=1, n_profile
          radout%flux_up_band(l, i, i_band) &
            = weight_incr*flux_total_incr(l, 2*i+1)
        END DO
      END DO
    END IF

    IF (l_clear) THEN

      IF (control%l_flux_direct_clear_band .OR. &
           (.NOT.control%l_spherical_solar .AND. &
             ( control%l_cloud_extinction .OR. &
               control%l_ls_cloud_extinction .OR. &
               control%l_cnv_cloud_extinction ) ) ) THEN
        DO i=0, n_layer
          DO l=1, n_profile
            radout%flux_direct_clear_band(l, i, i_band) &
              = weight_incr*flux_direct_incr_clear(l, i)
          END DO
        END DO
      END IF
      IF (control%l_flux_direct_clear_div_band .AND. &
          control%l_spherical_solar) THEN
        DO i=1, n_layer
          DO l=1, n_profile
            radout%flux_direct_clear_div_band(l, i, i_band) &
              = weight_incr*sph%clear%flux_direct_div(l, i)
          END DO
        END DO
      END IF
      IF (control%l_spherical_solar .AND. &
           (control%l_flux_direct_clear_sph_band .OR. &
            control%l_cloud_extinction .OR. &
            control%l_ls_cloud_extinction .OR. &
            control%l_cnv_cloud_extinction)) THEN
        DO i=0, n_layer+1
          DO l=1, n_profile
            radout%flux_direct_clear_sph_band(l, i, i_band) &
              = weight_incr*sph%clear%flux_direct(l, i)
          END DO
        END DO
      END IF
      IF (control%l_flux_down_clear_band) THEN
        DO i=0, n_layer
          DO l=1, n_profile
            radout%flux_down_clear_band(l, i, i_band) &
              = weight_incr*flux_total_incr_clear(l, 2*i+2)
          END DO
        END DO
      END IF
      IF (control%l_flux_up_clear_band .OR. &
          control%l_cloud_absorptivity .OR. &
          control%l_ls_cloud_absorptivity .OR. &
          control%l_cnv_cloud_absorptivity) THEN
        DO i=0, n_layer
          DO l=1, n_profile
            radout%flux_up_clear_band(l, i, i_band) &
              = weight_incr*flux_total_incr_clear(l, 2*i+1)
          END DO
        END DO
      END IF

    ELSE ! .NOT. l_clear_band

      IF (control%l_flux_direct_clear_band .OR. &
           (.NOT.control%l_spherical_solar .AND. &
             ( control%l_cloud_extinction .OR. &
               control%l_ls_cloud_extinction .OR. &
               control%l_cnv_cloud_extinction ) ) ) THEN
        DO i=0, n_layer
          DO l=1, n_profile
            radout%flux_direct_clear_band(l, i, i_band) = 0.0_RealK
          END DO
        END DO
      END IF
      IF (control%l_flux_direct_clear_div_band .AND. &
          control%l_spherical_solar) THEN
        DO i=1, n_layer
          DO l=1, n_profile
            radout%flux_direct_clear_div_band(l, i, i_band) = 0.0_RealK
          END DO
        END DO
      END IF
      IF (control%l_spherical_solar .AND. &
           (control%l_flux_direct_clear_sph_band .OR. &
            control%l_cloud_extinction .OR. &
            control%l_ls_cloud_extinction .OR. &
            control%l_cnv_cloud_extinction)) THEN
        DO i=0, n_layer+1
          DO l=1, n_profile
            radout%flux_direct_clear_sph_band(l, i, i_band) = 0.0_RealK
          END DO
        END DO
      END IF
      IF (control%l_flux_down_clear_band) THEN
        DO i=0, n_layer
          DO l=1, n_profile
            radout%flux_down_clear_band(l, i, i_band) = 0.0_RealK
          END DO
        END DO
      END IF
      IF (control%l_flux_up_clear_band .OR. &
          control%l_cloud_absorptivity .OR. &
          control%l_ls_cloud_absorptivity .OR. &
          control%l_cnv_cloud_absorptivity) THEN
        DO i=0, n_layer
          DO l=1, n_profile
            radout%flux_up_clear_band(l, i, i_band) = 0.0_RealK
          END DO
        END DO
      END IF

    END IF

    IF (control%l_contrib_func_band) THEN
      DO i=1, n_layer
        DO l=1, n_profile
          radout%contrib_funci_band(l, i, i_band) &
            = weight_incr*contrib_funci_incr(l, i)
          radout%contrib_funcf_band(l, i, i_band) &
            = weight_incr*contrib_funcf_incr(l, i)
        END DO
      END DO
    END IF

  ELSE

!   Increment the band-by-band fluxes
    IF (control%l_flux_direct_band) THEN
      DO i=0, n_layer
        DO l=1, n_profile
          radout%flux_direct_band(l, i, i_band) &
            = radout%flux_direct_band(l, i, i_band) &
            + weight_incr*flux_direct_incr(l, i)
        END DO
      END DO
    END IF
    IF (control%l_flux_direct_div_band .AND. &
        control%l_spherical_solar) THEN
      DO i=1, n_layer
        DO l=1, n_profile
          radout%flux_direct_div_band(l, i, i_band) &
            = radout%flux_direct_div_band(l, i, i_band) &
            + weight_incr*sph%allsky%flux_direct_div(l, i)
        END DO
      END DO
    END IF
    IF (control%l_flux_direct_sph_band .AND. &
        control%l_spherical_solar) THEN
      DO i=0, n_layer+1
        DO l=1, n_profile
          radout%flux_direct_sph_band(l, i, i_band) &
            = radout%flux_direct_sph_band(l, i, i_band) &
            + weight_incr*sph%allsky%flux_direct(l, i)
        END DO
      END DO
    END IF
    IF (control%l_flux_down_band) THEN
      DO i=0, n_layer
        DO l=1, n_profile
          radout%flux_down_band(l, i, i_band) &
            = radout%flux_down_band(l, i, i_band) &
            + weight_incr*flux_total_incr(l, 2*i+2)
        END DO
      END DO
    END IF
    IF (control%l_flux_up_band) THEN
      DO i=0, n_layer
        DO l=1, n_profile
          radout%flux_up_band(l, i, i_band) &
            = radout%flux_up_band(l, i, i_band) &
            + weight_incr*flux_total_incr(l, 2*i+1)
        END DO
      END DO
    END IF

    IF (l_clear) THEN
      IF (control%l_flux_direct_clear_band .OR. &
           (.NOT.control%l_spherical_solar .AND. &
             ( control%l_cloud_extinction .OR. &
               control%l_ls_cloud_extinction .OR. &
               control%l_cnv_cloud_extinction ) ) ) THEN
        DO i=0, n_layer
          DO l=1, n_profile
            radout%flux_direct_clear_band(l, i, i_band) &
              = radout%flux_direct_clear_band(l, i, i_band) &
              + weight_incr*flux_direct_incr_clear(l, i)
          END DO
        END DO
      END IF
      IF (control%l_flux_direct_clear_div_band .AND. &
          control%l_spherical_solar) THEN
        DO i=1, n_layer
          DO l=1, n_profile
            radout%flux_direct_clear_div_band(l, i, i_band) &
              = radout%flux_direct_clear_div_band(l, i, i_band) &
              + weight_incr*sph%clear%flux_direct_div(l, i)
          END DO
        END DO
      END IF
      IF (control%l_spherical_solar .AND. &
           (control%l_flux_direct_clear_sph_band .OR. &
            control%l_cloud_extinction .OR. &
            control%l_ls_cloud_extinction .OR. &
            control%l_cnv_cloud_extinction)) THEN
        DO i=0, n_layer+1
          DO l=1, n_profile
            radout%flux_direct_clear_sph_band(l, i, i_band) &
              = radout%flux_direct_clear_sph_band(l, i, i_band) &
              + weight_incr*sph%clear%flux_direct(l, i)
          END DO
        END DO
      END IF
      IF (control%l_flux_down_clear_band) THEN
        DO i=0, n_layer
          DO l=1, n_profile
            radout%flux_down_clear_band(l, i, i_band) &
              = radout%flux_down_clear_band(l, i, i_band) &
              + weight_incr*flux_total_incr_clear(l, 2*i+2)
          END DO
        END DO
      END IF
      IF (control%l_flux_up_clear_band .OR. &
          control%l_cloud_absorptivity .OR. &
          control%l_ls_cloud_absorptivity .OR. &
          control%l_cnv_cloud_absorptivity) THEN
        DO i=0, n_layer
          DO l=1, n_profile
            radout%flux_up_clear_band(l, i, i_band) &
              = radout%flux_up_clear_band(l, i, i_band) &
              + weight_incr*flux_total_incr_clear(l, 2*i+1)
          END DO
        END DO
      END IF
    END IF

    IF (control%l_contrib_func_band) THEN
      DO i=1, n_layer
        DO l=1, n_profile
          radout%contrib_funci_band(l, i, i_band) &
            = radout%contrib_funci_band(l, i, i_band) &
            + weight_incr*contrib_funci_incr(l, i)
          radout%contrib_funcf_band(l, i, i_band) &
            = radout%contrib_funcf_band(l, i, i_band) &
            + weight_incr*contrib_funcf_incr(l, i)
        END DO
      END DO
    END IF

  END IF

  IF (lhook) CALL dr_hook(RoutineName,zhook_out,zhook_handle)

END SUBROUTINE augment_radiance
