! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
!  Subroutine to convert differential IR radiances to actual ones.
!
! Purpose:
!   This subroutine receives differntial IR radiances or fluxes
!   and returns actual values.
!
! Method:
!   Striaghtforward.
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: Radiance Core
!
!- ---------------------------------------------------------------------
SUBROUTINE adjust_ir_radiance(n_profile, n_layer, n_viewing_level       &
    , n_direction, i_angular_integration, i_sph_mode                    &
    , planck_flux, planck_radiance                                      &
    , flux_down, flux_up, radiance                                      &
    , l_clear, flux_down_clear, flux_up_clear                           &
    , nd_2sg_profile, nd_flux_profile, nd_radiance_profile              &
    , nd_layer, nd_direction, nd_viewing_level                          &
    )


  USE realtype_rd, ONLY: RealK
  USE rad_pcf
  USE rad_ccf, ONLY: pi
  USE yomhook, ONLY: lhook, dr_hook
  USE parkind1, ONLY: jprb, jpim

  IMPLICIT NONE


! Dummy array sizes
  INTEGER, INTENT(IN) ::                                                &
      nd_2sg_profile                                                    &
!       Size allocated for profiles of fluxes
    , nd_flux_profile                                                   &
!       Size allocated for profiles of output fluxes
    , nd_radiance_profile                                               &
!       Size allocated for atmospheric profiles for
!       quantities used in calculations of radiances
    , nd_layer                                                          &
!       Size allocated for atmospheric layers
    , nd_viewing_level                                                  &
!       Size allocated for levels for radiances
    , nd_direction
!       Size allocated for directions


! Dummy arguments
  INTEGER, INTENT(IN) ::                                                &
      n_profile                                                         &
!       Number of atmospheric profiles
    , n_layer                                                           &
!       Number of atmospheric layers
    , n_direction                                                       &
!       Number of directions
    , n_viewing_level
!       Number of levels at which to calculate radiances
  INTEGER, INTENT(IN) ::                                                &
      i_angular_integration                                             &
!       Angular integration scheme
    , i_sph_mode
!       Mode in which the spherical solver is used
  REAL (RealK), INTENT(IN) ::                                           &
      planck_flux(nd_flux_profile, 0: nd_layer)                         &
!       Planckian fluxes
    , planck_radiance(nd_radiance_profile, nd_viewing_level)
!       Planckian radiances
  LOGICAL, INTENT(IN) ::                                                &
      l_clear
!       Calculate clear-sky fluxes

  REAL (RealK), INTENT(INOUT) ::                                        &
      flux_down(nd_flux_profile, 0: nd_layer)                           &
!       Downward fluxes
    , flux_up(nd_flux_profile, 0: nd_layer)                             &
!       Upward fluxes
    , radiance(nd_radiance_profile, nd_viewing_level, nd_direction)     &
!       Radiances in specified directions
    , flux_down_clear(nd_2sg_profile, 0: nd_layer)                      &
!       Clear downward flux
    , flux_up_clear(nd_2sg_profile, 0: nd_layer)
!       Clear upward flux


! Local arguments
  INTEGER                                                               &
      i                                                                 &
!       Loop variable
    , id                                                                &
!       Loop variable
    , l
!       Loop variable

  INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
  INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
  REAL(KIND=jprb)               :: zhook_handle

  CHARACTER(LEN=*), PARAMETER :: RoutineName='ADJUST_IR_RADIANCE'


  IF (lhook) CALL dr_hook(RoutineName,zhook_in,zhook_handle)

  IF ( (i_angular_integration == ip_two_stream).OR.                     &
       (i_angular_integration == ip_ir_gauss) ) THEN

    DO i=0, n_layer
      DO l=1, n_profile
        flux_up(l, i)=flux_up(l, i)+planck_flux(l, i)
        flux_down(l, i)=flux_down(l, i)+planck_flux(l, i)
      END DO
    END DO
    IF (l_clear) THEN
      DO i=0, n_layer
        DO l=1, n_profile
          flux_up_clear(l,i)=flux_up_clear(l,i)+planck_flux(l,i)
          flux_down_clear(l,i)=flux_down_clear(l,i)+planck_flux(l,i)
        END DO
      END DO
    END IF

  ELSE IF (i_angular_integration == ip_spherical_harmonic) THEN

!   Planckian radiances are always used with spherical harmonics,
!   even when calculating fluxes. The number of levels should
!   be set appropriately above.
    IF (i_sph_mode == ip_sph_mode_flux) THEN
      DO i=0, n_layer
        DO l=1, n_profile
          flux_up(l, i)=flux_up(l, i)+pi*planck_radiance(l, i+1)
          flux_down(l, i)=flux_down(l, i)                               &
            +pi*planck_radiance(l, i+1)
        END DO
      END DO
    ELSE IF (i_sph_mode == ip_sph_mode_rad) THEN
      DO id=1, n_direction
        DO i=1, n_viewing_level
          DO l=1, n_profile
            radiance(l, i, id)=radiance(l, i, id)                       &
              +planck_radiance(l, i)
          END DO
        END DO
      END DO
    END IF

  END IF

  IF (lhook) CALL dr_hook(RoutineName,zhook_out,zhook_handle)

END SUBROUTINE adjust_ir_radiance
