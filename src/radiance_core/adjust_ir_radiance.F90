! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Convert differential IR radiances or fluxes to actual ones.
!
!------------------------------------------------------------------------------
SUBROUTINE adjust_ir_radiance(control, dimen, atm, radout, &
    planck, i_band, l_clear)

  USE realtype_rd,  ONLY: RealK
  USE def_control,  ONLY: StrCtrl
  USE def_dimen,    ONLY: StrDim
  USE def_atm,      ONLY: StrAtm
  USE def_out,      ONLY: StrOut
  USE def_planck,   ONLY: StrPlanck
  USE rad_pcf
  USE rad_ccf, ONLY: pi
  USE yomhook, ONLY: lhook, dr_hook
  USE parkind1, ONLY: jprb, jpim

  IMPLICIT NONE


! Control options:
  TYPE(StrCtrl),     INTENT(IN)    :: control

! Dimensions:
  TYPE(StrDim),      INTENT(IN)    :: dimen

! Atmospheric properties:
  TYPE(StrAtm),      INTENT(IN)    :: atm

! Output fields:
  TYPE(StrOut),      INTENT(INOUT) :: radout


! Planckian emission fields
  TYPE(StrPlanck),   INTENT(IN)    :: planck

  INTEGER, INTENT(IN) :: i_band
!     Band being considered

  LOGICAL, INTENT(IN) :: l_clear
!     Calculate clear-sky fluxes


! Local arguments
  INTEGER :: i, id, l
!     Loop variables

  INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
  INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
  REAL(KIND=jprb)               :: zhook_handle

  CHARACTER(LEN=*), PARAMETER :: RoutineName='ADJUST_IR_RADIANCE'


  IF (lhook) CALL dr_hook(RoutineName,zhook_in,zhook_handle)

  IF ( (control%i_angular_integration == ip_two_stream).OR. &
       (control%i_angular_integration == ip_ir_gauss) ) THEN

    DO i=0, atm%n_layer
      DO l=1, atm%n_profile
        radout%flux_up(l, i, control%map_channel(i_band)) = &
          radout%flux_up(l, i, control%map_channel(i_band)) &
          + planck%flux(l, i)
        radout%flux_down(l, i, control%map_channel(i_band)) = &
          radout%flux_down(l, i, control%map_channel(i_band)) &
          + planck%flux(l, i)
      END DO
    END DO
    IF (l_clear) THEN
      DO i=0, atm%n_layer
        DO l=1, atm%n_profile
          radout%flux_up_clear(l, i, control%map_channel(i_band)) = &
            radout%flux_up_clear(l, i, control%map_channel(i_band)) &
            + planck%flux(l, i)
          radout%flux_down_clear(l, i, control%map_channel(i_band)) = &
            radout%flux_down_clear(l, i, control%map_channel(i_band)) &
            + planck%flux(l, i)
        END DO
      END DO
    END IF

    IF (control%l_flux_up_band) THEN
      DO i=0, atm%n_layer
        DO l=1, atm%n_profile
          radout%flux_up_band(l, i, i_band) = &
            radout%flux_up_band(l, i, i_band) + planck%flux(l, i)
        END DO
      END DO
    END IF
    IF (control%l_flux_down_band) THEN
      DO i=0, atm%n_layer
        DO l=1, atm%n_profile
          radout%flux_down_band(l, i, i_band) = &
            radout%flux_down_band(l, i, i_band) + planck%flux(l, i)
        END DO
      END DO
    END IF
    IF (l_clear) THEN
      IF (control%l_flux_up_clear_band) THEN
        DO i=0, atm%n_layer
          DO l=1, atm%n_profile
            radout%flux_up_clear_band(l, i, i_band) = &
              radout%flux_up_clear_band(l, i, i_band) + planck%flux(l, i)
          END DO
        END DO
      END IF
      IF (control%l_flux_down_clear_band) THEN
        DO i=0, atm%n_layer
          DO l=1, atm%n_profile
            radout%flux_down_clear_band(l, i, i_band) = &
              radout%flux_down_clear_band(l, i, i_band) + planck%flux(l, i)
          END DO
        END DO
      END IF
    END IF

  ELSE IF (control%i_angular_integration == ip_spherical_harmonic) THEN

!   Planckian radiances are always used with spherical harmonics,
!   even when calculating fluxes. The number of levels should
!   be set appropriately above.
    IF (control%i_sph_mode == ip_sph_mode_flux) THEN
      DO i=0, atm%n_layer
        DO l=1, atm%n_profile
          radout%flux_up(l, i, control%map_channel(i_band)) = &
            radout%flux_up(l, i, control%map_channel(i_band)) &
            + pi*planck%radiance(l, i+1)
          radout%flux_down(l, i, control%map_channel(i_band)) = &
            radout%flux_down(l, i, control%map_channel(i_band)) &
            + pi*planck%radiance(l, i+1)
        END DO
      END DO
      IF (control%l_flux_up_band) THEN
        DO i=0, atm%n_layer
          DO l=1, atm%n_profile
            radout%flux_up_band(l, i, i_band) = &
              radout%flux_up_band(l, i, i_band) &
              + pi*planck%radiance(l, i+1)
          END DO
        END DO
      END IF
      IF (control%l_flux_down_band) THEN
        DO i=0, atm%n_layer
          DO l=1, atm%n_profile
            radout%flux_down_band(l, i, i_band) = &
              radout%flux_down_band(l, i, i_band) &
              + pi*planck%radiance(l, i+1)
          END DO
        END DO
      END IF
    ELSE IF (control%i_sph_mode == ip_sph_mode_rad) THEN
      DO id=1, atm%n_direction
        DO i=1, atm%n_viewing_level
          DO l=1, atm%n_profile
            radout%radiance(l, i, id, control%map_channel(i_band)) = &
              radout%radiance(l, i, id, control%map_channel(i_band)) &
              + planck%radiance(l, i)
          END DO
        END DO
      END DO
    END IF

  END IF

  IF (lhook) CALL dr_hook(RoutineName,zhook_out,zhook_handle)

END SUBROUTINE adjust_ir_radiance
