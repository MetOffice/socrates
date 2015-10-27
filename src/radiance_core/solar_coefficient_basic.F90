! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
!  Subroutine to calculate the basic coefficients for the solar beam.
!
! Method:
!   Straightforward.
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: Radiance Core
!
!- ---------------------------------------------------------------------
SUBROUTINE solar_coefficient_basic(ierr                                 &
    , n_profile, i_layer_first, i_layer_last                            &
    , omega, asymmetry, sec_0                                           &
    , i_2stream                                                         &
    , sum, diff, lambda                                                 &
    , gamma_up, gamma_down                                              &
    , nd_profile, id_lt, id_lb                                          &
    )


  USE realtype_rd, ONLY: RealK
  USE rad_pcf
  USE yomhook, ONLY: lhook, dr_hook
  USE parkind1, ONLY: jprb, jpim
  USE ereport_mod, ONLY: ereport
  USE errormessagelength_mod, ONLY: errormessagelength

  IMPLICIT NONE


! Sizes of dummy arrays.
  INTEGER                                                               &
      nd_profile                                                        &
!       Size allocated for atmospheric profiles
    , id_lt                                                             &
!       Topmost declared layer
    , id_lb
!       Bottom declared layer

! Dummy variables.
  INTEGER, INTENT(INOUT) ::                                             &
      ierr
!       Error flag
  INTEGER, INTENT(IN) ::                                                &
      n_profile                                                         &
!       Number of profiles
    , i_layer_first                                                     &
!       First layer to consider
    , i_layer_last                                                      &
!       First layer to consider
    , i_2stream
!       Two-stream scheme

  REAL (RealK), INTENT(IN) ::                                           &
      omega(nd_profile, id_lt: id_lb)                                   &
!       Albedo of single scattering
    , asymmetry(nd_profile, id_lt: id_lb)                               &
!       Asymmetry
    , sec_0(nd_profile)
!       Secant of solar zenith angle

! Basic two-stream coefficients:
  REAL (RealK), INTENT(INOUT) ::                                        &
      sum(nd_profile, id_lt: id_lb)                                     &
!       Sum of two-stream coefficients
    , diff(nd_profile, id_lt: id_lb)                                    &
!       Difference of two-stream coefficients
    , lambda(nd_profile, id_lt: id_lb)
!       Lambda
  REAL (RealK), INTENT(OUT) ::                                          &
      gamma_up(nd_profile, id_lt: id_lb)                                &
!       Coefficient for upward radiation
    , gamma_down(nd_profile, id_lt: id_lb)
!       Coefficient for downwad radiation


! Local variables.
  INTEGER                                                               &
      i                                                                 &
!       Loop variable
    , l
!       Loop variable
  REAL (RealK) ::                                                       &
      ksi_0(nd_profile, id_lt: id_lb)                                   &
!       Difference in solar scattering fractions
    , factor
!       Temporary variable
  REAL (RealK) ::                                                       &
      root_3
!       Square root of 3
  PARAMETER(                                                            &
      root_3=1.7320508075688772e+00_RealK                               &
    )

! Variables related to the treatment of ill-conditioning
  REAL (RealK) ::                                                       &
      tol_perturb
!       The tolerance used to judge where the two-stream
!       expressions for the solar source become ill-conditioned

  INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
  INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
  REAL(KIND=jprb)               :: zhook_handle
  CHARACTER (LEN=errormessagelength)           :: cmessage
  CHARACTER (LEN=*), PARAMETER  :: RoutineName = 'SOLAR_COEFFICIENT_BASIC'


  IF (lhook) CALL dr_hook(RoutineName,zhook_in,zhook_handle)

! Set the tolerances used in avoiding ill-conditioning, testing
! on any variable.
  tol_perturb=3.2e+01_RealK*EPSILON(sec_0(1))

! If LAMBDA is too close to SEC_0 it must be perturbed.
  DO i=i_layer_first, i_layer_last
    DO l=1, n_profile
      IF ((ABS(lambda(l, i)-sec_0(l))) <  tol_perturb) THEN
        sum(l, i)=(1.0e+00_RealK+tol_perturb)*sum(l, i)
        diff(l, i)=(1.0e+00_RealK+tol_perturb)*diff(l, i)
        lambda(l, i)=(1.0e+00_RealK+tol_perturb)*lambda(l, i)
      END IF
    END DO
  END DO

  IF ( (i_2stream == ip_eddington).OR.                                  &
       (i_2stream == ip_elsasser).OR.                                   &
       (i_2stream == ip_pifm85).OR.                                     &
       (i_2stream == ip_2s_test).OR.                                    &
       (i_2stream == ip_hemi_mean).OR.                                  &
       (i_2stream == ip_pifm80) ) THEN

    DO i=i_layer_first, i_layer_last
      DO l=1, n_profile
        ksi_0(l, i)=1.5e+00_RealK*asymmetry(l, i)/sec_0(l)
      END DO
    END DO

  ELSE IF (i_2stream == ip_discrete_ord) THEN

    DO i=i_layer_first, i_layer_last
      DO l=1, n_profile
        ksi_0(l, i)=root_3*asymmetry(l, i)/sec_0(l)
      END DO
    END DO

  ELSE

    cmessage = '*** Error: An illegal solar two-stream scheme has '     &
      //'been selected.'
    ierr=i_err_fatal
    CALL ereport(RoutineName, ierr, cmessage)

  END IF


! Determine the basic solar coefficients for the two-stream equations.
  DO i=i_layer_first, i_layer_last
    DO l=1, n_profile
      factor=0.5e+00_RealK*omega(l, i)*sec_0(l)                         &
        /((lambda(l, i)-sec_0(l))*(lambda(l, i)+sec_0(l)))
      gamma_up(l, i)=factor*(sum(l, i)-sec_0(l)                         &
        -ksi_0(l, i)*(diff(l, i)-sec_0(l)))
      gamma_down(l, i)=factor*(sum(l, i)+sec_0(l)                       &
        +ksi_0(l, i)*(diff(l, i)+sec_0(l)))
    END DO
  END DO


  IF (lhook) CALL dr_hook(RoutineName,zhook_out,zhook_handle)

END SUBROUTINE solar_coefficient_basic
