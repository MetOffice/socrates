! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
!  Subroutine to calculate the solar flux and source terms.
!
! Method:
!   Straightforward.
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: Radiance Core
!
!- ---------------------------------------------------------------------
SUBROUTINE solar_source(control, bound, n_profile, n_layer              &
     , flux_inc_direct                                                  &
     , trans_0, source_coeff                                            &
     , l_scale_solar, adjust_solar_ke                                   &
     , flux_direct                                                      &
     , s_down, s_up                                                     &
     , nd_profile, nd_layer, nd_source_coeff                            &
     )


  USE realtype_rd, ONLY: RealK
  USE def_control, ONLY: StrCtrl
  USE def_bound,   ONLY: StrBound
  USE rad_pcf
  USE parkind1, ONLY: jprb, jpim
  USE yomhook, ONLY: lhook, dr_hook

  IMPLICIT NONE


! Control options:
  TYPE(StrCtrl),      INTENT(IN)    :: control

! Boundary conditions:
  TYPE(StrBound),     INTENT(IN)    :: bound

! Sizes of dummy arrays.
  INTEGER, INTENT(IN) ::                                                &
      nd_profile                                                        &
!       Size allocated for atmospheric profiles
    , nd_layer                                                          &
!       Size allocated for atmospheric layers
    , nd_source_coeff
!       Size allocated for coefficients in the source terms


! Dummy variables.
  INTEGER, INTENT(IN) ::                                                &
      n_profile                                                         &
!       Number of profiles
    , n_layer
!       Number of layers

  LOGICAL, INTENT(IN) ::                                                &
      l_scale_solar
!       Scaling applied to solar beam

  REAL (RealK), INTENT(IN) ::                                           &
      flux_inc_direct(nd_profile)                                       &
!       Incident solar flux
    , trans_0(nd_profile, nd_layer)                                     &
!       Direct transmission coefficient
    , source_coeff(nd_profile, nd_layer, nd_source_coeff)               &
!       Reflection coefficient
    , adjust_solar_ke(nd_profile, nd_layer)
!       Adjustment to solar flux


  REAL (RealK), INTENT(OUT) ::                                          &
      flux_direct(nd_profile, 0: nd_layer)                              &
!       Direct flux
    , s_down(nd_profile, nd_layer)                                      &
!       Downward source function
    , s_up(nd_profile, nd_layer)
!       Upward source function


! Local variables.
  INTEGER                                                               &
      i                                                                 &
!       Loop variable
    , l
!       Loop variable

  INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
  INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
  REAL(KIND=jprb)               :: zhook_handle

  CHARACTER(LEN=*), PARAMETER :: RoutineName='SOLAR_SOURCE'


  IF (lhook) CALL dr_hook(RoutineName,zhook_in,zhook_handle)

  DO l=1, n_profile
    flux_direct(l, 0)=flux_inc_direct(l)
  END DO

! The solar flux may be multiplied by a scaling factor if an
! equivalent extinction is used.
  IF (l_scale_solar) THEN

    DO i=1, n_layer
      DO l=1, n_profile
        flux_direct(l, i)                                               &
          =flux_direct(l, i-1)*trans_0(l, i)                            &
          *adjust_solar_ke(l, i)
        s_up(l, i)=source_coeff(l, i, ip_scf_solar_up)                  &
          *flux_direct(l, i-1)
        s_down(l, i)=(source_coeff(l, i, ip_scf_solar_down)             &
          -trans_0(l, i))*flux_direct(l, i-1)                           &
          +flux_direct(l, i)
      END DO
    END DO

  ELSE

    DO i=1, n_layer
      DO l=1, n_profile
        flux_direct(l, i)                                               &
          =flux_direct(l, i-1)*trans_0(l, i)
        s_up(l, i)=source_coeff(l, i, ip_scf_solar_up)                  &
          *flux_direct(l, i-1)
        s_down(l, i)=source_coeff(l, i, ip_scf_solar_down)              &
          *flux_direct(l, i-1)
      END DO
    END DO

  END IF


! Correct the direct flux at the ground for sloping terrain
  IF (control%l_orog) THEN
     flux_direct(1:n_profile, n_layer) =                                &
        flux_direct(1:n_profile, n_layer) *                             &
        bound%orog_corr(1:n_profile)

     s_down(1:n_profile, n_layer) =                                     &
           s_down(1:n_profile, n_layer) +                               &
           flux_direct(1:n_profile, n_layer) *                          &
           (bound%orog_corr(1:n_profile) - 1.0_RealK)
  END IF


  IF (lhook) CALL dr_hook(RoutineName,zhook_out,zhook_handle)

END SUBROUTINE solar_source
