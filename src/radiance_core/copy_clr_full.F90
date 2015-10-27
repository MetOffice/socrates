! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
!  Subroutine to set clear-sky optical properties.
!
! Method:
!   The arrays of clear-sky optical properties at the top
!   of the column and of total optical properties lower
!   down are combined to give a sinle array of clear-sky
!   optical properties.
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: Radiance Core
!
!- ---------------------------------------------------------------------
SUBROUTINE copy_clr_full(n_profile, n_layer, n_cloud_top                &
    , n_order_phase                                                     &
    , tau_clr, omega_clr, phase_fnc_clr                                 &
    , tau, omega, phase_fnc                                             &
    , tau_clr_f, omega_clr_f, phase_fnc_clr_f                           &
!                   Sizes of arrays
    , nd_profile, nd_layer, nd_layer_clr, id_ct, nd_max_order           &
    )


  USE realtype_rd, ONLY: RealK
  USE yomhook, ONLY: lhook, dr_hook
  USE parkind1, ONLY: jprb, jpim

  IMPLICIT NONE

! Sizes of arrays.
  INTEGER, INTENT(IN) ::                                                &
      nd_profile                                                        &
!       Size allocated for profiles
    , nd_layer                                                          &
!       Size allocated for layers
    , nd_layer_clr                                                      &
!       Size allocated for totally clear layers
    , id_ct                                                             &
!       Topmost declared layer for cloudy optical properties
    , nd_max_order
!       Size allowed for orders of spherical harmonics

!                 Atmospheric properties
  INTEGER, INTENT(IN) ::                                                &
      n_profile                                                         &
!       Number of profiles
    , n_layer                                                           &
!       Number of layers
    , n_cloud_top                                                       &
!       Topmost cloudy layer
    , n_order_phase
!       Number of terms in the phase function

!                 Optical properties
  REAL (RealK), INTENT(IN) ::                                           &
      tau_clr(nd_profile, nd_layer_clr)                                 &
!       Optical depth in totally clear region
    , omega_clr(nd_profile, nd_layer_clr)                               &
!       Single scattering albedo in totally clear region
    , phase_fnc_clr(nd_profile, nd_layer_clr, nd_max_order)
!       Phase function in totally clear region
  REAL (RealK), INTENT(IN) ::                                           &
      tau(nd_profile, id_ct: nd_layer)                                  &
!       Optical depth restricted to clear-sky regions
    , omega(nd_profile, id_ct: nd_layer)                                &
!       ALbedo of single scattering restricted to clear-sky regions
    , phase_fnc(nd_profile, id_ct: nd_layer, nd_max_order)
!       Phase function restricted to clear-sky regions

!                 Single scattering properties
  REAL (RealK), INTENT(OUT) ::                                          &
      tau_clr_f(nd_profile, nd_layer)                                   &
!       Optical depth
    , omega_clr_f(nd_profile, nd_layer)                                 &
!       Single scattering albedo
    , phase_fnc_clr_f(nd_profile, nd_layer, nd_max_order)
!       Phase function



! Local variables.
  INTEGER                                                               &
      l                                                                 &
!       Loop variable
    , i                                                                 &
!       Loop variable
    , k
!       Loop variable

  INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
  INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
  REAL(KIND=jprb)               :: zhook_handle

  CHARACTER(LEN=*), PARAMETER :: RoutineName='COPY_CLR_FULL'


  IF (lhook) CALL dr_hook(RoutineName,zhook_in,zhook_handle)

! Above cloud top.
  DO i=1, n_cloud_top-1
    DO l=1, n_profile
      tau_clr_f(l, i)=tau_clr(l, i)
      omega_clr_f(l, i)=omega_clr(l, i)
      phase_fnc_clr_f(l, i, 1)=phase_fnc_clr(l, i, 1)
    END DO
    DO k=2, n_order_phase
      DO l=1, n_profile
        phase_fnc_clr_f(l, i, k)=phase_fnc_clr(l, i, k)
      END DO
    END DO
  END DO

! Below cloud top.
  DO i=n_cloud_top, n_layer
    DO l=1, n_profile
      tau_clr_f(l, i)=tau(l, i)
      omega_clr_f(l, i)=omega(l, i)
      phase_fnc_clr_f(l, i, 1)=phase_fnc(l, i, 1)
    END DO
    DO k=2, n_order_phase
      DO l=1, n_profile
        phase_fnc_clr_f(l, i, k)=phase_fnc(l, i, k)
      END DO
    END DO
  END DO


  IF (lhook) CALL dr_hook(RoutineName,zhook_out,zhook_handle)

END SUBROUTINE copy_clr_full
