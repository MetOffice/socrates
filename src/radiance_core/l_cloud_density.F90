! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
!  Function to determine whether densities are required for clouds.
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: Radiation Control
!
!- ---------------------------------------------------------------------
FUNCTION l_cloud_density(n_condensed, i_phase_cmp, l_cloud_cmp          &
     , i_condensed_param                                                &
     , nd_cloud_component                                               &
     )


  USE rad_pcf
  USE yomhook, ONLY: lhook, dr_hook
  USE parkind1, ONLY: jprb, jpim
  IMPLICIT NONE


! Sizes of dummy arrays
  INTEGER, INTENT(IN) ::                                                &
      nd_cloud_component
!       Size allocated for components of clouds

! Dummy arguments.
  INTEGER, INTENT(IN) ::                                                &
      n_condensed                                                       &
!       Number of types of condensate
    , i_phase_cmp(nd_cloud_component)                                   &
!       Phases of components
    , i_condensed_param(nd_cloud_component)
!       Parametrizations of components
  LOGICAL, INTENT(IN) ::                                                &
      l_cloud_cmp(nd_cloud_component)
!       Flags for enabled components
  LOGICAL ::                                                            &
      l_cloud_density
!       Returned flag for calculating density


! Local variables.
  INTEGER                                                               &
      k
!       Loop variable

  INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
  INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
  REAL(KIND=jprb)               :: zhook_handle

  CHARACTER(LEN=*), PARAMETER :: RoutineName='L_CLOUD_DENSITY'


  IF (lhook) CALL dr_hook(RoutineName,zhook_in,zhook_handle)

  l_cloud_density=.FALSE.

! Densities must be calculated if Sun & Shine's parametrizations
! are used.
  DO k=1, n_condensed
    l_cloud_density=l_cloud_density                                     &
      .OR.( (i_phase_cmp(k) == ip_phase_water).AND.                     &
              (i_condensed_param(k) == ip_drop_unparametrized))         &
      .OR.( (i_phase_cmp(k) == ip_phase_ice).AND.                       &
              (i_condensed_param(k) == ip_ice_unparametrized))          &
      .OR.( l_cloud_cmp(k).AND.(i_phase_cmp(k) == ip_phase_ice)         &
            .AND.((i_condensed_param(k) == ip_sun_shine_vn2_vis)        &
              .OR.(i_condensed_param(k) == ip_sun_shine_vn2_ir)) )
  END DO


  IF (lhook) CALL dr_hook(RoutineName,zhook_out,zhook_handle)

END FUNCTION l_cloud_density
