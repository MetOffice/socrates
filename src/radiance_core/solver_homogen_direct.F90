! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
!  Subroutine to calculate fluxes in a homogeneous column directly.
!
! Method:
!   Straightforward.
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: Radiance Core
!
!- ---------------------------------------------------------------------
SUBROUTINE solver_homogen_direct(n_profile, n_layer                     &
    , trans, reflect                                                    &
    , s_down, s_up                                                      &
    , isolir, diffuse_albedo, direct_albedo                             &
    , flux_direct_ground, flux_inc_down                                 &
    , d_planck_flux_surface                                             &
    , flux_total                                                        &
    , nd_profile, nd_layer                                              &
    )


  USE realtype_rd, ONLY: RealK
  USE rad_pcf
  USE yomhook, ONLY: lhook, dr_hook
  USE parkind1, ONLY: jprb, jpim

  IMPLICIT NONE


! Sizes of dummy arrays.
  INTEGER, INTENT(IN) ::                                                &
      nd_profile                                                        &
!       Size allocated for atmospheric profiles
    , nd_layer
!       Size allocated for atmospheric layers

! Dummy arguments.
  INTEGER, INTENT(IN) ::                                                &
      n_profile                                                         &
!       Number of profiles
    , n_layer                                                           &
!       Number of layers
    , isolir
!       Spectral region
  REAL (RealK), INTENT(IN) ::                                           &
      trans(nd_profile, nd_layer)                                       &
!       Transmission coefficient
    , reflect(nd_profile, nd_layer)                                     &
!       Reflection coefficient
    , s_down(nd_profile, nd_layer)                                      &
!       Downward diffuse source
    , s_up(nd_profile, nd_layer)                                        &
!       Upward diffuse source
    , diffuse_albedo(nd_profile)                                        &
!       Diffuse surface albedo
    , direct_albedo(nd_profile)                                         &
!       Direct surface albedo
    , d_planck_flux_surface(nd_profile)                                 &
!       Difference between the Planckian flux at the surface
!       temperature and that of the overlaying air
    , flux_inc_down(nd_profile)                                         &
!       Incident total flux
    , flux_direct_ground(nd_profile)
!       Direct flux at ground level

  REAL (RealK), INTENT(OUT) ::                                          &
      flux_total(nd_profile, 2*nd_layer+2)
!       Total flux

! Declaration of local variables.
  INTEGER                                                               &
      i                                                                 &
!       Loop variable
    , l
!       Loop variable

  REAL (RealK) ::                                                       &
      alpha(nd_profile, nd_layer+1)                                     &
!       Combined albedo of lower layers
    , beta(nd_profile, nd_layer)                                        &
!       Working array
    , gamma(nd_profile, nd_layer)                                       &
!       Working array
    , h(nd_profile, nd_layer)                                           &
!       Working array
    , s_up_prime(nd_profile, nd_layer+1)
!       Modified upward source function

  INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
  INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
  REAL(KIND=jprb)               :: zhook_handle

  CHARACTER(LEN=*), PARAMETER :: RoutineName='SOLVER_HOMOGEN_DIRECT'


  IF (lhook) CALL dr_hook(RoutineName,zhook_in,zhook_handle)

! Initialization at the bottom for upward elimination:
  IF (isolir == ip_solar) THEN
    DO l=1, n_profile
      alpha(l, n_layer+1)=diffuse_albedo(l)
      s_up_prime(l, n_layer+1)                                          &
        =(direct_albedo(l)-diffuse_albedo(l))                           &
        *flux_direct_ground(l)
    END DO
  ELSE IF (isolir == ip_infra_red) THEN
    DO l=1, n_profile
      alpha(l, n_layer+1)=diffuse_albedo(l)
      s_up_prime(l, n_layer+1)                                          &
        =(1.0e+00_RealK-diffuse_albedo(l))                              &
        *d_planck_flux_surface(l)
    END DO
  END IF

! Eliminating loop:
  DO i=n_layer, 1, -1
    DO l=1, n_profile
      beta(l, i)=1.0e+00_RealK                                          &
        /(1.0e+00_RealK-alpha(l, i+1)*reflect(l, i))
      gamma(l, i)=alpha(l, i+1)*trans(l, i)
      h(l, i)=s_up_prime(l, i+1)+alpha(l, i+1)*s_down(l, i)
      alpha(l, i)=reflect(l, i)                                         &
        +beta(l, i)*gamma(l, i)*trans(l, i)
      s_up_prime(l, i)=s_up(l, i)+beta(l, i)*trans(l, i)*h(l, i)
    END DO
  END DO

! Initialize for backward substitution.
  DO l=1, n_profile
    flux_total(l, 2)=flux_inc_down(l)
    flux_total(l, 1)=alpha(l, 1)*flux_total(l, 2)+s_up_prime(l, 1)
  END DO

! Backward substitution:
  DO i=1, n_layer
    DO l=1, n_profile
!     Upward flux
      flux_total(l, 2*i+1)                                              &
        =beta(l, i)*(h(l, i)+gamma(l, i)*flux_total(l, 2*i))
!     Downward flux
      flux_total(l, 2*i+2)=s_down(l, i)                                 &
        +trans(l, i)*flux_total(l, 2*i)                                 &
        +reflect(l, i)*flux_total(l, 2*i+1)
    END DO
  END DO


  IF (lhook) CALL dr_hook(RoutineName,zhook_out,zhook_handle)

END SUBROUTINE solver_homogen_direct
