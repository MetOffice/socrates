! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
!  Subroutine to increment upward fluxes on a tiled surface.
!
! Method:
!       The arrays holding the local cumulative fluxes or radiances
!       on each tile are incremented by the variables suffixed
!       with _INCR, multiplied by appropriate weights. The routine
!       can be called to initialize fluxes.
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: Radiance Core
!
!- ---------------------------------------------------------------------
SUBROUTINE augment_tiled_radiance(ierr                                  &
    , n_point_tile, n_tile, list_tile                                   &
    , i_angular_integration, isolir, l_initial                          &
    , weight_incr, l_blue_flux_surf, weight_blue_incr                   &
!                 Surface characteristics
    , rho_alb                                                           &
!                 Actual radiances
    , flux_up_tile, flux_up_blue_tile                                   &
!                 Increments to radiances
    , flux_direct_incr, flux_down_incr                                  &
    , planck_flux_tile, planck_flux_air                                 &
!                 Dimensions
    , nd_flux_profile, nd_point_tile, nd_tile                           &
    , nd_brdf_basis_fnc                                                 &
    )


  USE realtype_rd, ONLY: RealK
  USE rad_pcf
  USE yomhook, ONLY: lhook, dr_hook
  USE parkind1, ONLY: jprb, jpim
  USE ereport_mod, ONLY: ereport
  USE errormessagelength_mod, ONLY: errormessagelength

  IMPLICIT NONE


! Sizes of dummy arrays.
  INTEGER, INTENT(IN) ::                                                &
      nd_flux_profile                                                   &
!       Size allocated for points where fluxes are calculated
    , nd_point_tile                                                     &
!       Size allocated for points where the surface is tiled
    , nd_tile                                                           &
!       Size allocated for surface tiles
    , nd_brdf_basis_fnc
!       Size allocated for BRDF basis functions

! Dummy arguments.
  INTEGER, INTENT(INOUT) ::                                             &
      ierr
!       Error flag

  INTEGER, INTENT(IN) ::                                                &
      n_point_tile                                                      &
!       Number of points where the surface is tiled
    , n_tile                                                            &
!       Number of tiles used
    , list_tile(nd_point_tile)
!       List of tiled points
  INTEGER, INTENT(IN) ::                                                &
      isolir                                                            &
!       Spectral region
    , i_angular_integration
!       Treatment of angular integration
  LOGICAL, INTENT(INOUT) ::                                             &
      l_initial
!       Flag to call the routine to initialize the outputs
  LOGICAL, INTENT(IN) ::                                                &
      l_blue_flux_surf
!       Flag to increment blue surface fluxes
  REAL (RealK), INTENT(IN) ::                                           &
      weight_incr                                                       &
!       Weight to apply to increments
    , weight_blue_incr
!       Weight to apply to increments to blue fluxes

!                 Surface Characteristics
  REAL (RealK), INTENT(IN) ::                                           &
      rho_alb(nd_point_tile, nd_brdf_basis_fnc, nd_tile)
!       Weighting functions for BRDFs

!                 Increments to Fluxes
  REAL (RealK), INTENT(IN) ::                                           &
      flux_direct_incr(nd_flux_profile)                                 &
!       Increment to mean direct flux
    , flux_down_incr(nd_flux_profile)
!       Increment to total downward flux

!                 Planckian Fluxes
  REAL (RealK), INTENT(IN) ::                                           &
      planck_flux_tile(nd_point_tile, nd_tile)                          &
!       Local Planckian flux emitted from each tile
    , planck_flux_air(nd_flux_profile)
!       Hemispheric Planckian flux at the temperature of the air

!                 Total Fluxes
  REAL (RealK), INTENT(INOUT) ::                                        &
      flux_up_tile(nd_point_tile, nd_tile)                              &
!       Local upward flux on each tile (not weighted by the
!       fractional coverage of the tile)
    , flux_up_blue_tile(nd_point_tile, nd_tile)
!       Local upward blue flux on each tile (not weighted by the
!       fractional coverage of the tile)


! Local arguments.
  INTEGER                                                               &
      l                                                                 &
!       Loop variable
    , ll                                                                &
!       Loop variable
    , k
!       Loop variable

  INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
  INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
  REAL(KIND=jprb)               :: zhook_handle
  CHARACTER (LEN=errormessagelength)           :: cmessage
  CHARACTER (LEN=*), PARAMETER  :: RoutineName = 'AUGMENT_TILED_RADIANCE'


  IF (lhook) CALL dr_hook(RoutineName,zhook_in,zhook_handle)

  IF (.NOT.l_initial) THEN

!   Most commonly, this routine will be called to increment
!   rather than to initialize fluxes.

    IF ( (i_angular_integration == ip_two_stream).OR.                   &
         (i_angular_integration == ip_ir_gauss) ) THEN

!     Increment the actual fluxes.
      IF (isolir == ip_solar) THEN

        DO k=1, n_tile
          DO ll=1, n_point_tile
            l=list_tile(ll)
            flux_up_tile(ll, k)=flux_up_tile(ll, k)                     &
              +weight_incr                                              &
              *(rho_alb(ll, ip_surf_alb_diff, k)*flux_down_incr(l)      &
              +(rho_alb(ll, ip_surf_alb_dir, k)                         &
              -rho_alb(ll, ip_surf_alb_diff, k))                        &
              *flux_direct_incr(l))
          END DO
        END DO

        IF (l_blue_flux_surf) THEN
          DO k=1, n_tile
            DO ll=1, n_point_tile
              l=list_tile(ll)
              flux_up_blue_tile(ll, k)=flux_up_blue_tile(ll, k)         &
                +weight_blue_incr                                       &
                *(rho_alb(ll, ip_surf_alb_diff, k)                      &
                *flux_down_incr(l)                                      &
                +(rho_alb(ll, ip_surf_alb_dir, k)                       &
                -rho_alb(ll, ip_surf_alb_diff, k))                      &
                *flux_direct_incr(l))
            END DO
          END DO
        END IF

      ELSE IF (isolir == ip_infra_red) THEN

        DO k=1, n_tile
          DO ll=1, n_point_tile
            l=list_tile(ll)
            flux_up_tile(ll, k)=flux_up_tile(ll, k)                     &
              +weight_incr*(planck_flux_tile(ll, k)                     &
              +rho_alb(ll, ip_surf_alb_diff, k)                         &
              *(flux_down_incr(l)                                       &
              +planck_flux_air(l)-planck_flux_tile(ll, k)))
          END DO
        END DO
      END IF

    ELSE IF (i_angular_integration == ip_spherical_harmonic) THEN

      cmessage = '*** Error: Tiled surfaces have not yet been ' //      &
        'implemented with the spherical harmonic solver.'
      ierr=i_err_fatal
      CALL ereport(RoutineName, ierr, cmessage)

    END IF

  ELSE

!   Initialization of the radiance field takes place here.

    IF ( (i_angular_integration == ip_two_stream).OR.                   &
         (i_angular_integration == ip_ir_gauss) ) THEN

!     Initialize the actual fluxes.
      IF (isolir == ip_solar) THEN
        DO k=1, n_tile
          DO ll=1, n_point_tile
            l=list_tile(ll)
            flux_up_tile(ll, k)=weight_incr                             &
              *(rho_alb(ll, ip_surf_alb_diff, k)*flux_down_incr(l)      &
              +(rho_alb(ll, ip_surf_alb_dir, k)                         &
              -rho_alb(ll, ip_surf_alb_diff, k))                        &
              *flux_direct_incr(l))
          END DO
        END DO

        IF (l_blue_flux_surf) THEN
          DO k=1, n_tile
            DO ll=1, n_point_tile
              l=list_tile(ll)
              flux_up_blue_tile(ll, k)                                  &
                =weight_blue_incr*(rho_alb(ll, ip_surf_alb_diff, k)     &
                *flux_down_incr(l)                                      &
                +(rho_alb(ll, ip_surf_alb_dir, k)                       &
                -rho_alb(ll, ip_surf_alb_diff, k))                      &
                *flux_direct_incr(l))
            END DO
          END DO
        END IF

      ELSE IF (isolir == ip_infra_red) THEN

        DO k=1, n_tile
          DO ll=1, n_point_tile
            l=list_tile(ll)
            flux_up_tile(ll, k)                                         &
              =weight_incr*(planck_flux_tile(ll, k)                     &
              +rho_alb(ll, ip_surf_alb_diff, k)                         &
              *(flux_down_incr(l)                                       &
              +planck_flux_air(l)-planck_flux_tile(ll, k)))
          END DO
        END DO

      END IF

    ELSE IF (i_angular_integration == ip_spherical_harmonic) THEN

      cmessage = '*** Error: Tiled surfaces have not yet been ' //      &
        'implemented with the spherical harmonic solver.'
      ierr=i_err_fatal
      CALL ereport(RoutineName, ierr, cmessage)

    END IF

!   Now reset the initialization flag as the arrays have been set.
    l_initial=.FALSE.

  END IF


  IF (lhook) CALL dr_hook(RoutineName,zhook_out,zhook_handle)

END SUBROUTINE augment_tiled_radiance
