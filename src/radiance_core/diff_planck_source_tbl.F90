! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
!  Subroutine to calculate differences in source functions.
!
! Method:
!   Using the table of the Planck function, values
!   of this function at the boundaries of layers are found
!   and differences across layers are determined. If the
!   Planckian is being taken to vary quadratically across
!   the layer second differences are found.
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: Radiance Core
!
!- ---------------------------------------------------------------------
SUBROUTINE diff_planck_source_tbl(n_profile, n_layer                    &
    , n_deg_fit, thermal_coefficient                                    &
    , t_ref_planck, theta_planck_tbl, t_level, t_ground                 &
    , planck_flux, diff_planck, planck_ground                           &
    , l_ir_source_quad, t, diff_planck_2                                &
    , i_angular_integration                                             &
    , n_viewing_level, i_rad_layer, frac_rad_layer                      &
    , planck_radiance                                                   &
    , l_tile, n_point_tile, n_tile, list_tile                           &
    , frac_tile, t_tile, planck_flux_tile                               &
    , nd_profile, nd_layer, nd_thermal_coeff                            &
    , nd_radiance_profile, nd_viewing_level                             &
    , nd_point_tile, nd_tile                                            &
    )


  USE realtype_rd, ONLY: RealK
  USE rad_pcf
  USE rad_ccf, ONLY: pi
  USE yomhook, ONLY: lhook, dr_hook
  USE parkind1, ONLY: jprb, jpim

  IMPLICIT NONE

! Sizes of dummy arrays
  INTEGER, INTENT(IN) ::                                                &
      nd_profile                                                        &
!       Size allocated for atmospheric profiles
    , nd_layer                                                          &
!       Size allocated for atmospheric layers
    , nd_thermal_coeff                                                  &
!       Size allocated for thermal coefficients
    , nd_radiance_profile                                               &
!       Size allocated for profiles where radiances are calculated
    , nd_viewing_level                                                  &
!       Size allocated for levels where radiances are calculated
    , nd_point_tile                                                     &
!       Size allocated for points with surface tiling
    , nd_tile
!       Size allocated for the number of tiles


! Dummy arguments.
  INTEGER, INTENT(IN) ::                                                &
      n_profile                                                         &
!       Number of profiles
    , n_layer                                                           &
!       Number of layers
    , n_deg_fit
!       Degree of fitting function

  INTEGER, INTENT(IN) ::                                                &
      n_viewing_level                                                   &
!       Number of levels where radiances are calculated
    , i_rad_layer(nd_viewing_level)
!       Layers in which to intercept radiances
  REAL (RealK), INTENT(IN) ::                                           &
      frac_rad_layer(nd_viewing_level)
!       Fractions below the tops of the layers

  INTEGER, INTENT(IN) ::                                                &
      i_angular_integration
!       Type of angular integration

  LOGICAL, INTENT(IN) ::                                                &
      l_ir_source_quad
!       Flag for quadratic IR-source
  REAL (RealK), INTENT(IN) ::                                           &
      thermal_coefficient(0: nd_thermal_coeff-1)                        &
!       Coefficients of fit to the Planckian flux function
    , t_ref_planck                                                      &
!       Planckian reference temperature
    , theta_planck_tbl(0: nd_thermal_coeff-1)                           &
!       Temperatures at which the band-integrated Planck function has
!       been evaluated.
    , t_level(nd_profile, 0: nd_layer)                                  &
!       Temperatures on levels
    , t(nd_profile, nd_layer)                                           &
!       Temperatures at centres of layers
    , t_ground(nd_profile)
!       Temperatures at ground

! Tiling of the surface:
  LOGICAL, INTENT(IN) ::                                                &
      l_tile
!       Local to allow tiling options
  INTEGER, INTENT(IN) ::                                                &
      n_point_tile                                                      &
!       Number of points to tile
    , n_tile                                                            &
!       Number of tiles used
    , list_tile(nd_point_tile)
!       List of points with surface tiling
  REAL (RealK), INTENT(IN) ::                                           &
      frac_tile(nd_point_tile, nd_tile)                                 &
!       Fraction of tiled grid-points occupied by each tile
    , t_tile(nd_point_tile, nd_tile)
!       Local surface temperatures on individual tiles

  REAL (RealK), INTENT(OUT) ::                                          &
      planck_flux(nd_profile, 0: nd_layer)                              &
!       Planckian flux on levels
    , diff_planck(nd_profile, nd_layer)                                 &
!       Differences in Planckian flux (bottom-top)
    , diff_planck_2(nd_profile, nd_layer)                               &
!       Twice 2nd differences in the Planckian flux
    , planck_ground(nd_profile)                                         &
!       Planckian flux at the surface temperature
    , planck_radiance(nd_radiance_profile, nd_viewing_level)            &
!       Planckian radiances at viewing levels
    , planck_flux_tile(nd_point_tile, nd_tile)
!       Local Planckian fluxes on surface tiles


! Local variables.
  INTEGER                                                               &
      i                                                                 &
!       Loop variable
    , k                                                                 &
!       Loop variable
    , l
!       Loop variable
  REAL (RealK) ::                                                       &
      t_ratio(nd_profile)
!       Temperature ratio

  INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
  INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
  REAL(KIND=jprb)               :: zhook_handle

  CHARACTER(LEN=*), PARAMETER :: RoutineName='DIFF_PLANCK_SOURCE_TBL'


  IF (lhook) CALL dr_hook(RoutineName,zhook_in,zhook_handle)

  IF (i_angular_integration == ip_spherical_harmonic) THEN

!   Calculate the Planckian radiance on viewing levels.
    DO i=1, n_viewing_level
      DO l=1, n_profile
!       Interpolate linearly in the temperature.
        t_ratio(l)=(t_level(l, i_rad_layer(i)-1)                        &
          +(t_level(l, i_rad_layer(i))-t_level(l, i_rad_layer(i)-1))    &
          *frac_rad_layer(i))/t_ref_planck
!       Use the second differences of the Planckian as temporary
!       storage.
        planck_radiance(l, i)                                           &
          =interp(theta_planck_tbl, thermal_coefficient, t_ratio(l))
        planck_radiance(l, i)=planck_radiance(l, i)/pi
      END DO
    END DO

  END IF

! Calculate the change in the Planckian flux across each layer.
  DO l=1, n_profile
    t_ratio(l)=t_level(l, 0)/t_ref_planck
    planck_flux(l, 0)                                                   &
      =interp(theta_planck_tbl, thermal_coefficient, t_ratio(l))
  END DO
  DO i=1, n_layer
    DO l=1, n_profile
      t_ratio(l)=t_level(l, i)/t_ref_planck
      planck_flux(l, i)                                                 &
        =interp(theta_planck_tbl, thermal_coefficient, t_ratio(l))
      diff_planck(l, i)=planck_flux(l, i)                               &
        -planck_flux(l, i-1)
    END DO
  END DO

! Calculate the second difference if required.
  IF (l_ir_source_quad) THEN
    DO i=1, n_layer
!     Use the second difference for temporary storage.
!     of the Planckian at the middle of the layer.
      DO l=1, n_profile
        t_ratio(l)=t(l, i)/t_ref_planck
        diff_planck_2(l, i)                                             &
          =interp(theta_planck_tbl, thermal_coefficient, t_ratio(l))
        diff_planck_2(l, i)=2.0e+00_RealK*(planck_flux(l, i)            &
          +planck_flux(l, i-1)-2.0e+00_RealK*diff_planck_2(l, i))
      END DO
    END DO
  END IF

! Planckian flux at the surface.
  DO l=1, n_profile
    t_ratio(l)=t_ground(l)/t_ref_planck
    planck_ground(l)=                                                   &
      interp(theta_planck_tbl, thermal_coefficient, t_ratio(l))
  END DO

! Local Planckian fluxes will be required on tiled surfaces.
! Furthermore, the overall Planckian will be calculated as a
! weighted sum of the individual components: this allows for
! variations in the Planckian between spectral bands more
! satisfactorily than the use of an equivalent temperature
! can.
  IF (l_tile) THEN

    DO k=1, n_tile
      DO l=1, n_point_tile
        t_ratio(l)=t_tile(l, k)/t_ref_planck
        planck_flux_tile(l, k)=                                         &
          interp(theta_planck_tbl, thermal_coefficient, t_ratio(l))
      END DO
    END DO

    DO l=1, n_point_tile
      planck_ground(list_tile(l))                                       &
        =frac_tile(l, 1)*planck_flux_tile(l, 1)
    END DO
    DO k=2, n_tile
      DO l=1, n_point_tile
        planck_ground(list_tile(l))=planck_ground(list_tile(l))         &
          +frac_tile(l, k)*planck_flux_tile(l, k)
      END DO
    END DO

  END IF

  IF (lhook) CALL dr_hook(RoutineName,zhook_out,zhook_handle)

CONTAINS

! Wrapper for the interpolation function.
  FUNCTION interp(x ,y, xi) RESULT(yi)

    IMPLICIT NONE

    REAL(RealK), INTENT(IN) ::                                          &
        x(:)                                                            &
!         Array with x-values
      , y(:)                                                            &
!         Array with y-values
      , xi
!         Value at which the y-coordinate is wanted

    REAL(RealK) ::                                                      &
        yi
!         Value at xi.

    yi = interp1(x, y, xi)


  END FUNCTION interp

! Liner interpolation function
  FUNCTION interp1(x ,y, xi) RESULT(yi)

    IMPLICIT NONE

    REAL(RealK), INTENT(IN) ::                                          &
        x(:)                                                            &
!         Array with x-values
      , y(:)                                                            &
!         Array with y-values
      , xi
!         Value at which the y-coordinate is wanted

    REAL(RealK) ::                                                      &
        yi
!         Value at xi.

    INTEGER ::                                                          &
        x_len                                                           &
!         Length of x-array
      , i
!         Loop index


    x_len = SIZE(x)

    IF (xi < x(1)) THEN
      yi = y(1)
      RETURN
    ELSE IF (xi > x(x_len)) THEN
      yi = y(x_len)
      RETURN
    ELSE
      DO i=2,x_len
        IF (xi <= x(i)) THEN
          yi = (y(i) - y(i-1))/(x(i) - x(i-1))*(xi - x(i-1)) + y(i-1)
          RETURN
        END IF
      END DO
    END IF

  END FUNCTION interp1

END SUBROUTINE diff_planck_source_tbl
