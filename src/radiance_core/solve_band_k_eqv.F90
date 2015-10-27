! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
!  Subroutine to calculate fluxes using equivalent extinction.
!
! Method:
!   For each minor gas an equivalent extinction is calculated
!   from a clear-sky calculation. These equivalent extinctions
!   are then used in a full calculation involving the major gas.
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: Radiance Core
!
!- ---------------------------------------------------------------------
SUBROUTINE solve_band_k_eqv(ierr                                        &
    , control, dimen, cld, bound                                        &
!                   Atmospheric properties
    , n_profile, n_layer, i_top, p, t, d_mass                           &
!                   Angular integration
    , i_angular_integration, i_2stream                                  &
    , n_order_phase, l_rescale, n_order_gauss                           &
    , ms_min, ms_max, i_truncation, ls_local_trunc                      &
    , accuracy_adaptive, euler_factor                                   &
    , i_sph_algorithm, i_sph_mode                                       &
!                   Precalculated angular arrays
    , ia_sph_mm, cg_coeff, uplm_zero, uplm_sol                          &
!                   Treatment of scattering
    , i_scatter_method_band, i_scatter_method_term                      &
!                   Options for solver
    , i_solver, i_gas_overlap                                           &
!                   Gaseous properties
    , i_band, n_gas                                                     &
    , index_absorb, i_band_esft, i_scale_esft, i_scale_fnc              &
    , k_esft, k_esft_layer, w_esft, scale_vector                        &
    , p_reference, t_reference                                          &
    , gas_mix_ratio, gas_frac_rescaled                                  &
    , l_doppler, doppler_correction                                     &
!                   Spectral region
    , isolir                                                            &
!                   Solar properties
    , zen_0, solar_irrad                                                &
!                   Infra-red properties
    , planck_flux_band                                                  &
    , diff_planck_band                                                  &
    , l_ir_source_quad, diff_planck_band_2                              &
!                   Surface properties
    , ls_brdf_trunc, n_brdf_basis_fnc, rho_alb                          &
    , f_brdf, brdf_sol, brdf_hemi                                       &
    , diff_albedo_basis                                                 &
    , planck_flux_surface                                               &
!                   Tiling of the surface
    , l_tile, n_point_tile, n_tile, list_tile, rho_alb_tile             &
    , planck_flux_tile                                                  &
!                   Optical properties
    , ss_prop                                                           &
!                   Cloudy properties
    , l_cloud, i_cloud                                                  &
!                   Cloud geometry
    , n_cloud_top                                                       &
    , n_region, k_clr, i_region_cloud, frac_region                      &
    , w_free, cloud_overlap                                             &
    , n_column_slv, list_column_slv                                     &
    , i_clm_lyr_chn, i_clm_cld_typ, area_column                         &
!                   Additional variables required for McICA
    , l_cloud_cmp, n_cloud_profile, i_cloud_profile                     &
    , i_cloud_type, nd_cloud_component, i_cloud_representation          &
!                   Levels for calculating radiances
    , n_viewing_level, i_rad_layer, frac_rad_layer                      &
!                   Viewing geometry
    , n_direction, direction                                            &
!                   Weighting factor for the band
    , weight_band, l_initial                                            &
!                   Fluxes calculated
    , flux_direct, flux_down, flux_up                                   &
!                   Radiances
    , i_direct, radiance                                                &
!                   Rate of photolysis
    , photolysis                                                        &
!                   Flags for clear-sky fluxes
    , l_clear, i_solver_clear                                           &
!                   Clear-sky fluxes calculated
    , flux_direct_clear, flux_down_clear, flux_up_clear                 &
!                   Tiled surface fluxes
    , flux_up_tile, flux_up_blue_tile                                   &
!                   Special surface fluxes
    , l_blue_flux_surf, weight_blue                                     &
    , flux_direct_blue_surf                                             &
    , flux_down_blue_surf, flux_up_blue_surf                            &
!                   Dimensions of arrays
    , nd_profile, nd_layer, nd_layer_clr, id_ct, nd_column              &
    , nd_flux_profile, nd_radiance_profile, nd_j_profile                &
    , nd_band, nd_species                                               &
    , nd_esft_term, nd_scale_variable                                   &
    , nd_cloud_type, nd_region, nd_overlap_coeff                        &
    , nd_max_order, nd_sph_coeff                                        &
    , nd_brdf_basis_fnc, nd_brdf_trunc, nd_viewing_level                &
    , nd_direction, nd_source_coeff                                     &
    , nd_point_tile, nd_tile                                            &
    )


  USE realtype_rd, ONLY: RealK
  USE def_control, ONLY: StrCtrl
  USE def_dimen,   ONLY: StrDim
  USE def_cld,     ONLY: StrCld
  USE def_bound,   ONLY: StrBound
  USE def_ss_prop
  USE rad_pcf
  USE rad_ccf, ONLY: diffusivity_factor_minor
  USE vectlib_mod, ONLY: exp_v
  USE yomhook, ONLY: lhook, dr_hook
  USE parkind1, ONLY: jprb, jpim

  IMPLICIT NONE


! Control options:
  TYPE(StrCtrl),      INTENT(IN)    :: control

! Dimensions:
  TYPE(StrDim),       INTENT(IN)    :: dimen

! Cloud properties:
  TYPE(StrCld),       INTENT(IN)    :: cld

! Boundary conditions:
  TYPE(StrBound),     INTENT(IN)    :: bound

! Sizes of dummy arrays.
  INTEGER, INTENT(IN) ::                                                &
      nd_profile                                                        &
!       Size allocated for profiles
    , nd_layer                                                          &
!       Size allocated for layers
    , nd_layer_clr                                                      &
!       Size allowed for totally clear layers
    , id_ct                                                             &
!       Topmost declared cloudy level
    , nd_band                                                           &
!       Size allocated for spectral bands
    , nd_species                                                        &
!       Size allocated for species
    , nd_esft_term                                                      &
!       Size allocated for ESFT terms
    , nd_scale_variable                                                 &
!       Size allocated for scale variables
    , nd_flux_profile                                                   &
!       Size allocated for profiles in arrays of fluxes
    , nd_radiance_profile                                               &
!       Size allocated for profiles in arrays of radiances
    , nd_j_profile                                                      &
!       Size allocated for profiles in arrays of mean radiances
    , nd_column                                                         &
!       Size allocated for sub-columns per point
    , nd_cloud_type                                                     &
!       Size allocated for cloud types
    , nd_region                                                         &
!       Size allocated for cloudy regions
    , nd_overlap_coeff                                                  &
!       Size allocated for cloudy overlap coefficients
    , nd_max_order                                                      &
!       Size allocated for orders of spherical harmonics
    , nd_sph_coeff                                                      &
!       Size allocated for spherical harmonic coefficients
    , nd_brdf_basis_fnc                                                 &
!       Size allowed for BRDF basis functions
    , nd_brdf_trunc                                                     &
!       Size allowed for orders of BRDFs
    , nd_viewing_level                                                  &
!       Size allocated for levels where radiances are calculated
    , nd_direction                                                      &
!       Size allocated for viewing directions
    , nd_source_coeff                                                   &
!       Size allocated for source coefficients
    , nd_point_tile                                                     &
!       Size allocated for points where the surface is tiled
    , nd_tile
!       Size allocated for surface tiles


! Dummy arguments.
  INTEGER, INTENT(INOUT) ::                                             &
      ierr
!       Error flag

!                   Atmospheric properties
  INTEGER, INTENT(IN) ::                                                &
      n_profile                                                         &
!       Number of profiles
    , n_layer                                                           &
!       Number of layers
    , i_top
!       Top of vertical grid
  REAL (RealK), INTENT(IN) ::                                           &
      d_mass(nd_profile, nd_layer)                                      &
!       Mass thickness of each layer
    , p(nd_profile, nd_layer)                                           &
!       Pressure
    , t(nd_profile, nd_layer)
!       Temperature

!                   Angular integration
  INTEGER, INTENT(IN) ::                                                &
      i_angular_integration                                             &
!       Angular integration scheme
    , i_2stream                                                         &
!       Two-stream scheme
    , n_order_phase                                                     &
!       Maximum order of terms in the phase function used in
!       the direct calculation of spherical harmonics
    , n_order_gauss                                                     &
!       Order of gaussian integration
    , ms_min                                                            &
!       Lowest azimuthal order used
    , ms_max                                                            &
!       Highest azimuthal order used
    , i_truncation                                                      &
!       Type of spherical truncation
    , ia_sph_mm(0: nd_max_order)                                        &
!       Address of spherical coefficient of (m, m) for each m
    , ls_local_trunc(0: nd_max_order)                                   &
!       Orders of truncation at each azimuthal order
    , i_sph_mode                                                        &
!       Mode in which the spherical solver runs
    , i_sph_algorithm
!       Algorithm used for spherical harmonic calculation
  LOGICAL, INTENT(IN) ::                                                &
      l_rescale
!       Rescale optical properties
  REAL (RealK), INTENT(IN) ::                                           &
      weight_band
!       Weighting factor for the current band
  LOGICAL, INTENT(INOUT) ::                                             &
      l_initial
!       Flag to initialize diagnostics

  REAL (RealK), INTENT(IN) ::                                           &
      cg_coeff(nd_sph_coeff)                                            &
!       Clebsch-Gordan coefficients
    , uplm_zero(nd_sph_coeff)                                           &
!       Values of spherical harmonics at polar angles pi/2
    , uplm_sol(nd_radiance_profile, nd_sph_coeff)                       &
!       Values of spherical harmonics in the solar direction
    , accuracy_adaptive                                                 &
!       Accuracy for adaptive truncation
    , euler_factor
!       Factor applied to the last term of an alternating series

!                   Treatment of scattering
  INTEGER, INTENT(IN) ::                                                &
      i_scatter_method_band                                             &
!       Method of treating scattering in the band
    , i_scatter_method_term(nd_esft_term, nd_band, nd_species)
!       Method of treating scattering for each k-term

!                   Options for solver
  INTEGER, INTENT(IN) ::                                                &
      i_solver                                                          &
!       Solver used
    , i_gas_overlap
!       Gas overlap assumption

!                   Gaseous properties
  INTEGER, INTENT(IN) ::                                                &
      i_band                                                            &
!       Band being considered
    , n_gas                                                             &
!       Number of gases in band
    , index_absorb(nd_species, nd_band)                                 &
!       List of absorbers in bands
    , i_band_esft(nd_band, nd_species)                                  &
!       Number of terms in band
    , i_scale_esft(nd_band, nd_species)                                 &
!       Type of ESFT scaling
    , i_scale_fnc(nd_band, nd_species)
!       Type of scaling function
  LOGICAL, INTENT(IN) ::                                                &
      l_doppler(nd_species)
!       Doppler broadening included

  REAL (RealK), INTENT(IN) ::                                           &
      k_esft(nd_esft_term, nd_band, nd_species)                         &
!       Exponential ESFT terms
    , k_esft_layer(nd_profile, nd_layer, nd_esft_term, nd_species)      &
!       Exponential ESFT terms at actual pressure layer
    , w_esft(nd_esft_term, nd_band, nd_species)                         &
!       Weights for ESFT
    , scale_vector(nd_scale_variable, nd_esft_term, nd_band             &
        , nd_species)                                                   &
!       Absorber scaling parameters
    , p_reference(nd_species, nd_band)                                  &
!       Reference scaling pressure
    , t_reference(nd_species, nd_band)                                  &
!       Reference scaling temperature
    , gas_mix_ratio(nd_profile, nd_layer, nd_species)                   &
!       Gas mass mixing ratios
    , doppler_correction(nd_species)
!       Doppler broadening terms
  REAL (RealK), INTENT(INOUT) ::                                        &
      gas_frac_rescaled(nd_profile, nd_layer, nd_species)
!       Rescaled gas mass fractions

!                   Spectral region
  INTEGER, INTENT(IN) ::                                                &
      isolir
!       Spectral region

!                   Solar properties
  REAL (RealK), INTENT(IN) ::                                           &
      zen_0(nd_profile)                                                 &
!       Secant (two-stream) or cosine (spherical harmonics)
!       of the solar zenith angle
    , solar_irrad(nd_profile)
!       Incident solar irradiance in band

!                   Infra-red properties
  LOGICAL, INTENT(IN) ::                                                &
      l_ir_source_quad
!       Use a quadratic source function
  REAL (RealK), INTENT(IN) ::                                           &
      planck_flux_band(nd_profile, 0: nd_layer)                         &
!       Flux Planckian source in band
    , diff_planck_band(nd_profile, nd_layer)                            &
!       First differences in the flux Planckian across layers
!       in this band
    , diff_planck_band_2(nd_profile, nd_layer)
!       Twice 2nd differences in the flux Planckian in band

!                   Surface properties
  REAL (RealK), INTENT(IN) ::                                           &
      planck_flux_surface(nd_profile)
!       Flux Planckian at the surface temperature
  INTEGER, INTENT(IN) ::                                                &
      ls_brdf_trunc                                                     &
!       Order of truncation of BRDFs
    , n_brdf_basis_fnc
!       Number of BRDF basis functions
  REAL (RealK), INTENT(IN) ::                                           &
      rho_alb(nd_profile, nd_brdf_basis_fnc)                            &
!       Weights of the basis functions
    , f_brdf(nd_brdf_basis_fnc, 0: nd_brdf_trunc/2                      &
        , 0: nd_brdf_trunc/2, 0: nd_brdf_trunc)                         &
!       Array of BRDF basis terms
    , brdf_sol(nd_profile, nd_brdf_basis_fnc, nd_direction)             &
!       The BRDF evaluated for scattering from the solar
!       beam into the viewing direction
    , brdf_hemi(nd_profile, nd_brdf_basis_fnc, nd_direction)            &
!       The BRDF evaluated for scattering from isotropic
!       radiation into the viewing direction
    , diff_albedo_basis(nd_brdf_basis_fnc)
!       The diffuse albedo of each basis function

!                   Variables related to tiling of the surface
  LOGICAL, INTENT(IN) ::                                                &
      l_tile
!       Logical to allow invoke options
  INTEGER, INTENT(IN) ::                                                &
      n_point_tile                                                      &
!       Number of points to tile
    , n_tile                                                            &
!       Number of tiles used
    , list_tile(nd_point_tile)
!       List of points with surface tiling
  REAL (RealK), INTENT(IN) ::                                           &
      rho_alb_tile(nd_point_tile, nd_brdf_basis_fnc, nd_tile)           &
!       Weights for the basis functions of the BRDFs
!       at the tiled points
    , planck_flux_tile(nd_point_tile, nd_tile)
!       Local Planckian fluxes on surface tiles

!                   Optical properties
  TYPE(str_ss_prop), INTENT(INOUT) :: ss_prop
!   Single scattering properties of the atmosphere

!                   Cloudy properties
  LOGICAL, INTENT(IN) ::                                                &
      l_cloud
!       Clouds required
  INTEGER, INTENT(IN) ::                                                &
      i_cloud
!       Cloud scheme used

!                   Cloud geometry
  INTEGER, INTENT(IN) ::                                                &
      n_cloud_top                                                       &
!       Topmost cloudy layer
    , n_region                                                          &
!       Number of cloudy regions
    , k_clr                                                             &
!       Index of clear-sky region
    , i_region_cloud(nd_cloud_type)
!       Regions in which types of clouds fall

  INTEGER, INTENT(IN) ::                                                &
      n_column_slv(nd_profile)                                          &
!       Number of columns to be solved in each profile
    , list_column_slv(nd_profile, nd_column)                            &
!       List of columns requiring an actual solution
    , i_clm_lyr_chn(nd_profile, nd_column)                              &
!       Layer in the current column to change
    , i_clm_cld_typ(nd_profile, nd_column)
!       Type of cloud to introduce in the changed layer
  REAL (RealK), INTENT(IN) ::                                           &
      w_free(nd_profile, id_ct: nd_layer)                               &
!       Clear-sky fraction
    , cloud_overlap(nd_profile, id_ct-1: nd_layer                       &
        , nd_overlap_coeff)                                             &
!       Coefficients for transfer for energy at interfaces
    , area_column(nd_profile, nd_column)                                &
!       Areas of columns
    , frac_region(nd_profile, id_ct: nd_layer, nd_region)
!       Fractions of total cloud occupied by each region


!                   Levels for calculating radiances
  INTEGER, INTENT(IN) ::                                                &
      n_viewing_level                                                   &
!       Number of levels where radiances are calculated
    , i_rad_layer(nd_viewing_level)
!       Layers in which radiances are calculated
  REAL (RealK), INTENT(IN) ::                                           &
      frac_rad_layer(nd_viewing_level)
!       Fractions below the tops of the layers

!                   Viewing geometry
  INTEGER, INTENT(IN) ::                                                &
      n_direction
!       Number of viewing directions
  REAL (RealK), INTENT(IN) ::                                           &
      direction(nd_radiance_profile, nd_direction, 2)
!       Viewing directions

!                   Calculated fluxes
  REAL (RealK), INTENT(INOUT) ::                                        &
      flux_direct(nd_flux_profile, 0: nd_layer)                         &
!       Direct flux in band
    , flux_down(nd_flux_profile, 0: nd_layer)                           &
!       Total downward flux
    , flux_up(nd_flux_profile, 0: nd_layer)
!       Upward flux

!                   Calculated radiances
  REAL (RealK), INTENT(INOUT) ::                                        &
      i_direct(nd_radiance_profile, 0: nd_layer)                        &
!       Direct solar irradiance on levels
    , radiance(nd_radiance_profile, nd_viewing_level, nd_direction)
!       Radiances in the current band

  REAL (RealK), INTENT(INOUT) ::                                        &
      photolysis(nd_j_profile, nd_viewing_level)
!       Rate of photolysis in the current band

!                   Flags for clear-sky fluxes
  LOGICAL, INTENT(IN) ::                                                &
      l_clear
!       Calculate clear-sky properties
  INTEGER, INTENT(IN) ::                                                &
      i_solver_clear
!       Clear solver used

!                   Clear-sky fluxes
  REAL (RealK), INTENT(OUT) ::                                          &
      flux_direct_clear(nd_flux_profile, 0: nd_layer)                   &
!       Clear-sky direct flux
    , flux_down_clear(nd_flux_profile, 0: nd_layer)                     &
!       Clear-sky total downward flux
    , flux_up_clear(nd_flux_profile, 0: nd_layer)                       &
!       Clear-sky total downward flux
    , flux_up_tile(nd_point_tile, nd_tile)                              &
!       Upward fluxes at tiled surface points
    , flux_up_blue_tile(nd_point_tile, nd_tile)
!       Upward blue fluxes at tiled surface points

!                   Special diagnostics:
  LOGICAL, INTENT(IN) ::                                                &
      l_blue_flux_surf
!       Flag to calculate blue fluxes at the surface
  REAL (RealK), INTENT(IN) ::                                           &
      weight_blue
!       Weights for blue fluxes in this band
  REAL (RealK), INTENT(INOUT) ::                                        &
      flux_direct_blue_surf(nd_flux_profile)                            &
!       Direct downward blue flux at the surface
    , flux_down_blue_surf(nd_flux_profile)                              &
!       Total downward blue flux at the surface
    , flux_up_blue_surf(nd_flux_profile)
!       Upward blue flux at the surface

!                   Variables required for McICA
  INTEGER, INTENT(IN) ::                                                &
      n_cloud_profile(id_ct: nd_layer)                                  &
!       Number of cloudy profiles in each layer
    , i_cloud_profile(nd_profile, id_ct: nd_layer)                      &
!       Profiles containing clouds
    , nd_cloud_component                                                &
!       Size allocated for components of clouds
    , i_cloud_type(nd_cloud_component)                                  &
!       Types of cloud to which each component contributes
    , i_cloud_representation
!       Representation of mixing rule chosen

  LOGICAL, INTENT(IN) ::                                                &
      l_cloud_cmp(nd_cloud_component)
!       Flags to activate cloudy components



! Local variables.
  INTEGER                                                               &
      i                                                                 &
!       Loop variable
    , j                                                                 &
!       Loop variable
    , k                                                                 &
!       Loop variable
    , l
!       Loop variable
  INTEGER                                                               &
      i_gas                                                             &
!       Index of main gas
    , i_gas_band                                                        &
!       Index of active gas
    , i_gas_pointer(nd_species)                                         &
!       Pointer array for monochromatic ESFTs
    , iex                                                               &
!       Index of ESFT term
    , i_scatter_method
!       Method of treating scattering
  REAL (RealK) ::                                                       &
      d_planck_flux_surface(nd_profile)                                 &
!       Difference in Planckian fluxes between the surface
!       and the air
    , flux_inc_direct(nd_profile)                                       &
!       Incident direct flux
    , flux_inc_down(nd_profile)                                         &
!       Incident downward flux
    , esft_weight                                                       &
!       ESFT weight for current calculation
    , adjust_solar_ke(nd_profile, nd_layer)                             &
!       Adjustment of solar transmission to `include' effects
!       of minor gases and take out equivalent extinction
    , k_eqv(nd_profile, nd_layer)                                       &
!       Equivalent extinction
    , tau_gas(nd_profile, nd_layer)                                     &
!       Optical depth of gas
    , k_esft_mono(nd_species)                                           &
!       Monochromatic exponents
    , k_esft_local(nd_esft_term)                                        &
!       Local copy of exponential ESFT terms or 1 if k_esft_layer used
    , k_gas_abs(nd_profile, nd_layer)                                   &
!       Gaseous extinction
    , diffuse_albedo(nd_profile)
!       Diffuse albedo of the surface
  REAL (RealK) ::                                                       &
      flux_direct_part(nd_flux_profile, 0: nd_layer)                    &
!       Partial direct flux
    , flux_total_part(nd_flux_profile, 2*nd_layer+2)                    &
!       Partial total flux
    , flux_direct_clear_part(nd_flux_profile, 0: nd_layer)              &
!       Clear partial direct flux
    , flux_total_clear_part(nd_flux_profile, 2*nd_layer+2)
!       Clear partial total flux
  REAL (RealK) ::                                                       &
      i_direct_part(nd_radiance_profile, 0: nd_layer)                   &
!       Partial solar irradiances
    , radiance_part(nd_radiance_profile, nd_viewing_level               &
        , nd_direction)
!       Partial radiances
  REAL (RealK) ::                                                       &
      photolysis_part(nd_j_profile, nd_viewing_level)
!       Partial rate of photolysis
  REAL (RealK) ::                                                       &
      weight_incr                                                       &
!       Weight applied to increments
    , weight_blue_incr
!       Weight applied to blue increments

! Fluxes used for equivalent extinction (we base the equivalent
! extinction on fluxes, even when calculating radiances, so
! full sizes are required for these arrays).
  REAL (RealK) ::                                                       &
      sum_flux(nd_profile, 2*nd_layer+2, nd_species)                    &
!       Sum of fluxes for weighting
    , sum_k_flux(nd_profile, 2*nd_layer+2, nd_species)                  &
!       Sum of k*fluxes for weighting
    , flux_term(nd_profile, 2*nd_layer+2)                               &
!       Flux with one term
    , flux_gas(nd_profile, 0: nd_layer)
!       Flux with one gas
  REAL (RealK) ::                                                       &
      mean_net_flux                                                     &
!       Mean net flux
    , mean_k_net_flux                                                   &
!       Mean k-weighted net flux
    , layer_inc_flux                                                    &
!       Layer incident fluxes (downward flux at top of layer
!       plus upward flux at bottom of layer)
    , layer_inc_k_flux                                                  &
!       Layer incident k-weighted fluxes
    , k_weak, k_min(nd_profile, nd_layer)
!       Weak absorption for minor gas

  REAL (RealK) :: temp(nd_profile),temp_exp(nd_profile)
  REAL (RealK) :: temp_max = LOG(1.0_RealK/EPSILON(temp_max))

  INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
  INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
  REAL(KIND=jprb)               :: zhook_handle

  CHARACTER(LEN=*), PARAMETER :: RoutineName='SOLVE_BAND_K_EQV'


  IF (lhook) CALL dr_hook(RoutineName,zhook_in,zhook_handle)

  i_gas=index_absorb(1, i_band)

  IF (isolir == ip_solar) THEN

!   An appropriate scaling factor is calculated for the direct
!   beam, whilst the equivalent extinction for the diffuse beam
!   is weighted with the solar scaling factor as evaluated
!   at the surface.

!   Initialize the scaling factors:
    DO i=1, n_layer
      DO l=1, n_profile
        adjust_solar_ke(l, i)=1.0e+00_RealK
        k_eqv(l, i)=0.0e+00_RealK
      END DO
    END DO

    DO j=2, n_gas

!     Initialize the normalized flux for the gas.
      DO l=1, n_profile
        flux_gas(l, 0)=1.0e+00_RealK
        sum_flux(l, n_layer, j)=0.0e+00_RealK
      END DO
      DO i=1, n_layer
        DO l=1, n_profile
          flux_gas(l, i)=0.0e+00_RealK
          sum_k_flux(l, i, j)=0.0e+00_RealK
        END DO
      END DO

      IF (i_gas_overlap == ip_overlap_t_eqv .OR.                        &
          i_gas_overlap == ip_overlap_k_eqv_scl) THEN
        k_min=HUGE(k_min)
      END IF

      i_gas_band=index_absorb(j, i_band)
      DO iex=1, i_band_esft(i_band, i_gas_band)

!       Store the ESFT weight for future use.
        esft_weight=w_esft(iex, i_band,  i_gas_band)

!       Store the absorption coefficient
        IF (i_scale_fnc(i_band, i_gas_band) == ip_scale_lookup) THEN
!         In this case gas_frac_rescaled has already been scaled by k
          k_esft_local(iex) = 1.0_RealK
        ELSE
          k_esft_local(iex) = k_esft(iex, i_band, i_gas_band)
        END IF

!       Rescale the amount of gas for this absorber if required.
        IF (i_scale_esft(i_band, i_gas_band) == ip_scale_term) THEN
! DEPENDS ON: scale_absorb
          CALL scale_absorb(ierr, n_profile, n_layer                    &
            , gas_mix_ratio(1, 1, i_gas_band), p, t                     &
            , i_top                                                     &
            , gas_frac_rescaled(1, 1, i_gas_band)                       &
            , k_esft_layer(1, 1, iex, i_gas_band)                       &
            , i_scale_fnc(i_band, i_gas_band)                           &
            , p_reference(i_gas_band, i_band)                           &
            , t_reference(i_gas_band, i_band)                           &
            , scale_vector(1, iex, i_band, i_gas_band)                  &
            , iex, i_band                                               &
            , l_doppler(i_gas_band)                                     &
            , doppler_correction(i_gas_band)                            &
            , nd_profile, nd_layer                                      &
            , nd_scale_variable                                         &
            )
        END IF

!       For use in the infra-red case flux_term is defined to start
!       at 1, so for this array only the flux at level i appears
!       as the i+1st element.
        DO l=1, n_profile
          flux_term(l, 1)=esft_weight
        END DO
!       Because the contents of zen_0 depend on the mode of
!       angular integration we need two different loops.
        IF (i_angular_integration == ip_two_stream) THEN
          DO i=1, n_layer
            DO l=1, n_profile
              temp(l)=-k_esft_local(iex)                                 &
                *gas_frac_rescaled(l, i, i_gas_band)                     &
                *d_mass(l, i)*zen_0(l)
            END DO
            CALL exp_v(n_profile,temp,temp_exp)
            DO l=1,n_profile
              flux_term(l, i+1)=flux_term(l, i)*temp_exp(l)
              flux_gas(l, i)=flux_gas(l, i)+flux_term(l, i+1)
            END DO
          END DO
        ELSE IF (i_angular_integration == ip_spherical_harmonic) THEN
          DO i=1, n_layer
            DO l=1, n_profile
              flux_term(l, i+1)=flux_term(l, i)                         &
                *EXP(-k_esft_local(iex)                                 &
                *gas_frac_rescaled(l, i, i_gas_band)                    &
                *d_mass(l, i)/zen_0(l))
              flux_gas(l, i)=flux_gas(l, i)+flux_term(l, i+1)
            END DO
          END DO
        END IF

!       Calculate the increment in the absorptive extinction
        IF (i_gas_overlap == ip_overlap_t_eqv) THEN
          DO i=1, n_layer
            DO l=1, n_profile
              temp(l) = -k_esft_local(iex)                              &
                *gas_frac_rescaled(l, i, i_gas_band)
            END DO
            CALL exp_v(n_profile,temp,temp_exp)
            DO l=1, n_profile
              sum_k_flux(l, i, j)                                       &
                =sum_k_flux(l, i, j)                                    &
                +temp_exp(l)*flux_term(l, n_layer+1)
              k_min(l, i)=MIN(k_min(l, i),                              &
                k_esft_local(iex)                                       &
                *gas_frac_rescaled(l, i, i_gas_band))
            END DO
          END DO
          DO l=1, n_profile
            sum_flux(l, n_layer, j)                                     &
              =sum_flux(l, n_layer, j)+flux_term(l, n_layer+1)
          END DO
        ELSE IF (i_gas_overlap == ip_overlap_k_eqv_scl) THEN
          DO i=1, n_layer
            DO l=1, n_profile
              sum_k_flux(l, i, j)                                       &
                =sum_k_flux(l, i, j)                                    &
                +k_esft_local(iex)                                      &
                *gas_frac_rescaled(l, i, i_gas_band)                    &
                *flux_term(l, n_layer+1)
              k_min(l, i)=MIN(k_min(l, i),                              &
                k_esft_local(iex)                                       &
                *gas_frac_rescaled(l, i, i_gas_band))
            END DO
          END DO
          DO l=1, n_profile
            sum_flux(l, n_layer, j)                                     &
              =sum_flux(l, n_layer, j)+flux_term(l, n_layer+1)
          END DO
        ELSE
          DO l=1, n_profile
            sum_k_flux(l, n_layer, j)                                   &
              =sum_k_flux(l, n_layer, j)                                &
              +k_esft_local(iex)                                        &
              *flux_term(l, n_layer+1)
            sum_flux(l, n_layer, j)                                     &
              =sum_flux(l, n_layer, j)+flux_term(l, n_layer+1)
          END DO
        END IF

      END DO

!     Set the equivalent extinction for the diffuse beam,
!     weighting with the direct surface flux.
      DO i=1, n_layer
        DO l=1, n_profile
          IF (i_gas_overlap == ip_overlap_t_eqv) THEN
            IF (sum_flux(l, n_layer, j) >  0.0e+00_RealK) THEN
              k_eqv(l, i)=k_eqv(l, i)                                   &
                -LOG(sum_k_flux(l, i, j)/sum_flux(l, n_layer, j))
            ELSE
              k_eqv(l, i)=k_eqv(l, i)+k_min(l, i)
            END IF
          ELSE IF (i_gas_overlap == ip_overlap_k_eqv_scl) THEN
            IF (sum_flux(l, n_layer, j) >  0.0e+00_RealK) THEN
              k_eqv(l, i)=k_eqv(l, i)                                   &
                +sum_k_flux(l, i, j)/sum_flux(l, n_layer, j)
            ELSE
              k_eqv(l, i)=k_eqv(l, i)+k_min(l, i)
            END IF
          ELSE
            IF (sum_flux(l, n_layer, j) >  0.0e+00_RealK) THEN
              k_eqv(l, i)=k_eqv(l, i)                                   &
                +gas_frac_rescaled(l, i, i_gas_band)                    &
                *sum_k_flux(l, n_layer, j)                              &
                /sum_flux(l, n_layer, j)
            ELSE
!             This case can arise only when the sun is close
!             to the horizon when the exponential may underflow
!             to 0. we use the weakest ESFT-term.
              k_eqv(l, i)=k_eqv(l, i)                                   &
                *k_esft_local(1)                                        &
                *gas_frac_rescaled(l, i, i_gas_band)
            END IF
          END IF
          IF (flux_gas(l, i-1) >  0.0e+00_RealK) THEN
!           If the flux has been reduced to 0 at the upper
!           level the adjusting factor is not of importance
!           and need not be adjusted. this will prevent
!           possible failures.
            adjust_solar_ke(l, i)                                       &
              =adjust_solar_ke(l, i)*flux_gas(l, i)                     &
              /flux_gas(l, i-1)
          END IF
        END DO
      END DO

    END DO

!   Since the grey extinction will later be modified we must
!   increase the transmission of the solar beam to compensate.
!   This may overflow for very large zenith angles (where the
!   transmission is effectively zero) so we restrict to a max value.
    IF (i_angular_integration == ip_two_stream) THEN
      DO i=1, n_layer
        DO l=1, n_profile
           temp(l) = MIN(k_eqv(l,i)*d_mass(l,i)*zen_0(l),temp_max)
        END DO
        CALL exp_v(n_profile,temp,temp_exp)
        DO l=1,n_profile
           adjust_solar_ke(l,i) = adjust_solar_ke(l,i)*temp_exp(l)
        END DO
      END DO
    ELSE IF (i_angular_integration == ip_spherical_harmonic) THEN
      DO i=1, n_layer
        DO l=1, n_profile
           temp(l) = k_eqv(l,i)*d_mass(l,i)/zen_0(l)
        END DO
        CALL exp_v(n_profile,temp,temp_exp)
        DO l=1,n_profile
           adjust_solar_ke(l,i) = adjust_solar_ke(l,i)*temp_exp(l)
        END DO
      END DO
    END IF

  ELSE IF (isolir == ip_infra_red) THEN

!   Calculate the diffuse albedo of the surface.
    IF (i_angular_integration == ip_two_stream) THEN
      DO l=1, n_profile
        diffuse_albedo(l)=rho_alb(l, ip_surf_alb_diff)
      END DO
    ELSE IF (i_angular_integration == ip_ir_gauss) THEN
!     Only a non-reflecting surface is consistent with this option.
      DO l=1, n_profile
        diffuse_albedo(l)=0.0e+00_RealK
      END DO
    ELSE IF (i_angular_integration == ip_spherical_harmonic) THEN
      DO l=1, n_profile
        diffuse_albedo(l)=rho_alb(l, 1)*diff_albedo_basis(1)
      END DO
      DO j=1, n_brdf_basis_fnc
        DO l=1, n_profile
          diffuse_albedo(l)=rho_alb(l, j)*diff_albedo_basis(j)
        END DO
      END DO
    END IF

!   Equivalent absorption is used for the minor gases.

    DO j=2, n_gas


!     Initialize the sums to form the ratio to 0.
      DO i=1, 2*n_layer+2
        DO l=1, n_profile
          sum_flux(l, i, j)=0.0e+00_RealK
          sum_k_flux(l, i, j)=0.0e+00_RealK
        END DO
      END DO

      i_gas_band=index_absorb(j, i_band)
      DO iex=1, i_band_esft(i_band, i_gas_band)

!       Store the ESFT weight for future use.
        esft_weight=w_esft(iex, i_band,  i_gas_band)

!       Store the absorption coefficient
        IF (i_scale_fnc(i_band, i_gas_band) == ip_scale_lookup) THEN
!         In this case gas_frac_rescaled has already been scaled by k
          k_esft_local(iex) = 1.0_RealK
        ELSE
          k_esft_local(iex) = k_esft(iex, i_band, i_gas_band)
        END IF

!       Rescale the amount of gas for this absorber if required.
        IF (i_scale_esft(i_band, i_gas_band) == ip_scale_term) THEN
          CALL scale_absorb(ierr, n_profile, n_layer                    &
            , gas_mix_ratio(1, 1, i_gas_band), p, t                     &
            , i_top                                                     &
            , gas_frac_rescaled(1, 1, i_gas_band)                       &
            , k_esft_layer(1, 1, iex, i_gas_band)                       &
            , i_scale_fnc(i_band, i_gas_band)                           &
            , p_reference(i_gas_band, i_band)                           &
            , t_reference(i_gas_band, i_band)                           &
            , scale_vector(1, iex, i_band, i_gas_band)                  &
            , iex, i_band                                               &
            , l_doppler(i_gas_band)                                     &
            , doppler_correction(i_gas_band)                            &
            , nd_profile, nd_layer                                      &
            , nd_scale_variable                                         &
            )
        END IF

!       Set the appropriate boundary terms for the
!       total upward and downward fluxes at the boundaries.

        DO l=1, n_profile
          flux_inc_direct(l)=0.0e+00_RealK
          flux_inc_down(l)=-planck_flux_band(l, 0)
          d_planck_flux_surface(l)=planck_flux_surface(l)               &
            -planck_flux_band(l, n_layer)
        END DO

!       Set the optical depths of each layer.
        DO i=1, n_layer
          DO l=1, n_profile
            tau_gas(l, i)=k_esft_local(iex)                             &
              *gas_frac_rescaled(l, i, i_gas_band)                      &
              *d_mass(l, i)
          END DO
        END DO

!       Calculate the fluxes with just this gas. flux_term is
!       passed to both the direct and total fluxes as we do
!       not calculate any direct flux here.
! DEPENDS ON: monochromatic_gas_flux
        CALL monochromatic_gas_flux(n_profile, n_layer                  &
          , tau_gas                                                     &
          , isolir, zen_0, flux_inc_direct, flux_inc_down               &
          , diff_planck_band, d_planck_flux_surface                     &
          , diffuse_albedo, diffuse_albedo                              &
          , diffusivity_factor_minor                                    &
          , flux_term, flux_term                                        &
          , nd_profile, nd_layer                                        &
          )

        IF (i_gas_overlap == ip_overlap_t_eqv) THEN
          DO i=2, 2*n_layer+1
            DO l=1, n_profile
              temp(l) = -k_esft_local(iex)                              &
                *gas_frac_rescaled(l, i/2, i_gas_band)
            END DO
            CALL exp_v(n_profile,temp,temp_exp)
            DO l=1, n_profile
              sum_k_flux(l, i, j)=sum_k_flux(l, i, j)                   &
                +temp_exp(l)*esft_weight*ABS(flux_term(l, i))
              sum_flux(l, i, j)=sum_flux(l, i, j)                       &
                +esft_weight*ABS(flux_term(l, i))
            END DO
          END DO
        ELSE IF (i_gas_overlap == ip_overlap_k_eqv_scl) THEN
          DO i=2, 2*n_layer+1
            DO l=1, n_profile
              sum_k_flux(l, i, j)=sum_k_flux(l, i, j)                   &
                +k_esft_local(iex)                                      &
                *gas_frac_rescaled(l, i/2, i_gas_band)                  &
                *esft_weight*ABS(flux_term(l, i))
              sum_flux(l, i, j)=sum_flux(l, i, j)                       &
                +esft_weight*ABS(flux_term(l, i))
            END DO
          END DO
        ELSE IF (i_gas_overlap == ip_overlap_k_eqv_mod) THEN
          DO i=1, 2*n_layer+2
            DO l=1, n_profile
              sum_k_flux(l, i, j)=sum_k_flux(l, i, j)                   &
                +k_esft_local(iex)                                      &
                *esft_weight*ABS(flux_term(l, i))
              sum_flux(l, i, j)=sum_flux(l, i, j)                       &
                +esft_weight*ABS(flux_term(l, i))
            END DO
          END DO
        ELSE
          DO i=1, 2*n_layer+2
            DO l=1, n_profile
              sum_k_flux(l, i, j)=sum_k_flux(l, i, j)                   &
                +k_esft_local(iex)                                      &
                *esft_weight*flux_term(l, i)
              sum_flux(l, i, j)=sum_flux(l, i, j)                       &
                +esft_weight*flux_term(l, i)
            END DO
          END DO
        END IF

      END DO

    END DO


    DO i=1, n_layer
      DO l=1, n_profile
        k_eqv(l, i)=0.0e+00_RealK
      END DO
    END DO

    IF (i_gas_overlap == ip_overlap_t_eqv) THEN
      DO j=2, n_gas
        DO i=1, n_layer
          DO l=1, n_profile
            layer_inc_k_flux=sum_k_flux(l, 2*i, j)                      &
               +sum_k_flux(l, 2*i+1, j)
            layer_inc_flux=sum_flux(l, 2*i, j)                          &
               +sum_flux(l, 2*i+1, j)
            k_weak=-LOG(layer_inc_k_flux/layer_inc_flux)
            k_eqv(l, i)=k_eqv(l, i)+k_weak
          END DO
        END DO
      END DO
    ELSE IF (i_gas_overlap == ip_overlap_k_eqv_scl) THEN
      DO j=2, n_gas
        DO i=1, n_layer
          DO l=1, n_profile
            layer_inc_k_flux=sum_k_flux(l, 2*i, j)                      &
               +sum_k_flux(l, 2*i+1, j)
            layer_inc_flux=sum_flux(l, 2*i, j)                          &
               +sum_flux(l, 2*i+1, j)
            k_eqv(l, i)=k_eqv(l, i)+layer_inc_k_flux/layer_inc_flux
          END DO
        END DO
      END DO
    ELSE IF (i_gas_overlap == ip_overlap_k_eqv_mod) THEN
      DO j=2, n_gas
        DO i=1, n_layer
          DO l=1, n_profile
            layer_inc_k_flux=sum_k_flux(l, 2*i, j)                      &
               +sum_k_flux(l, 2*i+1, j)
            layer_inc_flux=sum_flux(l, 2*i, j)                          &
               +sum_flux(l, 2*i+1, j)
            k_weak=layer_inc_k_flux/layer_inc_flux
            k_eqv(l, i)=k_eqv(l, i)+k_weak                              &
              *gas_frac_rescaled(l, i, index_absorb(j, i_band))
          END DO
        END DO
      END DO
    ELSE
      DO j=2, n_gas
        DO i=1, n_layer
          DO l=1, n_profile
            mean_k_net_flux=0.5e+00_RealK*(sum_k_flux(l, 2*i, j)        &
              +sum_k_flux(l, 2*i+2, j)                                  &
              -sum_k_flux(l, 2*i-1, j)                                  &
              -sum_k_flux(l, 2*i+1, j))
            mean_net_flux=0.5e+00_RealK*(sum_flux(l, 2*i, j)            &
              +sum_flux(l, 2*i+2, j)                                    &
              -sum_flux(l, 2*i-1, j)                                    &
              -sum_flux(l, 2*i+1, j))
!           Negative effective extinctions  are very unlikely
!           to arise, but must be removed.
            k_weak=MAX(0.0e+00_RealK,mean_k_net_flux/mean_net_flux)
            k_eqv(l, i)=k_eqv(l, i)+k_weak                              &
              *gas_frac_rescaled(l, i, index_absorb(j, i_band))
          END DO
        END DO
      END DO
    END IF

  END IF

! Augment the grey extinction with an effective value for each gas.
  DO i=1, n_cloud_top-1
    DO l=1, n_profile
      ss_prop%k_grey_tot_clr(l, i)=ss_prop%k_grey_tot_clr(l, i)         &
        +k_eqv(l, i)
    END DO
  END DO
  DO i=n_cloud_top, n_layer
    DO l=1, n_profile
      ss_prop%k_grey_tot(l, i, 0)=ss_prop%k_grey_tot(l, i, 0)           &
        +k_eqv(l, i)
    END DO
  END DO
  IF (l_cloud) THEN
    DO k=1, cld%n_cloud_type
      DO i=n_cloud_top, n_layer
        DO l=1, n_profile
          ss_prop%k_grey_tot(l, i, k)                                   &
            =ss_prop%k_grey_tot(l, i, k)+k_eqv(l, i)
        END DO
      END DO
    END DO
  END IF

! The ESFT terms for the major gas in the band are used with
! appropriate weighted terms for the minor gases.
  i_gas_pointer(1)=i_gas
  DO iex=1, i_band_esft(i_band, i_gas)

    IF (i_scatter_method_band == ip_scatter_hybrid) THEN
      i_scatter_method = i_scatter_method_term(iex, i_band,  i_gas)
    ELSE
      i_scatter_method = i_scatter_method_band
    END IF

!   Store the ESFT weight for future use.
    esft_weight=w_esft(iex, i_band,  i_gas)

!   Rescale for each ESFT term if that is required.
    IF (i_scale_esft(i_band, i_gas) == ip_scale_term) THEN
      CALL scale_absorb(ierr, n_profile, n_layer                        &
        , gas_mix_ratio(1, 1, i_gas), p, t                              &
        , i_top                                                         &
        , gas_frac_rescaled(1, 1, i_gas)                                &
        , k_esft_layer(1, 1, iex, i_gas)                                &
        , i_scale_fnc(i_band, i_gas)                                    &
        , p_reference(i_gas, i_band)                                    &
        , t_reference(i_gas, i_band)                                    &
        , scale_vector(1, iex, i_band, i_gas)                           &
        , iex, i_band                                                   &
        , l_doppler(i_gas), doppler_correction(i_gas)                   &
        , nd_profile, nd_layer                                          &
        , nd_scale_variable                                             &
        )
    END IF

!   Set the appropriate boundary terms for the total
!   upward and downward fluxes.

    IF ( (i_angular_integration == ip_two_stream).OR.                   &
         (i_angular_integration == ip_ir_gauss) ) THEN

      IF (isolir == ip_solar) THEN
!       Solar region.
        DO l=1, n_profile
          d_planck_flux_surface(l)=0.0e+00_RealK
          flux_inc_down(l)=solar_irrad(l)/zen_0(l)
          flux_inc_direct(l)=solar_irrad(l)/zen_0(l)
        END DO
      ELSE IF (isolir == ip_infra_red) THEN
!       Infra-red region.
        DO l=1, n_profile
          flux_inc_direct(l)=0.0e+00_RealK
          flux_direct_part(l, n_layer)=0.0e+00_RealK
          flux_inc_down(l)=-planck_flux_band(l, 0)
          d_planck_flux_surface(l)                                      &
            =planck_flux_surface(l)                                     &
            -planck_flux_band(l, n_layer)
        END DO
        IF (l_clear) THEN
          DO l=1, n_profile
            flux_direct_clear_part(l, n_layer)=0.0e+00_RealK
          END DO
        END IF
      END IF

    ELSE IF (i_angular_integration == ip_spherical_harmonic) THEN

      IF (isolir == ip_solar) THEN
        DO l=1, n_profile
          i_direct_part(l, 0)=solar_irrad(l)
          flux_inc_down(l)=0.0e+00_RealK
        END DO
      ELSE
        DO l=1, n_profile
          flux_inc_down(l)=-planck_flux_band(l, 0)
          d_planck_flux_surface(l)                                      &
            =planck_flux_surface(l)-planck_flux_band(l, n_layer)
        END DO
      END IF

    END IF


!   Assign the monochromatic absorption coefficient.
    IF (i_scale_fnc(i_band, i_gas) == ip_scale_lookup) THEN
!     In this case gas_frac_rescaled has already been scaled by k
      k_esft_mono(i_gas) = 1.0_RealK
    ELSE
      k_esft_mono(i_gas) = k_esft(iex, i_band, i_gas)
    END IF

! DEPENDS ON: gas_optical_properties
    CALL gas_optical_properties(n_profile, n_layer                      &
      , 1, i_gas_pointer, k_esft_mono                                   &
      , gas_frac_rescaled                                               &
      , k_gas_abs                                                       &
      , nd_profile, nd_layer, nd_species                                &
      )

    IF (i_cloud == ip_cloud_mcica) THEN

! DEPENDS ON: mcica_sample
      CALL mcica_sample(ierr                                            &
        , control, dimen, cld, bound                                    &
!                   Atmospheric properties
        , n_profile, n_layer, d_mass                                    &
!                   Angular integration
        , i_angular_integration, i_2stream                              &
        , l_rescale, n_order_gauss                                      &
        , n_order_phase, ms_min, ms_max, i_truncation                   &
        , ls_local_trunc                                                &
        , accuracy_adaptive, euler_factor                               &
        , i_sph_algorithm, i_sph_mode                                   &
!                   Precalculated angular arrays
        , ia_sph_mm, cg_coeff, uplm_zero, uplm_sol                      &
!                   Treatment of scattering
        , i_scatter_method                                              &
!                   Options for solver
        , i_solver                                                      &
!                   Gaseous propreties
        , k_gas_abs                                                     &
!                   Options for equivalent extinction
        , .TRUE., adjust_solar_ke                                       &
!                   Spectral region
        , isolir                                                        &
!                   Infra-red properties
        , diff_planck_band                                              &
        , l_ir_source_quad, diff_planck_band_2                          &
!                   Conditions at TOA
        , zen_0, flux_inc_direct, flux_inc_down                         &
        , i_direct_part                                                 &
!                   Surface properties
        , d_planck_flux_surface                                         &
        , ls_brdf_trunc, n_brdf_basis_fnc, rho_alb                      &
        , f_brdf, brdf_sol, brdf_hemi                                   &
!                   Optical properties
        , ss_prop                                                       &
!                   Cloudy properties
        , l_cloud, i_cloud                                              &
!                   Cloud geometry
        , n_cloud_top                                                   &
        , n_region, k_clr, i_region_cloud, frac_region                  &
        , w_free, cloud_overlap                                         &
        , n_column_slv, list_column_slv                                 &
        , i_clm_lyr_chn, i_clm_cld_typ, area_column                     &
!                   Additional variables required for McICA
        , l_cloud_cmp, n_cloud_profile, i_cloud_profile                 &
        , i_cloud_type, nd_cloud_component, iex, i_band                 &
        , i_cloud_representation                                        &
!                   Levels for the calculation of radiances
        , n_viewing_level, i_rad_layer, frac_rad_layer                  &
!                   Viewing geometry
        , n_direction, direction                                        &
!                   Calculated fluxes
        , flux_direct_part, flux_total_part                             &
!                   Calculated radiances
        , radiance_part                                                 &
!                   Calculated rate of photolysis
        , photolysis_part                                               &
!                   Flags for clear-sky calculations
        , l_clear, i_solver_clear                                       &
!                   Clear-sky fluxes calculated
        , flux_direct_clear_part, flux_total_clear_part                 &
!                   Dimensions of arrays
        , nd_profile, nd_layer, nd_layer_clr, id_ct, nd_column          &
        , nd_flux_profile, nd_radiance_profile, nd_j_profile            &
        , nd_cloud_type, nd_region, nd_overlap_coeff                    &
        , nd_max_order, nd_sph_coeff                                    &
        , nd_brdf_basis_fnc, nd_brdf_trunc, nd_viewing_level            &
        , nd_direction, nd_source_coeff                                 &
        )

    ELSE

! DEPENDS ON: monochromatic_radiance
      CALL monochromatic_radiance(ierr                                  &
        , control, cld, bound                                           &
!                   Atmospheric properties
        , n_profile, n_layer, d_mass                                    &
!                   Angular integration
        , i_angular_integration, i_2stream                              &
        , l_rescale, n_order_gauss                                      &
        , n_order_phase, ms_min, ms_max, i_truncation                   &
        , ls_local_trunc                                                &
        , accuracy_adaptive, euler_factor                               &
        , i_sph_algorithm, i_sph_mode                                   &
!                   Precalculated angular arrays
        , ia_sph_mm, cg_coeff, uplm_zero, uplm_sol                      &
!                   Treatment of scattering
        , i_scatter_method                                              &
!                   Options for solver
        , i_solver                                                      &
!                   Gaseous propreties
        , k_gas_abs                                                     &
!                   Options for equivalent extinction
        , .TRUE., adjust_solar_ke                                       &
!                   Spectral region
        , isolir                                                        &
!                   Infra-red properties
        , diff_planck_band                                              &
        , l_ir_source_quad, diff_planck_band_2                          &
!                   Conditions at TOA
        , zen_0, flux_inc_direct, flux_inc_down                         &
        , i_direct_part                                                 &
!                   Surface properties
        , d_planck_flux_surface                                         &
        , ls_brdf_trunc, n_brdf_basis_fnc, rho_alb                      &
        , f_brdf, brdf_sol, brdf_hemi                                   &
!                   Optical properties
        , ss_prop                                                       &
!                   Cloudy properties
        , l_cloud, i_cloud                                              &
!                   Cloud geometry
        , n_cloud_top, iex                                              &
        , n_region, k_clr, i_region_cloud, frac_region                  &
        , w_free, cloud_overlap                                         &
        , n_column_slv, list_column_slv                                 &
        , i_clm_lyr_chn, i_clm_cld_typ, area_column                     &
!                   Levels for the calculation of radiances
        , n_viewing_level, i_rad_layer, frac_rad_layer                  &
!                   Viewing geometry
        , n_direction, direction                                        &
!                   Calculated fluxes
        , flux_direct_part, flux_total_part                             &
!                   Calculated radiances
        , radiance_part                                                 &
!                   Calculated rate of photolysis
        , photolysis_part                                               &
!                   Flags for clear-sky calculations
        , l_clear, i_solver_clear                                       &
!                   Clear-sky fluxes calculated
        , flux_direct_clear_part, flux_total_clear_part                 &
!                   Dimensions of arrays
        , nd_profile, nd_layer, nd_layer_clr, id_ct, nd_column          &
        , nd_flux_profile, nd_radiance_profile, nd_j_profile            &
        , nd_cloud_type, nd_region, nd_overlap_coeff                    &
        , nd_max_order, nd_sph_coeff                                    &
        , nd_brdf_basis_fnc, nd_brdf_trunc, nd_viewing_level            &
        , nd_direction, nd_source_coeff                                 &
        )

    END IF

!   Increment the fluxes within the band.
    weight_incr=weight_band*esft_weight
    IF (l_blue_flux_surf)                                               &
      weight_blue_incr=weight_blue*esft_weight

! DEPENDS ON: augment_radiance
    CALL augment_radiance(n_profile, n_layer                            &
      , i_angular_integration, i_sph_mode                               &
      , n_viewing_level, n_direction                                    &
      , isolir, l_clear, l_initial, weight_incr                         &
      , l_blue_flux_surf, weight_blue_incr                              &
!                   Actual radiances
      , flux_direct, flux_down, flux_up                                 &
      , flux_direct_blue_surf                                           &
      , flux_down_blue_surf, flux_up_blue_surf                          &
      , i_direct, radiance, photolysis                                  &
      , flux_direct_clear, flux_down_clear, flux_up_clear               &
!                   Increments to radiances
      , flux_direct_part, flux_total_part                               &
      , i_direct_part, radiance_part, photolysis_part                   &
      , flux_direct_clear_part, flux_total_clear_part                   &
!                   Dimensions
      , nd_flux_profile, nd_radiance_profile, nd_j_profile              &
      , nd_layer, nd_viewing_level, nd_direction                        &
      )

!   Add in the increments from surface tiles
    IF (l_tile) THEN
! DEPENDS ON: augment_tiled_radiance
      CALL augment_tiled_radiance(ierr                                  &
        , n_point_tile, n_tile, list_tile                               &
        , i_angular_integration, isolir, l_initial                      &
        , weight_incr, l_blue_flux_surf, weight_blue_incr               &
!                   Surface characteristics
        , rho_alb_tile                                                  &
!                   Actual radiances
        , flux_up_tile, flux_up_blue_tile                               &
!                   Increments to radiances
        , flux_direct_part(1, n_layer)                                  &
        , flux_total_part(1, 2*n_layer+2)                               &
        , planck_flux_tile, planck_flux_band(1, n_layer)                &
!                   Dimensions
        , nd_flux_profile, nd_point_tile, nd_tile                       &
        , nd_brdf_basis_fnc                                             &
        )
    END IF

!   After the first call to these routines quantities should be
!   incremented rather than initialized, until the flag is reset.
    l_initial=.FALSE.

  END DO


  IF (lhook) CALL dr_hook(RoutineName,zhook_out,zhook_handle)

END SUBROUTINE solve_band_k_eqv
