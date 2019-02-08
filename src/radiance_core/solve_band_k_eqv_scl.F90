! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Subroutine to calculate fluxes using equivalent extinction.
!
! Method:
!   For each minor absorber an equivalent extinction is calculated
!   from a clear-sky calculation. These equivalent extinctions
!   are then used in a full calculation involving the major absorber.
!
!- ---------------------------------------------------------------------
SUBROUTINE solve_band_k_eqv_scl(ierr                                    &
    , control, dimen, atm, cld, bound, radout                           &
!                   Atmospheric properties
    , n_profile, n_layer, d_mass                                        &
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
    , i_solver                                                          &
!                   Gaseous properties
    , i_band, n_abs, index_abs, n_abs_esft, k_abs_layer, w_abs_esft     &
!                   Spectral region
    , isolir                                                            &
!                   Solar properties
    , zen_0, solar_irrad, sph                                           &
!                   Infra-red properties
    , planck                                                            &
!                   Surface properties
    , ls_brdf_trunc, n_brdf_basis_fnc, rho_alb                          &
    , f_brdf, brdf_sol, brdf_hemi                                       &
    , diff_albedo_basis                                                 &
!                   Tiling of the surface
    , l_tile, n_point_tile, n_tile, list_tile, rho_alb_tile             &
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
!                   Radiances
    , i_direct                                                          &
!                   Flags for clear-sky fluxes
    , l_clear, i_solver_clear                                           &
!                   Special surface fluxes
    , l_blue_flux_surf, weight_blue                                     &
!                   Dimensions of arrays
    , nd_profile, nd_layer, nd_layer_clr, id_ct, nd_column              &
    , nd_flux_profile, nd_radiance_profile, nd_j_profile                &
    , nd_abs, nd_esft_term                                              &
    , nd_cloud_type, nd_region, nd_overlap_coeff                        &
    , nd_max_order, nd_sph_coeff                                        &
    , nd_brdf_basis_fnc, nd_brdf_trunc, nd_viewing_level                &
    , nd_direction, nd_source_coeff                                     &
    , nd_point_tile, nd_tile                                            &
    )


  USE realtype_rd, ONLY: RealK
  USE def_control, ONLY: StrCtrl
  USE def_dimen,   ONLY: StrDim
  USE def_atm,     ONLY: StrAtm
  USE def_cld,     ONLY: StrCld
  USE def_bound,   ONLY: StrBound
  USE def_out,     ONLY: StrOut
  USE def_planck,  ONLY: StrPlanck
  USE def_ss_prop
  USE def_spherical_geometry, ONLY: StrSphGeo
  USE rad_pcf
  USE diffusivity_factor, ONLY: diffusivity_factor_minor
  USE vectlib_mod, ONLY: exp_v
  USE yomhook, ONLY: lhook, dr_hook
  USE parkind1, ONLY: jprb, jpim

  IMPLICIT NONE


! Control options:
  TYPE(StrCtrl),      INTENT(IN)    :: control

! Dimensions:
  TYPE(StrDim),       INTENT(IN)    :: dimen

! Atmospheric properties:
  TYPE(StrAtm),       INTENT(IN)    :: atm

! Cloud properties:
  TYPE(StrCld),       INTENT(IN)    :: cld

! Boundary conditions:
  TYPE(StrBound),     INTENT(IN)    :: bound

! Output fields:
  TYPE(StrOut),       INTENT(INOUT) :: radout

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
    , nd_abs                                                            &
!       Size allocated for absorbers
    , nd_esft_term                                                      &
!       Size allocated for ESFT terms
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
    , n_layer
!       Number of layers
  REAL (RealK), INTENT(IN) ::                                           &
      d_mass(nd_profile, nd_layer)
!       Mass thickness of each layer

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
    , i_scatter_method_term(nd_esft_term, nd_abs)
!       Method of treating scattering for each k-term

!                   Options for solver
  INTEGER, INTENT(IN) ::                                                &
      i_solver
!       Solver used

!                   Gaseous properties
  INTEGER, INTENT(IN) ::                                                &
      i_band                                                            &
!       Band being considered
    , n_abs                                                             &
!       Number of absorbers in band
    , index_abs(nd_abs)                                                 &
!       Local indexing numbers for gases and continua
    , n_abs_esft(nd_abs)
!       Number of terms in band
  REAL (RealK), INTENT(IN) ::                                           &
      w_abs_esft(nd_esft_term, nd_abs)
!       Weights for ESFT
  REAL (RealK), INTENT(INOUT) ::                                        &
      k_abs_layer(nd_profile, nd_layer, nd_esft_term, nd_abs)
!       Exponential ESFT terms at actual pressure layer

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

  TYPE(StrPlanck), INTENT(INOUT) :: planck
!   Planckian emission fields

  TYPE(StrSphGeo), INTENT(INOUT) :: sph
!   Spherical geometry fields

!                   Surface properties
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
      rho_alb_tile(nd_point_tile, nd_brdf_basis_fnc, nd_tile)
!       Weights for the basis functions of the BRDFs
!       at the tiled points

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

!                   Calculated radiances
  REAL (RealK), INTENT(INOUT) ::                                        &
      i_direct(nd_radiance_profile, 0: nd_layer)
!       Direct solar irradiance on levels

!                   Flags for clear-sky fluxes
  LOGICAL, INTENT(IN) ::                                                &
      l_clear
!       Calculate clear-sky properties
  INTEGER, INTENT(IN) ::                                                &
      i_solver_clear
!       Clear solver used

!                   Special diagnostics:
  LOGICAL, INTENT(IN) ::                                                &
      l_blue_flux_surf
!       Flag to calculate blue fluxes at the surface
  REAL (RealK), INTENT(IN) ::                                           &
      weight_blue
!       Weights for blue fluxes in this band

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
      i, ii, j, k, l
!       Loop variables
  INTEGER                                                               &
      i_abs                                                             &
!       Index of main absorber
    , i_abs_band                                                        &
!       Index of active absorber
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
!       of minor absorbers and take out equivalent extinction
    , k_eqv(nd_profile, nd_layer)                                       &
!       Equivalent extinction
    , tau_gas(nd_profile, nd_layer)                                     &
!       Optical depth of absorber
    , k_gas_abs(nd_profile, nd_layer)                                   &
!       Gaseous extinction
    , diffuse_albedo(nd_profile)
!       Diffuse albedo of the surface
  REAL (RealK) ::                                                       &
      flux_direct_part(nd_flux_profile, 0: nd_layer)                    &
!       Partial direct flux
    , flux_direct_ground_part(nd_flux_profile)                          &
!       Partial direct flux at the surface
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
      sum_flux(nd_profile, 2*nd_layer+2, nd_abs)                        &
!       Sum of fluxes for weighting
    , sum_k_flux(nd_profile, 2*nd_layer+2, nd_abs)                      &
!       Sum of k*fluxes for weighting
    , flux_term(nd_profile, 2*nd_layer+2)                               &
!       Flux with one term
    , flux_direct_term(nd_profile, 0:nd_layer+1)                        &
!       Direct flux with one term
    , flux_gas(nd_profile, 0: nd_layer+1)                               &
!       Flux with one absorber
    , sph_flux_gas(nd_profile, 0: nd_layer+1)
!       Flux with one absorber at the top of each layer for spherical geometry
  REAL (RealK) ::                                                       &
      layer_inc_flux                                                    &
!       Layer incident fluxes (downward flux at top of layer
!       plus upward flux at bottom of layer)
    , layer_inc_k_flux                                                  &
!       Layer incident k-weighted fluxes
    , k_min(nd_profile, nd_layer)                                       &
!       Weak absorption for minor absorber
    , sum_weight
!       Sum of the ESFT weights for a gas  

  REAL (RealK), ALLOCATABLE :: sph_mass_path(:,:,:)
!       Path through spherical geometry * layer mass

  REAL (RealK) ::                                                       &
      contrib_funci_part(nd_flux_profile, nd_layer)
!       Contribution (or weighting) function
  REAL (RealK) ::                                                       &
      contrib_funcf_part(nd_flux_profile, nd_layer)
!       Contribution (or weighting) function

  REAL (RealK) :: temp(n_profile),temp_exp(n_profile)
  REAL (RealK) :: temp_max = LOG(1.0_RealK/EPSILON(temp_max))
  REAL (RealK) :: temp_max_sph = LOG(HUGE(temp_max_sph))/2.0_RealK
  REAL (RealK) :: eps = EPSILON(eps)

  LOGICAL :: l_initial_band
!       Flag to initialise band-by-band diagnostics
  
  INTEGER :: path_base

  INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
  INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
  REAL(KIND=jprb)               :: zhook_handle

  CHARACTER(LEN=*), PARAMETER :: RoutineName='SOLVE_BAND_K_EQV_SCL'


  IF (lhook) CALL dr_hook(RoutineName,zhook_in,zhook_handle)

  i_abs=index_abs(1)
  l_initial_band = .TRUE.

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
    IF (control%l_spherical_solar) THEN
      DO i=0, n_layer+1
        DO l=1, n_profile
          sph%common%adjust_solar_ke(l, i)=1.0e+00_RealK
        END DO
      END DO
      ALLOCATE(sph_mass_path(n_profile, n_layer, 0:n_layer+1))
      DO ii=0, n_layer+1
        DO i=1, n_layer
          DO l=1, n_profile
            sph_mass_path(l,i,ii) = d_mass(l, i)*sph%common%path(l,i,ii)
          END DO
        END DO
      END DO
    END IF

    DO j=2, n_abs

!     Initialize the normalized flux for the absorber.
      IF (control%l_spherical_solar) THEN
        DO ii=0, n_layer+1
          DO l=1, n_profile
            sph_flux_gas(l, ii)=0.0e+00_RealK
          END DO
        END DO
        DO i=1, n_layer
          DO l=1, n_profile
            flux_gas(l, i)=0.0e+00_RealK
            sum_flux(l, i, j)=0.0e+00_RealK
            sum_k_flux(l, i, j)=0.0e+00_RealK
          END DO
        END DO
      ELSE
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
      END IF

      k_min=HUGE(k_min)
      i_abs_band=index_abs(j)
      sum_weight=0.0_RealK
      DO iex=1, n_abs_esft(i_abs_band)

!       Store the ESFT weight for future use.
        esft_weight=w_abs_esft(iex, i_abs_band)
        sum_weight=sum_weight+esft_weight

!       The treatment of the direct beam and the contents of zen_0 depend
!       on the mode of angular integration so we need three different loops.
        IF (control%l_spherical_solar) THEN
          ! Calculate flux arriving at each layer through spherical path
          DO ii=0,n_layer+1
            DO l=1,n_profile
              path_base = sph%common%path_base(l, ii)
              temp(l) = SUM( - k_abs_layer(l, 1:path_base, iex, i_abs_band) &
                * sph_mass_path(l, 1:path_base, ii) )
            END DO
            CALL exp_v(n_profile,temp,temp_exp)
            DO l=1,n_profile
              flux_direct_term(l, ii)=esft_weight*temp_exp(l)
              sph_flux_gas(l,ii)=sph_flux_gas(l,ii)+flux_direct_term(l, ii)
            END DO
          END DO
          ! Calculate transmission through each layer
          DO i=1, n_layer
            DO l=1, n_profile
              temp(l)=-k_abs_layer(l, i, iex, i_abs_band)*d_mass(l, i) &
                *sph%common%path_div(l,i)
            END DO
            CALL exp_v(n_profile,temp,temp_exp)
            DO l=1,n_profile
              flux_gas(l, i)=flux_gas(l, i)+flux_direct_term(l, i)*temp_exp(l)
            END DO
          END DO          
        ELSE IF (i_angular_integration == ip_two_stream) THEN
          DO l=1, n_profile
            flux_direct_term(l, 0)=esft_weight
          END DO
          DO i=1, n_layer
            DO l=1, n_profile
              temp(l)=-k_abs_layer(l, i, iex, i_abs_band)               &
                *d_mass(l, i)*zen_0(l)
            END DO
            CALL exp_v(n_profile,temp,temp_exp)
            DO l=1,n_profile
              flux_direct_term(l, i)=flux_direct_term(l, i-1)*temp_exp(l)
              flux_gas(l, i)=flux_gas(l, i)+flux_direct_term(l, i)
            END DO
          END DO
        ELSE IF (i_angular_integration == ip_spherical_harmonic) THEN
          DO l=1, n_profile
            flux_direct_term(l, 0)=esft_weight
          END DO
          DO i=1, n_layer
            DO l=1, n_profile
              flux_direct_term(l, i)=flux_direct_term(l, i-1)           &
                *EXP(-k_abs_layer(l, i, iex, i_abs_band)                &
                *d_mass(l, i)/zen_0(l))
              flux_gas(l, i)=flux_gas(l, i)+flux_direct_term(l, i)
            END DO
          END DO
        END IF

!       Calculate the increment in the absorptive extinction
        IF (control%l_spherical_solar) THEN
          DO i=1, n_layer
            DO l=1, n_profile
              DO ii=n_layer+1,i,-1
                ! Use the flux at the lowest lit layer as the weight
                IF (bound%lit(l,ii) > 0.0_RealK) EXIT
              END DO
              sum_k_flux(l, i, j)=sum_k_flux(l, i, j)                 &
                +k_abs_layer(l, i, iex, i_abs_band)                   &
                *flux_direct_term(l, ii)
              sum_flux(l, i, j)=sum_flux(l, i, j)                     &
                +flux_direct_term(l, ii)
              k_min(l, i)=MIN(k_min(l, i),                              &
                k_abs_layer(l, i, iex, i_abs_band))
            END DO
          END DO          
        ELSE
          DO i=1, n_layer
            DO l=1, n_profile
              sum_k_flux(l, i, j)                                       &
                =sum_k_flux(l, i, j)                                    &
                +k_abs_layer(l, i, iex, i_abs_band)                     &
                *flux_direct_term(l, n_layer)
              k_min(l, i)=MIN(k_min(l, i),                              &
                k_abs_layer(l, i, iex, i_abs_band))
            END DO
          END DO
          DO l=1, n_profile
            sum_flux(l, n_layer, j)                                     &
              =sum_flux(l, n_layer, j)+flux_direct_term(l, n_layer)
          END DO
        END IF

      END DO

      IF (control%l_spherical_solar) THEN
        DO i=1, n_layer
          DO l=1, n_profile
            IF (sum_flux(l, i, j) > eps) THEN
              k_eqv(l, i)=k_eqv(l, i)                                   &
                +sum_k_flux(l, i, j)/sum_flux(l, i, j)
            ELSE
              k_eqv(l, i)=k_eqv(l, i)+k_min(l, i)
            END IF
            IF (sph_flux_gas(l, i) > 0.0e+00_RealK) THEN
!             If the flux arriving at a layer has been reduced to 0
!             the adjusting factor is not of importance and need not
!             be adjusted. This will prevent possible failures.
              adjust_solar_ke(l, i)                                     &
                =adjust_solar_ke(l, i)*flux_gas(l, i)                   &
                /sph_flux_gas(l, i)
            END IF
          END DO
        END DO
        DO ii=0,n_layer+1
          DO l=1, n_profile
            sph%common%adjust_solar_ke(l, ii)                           &
              =sph%common%adjust_solar_ke(l, ii)*sph_flux_gas(l, ii)    &
              /sum_weight
          END DO
        END DO
      ELSE
!       Set the equivalent extinction for the diffuse beam,
!       weighting with the direct surface flux.
        DO i=1, n_layer
          DO l=1, n_profile
            IF (sum_flux(l, n_layer, j) > 0.0e+00_RealK) THEN
              k_eqv(l, i)=k_eqv(l, i)                                   &
                +sum_k_flux(l, i, j)/sum_flux(l, n_layer, j)
            ELSE
              k_eqv(l, i)=k_eqv(l, i)+k_min(l, i)
            END IF
            IF (flux_gas(l, i-1) > 0.0e+00_RealK) THEN
!             If the flux has been reduced to 0 at the upper
!             level the adjusting factor is not of importance
!             and need not be adjusted. This will prevent
!             possible failures.
              adjust_solar_ke(l, i)                                     &
                =adjust_solar_ke(l, i)*flux_gas(l, i)                   &
                /flux_gas(l, i-1)
            END IF
          END DO
        END DO
      END IF

    END DO

!   Since the grey extinction will later be modified we must
!   increase the transmission of the solar beam to compensate.
!   This may overflow for very large zenith angles (where the
!   transmission is effectively zero) so we restrict to a max value.
    IF (control%l_spherical_solar) THEN
      ! Correction for the spherical path to each layer
      DO ii=0,n_layer+1
        DO l=1,n_profile
          path_base = sph%common%path_base(l, ii)
          temp(l) = SUM( k_eqv(l, 1:path_base) &
            * sph_mass_path(l, 1:path_base, ii) )
          temp(l)=MIN(temp(l),temp_max_sph)
        END DO
        CALL exp_v(n_profile,temp,temp_exp)
        DO l=1,n_profile
          sph%common%adjust_solar_ke(l,ii) &
            = sph%common%adjust_solar_ke(l,ii)*temp_exp(l)
        END DO
      END DO
      DEALLOCATE(sph_mass_path)
      ! Correction for the path through each layer
      DO i=1, n_layer
        DO l=1, n_profile
          temp(l) = MIN(k_eqv(l,i)*d_mass(l,i)*sph%common%path_div(l,i), &
            temp_max_sph)
        END DO
        CALL exp_v(n_profile,temp,temp_exp)
        DO l=1,n_profile
          adjust_solar_ke(l,i) = adjust_solar_ke(l,i)*temp_exp(l)
        END DO
      END DO
    ELSE IF (i_angular_integration == ip_two_stream) THEN
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
    DO j=2, n_abs

!     Initialize the sums to form the ratio to 0.
      DO i=1, 2*n_layer+2
        DO l=1, n_profile
          sum_flux(l, i, j)=0.0e+00_RealK
          sum_k_flux(l, i, j)=0.0e+00_RealK
        END DO
      END DO

      i_abs_band=index_abs(j)
      DO iex=1, n_abs_esft(i_abs_band)

!       Store the ESFT weight for future use.
        esft_weight=w_abs_esft(iex, i_abs_band)

!       Set the appropriate boundary terms for the
!       total upward and downward fluxes at the boundaries.
        DO l=1, n_profile
          flux_inc_direct(l)=0.0e+00_RealK
          flux_inc_down(l)=-planck%flux(l, 0)
          d_planck_flux_surface(l)=planck%flux_ground(l)                &
            -planck%flux(l, n_layer)
        END DO

!       Set the optical depths of each layer.
        DO i=1, n_layer
          DO l=1, n_profile
            tau_gas(l, i)=k_abs_layer(l, i, iex, i_abs_band)            &
              *d_mass(l, i)
          END DO
        END DO

!       Calculate the fluxes with just this gas.
! DEPENDS ON: monochromatic_gas_flux
        CALL monochromatic_gas_flux(n_profile, n_layer                  &
          , tau_gas                                                     &
          , isolir, zen_0, flux_inc_direct, flux_inc_down               &
          , planck%diff, d_planck_flux_surface                          &
          , diffuse_albedo, diffuse_albedo                              &
          , diffusivity_factor_minor                                    &
          , flux_direct_term, flux_term                                 &
          , nd_profile, nd_layer                                        &
          )

        DO i=2, 2*n_layer+1
          DO l=1, n_profile
            sum_k_flux(l, i, j)=sum_k_flux(l, i, j)                     &
              +k_abs_layer(l, i/2, iex, i_abs_band)                     &
              *esft_weight*ABS(flux_term(l, i))
            sum_flux(l, i, j)=sum_flux(l, i, j)                         &
              +esft_weight*ABS(flux_term(l, i))
          END DO
        END DO

      END DO

    END DO


    DO i=1, n_layer
      DO l=1, n_profile
        k_eqv(l, i)=0.0e+00_RealK
      END DO
    END DO

    DO j=2, n_abs
      DO i=1, n_layer
        DO l=1, n_profile
          layer_inc_k_flux=sum_k_flux(l, 2*i, j)                        &
             +sum_k_flux(l, 2*i+1, j)
          layer_inc_flux=sum_flux(l, 2*i, j)                            &
             +sum_flux(l, 2*i+1, j)
          k_eqv(l, i)=k_eqv(l, i)+layer_inc_k_flux/layer_inc_flux
        END DO
      END DO
    END DO

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
  DO iex=1, n_abs_esft(i_abs)

    IF (i_scatter_method_band == ip_scatter_hybrid) THEN
      i_scatter_method = i_scatter_method_term(iex, i_abs)
    ELSE
      i_scatter_method = i_scatter_method_band
    END IF

!   Store the ESFT weight for future use.
    esft_weight=w_abs_esft(iex, i_abs)

!   Set the appropriate boundary terms for the total
!   upward and downward fluxes.

    IF ( (i_angular_integration == ip_two_stream).OR.                   &
         (i_angular_integration == ip_ir_gauss) ) THEN

      IF (isolir == ip_solar) THEN
!       Solar region.
        IF (control%l_spherical_solar) THEN
          DO l=1, n_profile
            d_planck_flux_surface(l) = 0.0e+00_RealK
            flux_inc_down(l)         = 0.0e+00_RealK
            flux_inc_direct(l)       = 0.0e+00_RealK
          END DO
        ELSE
          DO l=1, n_profile
            d_planck_flux_surface(l)=0.0e+00_RealK
            flux_inc_down(l)=solar_irrad(l)/zen_0(l)
            flux_inc_direct(l)=solar_irrad(l)/zen_0(l)
          END DO
        END IF
      ELSE IF (isolir == ip_infra_red) THEN
!       Infra-red region.
        DO l=1, n_profile
          flux_inc_direct(l)=0.0e+00_RealK
          flux_direct_part(l, n_layer)=0.0e+00_RealK
          flux_inc_down(l)=-planck%flux(l, 0)
          d_planck_flux_surface(l)                                      &
            =planck%flux_ground(l)-planck%flux(l, n_layer)
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
          flux_inc_down(l)=-planck%flux(l, 0)
          d_planck_flux_surface(l)                                      &
            =planck%flux_ground(l)-planck%flux(l, n_layer)
        END DO
      END IF

    END IF

!   Set the absorption for this absorber and k-term.
    DO i=1, n_layer
      DO l=1, n_profile
        k_gas_abs(l, i) = k_abs_layer(l, i, iex, i_abs)
      END DO
    END DO

    IF (i_cloud == ip_cloud_mcica) THEN

! DEPENDS ON: mcica_sample
      CALL mcica_sample(ierr                                            &
        , control, dimen, atm, cld, bound                               &
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
        , planck                                                        &
!                   Conditions at TOA
        , zen_0, flux_inc_direct, flux_inc_down                         &
        , i_direct_part                                                 &
!                   Surface properties
        , d_planck_flux_surface                                         &
        , ls_brdf_trunc, n_brdf_basis_fnc, rho_alb                      &
        , f_brdf, brdf_sol, brdf_hemi                                   &
!                   Spherical geometry
        , sph                                                           &
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
!                   Contribution function
        , contrib_funci_part, contrib_funcf_part                        &
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
        , control, atm, cld, bound                                      &
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
        , planck                                                        &
!                   Conditions at TOA
        , zen_0, flux_inc_direct, flux_inc_down                         &
        , i_direct_part                                                 &
!                   Surface properties
        , d_planck_flux_surface                                         &
        , ls_brdf_trunc, n_brdf_basis_fnc, rho_alb                      &
        , f_brdf, brdf_sol, brdf_hemi                                   &
!                   Spherical geometry
        , sph                                                           &
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
!                   Contribution function
        , contrib_funci_part, contrib_funcf_part                        &
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
    CALL augment_radiance(control, radout, i_band                       &
      , n_profile, n_layer, n_viewing_level, n_direction                &
      , l_clear, l_initial, l_initial_band                              &
      , weight_incr, weight_blue_incr                                   &
!                   Actual radiances
      , i_direct                                                        &
!                   Increments to radiances
      , flux_direct_part, flux_total_part                               &
      , i_direct_part, radiance_part, photolysis_part                   &
      , flux_direct_clear_part, flux_total_clear_part                   &
      , sph, contrib_funci_part, contrib_funcf_part                     &
!                   Dimensions
      , nd_flux_profile, nd_radiance_profile, nd_j_profile              &
      , nd_layer, nd_viewing_level, nd_direction                        &
      )

!   Add in the increments from surface tiles
    IF (l_tile) THEN
      IF ( (i_angular_integration == ip_two_stream).OR.                 &
           (i_angular_integration == ip_ir_gauss) ) THEN
        IF (control%l_spherical_solar) THEN
          DO l=1, n_profile
            flux_direct_ground_part(l)                                  &
              = sph%allsky%flux_direct(l, n_layer+1)
          END DO
        ELSE
          DO l=1, n_profile
            flux_direct_ground_part(l) = flux_direct_part(l, n_layer)
          END DO          
        END IF
      END IF
! DEPENDS ON: augment_tiled_radiance
      CALL augment_tiled_radiance(control, radout, i_band               &
        , n_point_tile, n_tile, list_tile                               &
        , l_initial, weight_incr, weight_blue_incr                      &
!                   Surface characteristics
        , rho_alb_tile                                                  &
!                   Increments to radiances
        , flux_direct_ground_part                                       &
        , flux_total_part(1, 2*n_layer+2)                               &
        , planck%flux_tile, planck%flux(1, n_layer)                     &
!                   Dimensions
        , nd_flux_profile, nd_point_tile, nd_tile                       &
        , nd_brdf_basis_fnc                                             &
        )
    END IF

!   After the first call to these routines quantities should be
!   incremented rather than initialized, until the flag is reset.
    l_initial = .FALSE.
    l_initial_band = .FALSE.

  END DO


  IF (lhook) CALL dr_hook(RoutineName,zhook_out,zhook_handle)

END SUBROUTINE solve_band_k_eqv_scl
