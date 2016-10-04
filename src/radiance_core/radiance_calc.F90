! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Subroutine to calculate the radiance field.
!
! Method:
!   Properties independent of the spectral bands are set.
!   A loop over bands is then entered, grey optical properties
!   are set and an appropriate subroutine is called to treat
!   the gaseous overlaps. The final radiances are assigned.
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: Radiance Core
!
!------------------------------------------------------------------------------
SUBROUTINE radiance_calc(control, dimen, spectrum, atm, cld, aer, bound, radout)

  USE realtype_rd,  ONLY: RealK
  USE rad_pcf
  USE def_spectrum, ONLY: StrSpecData
  USE def_dimen,    ONLY: StrDim
  USE def_control,  ONLY: StrCtrl
  USE def_atm,      ONLY: StrAtm
  USE def_cld,      ONLY: StrCld
  USE def_aer,      ONLY: StrAer
  USE def_bound,    ONLY: StrBound
  USE def_out,      ONLY: StrOut, allocate_out
  USE def_ss_prop
  USE errormessagelength_mod, ONLY: errormessagelength
  USE yomhook,      ONLY: lhook, dr_hook
  USE parkind1,     ONLY: jprb, jpim
  USE ereport_mod,  ONLY: ereport

  IMPLICIT NONE


! Control options:
  TYPE(StrCtrl),     INTENT(IN)  :: control

! Dimensions:
  TYPE(StrDim),      INTENT(IN)  :: dimen

! Spectral data:
  TYPE(StrSpecData), INTENT(IN)  :: spectrum

! Atmospheric properties:
  TYPE(StrAtm),      INTENT(IN)  :: atm

! Cloud properties:
  TYPE(StrCld),      INTENT(IN)  :: cld

! Aerosol properties:
  TYPE(StrAer),      INTENT(IN)  :: aer

! Boundary conditions:
  TYPE(StrBound),    INTENT(IN)  :: bound

! Output fields:
  TYPE(StrOut),      INTENT(OUT) :: radout


! Local arguments.
! General pointers:
  INTEGER                                                                      &
      i_top                                                                    &
!       Top level of profiles
    , i_band                                                                   &
!       Spectral band
    , n_gas                                                                    &
!       Number of active gases
    , i_gas_band                                                               &
!       Single variable for gas in band
    , n_continuum                                                              &
!       Number of continua in band
    , i_continuum                                                              &
!       Continuum number
    , i_continuum_pointer(spectrum%dim%nd_continuum)                           &
!       Pointers to continua
    , i_pointer_water
!       Pointer to water vapour

! Additional variables for angular integration:
  LOGICAL                                                                      &
      l_solar_phf                                                              &
!       Logical to specify a separate treatment of the singly
!       scattered solar beam
    , l_rescale_solar_phf
!       Logical to apply rescaling to the singly scattered
!       solar phase function
  INTEGER                                                                      &
      n_order_phase
!       Order of Legendre polynomials of the phase function

! Pointers to the contents of layers:
  INTEGER                                                                      &
      n_cloud_top                                                              &
!       Topmost cloudy layer
    , n_region                                                                 &
!       Number of cloudy regions
    , n_cloud_profile(dimen%id_cloud_top: dimen%nd_layer)                      &
!       Number of cloudy profiles
    , i_cloud_profile(dimen%nd_profile, dimen%id_cloud_top: dimen%nd_layer)
!       Profiles containing clouds

! Pointers to types of clouds:
  LOGICAL                                                                      &
      l_cloud_cmp(dimen%nd_cloud_component)
!       Logical switches to `include' components
  INTEGER                                                                      &
      i_phase_cmp(dimen%nd_cloud_component)                                    &
!       Phases of components
    , i_cloud_type(dimen%nd_cloud_component)                                   &
!       Pypes of cloud to which each component contributes
    , type_region(dimen%nd_region)                                             &
!       The types of the regions
    , k_clr                                                                    &
!       Index of clear-sky region
    , i_region_cloud(dimen%nd_cloud_type)
!       Regions in which particular type of cloud fall

! Fractional coverage of different regions:
  REAL (RealK) ::                                                              &
      frac_region(dimen%nd_profile,                                            &
                  dimen%id_cloud_top: dimen%nd_layer,                          &
                  dimen%nd_region)
!       Fraction of total cloud occupied by specific regions

! Pointer to table of humidities:
  INTEGER                                                                      &
      i_humidity_pointer(dimen%nd_profile, dimen%nd_layer)
!       Pointer to look-up table for aerosols

! Controlling variables:
  INTEGER                                                                      &
      i                                                                        &
!       Loop variable
    , j                                                                        &
!       Loop variable
    , k                                                                        &
!       Loop variable
    , l, ll
!       Loop variable

! Logical switches:
  LOGICAL                                                                      &
      l_gas_band                                                               &
!       Flag to `include' gaseous absorption in a particular band
    , l_moist_aerosol                                                          &
!       Flag for moist aerosol
    , l_clear_band
!       Flag to calculate clear-sky fluxes for this band

  REAL (RealK) ::                                                              &
      solar_irrad_band(dimen%nd_profile)                                       &
!       Solar irradiance in the band
    , solar_irrad_band_ses(dimen%nd_profile, spectrum%dim%nd_k_term)
!       Incident solar flux for each k-term
  REAL (RealK) ::                                                              &
      gas_frac_rescaled(dimen%nd_profile,                                      &
                        dimen%nd_layer,                                        &
                        spectrum%dim%nd_species)                               &
!       Rescaled gas mixing ratios
    , gas_mix_amt(dimen%nd_profile, dimen%nd_layer)                            &
!       Mixed gas mixing ratio
    , amount_continuum(dimen%nd_profile,                                       &
                       dimen%nd_layer,                                         &
                       spectrum%dim%nd_continuum)                              &
!       Amounts of continua
    , k_continuum_mono(spectrum%dim%nd_continuum)
!       Monochromatic continuum components

! Thermal arrays:
  REAL (RealK) ::                                                              &
      planck_flux_band(dimen%nd_profile, 0: dimen%nd_layer)                    &
!       Planckian flux in band at edges of layers
    , diff_planck_band(dimen%nd_profile, dimen%nd_layer)                       &
!       Difference in the Planckian flux across layers
    , diff_planck_band_2(dimen%nd_profile, dimen%nd_layer)                     &
!       Twice the 2nd difference of in the Planckian flux across
!       layers
    , planck_flux_ground(dimen%nd_profile)
!       Planckian flux at the surface temperature

! Surface BRDF terms
  LOGICAL                                                                      &
      l_diff_alb
!       Flag to calculate diffuse albedos
  REAL (RealK) ::                                                              &
      brdf_sol(dimen%nd_profile, dimen%nd_brdf_basis_fnc, dimen%nd_direction)  &
!       The BRDF evaluated for scattering from the solar
!       beam into the viewing direction
    , brdf_hemi(dimen%nd_profile, dimen%nd_brdf_basis_fnc, dimen%nd_direction) &
!       The BRDF evaluated for scattering from isotropic
!       radiation into the viewing direction
    , diffuse_alb_basis(dimen%nd_brdf_basis_fnc)
!       The diffuse albedo of isotropic radiation for each
!       basis function

! Fields for moist aerosols:
  REAL (RealK) ::                                                              &
      delta_humidity
!       Increment in look-up table for hum.
  INTEGER ::                                                                   &
      nhumidity_common
!       Common number of humidities for moist aerosols

! Fundamental optical properties of layers:
  TYPE(str_ss_prop) :: ss_prop
!   Single scattering properties of the atmosphere

  REAL (RealK) ::                                                              &
      k_esft_layer(dimen%nd_profile, dimen%nd_layer, spectrum%dim%nd_k_term,   &
                   spectrum%dim%nd_species)                                    &
!       Exponential ESFT terms at actual pressure layer
    , k_mix_gas_layer(dimen%nd_profile, spectrum%dim%nd_k_term, dimen%nd_layer)&
!       Exponential ESFT terms at actual pressure layer
    , k_contm_layer(dimen%nd_profile, spectrum%dim%nd_k_term, dimen%nd_layer,  &
                    spectrum%dim%nd_continuum)
!       Continuum absorption coefficients at layer pressure

! Local variables for spherical harmonic integration
  INTEGER                                                                      &
      ls_max_order                                                             &
!       Maximum order of terms required
    , ls_local_trunc(0: dimen%nd_max_order)                                    &
!       Actual truncation for each particular value of m
    , ms_trunc(0: dimen%nd_max_order)                                          &
!       Maximum azimuthal quantum number for each order
    , ia_sph_mm(0: dimen%nd_max_order)
!       Address of spherical coefficient of (m, m) for each m
  REAL (RealK) ::                                                              &
      cg_coeff(dimen%nd_sph_coeff)                                             &
!       Clebsch-gordon coefficients
    , uplm_zero(dimen%nd_sph_coeff)                                            &
!       Upsilon terms
    , uplm_sol(dimen%nd_radiance_profile, dimen%nd_sph_coeff)                  &
!       Upsilon terms for solar radiation
    , cos_sol_view(dimen%nd_radiance_profile, dimen%nd_direction)
!       Cosines of the angles between the solar direction
!       and the viewing direction

! Specification of the grid for radiances:
  INTEGER                                                                      &
      i_rad_layer(dimen%nd_viewing_level)
!       Layers in which to intercept radiances
  REAL (RealK) ::                                                              &
      frac_rad_layer(dimen%nd_viewing_level)
!       Fractions below the tops of the layers

  REAL (RealK) ::                                                              &
      i_direct(dimen%nd_radiance_profile, 0: dimen%nd_layer)
!       Direct solar irradiance on levels (not split by
!       diagnostic bands or returned, but retained for
!       future use)
  REAL (RealK) ::                                                              &
      planck_radiance_band(dimen%nd_radiance_profile, dimen%nd_viewing_level)
!       Planckian radiance in the current band
  LOGICAL                                                                      &
      l_initial
!       Flag to run the routine incrementing diagnostics in
!       its initializing mode

! Coefficients for the transfer of energy between
! Partially cloudy layers:
  REAL (RealK) ::                                                              &
      cloud_overlap(dimen%nd_profile, dimen%id_cloud_top-1: dimen%nd_layer     &
        , dimen%nd_overlap_coeff)                                              &
!       Coefficients defining overlapping options for clouds:
!       these also depend on the solver selected.
    , w_free(dimen%nd_profile, dimen%id_cloud_top: dimen%nd_layer)
!       Clear-sky fraction

! Cloud geometry
  INTEGER                                                                      &
      n_column_cld(dimen%nd_profile)                                           &
!       Number of columns in each profile (including those of
!       zero width)
    , n_column_slv(dimen%nd_profile)                                           &
!       Number of columns to be solved in each profile
    , list_column_slv(dimen%nd_profile, dimen%nd_column)                       &
!       List of columns requiring an actual solution
    , i_clm_lyr_chn(dimen%nd_profile, dimen%nd_column)                         &
!       Layer in the current column to change
    , i_clm_cld_typ(dimen%nd_profile, dimen%nd_column)
!       Type of cloud to introduce in the changed layer
  REAL (RealK) ::                                                              &
      area_column(dimen%nd_profile, dimen%nd_column)
!       Areas of columns

! Local variables for tiled fluxes:
  REAL (RealK) ::                                                              &
      planck_flux_tile(dimen%nd_point_tile, dimen%nd_tile)
!       Local Planckian fluxes on surface tiles

! Secondary arrays for diagnostics
  REAL (RealK) ::                                                              &
      cloud_absorptivity_band(dimen%nd_profile, dimen%nd_layer)                &
!       Absorptivity of cloud in a particular band
    , cloud_extinction_band(dimen%nd_profile, dimen%nd_layer)                  &
!       Absorptivity of cloud in a particular band
    , ls_cloud_absorptivity_band(dimen%nd_profile, dimen%nd_layer)             &
!       Absorptivity of cloud in a particular band
    , ls_cloud_extinction_band(dimen%nd_profile, dimen%nd_layer)               &
!       Absorptivity of cloud in a particular band
    , cnv_cloud_absorptivity_band(dimen%nd_profile, dimen%nd_layer)            &
!       Absorptivity of cloud in a particular band
    , cnv_cloud_extinction_band(dimen%nd_profile, dimen%nd_layer)              &
!       Absorptivity of cloud in a particular band
    , flux_direct_clear_prev(dimen%nd_2sg_profile, 0: dimen%nd_layer)          &
!       Clear direct flux in a particular band
    , flux_up_clear_prev(dimen%nd_2sg_profile, 0: dimen%nd_layer)
!       Clear upward flux in a particular band

  INTEGER ::                                                                   &
      jp(dimen%nd_profile, dimen%nd_layer), jp1                                &
!       Index for pressure interpolation of absorption coefficient
    , jph2oc(dimen%nd_profile, dimen%nd_layer)                                 &
!       Same as JP but for water vapour pressure
    , jt(dimen%nd_profile,dimen%nd_layer), jt1                                 &
!       Index for temperature interpolation of absorption coeff
    , jtt(dimen%nd_profile, dimen%nd_layer), jtt1                              &
!       Index of reference temperature at level i+1
!       such that the actual temperature is between JTT and JTT+1
    , jto2c(dimen%nd_profile, dimen%nd_layer)                                  &
!       Index of o2 continuum  reference temperature at level I
!       such that the actual temperature is between JTO2C and JTO2C+1
    , jtswo3(dimen%nd_profile, dimen%nd_layer)
!       Index of sw o3 reference temp

  REAL (RealK) ::                                                              &
      fac00(dimen%nd_profile, dimen%nd_layer),                                 &
      fac01(dimen%nd_profile, dimen%nd_layer),                                 &
      fac10(dimen%nd_profile, dimen%nd_layer),                                 &
      fac11(dimen%nd_profile, dimen%nd_layer),                                 &
!       Multiplication factors for P & T interpolation
      fac00c(dimen%nd_profile, dimen%nd_layer),                                &
      fac01c(dimen%nd_profile, dimen%nd_layer),                                &
      fac10c(dimen%nd_profile, dimen%nd_layer),                                &
      fac11c(dimen%nd_profile, dimen%nd_layer),                                &
!       Multiplication factors for H2O cont P & T interpolation
      facc00(dimen%nd_profile, dimen%nd_layer),                                &
      facc01(dimen%nd_profile, dimen%nd_layer)
!       Multiplication factors for O2 continuum T interpolation

  LOGICAL :: l_grey_cont
!       Flag to add continuum in grey_opt_prop

  INTEGER :: nd_esft_max
!   Maximum number of ESFT terms needed in each band (for
!   arrays when using random overlap with resorting and rebinning)

  INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
  INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
  REAL(KIND=jprb)               :: zhook_handle

  INTEGER                       :: ierr = i_normal
  CHARACTER (LEN=errormessagelength)           :: cmessage
  CHARACTER (LEN=*), PARAMETER  :: RoutineName = 'RADIANCE_CALC'


  IF (lhook) CALL dr_hook(RoutineName,zhook_in,zhook_handle)


  CALL allocate_out(radout, control, dimen, spectrum)


! Initial determination of flags and switches:
  IF (control%i_angular_integration == ip_two_stream) THEN

!   Only one term in the phase function is required.
    n_order_phase=1

    l_solar_phf=.FALSE.
    l_rescale_solar_phf=.FALSE.

  ELSE IF (control%i_angular_integration == ip_spherical_harmonic) THEN

!   Set limits on ranges of harmonics and set pointers to arrays.
! DEPENDS ON: set_truncation
    CALL set_truncation(ierr                                                   &
      , control%i_truncation, control%ls_global_trunc                          &
      , ls_max_order, ls_local_trunc                                           &
      , control%ms_min, control%ms_max, ms_trunc                               &
      , ia_sph_mm, n_order_phase                                               &
      , dimen%nd_max_order                                                     &
      )

!   Determine whether special treatment of the solar
!   beam is required.
    l_solar_phf=(control%isolir == ip_solar).AND.                              &
                (control%i_sph_algorithm == ip_sph_reduced_iter)
    l_rescale_solar_phf=control%l_rescale.AND.l_solar_phf
!   Calculate the solar scattering angles if treating the
!   solar beam separately.
    IF (l_solar_phf) THEN
! DEPENDS ON: sol_scat_cos
      CALL sol_scat_cos(atm%n_profile, atm%n_direction                         &
        , bound%zen_0, atm%direction, cos_sol_view                             &
        , dimen%nd_profile, dimen%nd_direction)
    END IF

!   Calculate Clebsch-Gordan coefficients once and for all.
! DEPENDS ON: calc_cg_coeff
    CALL calc_cg_coeff(ls_max_order                                            &
      , ia_sph_mm, control%ms_min, ms_trunc                                    &
      , cg_coeff                                                               &
      , dimen%nd_max_order, dimen%nd_sph_coeff)

!   Calculate spherical harmonics at polar angles of pi/2 for
!   use in Marshak's boundary conditions.
! DEPENDS ON: calc_uplm_zero
    CALL calc_uplm_zero(control%ms_min, control%ms_max, ia_sph_mm              &
      , ls_local_trunc, uplm_zero                                              &
      , dimen%nd_max_order, dimen%nd_sph_coeff)

    IF (control%isolir == ip_solar) THEN
!     Calculate the spherical harmonics of the solar direction.
! DEPENDS ON: calc_uplm_sol
      CALL calc_uplm_sol(atm%n_profile, control%ms_min, control%ms_max         &
        , ia_sph_mm                                                            &
        , ls_local_trunc, bound%zen_0, uplm_sol                                &
        , dimen%nd_profile, dimen%nd_max_order, dimen%nd_sph_coeff)
    END IF

    IF (control%i_sph_algorithm == ip_sph_reduced_iter) THEN
!     Calcuate some arrays of terms for the BRDF.
! DEPENDS ON: calc_brdf
      CALL calc_brdf(control%isolir, control%ms_min, control%ms_max            &
        , ia_sph_mm, uplm_sol, uplm_zero                                       &
        , bound%n_brdf_basis_fnc, control%ls_brdf_trunc, bound%f_brdf          &
        , atm%n_profile, atm%n_direction, atm%direction                        &
        , brdf_sol, brdf_hemi                                                  &
        , dimen%nd_profile, dimen%nd_radiance_profile, dimen%nd_direction      &
        , dimen%nd_max_order, dimen%nd_sph_coeff                               &
        , dimen%nd_brdf_basis_fnc, dimen%nd_brdf_trunc)
    END IF

!   For the calculation of equivalent extinction in the IR
!   we need the diffuse albedo for each basis function.
    l_diff_alb=.FALSE.
    DO i_band=1, spectrum%basic%n_band
      l_diff_alb=l_diff_alb.OR.                                                &
        (control%i_gas_overlap_band(i_band) == ip_overlap_t_eqv).OR.           &
        (control%i_gas_overlap_band(i_band) == ip_overlap_k_eqv).OR.           &
        (control%i_gas_overlap_band(i_band) == ip_overlap_k_eqv_scl).OR.       &
        (control%i_gas_overlap_band(i_band) == ip_overlap_k_eqv_mod)
    END DO
    IF ( (control%isolir == ip_infra_red).AND.l_diff_alb ) THEN
! DEPENDS ON: diff_albedo_basis
      CALL diff_albedo_basis(bound%n_brdf_basis_fnc                            &
        , control%ls_brdf_trunc, bound%f_brdf                                  &
        , uplm_zero(ia_sph_mm(0))                                              &
        , diffuse_alb_basis                                                    &
        , dimen%nd_brdf_basis_fnc, dimen%nd_brdf_trunc, dimen%nd_sph_coeff     &
        )
    END IF

!   Determine which layers will be required to give radiances.
! DEPENDS ON: set_rad_layer
    CALL set_rad_layer(ierr                                                    &
      , atm%n_layer, atm%n_viewing_level, atm%viewing_level                    &
      , i_rad_layer, frac_rad_layer                                            &
      , dimen%nd_viewing_level                                                 &
      )


  END IF

! Set the top level of the profiles. This is currently reatined
! for historical reasons.
  i_top=1

! Set the pointer for water vapour to a legal value: this must be done
! for cases where water vapour is not included in the spectral file.
  i_pointer_water=MAX(spectrum%cont%index_water, 1)


! Initial calculations for aerosols:

! Set the spectrally independent properties of moist aerosols.
  l_moist_aerosol  = .FALSE.
  DO j = 1, spectrum%aerosol%n_aerosol
    SELECT CASE ( spectrum%aerosol%i_aerosol_parm(j) )
    CASE (ip_aerosol_param_moist, ip_aerosol_param_phf_moist)
      l_moist_aerosol  = .TRUE.
      nhumidity_common = spectrum%aerosol%nhumidity(j)
    END SELECT
  END DO

  IF (l_moist_aerosol) THEN
!   Currently all aerosols use the same lookup table of humidities
    delta_humidity = 1.0e+00_RealK                                             &
      / ( REAL(nhumidity_common, RealK) - 1.0e+00_RealK )
    DO i = 1, atm%n_layer
      DO l = 1, atm%n_profile
        i_humidity_pointer(l, i) = 1 +                                         &
          INT( aer%mean_rel_humidity(l, i)*(nhumidity_common-1) )
      END DO
    END DO
  END IF



! Initial calculations for clouds:

  IF (control%l_cloud) THEN

!   Set pointers to the types of cloud.
! DEPENDS ON: set_cloud_pointer
    CALL set_cloud_pointer(ierr                                                &
      , cld%n_condensed, cld%type_condensed, control%i_cloud_representation    &
      , control%l_drop, control%l_ice                                          &
      , i_phase_cmp, i_cloud_type, l_cloud_cmp                                 &
      , dimen%nd_cloud_component                                               &
      )


!   Set the geometry of the clouds.
! DEPENDS ON: set_cloud_geometry
    CALL set_cloud_geometry(atm%n_profile, atm%n_layer                         &
      , control%l_global_cloud_top, cld%w_cloud                                &
      , n_cloud_top, n_cloud_profile, i_cloud_profile                          &
      , dimen%nd_profile, dimen%nd_layer, dimen%id_cloud_top                   &
      )

    k_clr=1
    IF ( (control%i_cloud == ip_cloud_triple).OR.                              &
         (control%i_cloud == ip_cloud_part_corr_cnv) ) THEN
!     Aggregate clouds into regions for solving.
!     Three regions are used with this option. Additionally,
!     flag the clear-sky region.
      n_region=3
      type_region(1)=ip_region_clear
      type_region(2)=ip_region_strat
      type_region(3)=ip_region_conv
! DEPENDS ON: aggregate_cloud
      CALL aggregate_cloud(ierr                                                &
        , atm%n_profile, atm%n_layer, n_cloud_top                              &
        , control%i_cloud, control%i_cloud_representation                      &
        , cld%n_cloud_type, cld%frac_cloud                                     &
        , i_region_cloud, frac_region                                          &
        , dimen%nd_profile, dimen%nd_layer, dimen%nd_cloud_type                &
        , dimen%nd_region, dimen%id_cloud_top                                  &
        )
    ELSE IF ( (control%i_cloud == ip_cloud_mix_max).OR.                        &
              (control%i_cloud == ip_cloud_mix_random).OR.                     &
              (control%i_cloud == ip_cloud_part_corr) ) THEN
!     There will be only one cloudy region.
      n_region=2
      type_region(1)=ip_region_clear
      type_region(2)=ip_region_strat
      DO i=n_cloud_top, atm%n_layer
        DO l=1, atm%n_profile
          frac_region(l, i, 2)=1.0e+00_RealK
        END DO
      END DO
    END IF

!   Calculate energy transfer coefficients in a mixed column,
!   or split the atmosphere into columns with a column model:
    IF ( (control%i_cloud == ip_cloud_mix_max).OR.                             &
         (control%i_cloud == ip_cloud_mix_random).OR.                          &
         (control%i_cloud == ip_cloud_triple).OR.                              &
         (control%i_cloud == ip_cloud_part_corr).OR.                           &
         (control%i_cloud == ip_cloud_part_corr_cnv) ) THEN

! DEPENDS ON: overlap_coupled
      CALL overlap_coupled(atm%n_profile, atm%n_layer, n_cloud_top             &
        , cld%w_cloud, w_free, n_region, type_region, frac_region, atm%p       &
        , control%i_cloud                                                      &
        , cloud_overlap                                                        &
        , dimen%nd_profile, dimen%nd_layer, dimen%nd_overlap_coeff             &
        , dimen%nd_region, dimen%id_cloud_top                                  &
        , cld%dp_corr_strat, cld%dp_corr_conv, radout%tot_cloud_cover          &
        )

    ELSE IF (control%i_cloud == ip_cloud_column_max) THEN

! DEPENDS ON: cloud_maxcs_split
        CALL cloud_maxcs_split(ierr, atm%n_profile, atm%n_layer                &
          , n_cloud_top, cld%w_cloud, cld%frac_cloud                           &
          , cld%n_cloud_type                                                   &
          , n_column_cld, n_column_slv, list_column_slv                        &
          , i_clm_lyr_chn, i_clm_cld_typ, area_column                          &
          , dimen%nd_profile, dimen%nd_layer, dimen%id_cloud_top               &
          , dimen%nd_column, dimen%nd_cloud_type                               &
          )

    END IF

  ELSE

    n_cloud_top=atm%n_layer+1

  END IF


! Calculate temperature and pressure interpolation factor for ESFT
  IF ( ANY(spectrum%gas%i_scale_fnc(                                           &
    control%first_band : control%last_band, 1) == ip_scale_ses2) ) THEN
! DEPENDS ON: inter_pt
    CALL inter_pt(dimen%nd_profile, dimen%nd_layer                             &
      , atm%n_profile, atm%n_layer, atm%gas_mix_ratio(1,1,i_pointer_water)     &
      , atm%p, atm%t, fac00, fac01, fac10, fac11                               &
      , fac00c, fac01c, fac10c, fac11c                                         &
      , facc00, facc01, jp, jph2oc, jt, jtt, jto2c, jtswo3)
  ELSE IF ( ANY(spectrum%gas%i_scale_fnc(                                      &
    control%first_band : control%last_band, :) == ip_scale_lookup) ) THEN
! DEPENDS ON: inter_pt_lookup
    CALL inter_pt_lookup(dimen%nd_profile, dimen%nd_layer, spectrum%dim%nd_pre &
      , spectrum%dim%nd_tmp, atm%n_profile, atm%n_layer, atm%p, atm%t          &
      , spectrum%gas%p_lookup, spectrum%gas%t_lookup                           &
      , fac00, fac01, fac10, fac11, jp, jt, jtt)
  END IF

! Initialise solar_tail_flux if necessary
  IF (control%l_solar_tail_flux) radout%solar_tail_flux=0.0

! Check that there is enough information in the case of spherical
! harmonics. This check is rather late in the logical order of
! things, but we had to wait for certain other calculations to be
! made.
  IF (control%i_angular_integration == ip_spherical_harmonic) THEN
! DEPENDS ON: check_phf_term
    CALL check_phf_term(ierr                                                   &
      , control%l_aerosol, spectrum%aerosol%n_aerosol                          &
      , spectrum%aerosol%i_aerosol_parm                                        &
      , spectrum%aerosol%n_aerosol_phf_term                                    &
      , aer%n_phase_term_prsc                                                  &
      , control%l_cloud, cld%n_condensed, cld%i_condensed_param, i_phase_cmp   &
      , cld%condensed_n_phf                                                    &
      , cld%n_phase_term_drop_prsc, cld%n_phase_term_ice_prsc                  &
      , n_order_phase, control%l_henyey_greenstein_pf                          &
      , control%l_rescale, control%n_order_forward                             &
      , l_solar_phf, control%n_order_phase_solar                               &
      , spectrum%dim%nd_aerosol_species, dimen%nd_cloud_component)
  END IF


! Initialization of cloud diagnostics
  IF (control%l_cloud_extinction) THEN
     radout%cloud_extinction        = 0.0
     radout%cloud_weight_extinction = 0.0
  END IF

  IF (control%l_ls_cloud_extinction) THEN
     radout%ls_cloud_extinction        = 0.0
     radout%ls_cloud_weight_extinction = 0.0
  END IF

  IF (control%l_cnv_cloud_extinction) THEN
     radout%cnv_cloud_extinction        = 0.0
     radout%cnv_cloud_weight_extinction = 0.0
  END IF

  IF (control%l_cloud_absorptivity) THEN
     radout%cloud_absorptivity        = 0.0
     radout%cloud_weight_absorptivity = 0.0
  END IF

  IF (control%l_ls_cloud_absorptivity) THEN
     radout%ls_cloud_absorptivity        = 0.0
     radout%ls_cloud_weight_absorptivity = 0.0
  END IF

  IF (control%l_cnv_cloud_absorptivity) THEN
     radout%cnv_cloud_absorptivity        = 0.0
     radout%cnv_cloud_weight_absorptivity = 0.0
  END IF


! Solve the equation of transfer in each band and
! increment the fluxes.

  DO i_band=control%first_band, control%last_band

!   Set the flag to initialize the diagnostic arrays.
    IF (i_band == control%first_band) THEN
      l_initial=.TRUE.
    ELSE
      l_initial=(control%map_channel(i_band) > control%map_channel(i_band-1))
    END IF

!   Initialise fluxes to calculate band increments for diagnostics
    IF (control%l_flux_direct_band) THEN
      IF (l_initial) THEN
        radout%flux_direct_band(:,:,i_band) = 0.0
      ELSE
        radout%flux_direct_band(:,:,i_band)                                    &
          = radout%flux_direct(:,:,control%map_channel(i_band))
      END IF
    END IF
    IF (control%l_flux_down_band) THEN
      IF (l_initial) THEN
        radout%flux_down_band(:,:,i_band) = 0.0
      ELSE
        radout%flux_down_band(:,:,i_band)                                      &
          = radout%flux_down(:,:,control%map_channel(i_band))
      END IF
    END IF
    IF (control%l_flux_up_band) THEN
      IF (l_initial) THEN
        radout%flux_up_band(:,:,i_band) = 0.0
      ELSE
        radout%flux_up_band(:,:,i_band)                                        &
          = radout%flux_up(:,:,control%map_channel(i_band))
      END IF
    END IF
    IF (control%l_cloud_extinction     .OR.                                    &
        control%l_ls_cloud_extinction  .OR.                                    &
        control%l_cnv_cloud_extinction .OR.                                    &
        control%l_flux_direct_clear_band) THEN
      IF (l_initial) THEN
        flux_direct_clear_prev = 0.0
      ELSE
        flux_direct_clear_prev                                                 &
          = radout%flux_direct_clear(:,:,control%map_channel(i_band))
      END IF
    END IF
    IF (control%l_flux_down_clear_band) THEN
      IF (l_initial) THEN
        radout%flux_down_clear_band(:,:,i_band) = 0.0
      ELSE
        radout%flux_down_clear_band(:,:,i_band)                                &
          = radout%flux_down_clear(:,:,control%map_channel(i_band))
      END IF
    END IF
    IF (control%l_cloud_absorptivity     .OR.                                  &
        control%l_ls_cloud_absorptivity  .OR.                                  &
        control%l_cnv_cloud_absorptivity .OR.                                  &
        control%l_flux_up_clear_band) THEN
      IF (l_initial) THEN
        flux_up_clear_prev = 0.0
      ELSE
        flux_up_clear_prev                                                     &
          = radout%flux_up_clear(:,:,control%map_channel(i_band))
      END IF
    END IF

!   Determine whether clear-sky fluxes are required for this band
    l_clear_band = control%l_clear
    IF (i_band == control%first_band) THEN
!     As currently implemented diagnostic (UV) flux must be in band 1
      l_clear_band = l_clear_band .OR. control%l_flux_down_clear_diag_surf
    END IF

!   Determine whether gaseous absorption is included in this band.
    IF ((control%l_gas).AND.(spectrum%gas%n_band_absorb(i_band) > 0)) THEN

!     Note: I_GAS_BAND is used extensively below since nested
!     array elements in a subroutine call (see later) can
!     confuse some compilers.

!     Normally the number of gases in the calculation will be
!     as in the spectral file, but particular options may result
!     in the omission of some gases.

      n_gas=spectrum%gas%n_band_absorb(i_band)

      IF (control%i_gas_overlap_band(i_band) == ip_overlap_single) THEN

!       There will be no gaseous absorption in this band
!       unless the selected gas appears.
        n_gas=0

        DO i=1, spectrum%gas%n_band_absorb(i_band)
          IF (spectrum%gas%index_absorb(i, i_band) == control%i_gas) n_gas=1
        END DO

      END IF


      IF (n_gas > 0) THEN

!       Set the flag for gaseous absorption in the band.
        l_gas_band=.TRUE.

        DO j=1, n_gas

          i_gas_band=spectrum%gas%index_absorb(j, i_band)

!         Reset the pointer if there is just one gas.
          IF (control%i_gas_overlap_band(i_band) == ip_overlap_single) THEN
!           Only the selected gas is active in the band.
            i_gas_band=control%i_gas
          END IF

          IF (spectrum%gas%i_scale_k(i_band, i_gas_band)                       &
              == ip_scale_band) THEN
!           Rescale the amount of gas for this band now.
! DEPENDS ON: scale_absorb
            CALL scale_absorb(ierr, atm%n_profile, atm%n_layer                 &
              , atm%gas_mix_ratio(1, 1, i_gas_band), atm%p, atm%t              &
              , i_top                                                          &
              , gas_frac_rescaled(1, 1, i_gas_band)                            &
              , k_esft_layer(1, 1, 1, i_gas_band)                              &
              , spectrum%gas%i_scale_fnc(i_band, i_gas_band)                   &
              , spectrum%gas%p_ref(i_gas_band, i_band)                         &
              , spectrum%gas%t_ref(i_gas_band, i_band)                         &
              , spectrum%gas%scale(1, 1, i_band, i_gas_band)                   &
              , 1, i_band, spectrum%gas%l_doppler(i_gas_band)                  &
              , spectrum%gas%doppler_cor(i_gas_band)                           &
              , dimen%nd_profile, dimen%nd_layer                               &
              , spectrum%dim%nd_scale_variable)

          ELSE IF (spectrum%gas%i_scale_k(i_band, i_gas_band)                  &
              == ip_scale_null) THEN
!           Copy across the unscaled array.
            DO i=i_top, atm%n_layer
              DO l=1, atm%n_profile
                gas_frac_rescaled(l, i, i_gas_band)                            &
                  =atm%gas_mix_ratio(l, i, i_gas_band)
              END DO
            END DO
          END IF
        END DO
      ELSE
        l_gas_band=.FALSE.
      END IF

    ELSE
      l_gas_band=.FALSE.
    END IF



!   Rescale amounts of continua.
    l_grey_cont = .FALSE.
    IF (control%l_continuum) THEN
      n_continuum=spectrum%cont%n_band_continuum(i_band)
      DO i=1, n_continuum
        i_continuum_pointer(i)=spectrum%cont%index_continuum(i_band, i)
        i_continuum=i_continuum_pointer(i)
        IF (spectrum%cont%i_scale_fnc_cont(i_band,i_continuum)                 &
          == ip_scale_ses2) THEN
          k_continuum_mono(i_continuum)=0.0
! DEPENDS ON: ses_rescale_contm
          CALL ses_rescale_contm(dimen%nd_profile, dimen%nd_layer              &
            , i_continuum, atm%n_profile, atm%n_layer                          &
            , atm%p, atm%t, atm%gas_mix_ratio(1,1,i_pointer_water)             &
            , amount_continuum(1, 1, i)                                        &
            )
        ELSE
          l_grey_cont = .TRUE.
          k_continuum_mono(i_continuum) =                                      &
            spectrum%cont%k_cont(i_band, i_continuum)
! DEPENDS ON: rescale_continuum
          CALL rescale_continuum(control, atm%n_profile, atm%n_layer           &
            , i_continuum, atm%p, atm%t                                        &
            , atm%density, atm%gas_mix_ratio(1, 1, i_pointer_water)            &
            , amount_continuum(1, 1, i_continuum)                              &
            , spectrum%cont%i_scale_fnc_cont(i_band, i_continuum)              &
            , spectrum%cont%p_ref_cont(i_continuum, i_band)                    &
            , spectrum%cont%t_ref_cont(i_continuum, i_band)                    &
            , spectrum%cont%scale_cont(1, i_band, i_continuum)                 &
            , dimen%nd_profile, dimen%nd_layer                                 &
            , spectrum%dim%nd_scale_variable)
        END IF
      END DO
    END IF

!   Allocate the single scattering propeties.
    ALLOCATE(ss_prop%k_grey_tot_clr                                            &
      (dimen%nd_profile, dimen%nd_layer_clr))
    ALLOCATE(ss_prop%k_ext_scat_clr                                            &
      (dimen%nd_profile, dimen%nd_layer_clr))
    ALLOCATE(ss_prop%phase_fnc_clr                                             &
      (dimen%nd_profile, dimen%nd_layer_clr, dimen%nd_max_order))
    ALLOCATE(ss_prop%forward_scatter_clr                                       &
      (dimen%nd_profile, dimen%nd_layer_clr))
    ALLOCATE(ss_prop%phase_fnc_solar_clr                                       &
      (dimen%nd_radiance_profile, dimen%nd_layer_clr, dimen%nd_direction))
    ALLOCATE(ss_prop%forward_solar_clr                                         &
      (dimen%nd_radiance_profile, dimen%nd_layer_clr))

    ALLOCATE(ss_prop%tau_clr                                                   &
      (dimen%nd_profile, dimen%nd_layer_clr))
    ALLOCATE(ss_prop%tau_clr_noscal                                            &
      (dimen%nd_profile, dimen%nd_layer_clr))
    ALLOCATE(ss_prop%omega_clr                                                 &
      (dimen%nd_profile, dimen%nd_layer_clr))

    ALLOCATE(ss_prop%k_grey_tot                                                &
      (dimen%nd_profile, dimen%id_cloud_top: dimen%nd_layer,                   &
       0: dimen%nd_cloud_type))
    ALLOCATE(ss_prop%k_ext_scat                                                &
      (dimen%nd_profile, dimen%id_cloud_top: dimen%nd_layer,                   &
       0: dimen%nd_cloud_type))
    ALLOCATE(ss_prop%phase_fnc                                                 &
      (dimen%nd_profile, dimen%id_cloud_top: dimen%nd_layer,                   &
       dimen%nd_max_order,                                                     &
       0: dimen%nd_cloud_type))
    ALLOCATE(ss_prop%forward_scatter                                           &
      (dimen%nd_profile, dimen%id_cloud_top: dimen%nd_layer,                   &
       0: dimen%nd_cloud_type))
    ALLOCATE(ss_prop%phase_fnc_solar                                           &
      (dimen%nd_radiance_profile, dimen%id_cloud_top: dimen%nd_layer,          &
       dimen%nd_direction,                                                     &
       0: dimen%nd_cloud_type))
    ALLOCATE(ss_prop%forward_solar                                             &
      (dimen%nd_radiance_profile, dimen%id_cloud_top: dimen%nd_layer,          &
       0: dimen%nd_cloud_type))

    ALLOCATE(ss_prop%tau                                                       &
      (dimen%nd_profile, dimen%id_cloud_top: dimen%nd_layer,                   &
       0: dimen%nd_cloud_type))
    ALLOCATE(ss_prop%tau_noscal                                                &
      (dimen%nd_profile, dimen%id_cloud_top: dimen%nd_layer,                   &
       0: dimen%nd_cloud_type))
    ALLOCATE(ss_prop%omega                                                     &
      (dimen%nd_profile, dimen%id_cloud_top: dimen%nd_layer,                   &
       0: dimen%nd_cloud_type))

    ALLOCATE(ss_prop%k_ext_tot_cloud_comp                                      &
      (dimen%nd_profile, dimen%id_cloud_top: dimen%nd_layer,                   &
       0: dimen%nd_cloud_component))
    ALLOCATE(ss_prop%k_ext_scat_cloud_comp                                     &
      (dimen%nd_profile, dimen%id_cloud_top: dimen%nd_layer,                   &
       0: dimen%nd_cloud_component))
    ALLOCATE(ss_prop%phase_fnc_cloud_comp                                      &
      (dimen%nd_profile, dimen%id_cloud_top: dimen%nd_layer,                   &
       dimen%nd_max_order,                                                     &
       0: dimen%nd_cloud_component))
    ALLOCATE(ss_prop%forward_scatter_cloud_comp                                &
      (dimen%nd_profile, dimen%id_cloud_top: dimen%nd_layer,                   &
       0: dimen%nd_cloud_component))
    ALLOCATE(ss_prop%phase_fnc_solar_cloud_comp                                &
      (dimen%nd_radiance_profile, dimen%id_cloud_top: dimen%nd_layer,          &
       dimen%nd_direction,                                                     &
       0: dimen%nd_cloud_component))
    ALLOCATE(ss_prop%forward_solar_cloud_comp                                  &
      (dimen%nd_radiance_profile, dimen%id_cloud_top: dimen%nd_layer,          &
       0: dimen%nd_cloud_component))

    ALLOCATE(ss_prop%phase_fnc_no_cloud                                        &
      (dimen%nd_profile, dimen%id_cloud_top: dimen%nd_layer,                   &
       dimen%nd_max_order))
    ALLOCATE(ss_prop%forward_scatter_no_cloud                                  &
      (dimen%nd_profile, dimen%id_cloud_top: dimen%nd_layer))

!   Calculate the grey extinction within the band.

! DEPENDS ON: grey_opt_prop
    CALL grey_opt_prop(ierr, control, radout, i_band                           &
      , atm%n_profile, atm%n_layer, atm%p, atm%t, atm%density                  &
      , n_order_phase, l_solar_phf, atm%n_direction, cos_sol_view              &
      , spectrum%rayleigh%rayleigh_coeff(i_band)                               &
      , l_grey_cont, n_continuum, i_continuum_pointer                          &
      , k_continuum_mono, amount_continuum                                     &
      , spectrum%aerosol%n_aerosol                                             &
      , spectrum%aerosol%n_aerosol_mr, aer%mix_ratio                           &
      , aer%mr_source, aer%mr_type_index                                       &
      , spectrum%aerosol%i_aerosol_parm                                        &
      , i_humidity_pointer, spectrum%aerosol%humidities, delta_humidity        &
      , aer%mean_rel_humidity                                                  &
      , spectrum%aerosol%abs(1, 1, i_band)                                     &
      , spectrum%aerosol%scat(1, 1, i_band)                                    &
      , spectrum%aerosol%phf_fnc(1, 1, 1, i_band)                              &
      , aer%n_mode, aer%mode_mix_ratio                                         &
      , aer%mode_absorption(1, 1, 1, i_band)                                   &
      , aer%mode_scattering(1, 1, 1, i_band)                                   &
      , aer%mode_asymmetry(1, 1, 1, i_band)                                    &
      , aer%n_opt_level_prsc, aer%pressure_prsc                                &
      , aer%absorption_prsc(1, 1, 1, i_band)                                   &
      , aer%scattering_prsc(1, 1, 1, i_band)                                   &
      , aer%phase_fnc_prsc(1, 1, 1, 1, i_band)                                 &
      , n_cloud_profile, i_cloud_profile                                       &
      , n_cloud_top, cld%n_condensed, l_cloud_cmp, i_phase_cmp                 &
      , cld%i_condensed_param, cld%condensed_n_phf                             &
      , cld%condensed_param_list(1, 1, i_band)                                 &
      , cld%condensed_mix_ratio, cld%condensed_dim_char                        &
      , cld%condensed_rel_var_dens                                             &
      , cld%n_cloud_type, i_cloud_type                                         &
      , cld%n_opt_level_drop_prsc, cld%drop_pressure_prsc                      &
      , cld%drop_absorption_prsc(1, 1, i_band)                                 &
      , cld%drop_scattering_prsc(1, 1, i_band)                                 &
      , cld%drop_phase_fnc_prsc(1, 1, 1, i_band)                               &
      , cld%n_opt_level_ice_prsc, cld%ice_pressure_prsc                        &
      , cld%ice_absorption_prsc(1, 1, i_band)                                  &
      , cld%ice_scattering_prsc(1, 1, i_band)                                  &
      , cld%ice_phase_fnc_prsc(1, 1, 1, i_band)                                &
      , ss_prop                                                                &
      , cld%frac_cloud                                                         &
      , cloud_extinction_band, cloud_absorptivity_band                         &
      , ls_cloud_extinction_band, ls_cloud_absorptivity_band                   &
      , cnv_cloud_extinction_band, cnv_cloud_absorptivity_band                 &
      , dimen%nd_profile, dimen%nd_radiance_profile, dimen%nd_layer            &
      , dimen%nd_layer_clr, dimen%id_cloud_top                                 &
      , spectrum%dim%nd_continuum, spectrum%dim%nd_aerosol_species             &
      , spectrum%dim%nd_aerosol_mr, spectrum%dim%nd_humidity                   &
      , spectrum%dim%nd_cloud_parameter, dimen%nd_cloud_component              &
      , dimen%nd_cloud_type, spectrum%dim%nd_phase_term, dimen%nd_max_order    &
      , dimen%nd_direction, dimen%nd_aerosol_mode                              &
      , dimen%nd_profile_aerosol_prsc, dimen%nd_profile_cloud_prsc             &
      , dimen%nd_opt_level_aerosol_prsc, dimen%nd_opt_level_cloud_prsc         &
      )


    IF ( (control%i_angular_integration == ip_two_stream).OR.                  &
         (control%i_angular_integration == ip_spherical_harmonic) ) THEN

!     Rescale the phase function and calculate the scattering
!     fractions. (These are grey and may be calculated outside
!     a loop over gases).

      IF (control%l_rescale) THEN

!       Rescale clear-sky phase function:

!       The section above clouds.
! DEPENDS ON: rescale_phase_fnc
        CALL rescale_phase_fnc(atm%n_profile, 1, n_cloud_top-1                 &
          , atm%n_direction, cos_sol_view                                      &
          , n_order_phase                                                      &
          , ss_prop%phase_fnc_clr, ss_prop%forward_scatter_clr                 &
          , ss_prop%forward_solar_clr                                          &
          , l_rescale_solar_phf, control%n_order_phase_solar                   &
          , ss_prop%phase_fnc_solar_clr                                        &
          , dimen%nd_profile, dimen%nd_radiance_profile, dimen%nd_layer_clr, 1 &
          , dimen%nd_direction, dimen%nd_max_order                             &
          )
!       The section including clouds.
        CALL rescale_phase_fnc(atm%n_profile, n_cloud_top                      &
          , atm%n_layer, atm%n_direction, cos_sol_view                         &
          , n_order_phase                                                      &
          , ss_prop%phase_fnc(1, dimen%id_cloud_top, 1, 0)                     &
          , ss_prop%forward_scatter(1, dimen%id_cloud_top, 0)                  &
          , ss_prop%forward_solar(1, dimen%id_cloud_top, 0)                    &
          , l_rescale_solar_phf, control%n_order_phase_solar                   &
          , ss_prop%phase_fnc_solar(1, dimen%id_cloud_top, 1, 0)               &
          , dimen%nd_profile, dimen%nd_radiance_profile, dimen%nd_layer        &
          , dimen%id_cloud_top, dimen%nd_direction, dimen%nd_max_order         &
          )


        IF (control%l_cloud .AND. control%i_cloud /= ip_cloud_mcica) THEN

!         Rescale cloudy phase functions:
          DO k=1, cld%n_cloud_type
            CALL rescale_phase_fnc(atm%n_profile, n_cloud_top                  &
              , atm%n_layer, atm%n_direction, cos_sol_view                     &
              , n_order_phase                                                  &
              , ss_prop%phase_fnc(1, dimen%id_cloud_top, 1, k)                 &
              , ss_prop%forward_scatter(1, dimen%id_cloud_top, k)              &
              , ss_prop%forward_solar(1, dimen%id_cloud_top, k)                &
              , l_rescale_solar_phf, control%n_order_phase_solar               &
              , ss_prop%phase_fnc_solar(1, dimen%id_cloud_top, 1, k)           &
              , dimen%nd_profile, dimen%nd_radiance_profile, dimen%nd_layer    &
              , dimen%id_cloud_top, dimen%nd_direction, dimen%nd_max_order     &
              )
          END DO

        END IF

      END IF

    END IF


!   Preliminary calculations for source terms:
    IF (control%i_gas_overlap_band(i_band) == ip_overlap_mix_ses2) THEN

!     Interpolate absorption coefficients onto model grid
! DEPENDS ON: inter_k
      CALL inter_k(atm%n_profile, atm%n_layer, spectrum%gas%n_band_absorb      &
        , spectrum%gas%mix_gas_band, spectrum%gas%n_mix_gas(i_band)            &
        , spectrum%gas%index_mix_gas                                           &
        , spectrum%gas%i_band_k_ses(i_band), i_band                            &
        , n_continuum, spectrum%cont%k_cont_ses, control%l_continuum           &
        , spectrum%cont%index_continuum                                        &
        , spectrum%cont%k_h2oc(1,1,1,i_band)                                   &
        , fac00, fac01, fac10, fac11                                           &
        , fac00c, fac01c, fac10c, fac11c, facc00, facc01                       &
        , jp, jph2oc, jt, jtt, jto2c, jtswo3                                   &
        , spectrum%gas%k_lookup(1,1,1,1,i_band), spectrum%gas%k_mix_gas        &
        , spectrum%gas%f_mix(i_band), atm%gas_mix_ratio, gas_mix_amt           &
        , k_esft_layer, k_mix_gas_layer, k_contm_layer                         &
!       Dimensions
        , dimen%nd_profile, dimen%nd_layer                                     &
        , spectrum%dim%nd_band, spectrum%dim%nd_species                        &
        , spectrum%dim%nd_continuum                                            &
        , spectrum%dim%nd_k_term, spectrum%dim%nd_mix                          &
        , spectrum%dim%nd_tmp, spectrum%dim%nd_pre                             &
        , spectrum%dim%nd_band_mix_gas)

      IF ((control%isolir == ip_solar) .OR. control%l_solar_tail_flux) THEN
!       Convert normalized band fluxes to actual energy fluxes.
        DO k=1,spectrum%gas%i_band_k_ses(i_band)
          DO l=1, atm%n_profile
            solar_irrad_band_ses(l,k)=bound%solar_irrad(l)                     &
              *spectrum%solar%solar_flux_band_ses(k, i_band)
          END DO
        END DO
      END IF

      IF (control%l_solar_tail_flux) THEN
!       Calculate solar tail flux and add it to the solar region
!       for diagnostic output
        DO k=1,spectrum%gas%i_band_k_ses(i_band)
          DO l=1, atm%n_profile
            radout%solar_tail_flux(l)=radout%solar_tail_flux(l)                &
              +spectrum%gas%w_ses(k,i_band)*solar_irrad_band_ses(l,k)
          END DO
        END DO
      END IF

    ELSE

      DO j=1, spectrum%gas%n_band_absorb(i_band)
      i_gas_band=spectrum%gas%index_absorb(j, i_band)
      IF (spectrum%gas%i_scale_fnc(i_band, i_gas_band)                         &
        == ip_scale_lookup) THEN
        DO k=1, spectrum%gas%i_band_k(i_band, i_gas_band)
          DO i=1, atm%n_layer
            DO l=1, atm%n_profile
              jp1=jp(l,i)+1
              jt1=jt(l,i)+1
              jtt1=jtt(l,i)+1
              k_esft_layer(l,i,k,i_gas_band) = MAX(0.0_RealK,                  &
                  fac00(l,i)*spectrum%gas%k_lookup(                            &
                  jt(l,i),  jp(l,i), k, i_gas_band, i_band )                   &
                + fac10(l,i)*spectrum%gas%k_lookup(                            &
                  jtt(l,i), jp1,     k, i_gas_band, i_band )                   &
                + fac01(l,i)*spectrum%gas%k_lookup(                            &
                  jt1,      jp(l,i), k, i_gas_band, i_band )                   &
                + fac11(l,i)*spectrum%gas%k_lookup(                            &
                  jtt1,     jp1,     k, i_gas_band, i_band ) )
            END DO
          END DO
        END DO
      END IF
      END DO

      IF (control%isolir == ip_solar) THEN
!       Convert normalized band fluxes to actual energy fluxes.
        DO l=1, atm%n_profile
          solar_irrad_band(l)=bound%solar_irrad(l)                             &
            *spectrum%solar%solar_flux_band(i_band)
        END DO
      END IF

    END IF


    IF (control%isolir == ip_infra_red) THEN

!     Calculate the change in the thermal source function
!     across each layer for the infra-red part of the spectrum.

      IF (spectrum%planck%l_planck_tbl) THEN
! DEPENDS ON: diff_planck_source_tbl
        CALL diff_planck_source_tbl(atm%n_profile, atm%n_layer                 &
          , spectrum%planck%n_deg_fit                                          &
          , spectrum%planck%thermal_coeff(0, i_band)                           &
          , spectrum%planck%t_ref_planck                                       &
          , spectrum%planck%theta_planck_tbl, atm%t_level, bound%t_ground      &
          , planck_flux_band, diff_planck_band                                 &
          , planck_flux_ground                                                 &
          , control%l_ir_source_quad, atm%t, diff_planck_band_2                &
          , control%i_angular_integration                                      &
          , atm%n_viewing_level, i_rad_layer, frac_rad_layer                   &
          , planck_radiance_band                                               &
          , control%l_tile, bound%n_point_tile, bound%n_tile, bound%list_tile  &
          , bound%frac_tile, bound%t_tile, planck_flux_tile                    &
          , dimen%nd_profile, dimen%nd_layer, spectrum%dim%nd_thermal_coeff    &
          , dimen%nd_radiance_profile, dimen%nd_viewing_level                  &
          , dimen%nd_point_tile, dimen%nd_tile                                 &
          )
      ELSE
! DEPENDS ON: diff_planck_source_poly
        CALL diff_planck_source_poly(atm%n_profile, atm%n_layer                &
          , spectrum%planck%n_deg_fit                                          &
          , spectrum%planck%thermal_coeff(0, i_band)                           &
          , spectrum%planck%t_ref_planck, atm%t_level, bound%t_ground          &
          , planck_flux_band, diff_planck_band                                 &
          , planck_flux_ground                                                 &
          , control%l_ir_source_quad, atm%t, diff_planck_band_2                &
          , control%i_angular_integration                                      &
          , atm%n_viewing_level, i_rad_layer, frac_rad_layer                   &
          , planck_radiance_band                                               &
          , control%l_tile, bound%n_point_tile, bound%n_tile, bound%list_tile  &
          , bound%frac_tile, bound%t_tile, planck_flux_tile                    &
          , dimen%nd_profile, dimen%nd_layer, spectrum%dim%nd_thermal_coeff    &
          , dimen%nd_radiance_profile, dimen%nd_viewing_level                  &
          , dimen%nd_point_tile, dimen%nd_tile                                 &
          )
      END IF
    END IF




!   Call a solver appropriate to the presence of gases and
!   the overlap assumed:

    IF (.NOT.l_gas_band) THEN

!     There is no gaseous absorption. Solve for the
!     radiances directly.

! DEPENDS ON: solve_band_without_gas
      CALL solve_band_without_gas(ierr                                         &
        , control, cld, bound                                                  &
!                 Atmospheric properties
        , atm%n_profile, atm%n_layer, atm%mass                                 &
!                 Angular integration
        , control%i_angular_integration, control%i_2stream                     &
        , n_order_phase, control%l_rescale, control%n_order_gauss              &
        , control%ms_min, control%ms_max, control%i_truncation                 &
        , ls_local_trunc                                                       &
        , control%accuracy_adaptive, control%euler_factor                      &
        , control%i_sph_algorithm, control%i_sph_mode                          &
!                 Precalculated angular arrays
        , ia_sph_mm, cg_coeff, uplm_zero, uplm_sol                             &
!                 Treatment of scattering
        , control%i_scatter_method_band(i_band)                                &
!                 Options for solver
        , control%i_solver                                                     &
!                 Spectral region
        , control%isolir                                                       &
!                 Solar properties
        , bound%zen_0, solar_irrad_band                                        &
!                 Infra-red properties
        , planck_flux_band(1, 0), planck_flux_band(1, atm%n_layer)             &
        , diff_planck_band, control%l_ir_source_quad, diff_planck_band_2       &
!                 Surface properties
        , control%ls_brdf_trunc, bound%n_brdf_basis_fnc                        &
        , bound%rho_alb(1, 1, i_band)                                          &
        , bound%f_brdf, brdf_sol, brdf_hemi                                    &
        , planck_flux_ground                                                   &
!                 Tiling of the surface
        , control%l_tile, bound%n_point_tile, bound%n_tile, bound%list_tile    &
        , bound%rho_alb_tile(1, 1, 1, i_band), planck_flux_tile                &
!                 Optical Properties
        , ss_prop                                                              &
!                 Cloudy properties
        , control%l_cloud, control%i_cloud                                     &
!                 Cloudy geometry
        , n_cloud_top                                                          &
        , n_region, k_clr, i_region_cloud, frac_region                         &
        , w_free, cloud_overlap                                                &
        , n_column_slv, list_column_slv                                        &
        , i_clm_lyr_chn, i_clm_cld_typ, area_column                            &
!                 Levels for calculating radiances
        , atm%n_viewing_level, i_rad_layer, frac_rad_layer                     &
!                 Viewing Geometry
        , atm%n_direction, atm%direction                                       &
!                 Weighting factor for the band
        , control%weight_band(i_band), l_initial                               &
!                 Calculated fluxes
        , radout%flux_direct(1, 0, control%map_channel(i_band))                &
        , radout%flux_down(1, 0, control%map_channel(i_band))                  &
        , radout%flux_up(1, 0, control%map_channel(i_band))                    &
!                 Radiances
        , i_direct, radout%radiance(1, 1, 1, control%map_channel(i_band))      &
!                 Rate of photolysis
        , radout%photolysis(1, 1, control%map_channel(i_band))                 &
!                 Flags for clear-sky fluxes
        , l_clear_band, control%i_solver_clear                                 &
!                 Calculated clear-sky fluxes
        , radout%flux_direct_clear(1, 0, control%map_channel(i_band))          &
        , radout%flux_down_clear(1, 0, control%map_channel(i_band))            &
        , radout%flux_up_clear(1, 0, control%map_channel(i_band))              &
!                 Tiled Surface Fluxes
        , radout%flux_up_tile(1, 1, control%map_channel(i_band))               &
        , radout%flux_up_blue_tile(1, 1, control%map_channel(i_band))          &
!                 Special Surface Fluxes
        , control%l_blue_flux_surf, spectrum%solar%weight_blue(i_band)         &
        , radout%flux_direct_blue_surf                                         &
        , radout%flux_down_blue_surf, radout%flux_up_blue_surf                 &
!                 Dimensions of arrays
        , dimen%nd_profile, dimen%nd_layer, dimen%nd_layer_clr                 &
        , dimen%id_cloud_top, dimen%nd_column                                  &
        , dimen%nd_flux_profile, dimen%nd_radiance_profile, dimen%nd_j_profile &
        , dimen%nd_cloud_type, dimen%nd_region, dimen%nd_overlap_coeff         &
        , dimen%nd_max_order, dimen%nd_sph_coeff                               &
        , dimen%nd_brdf_basis_fnc, dimen%nd_brdf_trunc                         &
        , dimen%nd_viewing_level, dimen%nd_direction                           &
        , dimen%nd_source_coeff, dimen%nd_point_tile, dimen%nd_tile            &
        )


    ELSE

!     Gases are included.

!     Treat the gaseous overlaps as directed by the overlap switch.
      SELECT CASE (control%i_gas_overlap_band(i_band))

      CASE (ip_overlap_single)
! DEPENDS ON: solve_band_one_gas
        CALL solve_band_one_gas(ierr                                           &
          , control, cld, bound                                                &
!                 Atmospheric properties
          , atm%n_profile, atm%n_layer, i_top, atm%p, atm%t, atm%mass          &
!                 Angular integration
          , control%i_angular_integration, control%i_2stream                   &
          , n_order_phase, control%l_rescale, control%n_order_gauss            &
          , control%ms_min, control%ms_max, control%i_truncation               &
          , ls_local_trunc                                                     &
          , control%accuracy_adaptive, control%euler_factor                    &
          , control%i_sph_algorithm, control%i_sph_mode                        &
!                 Precalculated angular arrays
          , ia_sph_mm, cg_coeff, uplm_zero, uplm_sol                           &
!                 Treatment of scattering
          , control%i_scatter_method_band(i_band)                              &
!                 Options for solver
          , control%i_solver                                                   &
!                 Gaseous properties
          , i_band, control%i_gas                                              &
          , spectrum%gas%i_band_k, spectrum%gas%i_scale_k                      &
          , spectrum%gas%i_scale_fnc                                           &
          , spectrum%gas%k, k_esft_layer, spectrum%gas%w                       &
          , spectrum%gas%scale                                                 &
          , spectrum%gas%p_ref, spectrum%gas%t_ref                             &
          , atm%gas_mix_ratio, gas_frac_rescaled                               &
          , spectrum%gas%l_doppler, spectrum%gas%doppler_cor                   &
!                 Spectral region
          , control%isolir                                                     &
!                 Solar properties
          , bound%zen_0, solar_irrad_band                                      &
!                 Infra-red properties
          , planck_flux_band(1, 0)                                             &
          , planck_flux_band(1, atm%n_layer)                                   &
          , diff_planck_band                                                   &
          , control%l_ir_source_quad, diff_planck_band_2                       &
!                 Surface properties
          , control%ls_brdf_trunc, bound%n_brdf_basis_fnc                      &
          , bound%rho_alb(1, 1, i_band)                                        &
          , bound%f_brdf, brdf_sol, brdf_hemi                                  &
          , planck_flux_ground                                                 &
!                 Tiling of the surface
          , control%l_tile, bound%n_point_tile, bound%n_tile, bound%list_tile  &
          , bound%rho_alb_tile(1, 1, 1, i_band)                                &
          , planck_flux_tile                                                   &
!                 Optical Properties
          , ss_prop                                                            &
!                 Cloudy properties
          , control%l_cloud, control%i_cloud                                   &
!                 Cloud geometry
          , n_cloud_top                                                        &
          , n_region, k_clr, i_region_cloud, frac_region                       &
          , w_free, cloud_overlap                                              &
          , n_column_slv, list_column_slv                                      &
          , i_clm_lyr_chn, i_clm_cld_typ, area_column                          &
!                 Levels for calculating radiances
          , atm%n_viewing_level, i_rad_layer, frac_rad_layer                   &
!                 Viewing Geometry
          , atm%n_direction, atm%direction                                     &
!                 Weighting factor for the band
          , control%weight_band(i_band), l_initial                             &
!                 Fluxes calculated
          , radout%flux_direct(1, 0, control%map_channel(i_band))              &
          , radout%flux_down(1, 0, control%map_channel(i_band))                &
          , radout%flux_up(1, 0, control%map_channel(i_band))                  &
!                 Radiances
          , i_direct, radout%radiance(1, 1, 1, control%map_channel(i_band))    &
!                 Rate of photolysis
          , radout%photolysis(1, 1, control%map_channel(i_band))               &
!                 Flags for clear-sky calculations
          , l_clear_band, control%i_solver_clear                               &
!                 Clear-sky fluxes
          , radout%flux_direct_clear(1, 0, control%map_channel(i_band))        &
          , radout%flux_down_clear(1, 0, control%map_channel(i_band))          &
          , radout%flux_up_clear(1, 0, control%map_channel(i_band))            &
!                 Tiled Surface Fluxes
          , radout%flux_up_tile(1, 1, control%map_channel(i_band))             &
          , radout%flux_up_blue_tile(1, 1, control%map_channel(i_band))        &
!                 Special Surface Fluxes
          , control%l_blue_flux_surf, spectrum%solar%weight_blue(i_band)       &
          , radout%flux_direct_blue_surf                                       &
          , radout%flux_down_blue_surf, radout%flux_up_blue_surf               &
!                 Dimensions of arrays
          , dimen%nd_profile, dimen%nd_layer, dimen%nd_layer_clr               &
          , dimen%id_cloud_top, dimen%nd_column                                &
          , dimen%nd_flux_profile, dimen%nd_radiance_profile                   &
          , dimen%nd_j_profile                                                 &
          , spectrum%dim%nd_band, spectrum%dim%nd_species                      &
          , spectrum%dim%nd_k_term, spectrum%dim%nd_scale_variable             &
          , dimen%nd_cloud_type, dimen%nd_region, dimen%nd_overlap_coeff       &
          , dimen%nd_max_order, dimen%nd_sph_coeff                             &
          , dimen%nd_brdf_basis_fnc, dimen%nd_brdf_trunc                       &
          , dimen%nd_viewing_level, dimen%nd_direction                         &
          , dimen%nd_source_coeff, dimen%nd_point_tile, dimen%nd_tile          &
          )

      CASE (ip_overlap_random)
! DEPENDS ON: solve_band_random_overlap
        CALL solve_band_random_overlap(ierr                                    &
          , control, cld, bound                                                &
!                 Atmospheric properties
          , atm%n_profile, atm%n_layer, i_top, atm%p, atm%t, atm%mass          &
!                 Angular integration
          , control%i_angular_integration, control%i_2stream                   &
          , n_order_phase, control%l_rescale, control%n_order_gauss            &
          , control%ms_min, control%ms_max, control%i_truncation               &
          , ls_local_trunc                                                     &
          , control%accuracy_adaptive, control%euler_factor                    &
          , control%i_sph_algorithm, control%i_sph_mode                        &
!                 Precalculated angular arrays
          , ia_sph_mm, cg_coeff, uplm_zero, uplm_sol                           &
!                 Treatment of scattering
          , control%i_scatter_method_band(i_band)                              &
!                 Options for solver
          , control%i_solver                                                   &
!                 Gaseous properties
          , i_band, n_gas                                                      &
          , spectrum%gas%index_absorb, spectrum%gas%i_band_k                   &
          , spectrum%gas%i_scale_k, spectrum%gas%i_scale_fnc                   &
          , spectrum%gas%k, k_esft_layer, spectrum%gas%w                       &
          , spectrum%gas%scale                                                 &
          , spectrum%gas%p_ref, spectrum%gas%t_ref                             &
          , atm%gas_mix_ratio, gas_frac_rescaled                               &
          , spectrum%gas%l_doppler, spectrum%gas%doppler_cor                   &
!                 Spectral region
          , control%isolir                                                     &
!                 Solar properties
          , bound%zen_0, solar_irrad_band                                      &
!                 Infra-red properties
          , planck_flux_band(1, 0)                                             &
          , planck_flux_band(1, atm%n_layer)                                   &
          , diff_planck_band                                                   &
          , control%l_ir_source_quad, diff_planck_band_2                       &
!                 Surface properties
          , control%ls_brdf_trunc, bound%n_brdf_basis_fnc                      &
          , bound%rho_alb(1, 1, i_band)                                        &
          , bound%f_brdf, brdf_sol, brdf_hemi                                  &
          , planck_flux_ground                                                 &
!                 Tiling of the surface
          , control%l_tile, bound%n_point_tile, bound%n_tile, bound%list_tile  &
          , bound%rho_alb_tile(1, 1, 1, i_band)                                &
          , planck_flux_tile                                                   &
!                 Optical Properties
          , ss_prop                                                            &
!                 Cloudy properties
          , control%l_cloud, control%i_cloud                                   &
!                 Cloud geometry
          , n_cloud_top                                                        &
          , n_region, k_clr, i_region_cloud, frac_region                       &
          , w_free, cloud_overlap                                              &
          , n_column_slv, list_column_slv                                      &
          , i_clm_lyr_chn, i_clm_cld_typ, area_column                          &
!                 Levels for calculating radiances
          , atm%n_viewing_level, i_rad_layer, frac_rad_layer                   &
!                 Viewing Geometry
          , atm%n_direction, atm%direction                                     &
!                 Weighting factor for the band
          , control%weight_band(i_band), l_initial                             &
!                 Fluxes calculated
          , radout%flux_direct(1, 0, control%map_channel(i_band))              &
          , radout%flux_down(1, 0, control%map_channel(i_band))                &
          , radout%flux_up(1, 0, control%map_channel(i_band))                  &
!                 Radiances
          , i_direct, radout%radiance(1, 1, 1, control%map_channel(i_band))    &
!                 Rate of photolysis
          , radout%photolysis(1, 1, control%map_channel(i_band))               &
!                 Flags for clear-sky calculations
          , l_clear_band, control%i_solver_clear                               &
!                 Clear-sky fluxes
          , radout%flux_direct_clear(1, 0, control%map_channel(i_band))        &
          , radout%flux_down_clear(1, 0, control%map_channel(i_band))          &
          , radout%flux_up_clear(1, 0, control%map_channel(i_band))            &
!                 Tiled Surface Fluxes
          , radout%flux_up_tile(1, 1, control%map_channel(i_band))             &
          , radout%flux_up_blue_tile(1, 1, control%map_channel(i_band))        &
!                 Special Surface Fluxes
          , control%l_blue_flux_surf, spectrum%solar%weight_blue(i_band)       &
          , radout%flux_direct_blue_surf                                       &
          , radout%flux_down_blue_surf, radout%flux_up_blue_surf               &
!                 Dimensions of arrays
          , dimen%nd_profile, dimen%nd_layer, dimen%nd_layer_clr               &
          , dimen%id_cloud_top, dimen%nd_column                                &
          , dimen%nd_flux_profile, dimen%nd_radiance_profile                   &
          , dimen%nd_j_profile                                                 &
          , spectrum%dim%nd_band, spectrum%dim%nd_species                      &
          , spectrum%dim%nd_k_term, spectrum%dim%nd_scale_variable             &
          , dimen%nd_cloud_type, dimen%nd_region, dimen%nd_overlap_coeff       &
          , dimen%nd_max_order, dimen%nd_sph_coeff                             &
          , dimen%nd_brdf_basis_fnc, dimen%nd_brdf_trunc                       &
          , dimen%nd_viewing_level, dimen%nd_direction                         &
          , dimen%nd_source_coeff, dimen%nd_point_tile, dimen%nd_tile          &
          )
          
      CASE (ip_overlap_random_resort_rebin)
!       Set maximum number of ESFT terms needed
        nd_esft_max = MAX(control%n_esft_red, spectrum%dim%nd_k_term)
! DEPENDS ON: solve_band_random_overlap_resort_rebin
        CALL solve_band_random_overlap_resort_rebin(ierr                       &
          , control, cld, bound                                                &
!                 Atmospheric properties
          , atm%n_profile, atm%n_layer, i_top, atm%p, atm%t, atm%mass          &
!                 Angular integration
          , control%i_angular_integration, control%i_2stream                   &
          , n_order_phase, control%l_rescale, control%n_order_gauss            &
          , control%ms_min, control%ms_max, control%i_truncation               &
          , ls_local_trunc                                                     &
          , control%accuracy_adaptive, control%euler_factor                    &
          , control%i_sph_algorithm, control%i_sph_mode                        &
!                 Precalculated angular arrays
          , ia_sph_mm, cg_coeff, uplm_zero, uplm_sol                           &
!                 Treatment of scattering
          , control%i_scatter_method_band(i_band)                              &
!                 Options for solver
          , control%i_solver                                                   &
          , control%n_esft_red, control%gpnt_split                             &
!                 Gaseous properties
          , i_band, n_gas                                                      &
          , spectrum%gas%index_absorb, spectrum%gas%i_band_k                   &
          , spectrum%gas%i_scale_k, spectrum%gas%i_scale_fnc                   &
          , spectrum%gas%k, k_esft_layer, spectrum%gas%w                       &
          , spectrum%gas%scale                                                 &
          , spectrum%gas%p_ref, spectrum%gas%t_ref                             &
          , atm%gas_mix_ratio, gas_frac_rescaled                               &
          , spectrum%gas%l_doppler, spectrum%gas%doppler_cor                   &
!                 Spectral region
          , control%isolir                                                     &
!                 Solar properties
          , bound%zen_0, solar_irrad_band                                      &
!                 Infra-red properties
          , planck_flux_band(1, 0)                                             &
          , planck_flux_band(1, atm%n_layer)                                   &
          , diff_planck_band                                                   &
          , control%l_ir_source_quad, diff_planck_band_2                       &
!                 Surface properties
          , control%ls_brdf_trunc, bound%n_brdf_basis_fnc                      &
          , bound%rho_alb(1, 1, i_band)                                        &
          , bound%f_brdf, brdf_sol, brdf_hemi                                  &
          , planck_flux_ground                                                 &
!                 Tiling of the surface
          , control%l_tile, bound%n_point_tile, bound%n_tile, bound%list_tile  &
          , bound%rho_alb_tile(1, 1, 1, i_band)                                &
          , planck_flux_tile                                                   &
!                 Optical Properties
          , ss_prop                                                            &
!                 Cloudy properties
          , control%l_cloud, control%i_cloud                                   &
!                 Cloud geometry
          , n_cloud_top                                                        &
          , n_region, k_clr, i_region_cloud, frac_region                       &
          , w_free, cloud_overlap                                              &
          , n_column_slv, list_column_slv                                      &
          , i_clm_lyr_chn, i_clm_cld_typ, area_column                          &
!                 Levels for calculating radiances
          , atm%n_viewing_level, i_rad_layer, frac_rad_layer                   &
!                 Viewing Geometry
          , atm%n_direction, atm%direction                                     &
!                 Weighting factor for the band
          , control%weight_band(i_band), l_initial                             &
!                 Fluxes calculated
          , radout%flux_direct(1, 0, control%map_channel(i_band))              &
          , radout%flux_down(1, 0, control%map_channel(i_band))                &
          , radout%flux_up(1, 0, control%map_channel(i_band))                  &
!                 Radiances
          , i_direct, radout%radiance(1, 1, 1, control%map_channel(i_band))    &
!                 Rate of photolysis
          , radout%photolysis(1, 1, control%map_channel(i_band))               &
!                 Flags for clear-sky calculations
          , l_clear_band, control%i_solver_clear                               &
!                 Clear-sky fluxes
          , radout%flux_direct_clear(1, 0, control%map_channel(i_band))        &
          , radout%flux_down_clear(1, 0, control%map_channel(i_band))          &
          , radout%flux_up_clear(1, 0, control%map_channel(i_band))            &
!                 Tiled Surface Fluxes
          , radout%flux_up_tile(1, 1, control%map_channel(i_band))             &
          , radout%flux_up_blue_tile(1, 1, control%map_channel(i_band))        &
!                 Special Surface Fluxes
          , control%l_blue_flux_surf, spectrum%solar%weight_blue(i_band)       &
          , radout%flux_direct_blue_surf                                       &
          , radout%flux_down_blue_surf, radout%flux_up_blue_surf               &
!                 Dimensions of arrays
          , dimen%nd_profile, dimen%nd_layer, dimen%nd_layer_clr               &
          , dimen%id_cloud_top, dimen%nd_column                                &
          , dimen%nd_flux_profile, dimen%nd_radiance_profile                   &
          , dimen%nd_j_profile                                                 &
          , spectrum%dim%nd_band, spectrum%dim%nd_species                      &
          , spectrum%dim%nd_k_term, nd_esft_max                                &
          , spectrum%dim%nd_scale_variable                                     &
          , dimen%nd_cloud_type, dimen%nd_region, dimen%nd_overlap_coeff       &
          , dimen%nd_max_order, dimen%nd_sph_coeff                             &
          , dimen%nd_brdf_basis_fnc, dimen%nd_brdf_trunc                       &
          , dimen%nd_viewing_level, dimen%nd_direction                         &
          , dimen%nd_source_coeff, dimen%nd_point_tile, dimen%nd_tile          &
          )

      CASE (ip_overlap_t_eqv, ip_overlap_k_eqv, ip_overlap_k_eqv_scl,          &
            ip_overlap_k_eqv_mod)
! DEPENDS ON: solve_band_k_eqv
        CALL solve_band_k_eqv(ierr                                             &
          , control, dimen, cld, bound                                         &
!                 Atmospheric properties
          , atm%n_profile, atm%n_layer, i_top, atm%p, atm%t, atm%mass          &
!                 Angular integration
          , control%i_angular_integration, control%i_2stream                   &
          , n_order_phase, control%l_rescale, control%n_order_gauss            &
          , control%ms_min, control%ms_max, control%i_truncation               &
          , ls_local_trunc                                                     &
          , control%accuracy_adaptive, control%euler_factor                    &
          , control%i_sph_algorithm, control%i_sph_mode                        &
!                 Precalculated angular arrays
          , ia_sph_mm, cg_coeff, uplm_zero, uplm_sol                           &
!                 Treatment of scattering
          , control%i_scatter_method_band(i_band), spectrum%gas%i_scat         &
!                 Options for solver
          , control%i_solver, control%i_gas_overlap_band(i_band)               &
!                 Gaseous properties
          , i_band, n_gas                                                      &
          , spectrum%gas%index_absorb, spectrum%gas%i_band_k                   &
          , spectrum%gas%i_scale_k, spectrum%gas%i_scale_fnc                   &
          , spectrum%gas%k, k_esft_layer, spectrum%gas%w                       &
          , spectrum%gas%scale                                                 &
          , spectrum%gas%p_ref, spectrum%gas%t_ref                             &
          , atm%gas_mix_ratio, gas_frac_rescaled                               &
          , spectrum%gas%l_doppler, spectrum%gas%doppler_cor                   &
!                 Spectral region
          , control%isolir                                                     &
!                 Solar properties
          , bound%zen_0, solar_irrad_band                                      &
!                 Infra-red properties
          , planck_flux_band                                                   &
          , diff_planck_band                                                   &
          , control%l_ir_source_quad, diff_planck_band_2                       &
!                 Surface properties
          , control%ls_brdf_trunc, bound%n_brdf_basis_fnc                      &
          , bound%rho_alb(1, 1, i_band)                                        &
          , bound%f_brdf, brdf_sol, brdf_hemi                                  &
          , diffuse_alb_basis                                                  &
          , planck_flux_ground                                                 &
!                 Tiling of the surface
          , control%l_tile, bound%n_point_tile, bound%n_tile, bound%list_tile  &
          , bound%rho_alb_tile(1, 1, 1, i_band)                                &
          , planck_flux_tile                                                   &
!                 Optical Properties
          , ss_prop                                                            &
!                 Cloudy properties
          , control%l_cloud, control%i_cloud                                   &
!                 Cloud geometry
          , n_cloud_top                                                        &
          , n_region, k_clr, i_region_cloud, frac_region                       &
          , w_free, cloud_overlap                                              &
          , n_column_slv, list_column_slv                                      &
          , i_clm_lyr_chn, i_clm_cld_typ, area_column                          &
!                 Additional variables required for mcica
          , l_cloud_cmp, n_cloud_profile, i_cloud_profile                      &
          , i_cloud_type, dimen%nd_cloud_component                             &
          , control%i_cloud_representation                                     &
!                 Levels for calculating radiances
          , atm%n_viewing_level, i_rad_layer, frac_rad_layer                   &
!                 Viewing Geometry
          , atm%n_direction, atm%direction                                     &
!                 Weighting factor for the band
          , control%weight_band(i_band), l_initial                             &
!                 Fluxes calculated
          , radout%flux_direct(1, 0, control%map_channel(i_band))              &
          , radout%flux_down(1, 0, control%map_channel(i_band))                &
          , radout%flux_up(1, 0, control%map_channel(i_band))                  &
!                 Radiances
          , i_direct, radout%radiance(1, 1, 1, control%map_channel(i_band))    &
!                 Rate of photolysis
          , radout%photolysis(1, 1, control%map_channel(i_band))               &
!                 Flags for clear-sky calculations
          , l_clear_band, control%i_solver_clear                               &
!                 Clear-sky fluxes calculated
          , radout%flux_direct_clear(1, 0, control%map_channel(i_band))        &
          , radout%flux_down_clear(1, 0, control%map_channel(i_band))          &
          , radout%flux_up_clear(1, 0, control%map_channel(i_band))            &
!                 Tiled Surface Fluxes
          , radout%flux_up_tile(1, 1, control%map_channel(i_band))             &
          , radout%flux_up_blue_tile(1, 1, control%map_channel(i_band))        &
!                 Special Surface Fluxes
          , control%l_blue_flux_surf, spectrum%solar%weight_blue(i_band)       &
          , radout%flux_direct_blue_surf                                       &
          , radout%flux_down_blue_surf, radout%flux_up_blue_surf               &
!                 Dimensions of arrays
          , dimen%nd_profile, dimen%nd_layer, dimen%nd_layer_clr               &
          , dimen%id_cloud_top, dimen%nd_column                                &
          , dimen%nd_flux_profile, dimen%nd_radiance_profile                   &
          , dimen%nd_j_profile                                                 &
          , spectrum%dim%nd_band, spectrum%dim%nd_species                      &
          , spectrum%dim%nd_k_term, spectrum%dim%nd_scale_variable             &
          , dimen%nd_cloud_type, dimen%nd_region, dimen%nd_overlap_coeff       &
          , dimen%nd_max_order, dimen%nd_sph_coeff                             &
          , dimen%nd_brdf_basis_fnc, dimen%nd_brdf_trunc                       &
          , dimen%nd_viewing_level, dimen%nd_direction                         &
          , dimen%nd_source_coeff, dimen%nd_point_tile, dimen%nd_tile          &
          )

      CASE (ip_overlap_mix_ses2)
! DEPENDS ON: solve_band_ses
        CALL solve_band_ses(ierr                                               &
          , control, dimen, cld, bound                                         &
!                 Atmospheric properties
          , atm%n_profile, atm%n_layer, atm%mass                               &
!                 Angular integration
          , control%i_angular_integration, control%i_2stream                   &
          , n_order_phase, control%l_rescale, control%n_order_gauss            &
          , control%ms_min, control%ms_max, control%i_truncation               &
          , ls_local_trunc                                                     &
          , control%accuracy_adaptive, control%euler_factor                    &
          , control%i_sph_algorithm, control%i_sph_mode                        &
!                 Precalculated angular arrays
          , ia_sph_mm, cg_coeff, uplm_zero, uplm_sol                           &
!                 Treatment of scattering
          , control%i_scatter_method_band(i_band)                              &
!                 Options for solver
          , control%i_solver                                                   &
!                 Gaseous properties
          , i_band, n_gas, spectrum%gas%index_absorb                           &
          , spectrum%gas%i_band_k_ses, k_esft_layer, spectrum%gas%w_ses        &
          , k_mix_gas_layer, spectrum%gas%n_mix_gas(i_band)                    &
          , atm%gas_mix_ratio, gas_mix_amt                                     &
!                 Continuum absorption
          , k_contm_layer, control%l_continuum                                 &
          , n_continuum, amount_continuum                                      &
!                 Spectral region
          , control%isolir                                                     &
!                 Solar properties
          , bound%zen_0, solar_irrad_band_ses, control%l_solar_tail_flux       &
!                 Infra-red properties
          , planck_flux_band(1, 0)                                             &
          , planck_flux_band(1, atm%n_layer)                                   &
          , diff_planck_band                                                   &
          , control%l_ir_source_quad, diff_planck_band_2                       &
!                 Surface properties
          , control%ls_brdf_trunc, bound%n_brdf_basis_fnc                      &
          , bound%rho_alb(1, 1, i_band)                                        &
          , bound%f_brdf, brdf_sol, brdf_hemi                                  &
          , planck_flux_ground                                                 &
!                 Tiling of the surface
          , control%l_tile, bound%n_point_tile, bound%n_tile, bound%list_tile  &
          , bound%rho_alb_tile(1, 1, 1, i_band)                                &
          , planck_flux_tile                                                   &
!                 Optical Properties
          , ss_prop                                                            &
!                 Cloudy properties
          , control%l_cloud, control%i_cloud                                   &
!                 Cloud geometry
          , n_cloud_top                                                        &
          , n_region, k_clr, i_region_cloud, frac_region                       &
          , w_free, cloud_overlap                                              &
          , n_column_slv, list_column_slv                                      &
          , i_clm_lyr_chn, i_clm_cld_typ, area_column                          &
!                 Additional variables required for mcica
          , l_cloud_cmp, n_cloud_profile, i_cloud_profile                      &
          , i_cloud_type, dimen%nd_cloud_component                             &
          , control%i_cloud_representation                                     &
!                 Levels for calculating radiances
          , atm%n_viewing_level, i_rad_layer, frac_rad_layer                   &
!                 Viewing Geometry
          , atm%n_direction, atm%direction                                     &
!                 Weighting factor for the band
          , control%weight_band(i_band), l_initial                             &
!                 Fluxes calculated
          , radout%flux_direct(1, 0, control%map_channel(i_band))              &
          , radout%flux_down(1, 0, control%map_channel(i_band))                &
          , radout%flux_up(1, 0, control%map_channel(i_band))                  &
!                 Radiances
          , i_direct, radout%radiance(1, 1, 1, control%map_channel(i_band))    &
!                 Rate of photolysis
          , radout%photolysis(1, 1, control%map_channel(i_band))               &
!                 Flags for clear-sky calculations
          , l_clear_band, control%i_solver_clear                               &
!                 Clear-sky fluxes
          , radout%flux_direct_clear(1, 0, control%map_channel(i_band))        &
          , radout%flux_down_clear(1, 0, control%map_channel(i_band))          &
          , radout%flux_up_clear(1, 0, control%map_channel(i_band))            &
!                 Tiled Surface Fluxes
          , radout%flux_up_tile(1, 1, control%map_channel(i_band))             &
          , radout%flux_up_blue_tile(1, 1, control%map_channel(i_band))        &
!                 Special Surface Fluxes
          , control%l_blue_flux_surf, spectrum%solar%weight_blue(i_band)       &
          , radout%flux_direct_blue_surf                                       &
          , radout%flux_down_blue_surf, radout%flux_up_blue_surf               &
!                 Dimensions of arrays
          , dimen%nd_profile, dimen%nd_layer, dimen%nd_layer_clr               &
          , dimen%id_cloud_top, dimen%nd_column                                &
          , dimen%nd_flux_profile, dimen%nd_radiance_profile                   &
          , dimen%nd_j_profile                                                 &
          , spectrum%dim%nd_band, spectrum%dim%nd_species                      &
          , spectrum%dim%nd_k_term, spectrum%dim%nd_continuum                  &
          , dimen%nd_cloud_type, dimen%nd_region, dimen%nd_overlap_coeff       &
          , dimen%nd_max_order, dimen%nd_sph_coeff                             &
          , dimen%nd_brdf_basis_fnc, dimen%nd_brdf_trunc                       &
          , dimen%nd_viewing_level, dimen%nd_direction                         &
          , dimen%nd_source_coeff, dimen%nd_point_tile, dimen%nd_tile          &
          )

      CASE DEFAULT
        cmessage = '*** Error: An appropriate gaseous overlap ' //             &
          'has not been specified, even though gaseous ' //                    &
          'absorption is to be included.'
        ierr=i_err_fatal
        GO TO 9999
      END SELECT
    END IF

    IF (i_band == control%first_band) THEN

!     Calculate weighted diagnostic fluxes
!     (Note: as presently coded these diagnostics will only work if
!      the weight is non-zero for band 1 only. A complete
!      treatment would require passing WEIGHT_DIAG through to the
!      routine AUGMENT_RADIANCE as done for WEIGHT_BLUE.)

      IF (control%l_flux_direct_diag) THEN
        DO i=0, atm%n_layer
          DO l=1, atm%n_profile
            radout%flux_direct_diag(l,i,1)=                                    &
              control%weight_diag(i_band)*radout%flux_direct(l,i,1)
          END DO
        END DO
      END IF
      IF (control%l_flux_up_diag) THEN
        DO i=0, atm%n_layer
          DO l=1, atm%n_profile
            radout%flux_up_diag(l,i,1)=                                        &
              control%weight_diag(i_band)*radout%flux_up(l,i,1)
          END DO
        END DO
      END IF
      IF (control%l_flux_down_diag) THEN
        DO i=0, atm%n_layer
          DO l=1, atm%n_profile
            radout%flux_down_diag(l,i,1)=                                      &
              control%weight_diag(i_band)*radout%flux_down(l,i,1)
          END DO
        END DO
      END IF
      IF (control%l_flux_down_diag_surf) THEN
        DO l=1, atm%n_profile
          radout%flux_down_diag_surf(l)=                                       &
            control%weight_diag(i_band)*radout%flux_down(l,atm%n_layer,1)
        END DO
      END IF
      IF (control%l_flux_down_clear_diag_surf) THEN
        DO l=1, atm%n_profile
          radout%flux_down_clear_diag_surf(l)=                                 &
            control%weight_diag(i_band)*radout%flux_down_clear(l,atm%n_layer,1)
        END DO
      END IF

    END IF


!   Spectral diagnostics for clouds:

    IF (control%l_cloud_extinction) THEN

!     Increment the arrays of diagnostics. The extinction
!     calculated in this band is a mean value weighted with the
!     fractions of individual types of cloud (which sum to 1).
!     Here it is weighted with the clear-sky direct solar
!     flux in the band at the top of the current
!     layer and the total amount of cloud in the grid-box.
!     This definition has the advantage of convenience, but there
!     appears to be no optimal definition of an average extinction.

      DO i=n_cloud_top, atm%n_layer
!CDIR NODEP
        DO ll=1, n_cloud_profile(i)
           l=i_cloud_profile(ll, i)
           radout%cloud_weight_extinction(l, i)                                &
              =radout%cloud_weight_extinction(l, i) + cld%w_cloud(l, i)*       &
              (radout%flux_direct_clear(l, i-1,control%map_channel(i_band))    &
              -flux_direct_clear_prev(l, i-1))
           radout%cloud_extinction(l, i)                                       &
              =radout%cloud_extinction(l, i) + cld%w_cloud(l, i)*              &
              (radout%flux_direct_clear(l, i-1,control%map_channel(i_band))    &
              -flux_direct_clear_prev(l, i-1))                                 &
              *cloud_extinction_band(l, i)
        END DO
      END DO

    END IF

    IF (control%l_cloud_absorptivity) THEN

!     Increment the arrays of diagnostics. The absorptivity
!     calculated in this band is a mean value weighted with the
!     fractions of individual types of cloud (which sum to 1).
!     Here it is weighted with the modulus of the clear_sky
!     differential flux in the band at the top of the current
!     layer and the total amount of cloud in the grid-box, as the
!     diagnostic is a measure of the effect of introducing an
!     infinitesimal layer of layer at the top of the current
!     layer into a clear atmosphere on the upward flux at the
!     top of the cloud.

      DO i=n_cloud_top, atm%n_layer
!CDIR NODEP
        DO ll=1, n_cloud_profile(i)
           l=i_cloud_profile(ll, i)
           radout%cloud_weight_absorptivity(l, i)                              &
              =radout%cloud_weight_absorptivity(l, i) + cld%w_cloud(l, i)*     &
              ABS(radout%flux_up_clear(l, i-1,control%map_channel(i_band))     &
              -flux_up_clear_prev(l, i-1))
           radout%cloud_absorptivity(l, i)                                     &
              =radout%cloud_absorptivity(l, i) + cld%w_cloud(l, i)*            &
              ABS(radout%flux_up_clear(l, i-1,control%map_channel(i_band))     &
              -flux_up_clear_prev(l, i-1))                                     &
              *cloud_absorptivity_band(l, i)
        END DO
      END DO

    END IF
    IF (control%l_ls_cloud_extinction) THEN

!     Increment the arrays of diagnostics. The extinction
!     calculated in this band is a mean value weighted with the
!     fractions of individual types of cloud (which sum to 1).
!     Here it is weighted with the clear-sky direct solar
!     flux in the band at the top of the current
!     layer and the total amount of cloud in the grid-box.
!     This definition has the advantage of convenience, but there
!     appears to be no optimal definition of an average extinction.

      DO i=n_cloud_top, atm%n_layer
!CDIR NODEP
        DO ll=1, n_cloud_profile(i)
           l=i_cloud_profile(ll, i)
           radout%ls_cloud_weight_extinction(l, i)                             &
              =radout%ls_cloud_weight_extinction(l, i)                         &
              +cld%w_cloud(l, i)*(cld%frac_cloud(l,i,1)+cld%frac_cloud(l,i,2)) &
              *(radout%flux_direct_clear(l, i-1,control%map_channel(i_band))   &
               -flux_direct_clear_prev(l, i-1))
           radout%ls_cloud_extinction(l, i)                                    &
              =radout%ls_cloud_extinction(l, i)                                &
              +cld%w_cloud(l, i)*(cld%frac_cloud(l,i,1)+cld%frac_cloud(l,i,2)) &
              *(radout%flux_direct_clear(l, i-1,control%map_channel(i_band))   &
               -flux_direct_clear_prev(l, i-1))                                &
              *ls_cloud_extinction_band(l, i)
        END DO
      END DO

    END IF

    IF (control%l_ls_cloud_absorptivity) THEN

!     Increment the arrays of diagnostics. The absorptivity
!     calculated in this band is a mean value weighted with the
!     fractions of individual types of cloud (which sum to 1).
!     Here it is weighted with the modulus of the clear_sky
!     differential flux in the band at the top of the current
!     layer and the total amount of cloud in the grid-box as the
!     diagnostic is a measure of the effect of introducing an
!     infinitesimal layer of layer at the top of the current
!     layer into a clear atmosphere on the upward flux at the
!     top of the cloud.

      DO i=n_cloud_top, atm%n_layer
!CDIR NODEP
        DO ll=1, n_cloud_profile(i)
           l=i_cloud_profile(ll, i)
           radout%ls_cloud_weight_absorptivity(l, i)                           &
              =radout%ls_cloud_weight_absorptivity(l, i)                       &
              +cld%w_cloud(l, i)*(cld%frac_cloud(l,i,1)+cld%frac_cloud(l,i,2)) &
              *ABS(radout%flux_up_clear(l, i-1,control%map_channel(i_band))    &
              -flux_up_clear_prev(l, i-1))
           radout%ls_cloud_absorptivity(l, i)                                  &
              =radout%ls_cloud_absorptivity(l, i)                              &
              +cld%w_cloud(l, i)*(cld%frac_cloud(l,i,1)+cld%frac_cloud(l,i,2)) &
              *ABS(radout%flux_up_clear(l, i-1,control%map_channel(i_band))    &
              -flux_up_clear_prev(l, i-1))                                     &
              *ls_cloud_absorptivity_band(l, i)
        END DO
      END DO

    END IF
    IF (control%l_cnv_cloud_extinction) THEN

!     Increment the arrays of diagnostics. The extinction
!     calculated in this band is a mean value weighted with the
!     fractions of individual types of cloud (which sum to 1).
!     Here it is weighted with the clear-sky direct solar
!     flux in the band at the top of the current
!     layer and the total amount of cloud in the grid-box.
!     This definition has the advantage of convenience, but there
!     appears to be no optimal definition of an average extinction.

      DO i=n_cloud_top, atm%n_layer
!CDIR NODEP
        DO ll=1, n_cloud_profile(i)
           l=i_cloud_profile(ll, i)
           radout%cnv_cloud_weight_extinction(l, i)                            &
              =radout%cnv_cloud_weight_extinction(l, i)                        &
              +cld%w_cloud(l, i)*(cld%frac_cloud(l,i,3)+cld%frac_cloud(l,i,4)) &
              *(radout%flux_direct_clear(l, i-1,control%map_channel(i_band))   &
               -flux_direct_clear_prev(l, i-1))
           radout%cnv_cloud_extinction(l, i)                                   &
              =radout%cnv_cloud_extinction(l, i)                               &
              +cld%w_cloud(l, i)*(cld%frac_cloud(l,i,3)+cld%frac_cloud(l,i,4)) &
              *(radout%flux_direct_clear(l, i-1,control%map_channel(i_band))   &
               -flux_direct_clear_prev(l, i-1))                                &
              *cnv_cloud_extinction_band(l, i)
        END DO
      END DO

    END IF

    IF (control%l_cnv_cloud_absorptivity) THEN

!     Increment the arrays of diagnostics. The absorptivity
!     calculated in this band is a mean value weighted with the
!     fractions of individual types of cloud (which sum to 1).
!     Here it is weighted with the modulus of the clear_sky
!     differential flux in the band at the top of the current
!     layer and the total amount of cloud in the grid-box as the
!     diagnostic is a measure of the effect of introducing an
!     infinitesimal layer of layer at the top of the current
!     layer into a clear atmosphere on the upward flux at the
!     top of the cloud.

      DO i=n_cloud_top, atm%n_layer
!CDIR NODEP
        DO ll=1, n_cloud_profile(i)
           l=i_cloud_profile(ll, i)
           radout%cnv_cloud_weight_absorptivity(l, i)                          &
              =radout%cnv_cloud_weight_absorptivity(l, i)                      &
              +cld%w_cloud(l, i)*(cld%frac_cloud(l,i,3)+cld%frac_cloud(l,i,4)) &
              *ABS(radout%flux_up_clear(l, i-1,control%map_channel(i_band))    &
              -flux_up_clear_prev(l, i-1))
           radout%cnv_cloud_absorptivity(l, i)                                 &
              =radout%cnv_cloud_absorptivity(l, i)                             &
              +cld%w_cloud(l, i)*(cld%frac_cloud(l,i,3)+cld%frac_cloud(l,i,4)) &
              *ABS(radout%flux_up_clear(l, i-1,control%map_channel(i_band))    &
              -flux_up_clear_prev(l, i-1))                                     &
              *cnv_cloud_absorptivity_band(l, i)
        END DO
      END DO

    END IF

!   Deallocate the single scattering propeties (in reverse order).
    DEALLOCATE(ss_prop%forward_scatter_no_cloud)
    DEALLOCATE(ss_prop%phase_fnc_no_cloud)

    DEALLOCATE(ss_prop%forward_solar_cloud_comp)
    DEALLOCATE(ss_prop%phase_fnc_solar_cloud_comp)
    DEALLOCATE(ss_prop%forward_scatter_cloud_comp)
    DEALLOCATE(ss_prop%phase_fnc_cloud_comp)
    DEALLOCATE(ss_prop%k_ext_scat_cloud_comp)
    DEALLOCATE(ss_prop%k_ext_tot_cloud_comp)

    DEALLOCATE(ss_prop%omega)
    DEALLOCATE(ss_prop%tau)
    DEALLOCATE(ss_prop%tau_noscal)
    DEALLOCATE(ss_prop%forward_solar)
    DEALLOCATE(ss_prop%phase_fnc_solar)
    DEALLOCATE(ss_prop%forward_scatter)
    DEALLOCATE(ss_prop%phase_fnc)
    DEALLOCATE(ss_prop%k_ext_scat)
    DEALLOCATE(ss_prop%k_grey_tot)

    DEALLOCATE(ss_prop%omega_clr)
    DEALLOCATE(ss_prop%tau_clr)
    DEALLOCATE(ss_prop%tau_clr_noscal)

    DEALLOCATE(ss_prop%forward_solar_clr)
    DEALLOCATE(ss_prop%phase_fnc_solar_clr)
    DEALLOCATE(ss_prop%forward_scatter_clr)
    DEALLOCATE(ss_prop%phase_fnc_clr)
    DEALLOCATE(ss_prop%k_ext_scat_clr)
    DEALLOCATE(ss_prop%k_grey_tot_clr)


!   Make any adjustments to fluxes and radiances to convert
!   to actual values. This is done inside the loop over bands
!   to allow for division of the output fluxes between
!   separate diagnostic bands.
    IF (control%isolir == ip_infra_red) THEN
! DEPENDS ON: adjust_ir_radiance
      CALL adjust_ir_radiance(atm%n_profile, atm%n_layer, atm%n_viewing_level  &
        , atm%n_direction, control%i_angular_integration, control%i_sph_mode   &
        , planck_flux_band, planck_radiance_band                               &
        , radout%flux_down(1, 0, control%map_channel(i_band))                  &
        , radout%flux_up(1, 0, control%map_channel(i_band))                    &
        , radout%radiance(1, 1, 1, control%map_channel(i_band))                &
        , l_clear_band                                                         &
        , radout%flux_down_clear(1, 0, control%map_channel(i_band))            &
        , radout%flux_up_clear(1, 0, control%map_channel(i_band))              &
        , dimen%nd_2sg_profile, dimen%nd_flux_profile                          &
        , dimen%nd_radiance_profile                                            &
        , dimen%nd_layer, dimen%nd_direction, dimen%nd_viewing_level           &
        )
    END IF

!   Set band-by-band flux diagnostics
    IF (control%l_flux_direct_band) THEN
      radout%flux_direct_band(:,:,i_band)                                      &
        = radout%flux_direct(:,:,control%map_channel(i_band))                  &
        - radout%flux_direct_band(:,:,i_band)
    END IF
    IF (control%l_flux_down_band) THEN
      radout%flux_down_band(:,:,i_band)                                        &
        = radout%flux_down(:,:,control%map_channel(i_band))                    &
        - radout%flux_down_band(:,:,i_band)
    END IF
    IF (control%l_flux_up_band) THEN
      radout%flux_up_band(:,:,i_band)                                          &
        = radout%flux_up(:,:,control%map_channel(i_band))                      &
        - radout%flux_up_band(:,:,i_band)
    END IF
    IF (control%l_flux_direct_clear_band) THEN
      radout%flux_direct_clear_band(:,:,i_band)                                &
        = radout%flux_direct_clear(:,:,control%map_channel(i_band))            &
        - flux_direct_clear_prev(:,:)
    END IF
    IF (control%l_flux_down_clear_band) THEN
      radout%flux_down_clear_band(:,:,i_band)                                  &
        = radout%flux_down_clear(:,:,control%map_channel(i_band))              &
        - radout%flux_down_clear_band(:,:,i_band)
    END IF
    IF (control%l_flux_up_clear_band) THEN
      radout%flux_up_clear_band(:,:,i_band)                                    &
        = radout%flux_up_clear(:,:,control%map_channel(i_band))                &
        - flux_up_clear_prev(:,:)
    END IF

  END DO ! i_band

  9999 CONTINUE
  IF (ierr /= i_normal) THEN
    CALL ereport(RoutineName, ierr, cmessage)
  END IF

  IF (lhook) CALL dr_hook(RoutineName,zhook_out,zhook_handle)

END SUBROUTINE radiance_calc
