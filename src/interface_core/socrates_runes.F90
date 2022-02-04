! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
! @brief Run the Socrates radiative transfer code

module socrates_runes

use rad_pcf, only: &
  ip_source_illuminate                      => ip_solar, &
  ip_source_thermal                         => ip_infra_red, &
  ip_cloud_representation_off               => ip_cloud_off, &
  ip_cloud_representation_ice_water         => ip_cloud_ice_water, &
  ip_cloud_representation_combine_ice_water => ip_cloud_combine_ice_water, &
  ip_cloud_representation_csiw              => ip_cloud_csiw, &
  ip_cloud_representation_split_ice_water   => ip_cloud_split_ice_water, &
  ip_overlap_max_random                     => ip_max_rand, &
  ip_overlap_random                         => ip_rand, &
  ip_overlap_exponential_random             => ip_exponential_rand, &
  ip_inhom_homogeneous                      => ip_homogeneous, &
  ip_inhom_scaling                          => ip_scaling, &
  ip_inhom_mcica                            => ip_mcica, &
  ip_inhom_cairns                           => ip_cairns, &
  ip_inhom_tripleclouds_2019                => ip_tripleclouds_2019, &
  ip_droplet_re_external                    => ip_re_external, &
  ip_droplet_re_liu                         => ip_re_liu, &
  ip_droplet_re_default                     => ip_re_default

use socrates_def_diag, only: StrDiag

implicit none
character(len=*), parameter, private :: ModuleName = 'SOCRATES_RUNES'
contains

subroutine runes(n_profile, n_layer, diag, &
  spectrum, spectrum_name, mcica_data, &
  profile_list, n_layer_stride, n_level_stride, &
  n_cloud_layer, n_aer_mode, n_aer_layer, n_tile, &
  p_layer, t_layer, t_level, mass, density, &
  h2o, o3, &
  p_layer_1d, t_layer_1d, t_level_1d, mass_1d, density_1d, &
  h2o_1d, o3_1d, &
  co2_mix_ratio, n2o_mix_ratio, ch4_mix_ratio, &
  o2_mix_ratio, so2_mix_ratio, cfc11_mix_ratio, cfc12_mix_ratio, &
  cfc113_mix_ratio, hcfc22_mix_ratio, hfc134a_mix_ratio, &
  t_ground, cos_zenith_angle, solar_irrad, orog_corr, &
  l_grey_albedo, grey_albedo, albedo_diff, albedo_dir, &
  albedo_diff_1d, albedo_dir_1d, &
  l_tile, frac_tile, t_tile, albedo_diff_tile, albedo_dir_tile, &
  frac_tile_1d, t_tile_1d, albedo_diff_tile_1d, albedo_dir_tile_1d, &
  cloud_frac, conv_frac, &
  liq_frac, ice_frac, liq_conv_frac, ice_conv_frac, &
  liq_mmr, ice_mmr, liq_conv_mmr, ice_conv_mmr, &
  liq_dim, ice_dim, liq_conv_dim, ice_conv_dim, &
  liq_rsd, ice_rsd, liq_conv_rsd, ice_conv_rsd, &
  liq_nc, ice_nc, liq_conv_nc, ice_conv_nc, &
  cloud_frac_1d, conv_frac_1d, &
  liq_frac_1d, ice_frac_1d, liq_conv_frac_1d, ice_conv_frac_1d, &
  liq_mmr_1d, ice_mmr_1d, liq_conv_mmr_1d, ice_conv_mmr_1d, &
  liq_dim_1d, ice_dim_1d, liq_conv_dim_1d, ice_conv_dim_1d, &
  liq_rsd_1d, ice_rsd_1d, liq_conv_rsd_1d, ice_conv_rsd_1d, &
  liq_nc_1d, ice_nc_1d, liq_conv_nc_1d, ice_conv_nc_1d, &
  cloud_vertical_decorr, conv_vertical_decorr, &
  cloud_horizontal_rsd, &
  layer_heat_capacity, layer_heat_capacity_1d, &
  i_source, i_cloud_representation, i_overlap, i_inhom, &
  i_mcica_sampling, i_st_water, i_cnv_water, i_st_ice, i_cnv_ice, i_drop_re, &
  rand_seed, &
  l_rayleigh, l_mixing_ratio, l_aerosol_mode, &
  aer_mix_ratio, aer_absorption, aer_scattering, aer_asymmetry, &
  aer_mix_ratio_1d, aer_absorption_1d, aer_scattering_1d, aer_asymmetry_1d, &
  mean_rel_humidity, mean_rel_humidity_1d, &
  l_water_soluble, water_soluble, water_soluble_1d, &
  l_dust_like, dust_like, dust_like_1d, &
  l_oceanic, oceanic, oceanic_1d, &
  l_soot, soot, soot_1d, &
  l_ash, ash, ash_1d, &
  l_sulphuric, sulphuric, sulphuric_1d, &
  l_ammonium_sulphate, ammonium_sulphate, ammonium_sulphate_1d, &
  l_saharan_dust, saharan_dust, saharan_dust_1d, &
  l_accum_sulphate, accum_sulphate, accum_sulphate_1d, &
  l_aitken_sulphate, aitken_sulphate, aitken_sulphate_1d, &
  l_fresh_soot, fresh_soot, fresh_soot_1d, &
  l_aged_soot, aged_soot, aged_soot_1d, &
  l_sodium_chloride, sodium_chloride, sodium_chloride_1d, &
  l_seasalt_film, seasalt_film, seasalt_film_1d, &
  l_seasalt_jet, seasalt_jet, seasalt_jet_1d, &
  l_dust_div1, dust_div1, dust_div1_1d, &
  l_dust_div2, dust_div2, dust_div2_1d, &
  l_dust_div3, dust_div3, dust_div3_1d, &
  l_dust_div4, dust_div4, dust_div4_1d, &
  l_dust_div5, dust_div5, dust_div5_1d, &
  l_dust_div6, dust_div6, dust_div6_1d, &
  l_biomass_1, biomass_1, biomass_1_1d, &
  l_biomass_2, biomass_2, biomass_2_1d, &
  l_biogenic, biogenic, biogenic_1d, &
  l_ocff_fresh, ocff_fresh, ocff_fresh_1d, &
  l_ocff_aged, ocff_aged, ocff_aged_1d, &
  l_delta, delta, delta_1d, &
  l_murk, murk, murk_1d, &
  l_nitrate, nitrate, nitrate_1d, &
  l_twobindust_1, twobindust_1, twobindust_1_1d, &
  l_twobindust_2, twobindust_2, twobindust_2_1d, &
  l_invert, l_profile_last, l_debug, i_profile_debug)


use def_spectrum, only: StrSpecData
use def_mcica,    only: StrMcica
use def_control,  only: StrCtrl,  deallocate_control
use def_dimen,    only: StrDim
use def_atm,      only: StrAtm,   deallocate_atm
use def_bound,    only: StrBound, deallocate_bound
use def_cld,      only: StrCld,   deallocate_cld, deallocate_cld_prsc, &
                                  deallocate_cld_mcica
use def_aer,      only: StrAer,   deallocate_aer, deallocate_aer_prsc
use def_out,      only: StrOut,   deallocate_out

use socrates_set_spectrum, only: spectrum_array_name, spectrum_array, &
                                 mcica_spectrum_name, mcica_data_array

use socrates_set_control,   only: set_control
use socrates_set_dimen,     only: set_dimen
use socrates_set_atm,       only: set_atm
use socrates_set_bound,     only: set_bound
use socrates_set_cld,       only: set_cld
use socrates_set_cld_dim,   only: set_cld_dim
use socrates_set_cld_mcica, only: set_cld_mcica
use socrates_set_aer,       only: set_aer
use socrates_set_diag,      only: set_diag

use realtype_rd, only: RealExt
use ereport_mod, only: ereport
use errormessagelength_mod, only: errormessagelength
use rad_pcf, only: i_normal, i_err_fatal

implicit none

! Output diagnostic fields
type(StrDiag), intent(inout) :: diag

! Spectral data:
type (StrSpecData), intent(in), target, optional :: spectrum
character(len=*), intent(in), optional :: spectrum_name

! Mcica data
type (StrMcica), intent(in), target, optional :: mcica_data

integer, intent(in) :: n_profile
!   Number of columns to operate on
integer, intent(in) :: n_layer
!   Number of layers for radiation
integer, intent(in), optional :: profile_list(:)
!   List of profiles to use from input fields
integer, intent(in), optional :: n_layer_stride
!   Number of layers in input 1d arrays
integer, intent(in), optional :: n_level_stride
!   Number of levels in input 1d arrays
integer, intent(in), optional :: n_tile
!   Number of surface tiles
integer, intent(in), optional :: n_cloud_layer
!   Number of potentially cloudy layers
integer, intent(in), optional :: n_aer_mode
!   Number of aerosol modes
integer, intent(in), optional :: n_aer_layer
!   Number of aerosol layers in 1d arrays

real(RealExt), intent(in), optional :: p_layer(:, :)
real(RealExt), intent(in), optional :: p_layer_1d(:)
!   Pressure at layer centres
real(RealExt), intent(in), optional :: t_layer(:, :)
real(RealExt), intent(in), optional :: t_layer_1d(:)
!   Temperature at layer centres
real(RealExt), intent(in), optional :: t_level(:, :)
real(RealExt), intent(in), optional :: t_level_1d(:)
!   Temperature at layer boundaries
real(RealExt), intent(in), optional :: mass(:, :)
real(RealExt), intent(in), optional :: mass_1d(:)
!   Mass of layer (kg m-2)
real(RealExt), intent(in), optional :: density(:, :)
real(RealExt), intent(in), optional :: density_1d(:)
!   Density of layer (kg m-3)
real(RealExt), intent(in), optional :: h2o(:, :)
real(RealExt), intent(in), optional :: h2o_1d(:)
!   Mass mixing ratio of water vapour
real(RealExt), intent(in), optional :: o3(:, :)
real(RealExt), intent(in), optional :: o3_1d(:)
!   Mass mixing ratio of ozone

real(RealExt), intent(in), optional :: &
  co2_mix_ratio, n2o_mix_ratio, ch4_mix_ratio, &
  o2_mix_ratio, so2_mix_ratio, cfc11_mix_ratio, cfc12_mix_ratio, &
  cfc113_mix_ratio, hcfc22_mix_ratio, hfc134a_mix_ratio
!   Trace gas mass mixing ratios

real(RealExt), intent(in), optional :: t_ground(:)
!   Effective radiative temperature over whole grid-box
real(RealExt), intent(in), optional :: cos_zenith_angle(:)
!   Cosine of solar zenith angle
real(RealExt), intent(in), optional :: solar_irrad(:)
!   Solar irradiance at top-of-atmosphere (mean over timestep)
real(RealExt), intent(in), optional :: orog_corr(:)
!   Orographic correction factor

logical, intent(in), optional :: l_grey_albedo
!   Set a single grey albedo / emissivity for the surface
real(RealExt), intent(in), optional :: grey_albedo
!   Grey surface albedo

real(RealExt), intent(in), optional :: albedo_diff(:, :)
!   Spectral diffuse albedo (n_profile, n_band)
real(RealExt), intent(in), optional :: albedo_dir(:, :)
!   Spectral direct albedo (n_profile, n_band)
real(RealExt), intent(in), optional :: albedo_diff_1d(:)
!   1d spectral diffuse albedo (n_band)
real(RealExt), intent(in), optional :: albedo_dir_1d(:)
!   1d spectral direct albedo (n_band)

logical, intent(in), optional :: l_tile
!   Use tiled surface properties
real(RealExt), intent(in), optional :: frac_tile(:, :)
!   Tile fractions (n_profile, n_tile)
real(RealExt), intent(in), optional :: t_tile(:, :)
!   Tile temperatures (n_profile, n_tile)
real(RealExt), intent(in), optional :: albedo_diff_tile(:, :, :)
!   Diffuse tile albedo (n_profile, n_tile, n_band)
real(RealExt), intent(in), optional :: albedo_dir_tile(:, :, :)
!   Direct tile albedo (n_profile, n_tile, n_band)
real(RealExt), intent(in), optional :: frac_tile_1d(:)
!   1d tile fractions (n_tile)
real(RealExt), intent(in), optional :: t_tile_1d(:)
!   1d tile temperatures (n_tile)
real(RealExt), intent(in), optional :: albedo_diff_tile_1d(:)
!   1d diffuse tile albedo (n_tile*n_band)
real(RealExt), intent(in), optional :: albedo_dir_tile_1d(:)
!   1d direct tile albedo (n_tile*n_band)

real(RealExt), intent(in), dimension (:, :), optional :: &
  cloud_frac, conv_frac, &
  liq_frac, ice_frac, liq_conv_frac, ice_conv_frac, &
  liq_mmr, ice_mmr, liq_conv_mmr, ice_conv_mmr, &
  liq_dim, ice_dim, liq_conv_dim, ice_conv_dim, &
  liq_rsd, ice_rsd, liq_conv_rsd, ice_conv_rsd, &
  liq_nc, ice_nc, liq_conv_nc, ice_conv_nc
real(RealExt), intent(in), dimension (:), optional :: &
  cloud_frac_1d, conv_frac_1d, &
  liq_frac_1d, ice_frac_1d, liq_conv_frac_1d, ice_conv_frac_1d, &
  liq_mmr_1d, ice_mmr_1d, liq_conv_mmr_1d, ice_conv_mmr_1d, &
  liq_dim_1d, ice_dim_1d, liq_conv_dim_1d, ice_conv_dim_1d, &
  liq_rsd_1d, ice_rsd_1d, liq_conv_rsd_1d, ice_conv_rsd_1d, &
  liq_nc_1d, ice_nc_1d, liq_conv_nc_1d, ice_conv_nc_1d
!   Liquid and ice cloud fractions, gridbox mean mixing ratios,
!   effective dimensions, relative standard deviation of condensate,
!   and number concentration

real(RealExt), intent(in), optional :: cloud_vertical_decorr
!   Decorrelation pressure scale for cloud vertical overlap
real(RealExt), intent(in), optional :: conv_vertical_decorr
!   Decorrelation pressure scale for convective cloud vertical overlap
real(RealExt), intent(in), optional :: cloud_horizontal_rsd
!   Relative standard deviation of sub-grid cloud condensate

real(RealExt), intent(in), optional :: layer_heat_capacity(:, :)
real(RealExt), intent(in), optional :: layer_heat_capacity_1d(:)
!   Heat capacity of layer

integer, intent(in), optional :: i_source
!   Select source of radiation
integer, intent(in), optional :: &
  i_cloud_representation, i_overlap, i_inhom, &
  i_mcica_sampling, i_st_water, i_st_ice, i_cnv_water, i_cnv_ice, i_drop_re
!   Select treatment of cloud
integer, intent(in), optional :: rand_seed(:)
!   Random seed for cloud generator

logical, intent(in), optional :: l_rayleigh
!   Include Rayleigh scattering

logical, intent(in), optional :: l_mixing_ratio
!   Assume mass mixing ratios are with respect to dry mass

logical, intent(in), optional :: l_aerosol_mode
!   Include aerosol optical properties specified by mode

real(RealExt), intent(in), optional :: aer_mix_ratio(:, :, :)
!   MODE aerosol mass-mixing ratio (n_profile, n_layer, n_mode)

real(RealExt), intent(in), optional :: aer_mix_ratio_1d(:)
!   1d MODE aerosol mass-mixing ratio (n_aer_layer*n_mode)

real(RealExt), intent(in), dimension(:, :, :, :), optional :: &
  aer_absorption, aer_scattering, aer_asymmetry
!   MODE aerosol optical properties (n_profile, n_layer, n_mode, n_band)

real(RealExt), intent(in), dimension(:), optional :: &
  aer_absorption_1d, aer_scattering_1d, aer_asymmetry_1d
!   1d MODE aerosol optical properties (n_aer_layer*n_mode*n_band)

real(RealExt), intent(in), optional :: mean_rel_humidity(:, :)
real(RealExt), intent(in), optional :: mean_rel_humidity_1d(:)
!   Mean relative humidity applicable for CLASSIC aerosols (clear-sky)

logical, intent(in), optional :: &
  l_water_soluble, l_dust_like, l_oceanic, l_soot, l_ash, l_sulphuric, &
  l_ammonium_sulphate, l_saharan_dust, &
  l_accum_sulphate, l_aitken_sulphate, &
  l_fresh_soot, l_aged_soot, &
  l_sodium_chloride, l_seasalt_film, l_seasalt_jet, &
  l_dust_div1, l_dust_div2, l_dust_div3, &
  l_dust_div4, l_dust_div5, l_dust_div6, &
  l_biomass_1, l_biomass_2, &
  l_biogenic, &
  l_ocff_fresh, l_ocff_aged, &
  l_delta, l_murk, &
  l_nitrate, &
  l_twobindust_1, l_twobindust_2
! Flags to include CLASSIC aerosols

real(RealExt), intent(in), dimension(:, :), optional :: &
  water_soluble, dust_like, oceanic, soot, ash, sulphuric, &
  ammonium_sulphate, saharan_dust, &
  accum_sulphate, aitken_sulphate, &
  fresh_soot, aged_soot, &
  sodium_chloride, seasalt_film, seasalt_jet, &
  dust_div1, dust_div2, dust_div3, &
  dust_div4, dust_div5, dust_div6, &
  biomass_1, biomass_2, &
  biogenic, &
  ocff_fresh, ocff_aged, &
  delta, murk, &
  nitrate, &
  twobindust_1, twobindust_2
! CLASSIC aerosol mass mixing ratios

real(RealExt), intent(in), dimension(:), optional :: &
  water_soluble_1d, dust_like_1d, oceanic_1d, soot_1d, ash_1d, sulphuric_1d, &
  ammonium_sulphate_1d, saharan_dust_1d, &
  accum_sulphate_1d, aitken_sulphate_1d, &
  fresh_soot_1d, aged_soot_1d, &
  sodium_chloride_1d, seasalt_film_1d, seasalt_jet_1d, &
  dust_div1_1d, dust_div2_1d, dust_div3_1d, &
  dust_div4_1d, dust_div5_1d, dust_div6_1d, &
  biomass_1_1d, biomass_2_1d, &
  biogenic_1d, &
  ocff_fresh_1d, ocff_aged_1d, &
  delta_1d, murk_1d, &
  nitrate_1d, &
  twobindust_1_1d, twobindust_2_1d
! 1d CLASSIC aerosol mass mixing ratios

logical, intent(in), optional :: l_invert
!   Flag to invert fields in the vertical
logical, intent(in), optional :: l_profile_last
!   Loop over profiles is last in input fields and diagnostics

logical, intent(in), optional :: l_debug
integer, intent(in), optional :: i_profile_debug
!   Options for outputting debugging information


! Spectral data:
type(StrSpecData), pointer :: spec => null()

! Mcica data:
type(StrMcica), pointer :: mcica => null()
type(StrMcica), target :: mcica_dummy

! Controlling options:
type(StrCtrl) :: control

! Dimensions:
type(StrDim) :: dimen

! Atmospheric properties:
type(StrAtm) :: atm

! Boundary conditions:
type(StrBound) :: bound

! Cloud properties:
type(StrCld) :: cld

! Aerosol properties:
type(StrAer) :: aer

! Output fields from core radiation code:
type(StrOut) :: radout

integer :: id_spec, id_mcica
!   Loop variables

integer :: ierr = i_normal
character (len=errormessagelength) :: cmessage
character (len=*), parameter :: RoutineName = 'RUNES'


if (present(spectrum_name)) then
  do id_spec=1, size(spectrum_array)
    if (spectrum_array_name(id_spec) == spectrum_name) exit
    if (id_spec == size(spectrum_array)) then
      cmessage = 'Spectrum name not found.'
      ierr=i_err_fatal
      call ereport(ModuleName//':'//RoutineName, ierr, cmessage)
    end if
  end do
  spec => spectrum_array(id_spec)
  mcica => mcica_dummy
  if (present(i_cloud_representation).and.present(i_inhom)) then
    if ( (i_cloud_representation /= ip_cloud_representation_off) .and. &
         (i_inhom == ip_inhom_mcica) ) then
      if (allocated(mcica_data_array).and.allocated(mcica_spectrum_name)) then
        do id_mcica=1, size(mcica_data_array)
          if (mcica_spectrum_name(i_source, id_mcica) == spectrum_name) exit
          if (id_mcica == size(mcica_data_array)) then
            cmessage = 'Spectrum name not associated with MCICA data.'
            ierr=i_err_fatal
            call ereport(ModuleName//':'//RoutineName, ierr, cmessage)
          end if
        end do
        mcica => mcica_data_array(id_mcica)
      else
        cmessage = 'MCICA data has not been read in correctly.'
        ierr=i_err_fatal
        call ereport(ModuleName//':'//RoutineName, ierr, cmessage)
      end if
    end if
  end if
else if (present(spectrum)) then
  spec => spectrum
  mcica => mcica_dummy
  if (present(i_cloud_representation).and.present(i_inhom)) then
    if ( (i_cloud_representation /= ip_cloud_representation_off) .and. &
         (i_inhom == ip_inhom_mcica) ) then
      if (present(mcica_data)) then
        mcica => mcica_data
      else
        cmessage = 'No mcica_data object has been passed to socrates_runes.'
        ierr=i_err_fatal
        call ereport(ModuleName//':'//RoutineName, ierr, cmessage)
      end if
    end if
  end if
else
  cmessage = 'No spectrum name or object supplied.'
  ierr=i_err_fatal
  call ereport(ModuleName//':'//RoutineName, ierr, cmessage)
end if

call set_control(control, diag, spec, &
  isolir                 = i_source, &
  l_rayleigh             = l_rayleigh, &
  l_mixing_ratio         = l_mixing_ratio, &
  l_aerosol_mode         = l_aerosol_mode, &
  l_tile                 = l_tile, &
  n_tile                 = n_tile, &
  n_cloud_layer          = n_cloud_layer, &
  n_aer_mode             = n_aer_mode, &
  i_cloud_representation = i_cloud_representation, &
  i_overlap              = i_overlap, &
  i_inhom                = i_inhom, &
  i_mcica_sampling       = i_mcica_sampling, &
  i_st_water             = i_st_water, &
  i_cnv_water            = i_cnv_water, &
  i_st_ice               = i_st_ice, &
  i_cnv_ice              = i_cnv_ice, &
  i_drop_re              = i_drop_re, &
  l_set_defaults         = .true.)

call set_dimen(dimen, control, n_profile, n_layer, &
  mcica_data    = mcica, &
  n_tile        = n_tile, &
  n_cloud_layer = n_cloud_layer, &
  n_aer_mode    = n_aer_mode )

call set_atm(atm, dimen, spec, n_profile, n_layer, &
  profile_list      = profile_list, &
  n_layer_stride    = n_layer_stride, &
  n_level_stride    = n_level_stride, &
  p_layer           = p_layer, &
  t_layer           = t_layer, &
  mass              = mass, &
  density           = density, &
  t_level           = t_level, &
  h2o               = h2o, &
  o3                = o3, &
  p_layer_1d        = p_layer_1d, &
  t_layer_1d        = t_layer_1d, &
  mass_1d           = mass_1d, &
  density_1d        = density_1d, &
  t_level_1d        = t_level_1d, &
  h2o_1d            = h2o_1d, &
  o3_1d             = o3_1d, &
  co2_mix_ratio     = co2_mix_ratio, &
  n2o_mix_ratio     = n2o_mix_ratio, &
  ch4_mix_ratio     = ch4_mix_ratio, &
  o2_mix_ratio      = o2_mix_ratio, &
  so2_mix_ratio     = so2_mix_ratio, &
  cfc11_mix_ratio   = cfc11_mix_ratio, &
  cfc12_mix_ratio   = cfc12_mix_ratio, &
  cfc113_mix_ratio  = cfc113_mix_ratio, &
  hcfc22_mix_ratio  = hcfc22_mix_ratio, &
  hfc134a_mix_ratio = hfc134a_mix_ratio, &
  l_invert          = l_invert, &
  l_profile_last    = l_profile_last, &
  l_debug           = l_debug, &
  i_profile_debug   = i_profile_debug )

call set_bound(bound, control, dimen, spec, n_profile, &
  profile_list        = profile_list, &
  n_tile              = n_tile, &
  t_ground            = t_ground, &
  cos_zenith_angle    = cos_zenith_angle, &
  solar_irrad         = solar_irrad, &
  orog_corr           = orog_corr, &
  l_grey_albedo       = l_grey_albedo, &
  grey_albedo         = grey_albedo, &
  albedo_diff         = albedo_diff, &
  albedo_dir          = albedo_dir, &
  albedo_diff_1d      = albedo_diff_1d, &
  albedo_dir_1d       = albedo_dir_1d, &
  frac_tile           = frac_tile, &
  t_tile              = t_tile, &
  albedo_diff_tile    = albedo_diff_tile, &
  albedo_dir_tile     = albedo_dir_tile, &
  frac_tile_1d        = frac_tile_1d, &
  t_tile_1d           = t_tile_1d, &
  albedo_diff_tile_1d = albedo_diff_tile_1d, &
  albedo_dir_tile_1d  = albedo_dir_tile_1d, &
  l_profile_last      = l_profile_last, &
  l_debug             = l_debug, &
  i_profile_debug     = i_profile_debug )

call set_cld(cld, control, dimen, spec, atm, &
  profile_list          = profile_list, &
  n_layer_stride        = n_layer_stride, &
  cloud_frac            = cloud_frac, &
  conv_frac             = conv_frac, &
  liq_frac              = liq_frac, &
  ice_frac              = ice_frac, &
  liq_conv_frac         = liq_conv_frac, &
  ice_conv_frac         = ice_conv_frac, &
  liq_mmr               = liq_mmr, &
  ice_mmr               = ice_mmr, &
  liq_conv_mmr          = liq_conv_mmr, &
  ice_conv_mmr          = ice_conv_mmr, &
  liq_rsd               = liq_rsd, &
  ice_rsd               = ice_rsd, &
  liq_conv_rsd          = liq_conv_rsd, &
  ice_conv_rsd          = ice_conv_rsd, &
  cloud_frac_1d         = cloud_frac_1d, &
  conv_frac_1d          = conv_frac_1d, &
  liq_frac_1d           = liq_frac_1d, &
  ice_frac_1d           = ice_frac_1d, &
  liq_conv_frac_1d      = liq_conv_frac_1d, &
  ice_conv_frac_1d      = ice_conv_frac_1d, &
  liq_mmr_1d            = liq_mmr_1d, &
  ice_mmr_1d            = ice_mmr_1d, &
  liq_conv_mmr_1d       = liq_conv_mmr_1d, &
  ice_conv_mmr_1d       = ice_conv_mmr_1d, &
  liq_rsd_1d            = liq_rsd_1d, &
  ice_rsd_1d            = ice_rsd_1d, &
  liq_conv_rsd_1d       = liq_conv_rsd_1d, &
  ice_conv_rsd_1d       = ice_conv_rsd_1d, &
  cloud_vertical_decorr = cloud_vertical_decorr, &
  conv_vertical_decorr  = conv_vertical_decorr, &
  cloud_horizontal_rsd  = cloud_horizontal_rsd, &
  l_invert              = l_invert, &
  l_profile_last        = l_profile_last, &
  l_debug               = l_debug, &
  i_profile_debug       = i_profile_debug )

call set_cld_dim(cld, control, dimen, spec, atm, &
  profile_list    = profile_list, &
  n_layer_stride  = n_layer_stride, &
  liq_nc          = liq_nc, &
  ice_nc          = ice_nc, &
  liq_conv_nc     = liq_conv_nc, &
  ice_conv_nc     = ice_conv_nc, &
  liq_dim         = liq_dim, &
  ice_dim         = ice_dim, &
  liq_conv_dim    = liq_conv_dim, &
  ice_conv_dim    = ice_conv_dim, &
  liq_nc_1d       = liq_nc_1d, &
  ice_nc_1d       = ice_nc_1d, &
  liq_conv_nc_1d  = liq_conv_nc_1d, &
  ice_conv_nc_1d  = ice_conv_nc_1d, &
  liq_dim_1d      = liq_dim_1d, &
  ice_dim_1d      = ice_dim_1d, &
  liq_conv_dim_1d = liq_conv_dim_1d, &
  ice_conv_dim_1d = ice_conv_dim_1d, &
  l_invert        = l_invert, &
  l_profile_last  = l_profile_last, &
  l_debug         = l_debug, &
  i_profile_debug = i_profile_debug )

call set_cld_mcica(cld, mcica, control, dimen, spec, atm, &
  profile_list = profile_list, &
  rand_seed    = rand_seed )

call set_aer(aer, control, dimen, spec, &
  n_profile, n_layer, n_aer_mode, profile_list, n_layer_stride, n_aer_layer, &
  aer_mix_ratio, aer_absorption, aer_scattering, aer_asymmetry, &
  aer_mix_ratio_1d, aer_absorption_1d, aer_scattering_1d, aer_asymmetry_1d, &
  mean_rel_humidity, mean_rel_humidity_1d, &
  l_water_soluble, water_soluble, water_soluble_1d, &
  l_dust_like, dust_like, dust_like_1d, &
  l_oceanic, oceanic, oceanic_1d, &
  l_soot, soot, soot_1d, &
  l_ash, ash, ash_1d, &
  l_sulphuric, sulphuric, sulphuric_1d, &
  l_ammonium_sulphate, ammonium_sulphate, ammonium_sulphate_1d, &
  l_saharan_dust, saharan_dust, saharan_dust_1d, &
  l_accum_sulphate, accum_sulphate, accum_sulphate_1d, &
  l_aitken_sulphate, aitken_sulphate, aitken_sulphate_1d, &
  l_fresh_soot, fresh_soot, fresh_soot_1d, &
  l_aged_soot, aged_soot, aged_soot_1d, &
  l_sodium_chloride, sodium_chloride, sodium_chloride_1d, &
  l_seasalt_film, seasalt_film, seasalt_film_1d, &
  l_seasalt_jet, seasalt_jet, seasalt_jet_1d, &
  l_dust_div1, dust_div1, dust_div1_1d, &
  l_dust_div2, dust_div2, dust_div2_1d, &
  l_dust_div3, dust_div3, dust_div3_1d, &
  l_dust_div4, dust_div4, dust_div4_1d, &
  l_dust_div5, dust_div5, dust_div5_1d, &
  l_dust_div6, dust_div6, dust_div6_1d, &
  l_biomass_1, biomass_1, biomass_1_1d, &
  l_biomass_2, biomass_2, biomass_2_1d, &
  l_biogenic, biogenic, biogenic_1d, &
  l_ocff_fresh, ocff_fresh, ocff_fresh_1d, &
  l_ocff_aged, ocff_aged, ocff_aged_1d, &
  l_delta, delta, delta_1d, &
  l_murk, murk, murk_1d, &
  l_nitrate, nitrate, nitrate_1d, &
  l_twobindust_1, twobindust_1, twobindust_1_1d, &
  l_twobindust_2, twobindust_2, twobindust_2_1d, &
  l_invert, l_profile_last)

! DEPENDS ON: radiance_calc
call radiance_calc(control, dimen, spec, atm, cld, aer, bound, radout)

call set_diag(diag, &
  control, dimen, spec, atm, cld, mcica, aer, bound, radout, &
  n_profile, n_layer, &
  profile_list           = profile_list, &
  n_layer_stride         = n_layer_stride, &
  n_tile                 = n_tile, &
  layer_heat_capacity    = layer_heat_capacity, &
  layer_heat_capacity_1d = layer_heat_capacity_1d, &
  l_invert               = l_invert, &
  l_profile_last         = l_profile_last)

call deallocate_out(radout)
call deallocate_aer_prsc(aer)
call deallocate_aer(aer)
call deallocate_cld_mcica(cld)
call deallocate_cld_prsc(cld)
call deallocate_cld(cld)
call deallocate_bound(bound)
call deallocate_atm(atm)
call deallocate_control(control)

end subroutine runes
end module socrates_runes
