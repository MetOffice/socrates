! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! This module holds the interface to COSP2 which is called from LFRic
!
!----------------------------------------------------------------------

module cosp_mod

use mod_cosp_config, only: n_backscatter_bins => sr_bins, &
                           n_dbze_bins => cloudsat_dbze_bins, &
                           n_isccp_tau_bins => numisccptaubins, &
                           n_isccp_pressure_bins => numisccppresbins, &
                           vgrid_zl, vgrid_zu
use cosp_input_mod, only: n_cloudsat_levels
use mod_cosp_stats, only: cosp_change_vertical_grid
use mod_cosp_stats_extra, only: cosp_gridbox_mean, &
                           cosp_radar_and_lidar_cloud_fraction

implicit none
character(len=*), parameter, private :: ModuleName = 'cosp_mod'
contains

subroutine cosp( nlevels, &
        npoints, &
        ncolumns, &
        nclds, &
        ncldy, &
        p_full_levels, &
        p_half_levels, &
        hgt_full_levels, &
        hgt_half_levels, &
        d_mass, &
        t_n, &
        q_n, &
        w_cloud, &
        condensed_mix_ratio_water, &
        condensed_mix_ratio_ice, &
        cosp_lsrain, &
        cosp_crain, &
        cosp_csnow, &
        condensed_re_water, &
        condensed_re_ice, &
        cloud_extinction, &
        cloud_absorptivity, &
        frac_cloud_water, &
        frac_cloud_ice, &
        clw_sub_full, &
        t_surf, &
        p_surf, &
        hgt_surf, &
        cosp_sunlit, &
        x1r, &
        x1g, &
        x2r, &
        x2g, &
        x4g, &
        cosp_diag, &
        profile_list, &
        l_profile_last, &
        cosp_out_ext )

  use realtype_rd, only: RealExt
  use cosp_kinds, only: wp
  use cosp2_types_mod, only: &
      cosp2_config, cosp_inputs_host_model, &
      construct_cosp_inputs_host_model, &
      construct_cosp_optical_inputs, &
      construct_cosp_column_inputs, &
      construct_cosp_outputs, &
      destroy_cosp_optical_inputs, destroy_cosp_column_inputs, &
      destroy_cosp_outputs, destroy_cosp_inputs_host_model
  use cosp_input_mod, only: &
      cosp_radar_freq, cosp_k2, &
      cosp_use_gas_abs, cosp_do_ray, &
      cosp_isccp_topheight, &
      cosp_isccp_topheight_direction, &
      cosp_surface_radar, &
      cosp_nchannels, cosp_nlr, &
      cloudsat_micro_scheme, &
      cosp_emsfc_lw, &
      cosp_use_vgrid, cosp_csat_vgrid
  use cosp2_constants_mod, only: &
      cosp_qb_dist, cosp_hydroclass_init, &
      cosp_microphys_init, &
      i_lscliq, i_lscice, i_lsrain, &
      i_cvrain, i_cvsnow
  use cosp_radiation_mod, only: &
      cosp_radiative_properties
  use mod_cosp, only: cosp_init, &
      cosp_column_inputs, cosp_simulator, &
      cosp_optical_inputs, cosp_outputs, &
      cosp_cleanup
  use mod_cosp_config, only: r_undef
  use mod_quickbeam_optics, only: &
      quickbeam_optics_init
  use cosp_def_diag, only: cospdiag
  use cosp2_diagnostics_mod, only: create_mask
  use quickbeam, only: radar_cfg
  use cosp_subgrid_mod, only: cosp_subgrid_homogeneous_precip
  use cosp_precip_mod, only: cosp_gridbox_precip
  
  use ereport_mod, only: ereport
  use errormessagelength_mod, only: errormessagelength
  use rad_pcf, only: i_normal, i_err_fatal

  implicit none

  ! Input arguments
  integer, intent(in) :: nlevels
  integer, intent(in) :: npoints
  integer, intent(in) :: ncolumns
  integer, intent(in) :: nclds
  integer, intent(in), pointer :: ncldy(:)
  real(RealExt), intent(in), pointer :: p_full_levels(:,:)
  real(RealExt), intent(in), pointer :: p_half_levels(:,:)
  real(RealExt), intent(in), pointer :: hgt_full_levels(:,:)
  real(RealExt), intent(in), pointer :: hgt_half_levels(:,:)
  real(RealExt), intent(in), pointer :: d_mass(:,:)
  real(RealExt), intent(in), pointer :: T_n(:,:)
  real(RealExt), intent(in), pointer :: q_n(:,:)
  real(RealExt), intent(in), pointer :: w_cloud(:,:)
  real(RealExt), intent(in), pointer :: condensed_mix_ratio_water(:,:)
  real(RealExt), intent(in), pointer :: condensed_mix_ratio_ice(:,:)
  real(RealExt), intent(in), pointer :: cosp_lsrain(:,:)
  real(RealExt), intent(in), pointer :: cosp_crain(:,:)
  real(RealExt), intent(in), pointer :: cosp_csnow(:,:)
  real(RealExt), intent(in), pointer :: condensed_re_water(:,:)
  real(RealExt), intent(in), pointer :: condensed_re_ice(:,:)
  real(RealExt), intent(in), pointer :: cloud_extinction(:,:)
  real(RealExt), intent(in), pointer :: cloud_absorptivity(:,:)
  real(RealExt), intent(in), pointer :: frac_cloud_water(:,:)
  real(RealExt), intent(in), pointer :: frac_cloud_ice(:,:)
  real(RealExt), intent(in), pointer :: clw_sub_full(:,:,:)
  real(RealExt), intent(in), pointer :: t_surf(:)
  real(RealExt), intent(in), pointer :: p_surf(:)
  real(RealExt), intent(in), pointer :: hgt_surf(:)
  real(RealExt), intent(in), pointer :: cosp_sunlit(:)

  ! Microphysics
  real(RealExt), intent(in) :: x1r
  real(RealExt), intent(in) :: x1g
  real(RealExt), intent(in) :: x2r
  real(RealExt), intent(in) :: x2g
  real(RealExt), intent(in) :: x4g

  ! Output arguments
  type(cospdiag), intent(inout) :: cosp_diag

  ! Optional input arguments
  integer, intent(in), optional :: profile_list(:)
  logical, intent(in), optional :: l_profile_last

  ! Optional output arguments
  type(cosp_outputs), intent(out), target, optional :: cosp_out_ext

  ! Local variables
  integer :: list(npoints)
  integer :: i
  integer :: ii
  integer :: j
  integer :: l
  integer :: lll

  logical :: l_last

  real(wp) :: x1r_wp
  real(wp) :: x1g_wp
  real(wp) :: x2r_wp
  real(wp) :: x2g_wp
  real(wp) :: x4g_wp

  real(wp) :: cosp_temp
  real(wp) :: cosp_temp1
  real(wp) :: cosp_temp2
  
  real(wp) :: calipso_beta_mol_40(npoints, cosp_nlr)
  real(wp) :: calipso_beta_tot_40(npoints, ncolumns, cosp_nlr)
  real(wp) :: cloudsat_ze_tot_40(npoints, ncolumns, cosp_nlr)
  real(wp) :: calipso_gbxmean_atb_mdl(npoints, nlevels)
  real(wp) :: calipso_gbxmean_atb_40(npoints, cosp_nlr)
  real(wp) :: cloudsat_gbxmean_ze_mdl(npoints, nlevels)
  real(wp) :: cloudsat_gbxmean_ze_40(npoints, cosp_nlr)
  real(wp) :: calipso_cloudsat_mdl_cl(npoints, nlevels)
  real(wp) :: calipso_cloudsat_40_cl(npoints, cosp_nlr)

  ! COSP configuration options
  type(radar_cfg) :: rcfg_cloudsat
  type(cosp2_config) :: cosp_cfg
  ! COSP inputs of host model
  type(cosp_inputs_host_model) :: cosp_hmodel
  ! COSP optical inputs
  type(cosp_optical_inputs) :: cosp_optical_in
  ! COSP column inputs
  type(cosp_column_inputs) :: cosp_column_in

  type(cosp_outputs), target :: cosp_out_int
  type(cosp_outputs), pointer :: cosp_out

  character(len=256) :: cosp_status(100)

  integer :: ierr = i_normal
  character (len=errormessagelength) :: cmessage
  character (len=*), parameter :: RoutineName = 'COSP'


  if (present(cosp_out_ext)) then
    cosp_out => cosp_out_ext
  else
    cosp_out => cosp_out_int
  end if

  ! Logicals for outputs
  ! ISCCP
  cosp_cfg%lalbisccp       = &
    associated(cosp_diag%cosp_weighted_cloud_albedo) ! 2331
  cosp_cfg%ltauisccp       = .false.
  cosp_cfg%lpctisccp       = &
    associated(cosp_diag%cosp_weighted_ctp) ! 2333
  cosp_cfg%lcltisccp       = &
    associated(cosp_diag%cosp_tot_cloud_area) ! 2334
  cosp_cfg%lmeantbisccp    = .false.
  cosp_cfg%lmeantbclrisccp = .false.
  cosp_cfg%lclisccp        =  &
    associated(cosp_diag%cosp_ctp_tau_histogram) ! 2337
  cosp_cfg%lboxptopisccp   = .false.
  cosp_cfg%lboxtauisccp    = .false.
  ! CALIPSO
  cosp_cfg%llidarbetamol532 = &
    associated(cosp_diag%cosp_calipso_mol_atb_mdl) .or. & ! 2340
    associated(cosp_diag%cosp_calipso_mol_atb_40) ! 2357
  cosp_cfg%latb532          =  &
    associated(cosp_diag%cosp_calipso_tot_backscatter) ! 2341
  cosp_cfg%latb532gbx       = &
    associated(cosp_diag%cosp_calipso_gbxmean_atb_40) ! 2356
  cosp_cfg%lcfadlidarsr532  =  &
    associated(cosp_diag%cosp_calipso_cfad_sr_40) ! 2370
  cosp_cfg%lclcalipso       = &
    associated(cosp_diag%cosp_calipso_cloud_area_40) ! 2371
  cosp_cfg%lcllcalipso      =  &
    associated(cosp_diag%cosp_calipso_low_level_cl) .or. & ! 2344
    associated(cosp_diag%cosp_calipso_low_level_cl_mask)   ! 2321
  cosp_cfg%lclmcalipso      =  &
    associated(cosp_diag%cosp_calipso_mid_level_cl) .or. & ! 2345
    associated(cosp_diag%cosp_calipso_mid_level_cl_mask)   ! 2322
  cosp_cfg%lclhcalipso      =  &
    associated(cosp_diag%cosp_calipso_high_level_cl) .or. & ! 2346
    associated(cosp_diag%cosp_calipso_high_level_cl_mask)   ! 2323
  cosp_cfg%lcltcalipso      = .false.
  cosp_cfg%lparasolrefl     = .false.
  cosp_cfg%lclcalipsoliq    =  &
    associated(cosp_diag%cosp_calipso_cf_40_liq) .or. & ! 2473
    associated(cosp_diag%cosp_calipso_cf_40_mask)       ! 2325
  cosp_cfg%lclcalipsoice    =  &
    associated(cosp_diag%cosp_calipso_cf_40_ice) ! 2474
  cosp_cfg%lclcalipsoun     =  &
    associated(cosp_diag%cosp_calipso_cf_40_undet) ! 2475
  cosp_cfg%lcllcalipsoliq   = .false.
  cosp_cfg%lclmcalipsoliq   = .false.
  cosp_cfg%lclhcalipsoliq   = .false.
  cosp_cfg%lcltcalipsoliq   = .false.
  cosp_cfg%lcllcalipsoice   = .false.
  cosp_cfg%lclmcalipsoice   = .false.
  cosp_cfg%lclhcalipsoice   = .false.
  cosp_cfg%lcltcalipsoice   = .false.
  cosp_cfg%lcllcalipsoun    = .false.
  cosp_cfg%lclmcalipsoun    = .false.
  cosp_cfg%lclhcalipsoun    = .false.
  cosp_cfg%lcltcalipsoun    = .false.
  cosp_cfg%lclcalipsotmp    = .false.
  cosp_cfg%lclcalipsotmpliq = .false.
  cosp_cfg%lclcalipsotmpice = .false.
  cosp_cfg%lclcalipsotmpun  = .false.
  cosp_cfg%lclopaquecalipso = .false.
  cosp_cfg%lclthincalipso   = .false.
  cosp_cfg%lclzopaquecalipso = .false.
  cosp_cfg%lclcalipsoopaque = .false.
  cosp_cfg%lclcalipsothin   = .false.
  cosp_cfg%lclcalipsozopaque = .false.
  cosp_cfg%lclcalipsoopacity = .false.
  cosp_cfg%lclopaquetemp    = .false.
  cosp_cfg%lclthintemp      = .false.
  cosp_cfg%lclzopaquetemp   = .false.
  cosp_cfg%lclopaquemeanz   = .false.
  cosp_cfg%lclthinmeanz     = .false.
  cosp_cfg%lclthinemis      = .false.
  cosp_cfg%lclopaquemeanzse = .false.
  cosp_cfg%lclthinmeanzse   = .false.
  cosp_cfg%lclzopaquecalipsose = .false.
  ! Ground lidar
  cosp_cfg%llidarbetamol532gr = .false.
  cosp_cfg%lcfadlidarsr532gr = .false.
  cosp_cfg%latb532gr = .false.
  cosp_cfg%lclgrlidar532 = .false.
  cosp_cfg%lclhgrlidar532 = .false.
  cosp_cfg%lcllgrlidar532 = .false.
  cosp_cfg%lclmgrlidar532 = .false.
  cosp_cfg%lcltgrlidar532 = .false.
  cosp_cfg%llidarbetamol355 = .false.
  cosp_cfg%lcfadlidarsr355 = .false.
  ! ATLID
  cosp_cfg%latb355 = .false.
  cosp_cfg%lclatlid = .false.
  cosp_cfg%lclhatlid = .false.
  cosp_cfg%lcllatlid = .false.
  cosp_cfg%lclmatlid = .false.
  cosp_cfg%lcltatlid = .false.
  ! CloudSat
  cosp_cfg%lcfaddbze94  = &
    associated(cosp_diag%cosp_cloudsat_cfad_ze_40) ! 2372
  cosp_cfg%ldbze94      = &
    associated(cosp_diag%cosp_cloudsat_gbxmean_ze_40) ! 2354
  cosp_cfg%ldbze94gbx   = &
    associated(cosp_diag%cosp_cloudsat_gbxmean_ze_40) ! 2354
  cosp_cfg%lcloudsat_tcc = .false.
  cosp_cfg%lcloudsat_tcc2 = .false.
  ! CloudSat and CALIPSO
  cosp_cfg%lclcalipso2    = .false.
  cosp_cfg%lcltlidarradar = .false.
  cosp_cfg%lcllidarradar  = &
   (associated(cosp_diag%cosp_calipso_cloudsat_mdl_cl) .or. &       ! 2358
    associated(cosp_diag%cosp_calipso_cloudsat_mdl_cl_mask)) .or. & ! 2326
   (associated(cosp_diag%cosp_calipso_cloudsat_40_cl) .or. &        ! 2359
    associated(cosp_diag%cosp_calipso_cloudsat_40_cl_mask))         ! 2327
  ! RTTOV
  cosp_cfg%ltbrttov = .false.
  ! MISR
  cosp_cfg%lclmisr = .false.
  ! MODIS
  cosp_cfg%lclmodis      = .false.
  cosp_cfg%lcltmodis     = .false.
  cosp_cfg%lclwmodis     = .false.
  cosp_cfg%lclimodis     = .false.
  cosp_cfg%lclhmodis     = .false.
  cosp_cfg%lclmmodis     = .false.
  cosp_cfg%lcllmodis     = .false.
  cosp_cfg%ltautmodis    = .false.
  cosp_cfg%ltauwmodis    = .false.
  cosp_cfg%ltauimodis    = .false.
  cosp_cfg%ltautlogmodis = .false.
  cosp_cfg%ltauwlogmodis = .false.
  cosp_cfg%ltauilogmodis = .false.
  cosp_cfg%lreffclwmodis = .false.
  cosp_cfg%lreffclimodis = .false.
  cosp_cfg%lpctmodis     = .false.
  cosp_cfg%llwpmodis     = .false.
  cosp_cfg%liwpmodis     = .false.
  ! CloudSat and MODIS
  cosp_cfg%lptradarflag0 = .false.
  cosp_cfg%lptradarflag1 = .false.
  cosp_cfg%lptradarflag2 = .false.
  cosp_cfg%lptradarflag3 = .false.
  cosp_cfg%lptradarflag4 = .false.
  cosp_cfg%lptradarflag5 = .false.
  cosp_cfg%lptradarflag6 = .false.
  cosp_cfg%lptradarflag7 = .false.
  cosp_cfg%lptradarflag8 = .false.
  cosp_cfg%lptradarflag9 = .false.
  cosp_cfg%lradarpia     = .false.
  cosp_cfg%lwr_occfreq   = .false.
  cosp_cfg%lcfodd        = .false.
  ! Other
  cosp_cfg%lfracout = .false.

  ! Instrument flags
  cosp_cfg%lisccp =               &
         cosp_cfg%lalbisccp       &
    .or. cosp_cfg%ltauisccp       &
    .or. cosp_cfg%lpctisccp       &
    .or. cosp_cfg%lcltisccp       &
    .or. cosp_cfg%lmeantbisccp    &
    .or. cosp_cfg%lmeantbclrisccp &
    .or. cosp_cfg%lclisccp        &
    .or. cosp_cfg%lboxptopisccp   &
    .or. cosp_cfg%lboxtauisccp
  cosp_cfg%lcalipso  =                &
         cosp_cfg%llidarbetamol532    &
    .or. cosp_cfg%latb532             &
    .or. cosp_cfg%latb532gbx          &
    .or. cosp_cfg%lcfadlidarsr532     &
    .or. cosp_cfg%lclcalipso          &
    .or. cosp_cfg%lcllcalipso         &
    .or. cosp_cfg%lclmcalipso         &
    .or. cosp_cfg%lclhcalipso         &
    .or. cosp_cfg%lcltcalipso         &
    .or. cosp_cfg%lparasolrefl        &
    .or. cosp_cfg%lclcalipsoliq       &
    .or. cosp_cfg%lclcalipsoice       &
    .or. cosp_cfg%lclcalipsoun        &
    .or. cosp_cfg%lcllcalipsoliq      &
    .or. cosp_cfg%lclmcalipsoliq      &
    .or. cosp_cfg%lclhcalipsoliq      &
    .or. cosp_cfg%lcltcalipsoliq      &
    .or. cosp_cfg%lcllcalipsoice      &
    .or. cosp_cfg%lclmcalipsoice      &
    .or. cosp_cfg%lclhcalipsoice      &
    .or. cosp_cfg%lcltcalipsoice      &
    .or. cosp_cfg%lcllcalipsoun       &
    .or. cosp_cfg%lclmcalipsoun       &
    .or. cosp_cfg%lclhcalipsoun       &
    .or. cosp_cfg%lcltcalipsoun       &
    .or. cosp_cfg%lclcalipsotmp       &
    .or. cosp_cfg%lclcalipsotmpliq    &
    .or. cosp_cfg%lclcalipsotmpice    &
    .or. cosp_cfg%lclcalipsotmpun     &
    .or. cosp_cfg%lclopaquecalipso    &
    .or. cosp_cfg%lclthincalipso      &
    .or. cosp_cfg%lclzopaquecalipso   &
    .or. cosp_cfg%lclcalipsoopaque    &
    .or. cosp_cfg%lclcalipsothin      &
    .or. cosp_cfg%lclcalipsozopaque   &
    .or. cosp_cfg%lclcalipsoopacity   &
    .or. cosp_cfg%lclopaquetemp       &
    .or. cosp_cfg%lclthintemp         &
    .or. cosp_cfg%lclzopaquetemp      &
    .or. cosp_cfg%lclopaquemeanz      &
    .or. cosp_cfg%lclthinmeanz        &
    .or. cosp_cfg%lclthinemis         &
    .or. cosp_cfg%lclopaquemeanzse    &
    .or. cosp_cfg%lclthinmeanzse      &
    .or. cosp_cfg%lclzopaquecalipsose &
    .or. cosp_cfg%lclcalipso2         &
    .or. cosp_cfg%lcltlidarradar      &
    .or. cosp_cfg%lcllidarradar
  cosp_cfg%lcloudsat =           &
         cosp_cfg%lcfaddbze94    &
    .or. cosp_cfg%ldbze94        &
    .or. cosp_cfg%ldbze94gbx     &
    .or. cosp_cfg%lcloudsat_tcc  &
    .or. cosp_cfg%lcloudsat_tcc2 &
    .or. cosp_cfg%lclcalipso2    &
    .or. cosp_cfg%lcltlidarradar &
    .or. cosp_cfg%lcllidarradar  &
    .or. cosp_cfg%lptradarflag0  &
    .or. cosp_cfg%lptradarflag1  &
    .or. cosp_cfg%lptradarflag2  &
    .or. cosp_cfg%lptradarflag3  &
    .or. cosp_cfg%lptradarflag4  &
    .or. cosp_cfg%lptradarflag5  &
    .or. cosp_cfg%lptradarflag6  &
    .or. cosp_cfg%lptradarflag7  &
    .or. cosp_cfg%lptradarflag8  &
    .or. cosp_cfg%lptradarflag9  &
    .or. cosp_cfg%lradarpia      &
    .or. cosp_cfg%lwr_occfreq    &
    .or. cosp_cfg%lcfodd
  cosp_cfg%lmisr = cosp_cfg%lclmisr
  cosp_cfg%lmodis =             &
         cosp_cfg%lclmodis      &
    .or. cosp_cfg%lcltmodis     &
    .or. cosp_cfg%lclwmodis     &
    .or. cosp_cfg%lclimodis     &
    .or. cosp_cfg%lclhmodis     &
    .or. cosp_cfg%lclmmodis     &
    .or. cosp_cfg%lcllmodis     &
    .or. cosp_cfg%ltautmodis    &
    .or. cosp_cfg%ltauwmodis    &
    .or. cosp_cfg%ltauimodis    &
    .or. cosp_cfg%ltautlogmodis &
    .or. cosp_cfg%ltauwlogmodis &
    .or. cosp_cfg%ltauilogmodis &
    .or. cosp_cfg%lreffclwmodis &
    .or. cosp_cfg%lreffclimodis &
    .or. cosp_cfg%lpctmodis     &
    .or. cosp_cfg%llwpmodis     &
    .or. cosp_cfg%liwpmodis     &
    .or. cosp_cfg%lptradarflag0 &
    .or. cosp_cfg%lptradarflag1 &
    .or. cosp_cfg%lptradarflag2 &
    .or. cosp_cfg%lptradarflag3 &
    .or. cosp_cfg%lptradarflag4 &
    .or. cosp_cfg%lptradarflag5 &
    .or. cosp_cfg%lptradarflag6 &
    .or. cosp_cfg%lptradarflag7 &
    .or. cosp_cfg%lptradarflag8 &
    .or. cosp_cfg%lptradarflag9 &
    .or. cosp_cfg%lradarpia     &
    .or. cosp_cfg%lwr_occfreq   &
    .or. cosp_cfg%lcfodd
  cosp_cfg%lrttov = cosp_cfg%ltbrttov
  cosp_cfg%lgrlidar532 =             &
         cosp_cfg%llidarbetamol532gr &
    .or. cosp_cfg%lcfadlidarsr532gr  &
    .or. cosp_cfg%latb532gr          &
    .or. cosp_cfg%lclgrlidar532      &
    .or. cosp_cfg%lclhgrlidar532     &
    .or. cosp_cfg%lcllgrlidar532     &
    .or. cosp_cfg%lclmgrlidar532     &
    .or. cosp_cfg%lcltgrlidar532     &
    .or. cosp_cfg%llidarbetamol355   &
    .or. cosp_cfg%lcfadlidarsr355
  cosp_cfg%latlid =         &
         cosp_cfg%latb355   &
    .or. cosp_cfg%lclatlid  &
    .or. cosp_cfg%lclhatlid &
    .or. cosp_cfg%lcllatlid &
    .or. cosp_cfg%lclmatlid &
    .or. cosp_cfg%lcltatlid
  cosp_cfg%lparasol = cosp_cfg%lcalipso

  cosp_use_vgrid = cosp_cfg%lcloudsat .or. cosp_cfg%lcalipso


  ! Initialize PSDs
  x1r_wp = x1r
  x1g_wp = x1g
  x2r_wp = x2r
  x2g_wp = x2g
  x4g_wp = x4g
  call cosp_microphys_init(x1r_wp,x1g_wp,x2r_wp,x2g_wp,x4g_wp)

  ! Initialize Quickbeam
  call quickbeam_optics_init()

  ! Distributional parameters for hydrometeors in radar simulator
  call cosp_hydroclass_init(cosp_qb_dist)

  ! Initialize COSP simulator
  call cosp_init(cosp_cfg%lisccp, cosp_cfg%lmodis, cosp_cfg%lmisr, &
    cosp_cfg%lcloudsat, cosp_cfg%lcalipso, cosp_cfg%lgrlidar532, &
    cosp_cfg%latlid, cosp_cfg%lparasol, cosp_cfg%lrttov, &
    cosp_radar_freq, cosp_k2, cosp_use_gas_abs, cosp_do_ray, &
    cosp_isccp_topheight, cosp_isccp_topheight_direction, &
    cosp_surface_radar, rcfg_cloudsat, cosp_use_vgrid, &
    cosp_csat_vgrid, cosp_nlr, nlevels, cloudsat_micro_scheme)

  ! Allocate memory for COSP types
  call construct_cosp_inputs_host_model(npoints, ncolumns, &
                        nlevels, cosp_hmodel)
  call construct_cosp_optical_inputs(cosp_cfg, npoints, ncolumns, &
                        nlevels, cosp_optical_in)
  call construct_cosp_column_inputs(npoints, ncolumns, &
                        nlevels, cosp_nchannels, cosp_column_in)
  call construct_cosp_outputs(cosp_cfg, npoints, ncolumns, &
                        nlevels, cosp_nlr, cosp_nchannels, cosp_out)

  ! Populate COSP types
  cosp_optical_in%emsfc_lw = cosp_emsfc_lw
  cosp_optical_in%rcfg_cloudsat = rcfg_cloudsat

  ! Profile list
  if (present(profile_list)) then
    list = profile_list(1:npoints)
  else
    do l=1, npoints
      list(l) = l
    end do
  end if

  ! Position of profile index in input arrays
  if (present(l_profile_last)) then
    l_last = l_profile_last
  else
    l_last = .false.
  end if

  if (l_last) then
    do i=1, nlevels
      ii = nlevels-i+1
        do l=1, npoints
          cosp_column_in%qv(l,i) = q_n(ii,list(l))
          cosp_column_in%pfull(l,i) = p_full_levels(ii,list(l))
          cosp_column_in%phalf(l,i+1) = p_half_levels(ii,list(l))
          cosp_column_in%hgt_matrix(l,i) = hgt_full_levels(ii,list(l))
          cosp_column_in%hgt_matrix_half(l,i+1) = hgt_half_levels(ii,list(l))
          cosp_column_in%at(l,i) = t_n(ii,list(l))
          cosp_hmodel%mr_gbx(l,i,i_lsrain) = cosp_lsrain(ii,list(l))
          cosp_hmodel%mr_gbx(l,i,i_cvrain) = cosp_crain(ii,list(l))
          cosp_hmodel%mr_gbx(l,i,i_cvsnow) = cosp_csnow(ii,list(l))
        end do ! l
    end do ! ii
  else
    do i=1, nlevels
      ii = nlevels-i+1
        do l=1, npoints
          cosp_column_in%qv(l,i) = q_n(list(l),ii)
          cosp_column_in%pfull(l,i) = p_full_levels(list(l),ii)
          cosp_column_in%phalf(l,i+1) = p_half_levels(list(l),ii)
          cosp_column_in%hgt_matrix(l,i) = hgt_full_levels(list(l),ii)
          cosp_column_in%hgt_matrix_half(l,i+1) = hgt_half_levels(list(l),ii)
          cosp_column_in%at(l,i) = t_n(list(l),ii)
          cosp_hmodel%mr_gbx(l,i,i_lsrain) = cosp_lsrain(list(l),ii)
          cosp_hmodel%mr_gbx(l,i,i_cvrain) = cosp_crain(list(l),ii)
          cosp_hmodel%mr_gbx(l,i,i_cvsnow) = cosp_csnow(list(l),ii)
        end do ! l
    end do ! ii
  end if ! l_last

  do l=1, npoints
    cosp_column_in%phalf(l,1) = 0.0_wp
    cosp_column_in%phalf(l,nlevels+1) = p_surf(list(l))
    cosp_column_in%surfelev(l) = hgt_surf(list(l))
    cosp_column_in%hgt_matrix_half(l,nlevels+1) = cosp_column_in%surfelev(l)
    cosp_column_in%hgt_matrix_half(l,1) = cosp_column_in%hgt_matrix(l,1) + &
      cosp_column_in%hgt_matrix(l,1) - cosp_column_in%hgt_matrix_half(l,2)
    cosp_column_in%skt(l) = t_surf(list(l))
    cosp_column_in%land(l) = 0.0_wp
  end do ! l

  ! Populate COSP types from SOCRATES radiation LW
  cosp_optical_in%ncolumns = ncolumns

  if (l_last) then
    do ii=1, nclds
      i = nlevels-ii+1
      do l=1, npoints
        cosp_temp1 = &
          condensed_mix_ratio_water(ii,list(l))*frac_cloud_water(ii,list(l))
        cosp_temp2 = &
          condensed_mix_ratio_ice(ii,list(l))*frac_cloud_ice(ii,list(l))
        do lll=1, ncldy(list(l))
          ! Cloud water SUBGRID mixing ratio (LIQUID)
          cosp_hmodel%mr_hydro(l,lll,i,i_lscliq) = &
            clw_sub_full(ii,lll,list(l))*cosp_temp1
          ! cloud water subgrid mixing ratio (ice)
          cosp_hmodel%mr_hydro(l,lll,i,i_lscice) = &
            clw_sub_full(ii,lll,list(l))*cosp_temp2
          ! cloud water effective radius (liquid)
          cosp_hmodel%reff_hydro(l,lll,i,i_lscliq) = &
            condensed_re_water(ii,list(l))
          ! cloud water effective dimension (ice)
          cosp_hmodel%reff_hydro(l,lll,i,i_lscice) = &
            condensed_re_ice(ii,list(l))
        end do ! lll
        ! total cloud gridbox fraction seen by radiation
        cosp_hmodel%tca_gbx(l,i) = w_cloud(ii,list(l))
        ! gridbox effective radii
        ! cloud water effective radius (liquid)
        cosp_hmodel%reff_gbx(l,i,i_lscliq) = &
          condensed_re_water(ii,list(l))
        ! cloud water effective dimension (ice)
        cosp_hmodel%reff_gbx(l,i,i_lscice) = &
          condensed_re_ice(ii,list(l))
      end do ! l
    end do ! ii
  else
    do ii=1, nclds
      i = nlevels-ii+1
      do l=1, npoints
        cosp_temp1 = &
          condensed_mix_ratio_water(list(l),ii)*frac_cloud_water(list(l),ii)
        cosp_temp2 = &
          condensed_mix_ratio_ice(list(l),ii)*frac_cloud_ice(list(l),ii)
        do lll=1, ncldy(list(l))
          ! Cloud water SUBGRID mixing ratio (LIQUID)
          cosp_hmodel%mr_hydro(l,lll,i,i_lscliq) = &
            clw_sub_full(list(l),ii,lll)*cosp_temp1
          ! cloud water subgrid mixing ratio (ice)
          cosp_hmodel%mr_hydro(l,lll,i,i_lscice) = &
            clw_sub_full(list(l),ii,lll)*cosp_temp2
          ! cloud water effective radius (liquid)
          cosp_hmodel%reff_hydro(l,lll,i,i_lscliq) = &
            condensed_re_water(list(l),ii)
          ! cloud water effective dimension (ice)
          cosp_hmodel%reff_hydro(l,lll,i,i_lscice) = &
            condensed_re_ice(list(l),ii)
        end do ! l
        ! total cloud gridbox fraction seen by radiation
        cosp_hmodel%tca_gbx(l,i) = w_cloud(list(l),ii)
        ! gridbox effective radii
        ! cloud water effective radius (liquid)
        cosp_hmodel%reff_gbx(l,i,i_lscliq) = &
          condensed_re_water(list(l),ii)
        ! cloud water effective dimension (ice)
        cosp_hmodel%reff_gbx(l,i,i_lscice) = &
          condensed_re_ice(list(l),ii)
      end do ! l
    end do ! ii
  end if ! l_last

  ! Populate COSP types from SOCRATES radiation SW and LW
  if (cosp_cfg%lmodis .or. cosp_cfg%lmisr .or. cosp_cfg%lisccp) then
    if (l_last) then
      do ii=1, nclds
        i = nlevels-ii+1
        do l= 1, npoints
          ! sub-grid cloud optical depth and emissivity for cosp assumes
          ! same generated sub-columns for liquid and ice.
          cosp_temp = cloud_extinction(ii,list(l)) * d_mass(ii,list(l))
          cosp_temp1 = cloud_absorptivity(ii,list(l)) * d_mass(ii,list(l))
          do lll=1, ncldy(list(l))
            cosp_optical_in%tau_067(l,lll,i) = &
              clw_sub_full(ii,lll,list(l))*cosp_temp
            cosp_optical_in%emiss_11(l,lll,i) = &
              1.0_wp-exp(-1.666_wp*cosp_temp1*clw_sub_full(ii,lll,list(l)))
          end do ! lll
        end do ! l
      end do ! ii
    else
      do ii=1, nclds
        i = nlevels-ii+1
        do l= 1, npoints
          ! sub-grid cloud optical depth and emissivity for cosp assumes
          ! same generated sub-columns for liquid and ice.
          cosp_temp = cloud_extinction(list(l),ii) * d_mass(list(l),ii)
          cosp_temp1 = cloud_absorptivity(list(l),ii) * d_mass(list(l),ii)
          do lll=1, ncldy(list(l))
            cosp_optical_in%tau_067(l,lll,i) = &
              clw_sub_full(list(l),ii,lll)*cosp_temp
            cosp_optical_in%emiss_11(l,lll,i) = &
              1.0_wp-exp(-1.666_wp*cosp_temp1*clw_sub_full(list(l),ii,lll))
          end do ! lll
        end do ! l
      end do ! ii
    end if ! l_last
  end if

  do l=1, npoints
    cosp_column_in%sunlit(l) = nint(cosp_sunlit(list(l)))
  end do

  ! Populate gridbox-mean effective radii and mixing ratios.
  call cosp_gridbox_precip(cosp_column_in, cosp_hmodel)

  ! Fill in subgrid precipitation variables imposing horizontal homgeneity
  call cosp_subgrid_homogeneous_precip(cosp_optical_in, cosp_hmodel)

  ! Calculation of radiative properties for each instrument
  call cosp_radiative_properties(cosp_cfg,cosp_hmodel,cosp_column_in, &
    cosp_optical_in, cosp_qb_dist)

  ! Call to COSP
  cosp_status = cosp_simulator(cosp_optical_in, cosp_column_in, cosp_out, &
                               1, npoints, .false.)

  do i=1,size(cosp_status,1)
    if (cosp_status(i) /= '') then
      write(cmessage,'(i3,1x,a)') i, trim(cosp_status(i))
      ierr=i_err_fatal
      call ereport(ModuleName//':'//RoutineName, ierr, cmessage)
    end if
  end do


  ! Generating the COSP diagnostics

  ! COSP: MASK FOR CALIPSO LOW-LEVEL CF (was 2321)
  if (associated(cosp_diag%cosp_calipso_low_level_cl_mask)) then

    do l=1, npoints
      call create_mask(r_undef, cosp_out%calipso_cldlayer(l,1), &
      cosp_diag%cosp_calipso_low_level_cl_mask(list(l)))
    end do

  end if ! 2321

  ! COSP: CALIPSO LOW-LEVEL CLOUD (was 2344)
  if (associated(cosp_diag%cosp_calipso_low_level_cl)) then

    do l=1, npoints
      cosp_diag%cosp_calipso_low_level_cl(list(l)) &
      = 0.01_RealExt * cosp_out%calipso_cldlayer(l,1)
    end do

  end if ! 2344

  ! COSP: MASK FOR CALIPSO MID-LEVEL CF (was 2322)
  if (associated(cosp_diag%cosp_calipso_mid_level_cl_mask)) then

    do l=1, npoints
      call create_mask(r_undef, cosp_out%calipso_cldlayer(l,2), &
      cosp_diag%cosp_calipso_mid_level_cl_mask(list(l)))
    end do

  end if ! 2322

  ! COSP: CALIPSO MID-LEVEL CLOUD (was 2345)
  if (associated(cosp_diag%cosp_calipso_mid_level_cl)) then

    do l=1, npoints
      cosp_diag%cosp_calipso_mid_level_cl(list(l)) &
      = 0.01_RealExt * cosp_out%calipso_cldlayer(l,2)
    end do

  end if ! 232

  ! COSP: MASK FOR CALIPSO HIGH-LEVEL CF (was 2323)
  if (associated(cosp_diag%cosp_calipso_high_level_cl_mask)) then

    do l=1, npoints
      call create_mask(r_undef, cosp_out%calipso_cldlayer(l,3), &
      cosp_diag%cosp_calipso_high_level_cl_mask(list(l)))
    end do

  end if ! 2323

  ! COSP: CALIPSO HIGH-LEVEL CLOUD (was 2346)
  if (associated(cosp_diag%cosp_calipso_high_level_cl)) then

    do l=1, npoints
      cosp_diag%cosp_calipso_high_level_cl(list(l)) &
      = 0.01_RealExt * cosp_out%calipso_cldlayer(l,3)
    end do

  end if ! 2346

  ! COSP: ISCCP CTP-TAU HISTOGRAM (was 2330)
  if (associated(cosp_diag%cosp_cloud_weights)) then

    do l=1, npoints
      cosp_diag%cosp_cloud_weights(list(l)) &
        = real(cosp_column_in%sunlit(l), RealExt)
    end do

  end if ! 2330

  ! COSP: ISCCP CTP-TAU HISTOGRAM (was 2337)
  if (associated(cosp_diag%cosp_ctp_tau_histogram)) then

    if (l_last) then
      do i=1, n_isccp_pressure_bins
        do j=1, n_isccp_tau_bins
          do l=1, npoints
            if (cosp_out%isccp_fq(l,j,i) == r_undef) then
               cosp_diag%cosp_ctp_tau_histogram(j,i,list(l)) &
               = 0.0_RealExt
            else
               cosp_diag%cosp_ctp_tau_histogram(j,i,list(l)) &
               = 0.01_RealExt * cosp_out%isccp_fq(l,j,i)
            end if
          end do
        end do
      end do
    else
      do i=1, n_isccp_pressure_bins
        do j=1, n_isccp_tau_bins
          do l=1, npoints
            if (cosp_out%isccp_fq(l,j,i) == r_undef) then
               cosp_diag%cosp_ctp_tau_histogram(list(l),j,i) &
               = 0.0_RealExt
            else
               cosp_diag%cosp_ctp_tau_histogram(list(l),j,i) &
               = 0.01_RealExt * cosp_out%isccp_fq(l,j,i)
            end if
          end do
        end do
      end do
    end if

  end if ! 2337

  ! COSP: CALIPSO TOTAL BACKSCATTER (was 2341)
  if (associated(cosp_diag%cosp_calipso_tot_backscatter)) then

    if (l_last) then
      do i=1, nlevels
        do lll=1, ncolumns
          do l=1, npoints
            cosp_diag%cosp_calipso_tot_backscatter(i, lll, list(l)) &
              = cosp_out%calipso_beta_tot(l, lll, i)
          end do
        end do
      end do
      ! Fill uncalculated sub-columns with zeros
      do l=1, npoints
        do lll=ncolumns+1, ubound(cosp_diag%cosp_calipso_tot_backscatter, 2)
          do i=1, nlevels
            cosp_diag%cosp_calipso_tot_backscatter(i, lll, list(l)) &
              = 0.0_RealExt
          end do
        end do
      end do
    else
      do i=1, nlevels
        do lll=1, ncolumns
          do l=1, npoints
            cosp_diag%cosp_calipso_tot_backscatter(list(l), i, lll) &
              = cosp_out%calipso_beta_tot(l, lll, i)
          end do
        end do
      end do
      do lll=ncolumns+1, ubound(cosp_diag%cosp_calipso_tot_backscatter, 3)
        do i=1, nlevels
          do l=1, npoints
            cosp_diag%cosp_calipso_tot_backscatter(list(l), i, lll) &
              = 0.0_RealExt
          end do
        end do
      end do
    end if

  end if ! 2341

  ! COSP CALIPSO CFAD SR 40 CSAT LEVELS (was 2370)
  if (associated(cosp_diag%cosp_calipso_cfad_sr_40)) then

    if (l_last) then
      do i=1, cosp_nlr
        do j=1, n_backscatter_bins
          do l=1, npoints
            cosp_diag%cosp_calipso_cfad_sr_40(j, i, list(l)) &
            = cosp_out%calipso_cfad_sr(l, j, i)
          end do
        end do
      end do
    else
      do i=1, cosp_nlr
        do j=1, n_backscatter_bins
          do l=1, npoints
            cosp_diag%cosp_calipso_cfad_sr_40(list(l), j, i) &
            = cosp_out%calipso_cfad_sr(l, j, i)
          end do
        end do
      end do
    end if

  end if ! 2370

  ! COSP: MASK FOR CALIPSO CF 40 LVLS
  if (associated(cosp_diag%cosp_calipso_cf_40_mask)) then
    ! create_mask needs to be called for all cases to set r_undef to zero

    if (associated(cosp_diag%cosp_calipso_cf_40_ice)) then
      if (l_last) then
        do i=1, cosp_nlr
          do l=1, npoints
            call create_mask(r_undef, cosp_out%calipso_lidarcldphase(l, i, 1), &
            cosp_diag%cosp_calipso_cf_40_mask(i, list(l)))
          end do
        end do
      else
        do i=1, cosp_nlr
          do l=1, npoints
            call create_mask(r_undef, cosp_out%calipso_lidarcldphase(l, i, 1), &
            cosp_diag%cosp_calipso_cf_40_mask(list(l), i))
          end do
        end do
      end if
    end if
      
    if (associated(cosp_diag%cosp_calipso_cf_40_undet)) then
      if (l_last) then
        do i=1, cosp_nlr
          do l=1, npoints
            call create_mask(r_undef, cosp_out%calipso_lidarcldphase(l, i, 3), &
            cosp_diag%cosp_calipso_cf_40_mask(i, list(l)))
          end do
        end do
      else
        do i=1, cosp_nlr
          do l=1, npoints
            call create_mask(r_undef, cosp_out%calipso_lidarcldphase(l, i, 3), &
            cosp_diag%cosp_calipso_cf_40_mask(list(l), i))
          end do
        end do
      end if
    end if
    
    ! The liquid phase will always be calculated if the mask is requested
    if (l_last) then
      do i=1, cosp_nlr
        do l=1, npoints
          call create_mask(r_undef, cosp_out%calipso_lidarcldphase(l, i, 2), &
          cosp_diag%cosp_calipso_cf_40_mask(i, list(l)))
        end do
      end do
    else
      do i=1, cosp_nlr
        do l=1, npoints
          call create_mask(r_undef, cosp_out%calipso_lidarcldphase(l, i, 2), &
          cosp_diag%cosp_calipso_cf_40_mask(list(l), i))
        end do
      end do
    end if

  end if

  ! COSP: CALIPSO CF 40 LVLS (LIQ) (was 2473)
  if (associated(cosp_diag%cosp_calipso_cf_40_liq)) then

    if (l_last) then
      do i=1, cosp_nlr
        do l=1, npoints
          cosp_diag%cosp_calipso_cf_40_liq(i, list(l)) &
          = 0.01_RealExt * cosp_out%calipso_lidarcldphase(l, i, 2)
        end do
      end do
    else
      do i=1, cosp_nlr
        do l=1, npoints
          cosp_diag%cosp_calipso_cf_40_liq(list(l), i) &
          = 0.01_RealExt * cosp_out%calipso_lidarcldphase(l, i, 2)
        end do
      end do
    end if

  end if ! 2473

  ! COSP: CALIPSO CF 40 LVLS (ICE) (was 2474)
  if (associated(cosp_diag%cosp_calipso_cf_40_ice)) then

    if (l_last) then
      do i=1, cosp_nlr
        do l=1, npoints
          cosp_diag%cosp_calipso_cf_40_ice(i, list(l)) &
          = 0.01_RealExt * cosp_out%calipso_lidarcldphase(l, i, 1)
        end do
      end do
    else
      do i=1, cosp_nlr
        do l=1, npoints
          cosp_diag%cosp_calipso_cf_40_ice(list(l), i) &
          = 0.01_RealExt * cosp_out%calipso_lidarcldphase(l, i, 1)
        end do
      end do
    end if

  end if ! 2474

  ! COSP: CALIPSO CF 40 LVLS (UNDET) (was 2475)
  if (associated(cosp_diag%cosp_calipso_cf_40_undet)) then

    if (l_last) then
      do i=1, cosp_nlr
        do l=1, npoints
          cosp_diag%cosp_calipso_cf_40_undet(i, list(l)) &
          = 0.01_RealExt * cosp_out%calipso_lidarcldphase(l, i, 3)
        end do
      end do
    else
      do i=1, cosp_nlr
        do l=1, npoints
          cosp_diag%cosp_calipso_cf_40_undet(list(l), i) &
          = 0.01_RealExt * cosp_out%calipso_lidarcldphase(l, i, 3)
        end do
      end do
    end if

  end if ! 2475

  ! COSP: ISCCP WEIGHTED CLOUD ALBEDO (was 2331)
  if (associated(cosp_diag%cosp_weighted_cloud_albedo)) then

    do l=1, npoints
      cosp_diag%cosp_weighted_cloud_albedo(list(l)) &
      = 0.01_RealExt * cosp_out%isccp_totalcldarea(l) &
                     * cosp_out%isccp_meanalbedocld(l)
    end do

  end if ! 2331

  ! COSP: ISCCP WEIGHTED CLOUD TOP PRESSURE (was 2333)
  if (associated(cosp_diag%cosp_weighted_ctp)) then

    do l=1, npoints
      cosp_diag%cosp_weighted_ctp(list(l)) &
      = 0.01_RealExt * cosp_out%isccp_totalcldarea(l) &
                     * cosp_out%isccp_meanptop(l)
    end do

  end if ! 2333

  ! COSP: ISCCP TOTAL CLOUD AREA (was 2334)
  if (associated(cosp_diag%cosp_tot_cloud_area)) then

    do l=1, npoints
      cosp_diag%cosp_tot_cloud_area(list(l)) &
      = 0.01_RealExt * cosp_out%isccp_totalcldarea(l)
    end do

  end if ! 2334

  ! COSP: CALIPSO MOLECULAR ATB MDL LVLS (was 2340)
  if (associated(cosp_diag%cosp_calipso_mol_atb_mdl)) then

    if (l_last) then
      do i=1, nlevels
        do l=1, npoints
          cosp_diag%cosp_calipso_mol_atb_mdl(i, list(l)) &
          = cosp_out%calipso_beta_mol(l, i)
        end do
      end do
    else
      do i=1, nlevels
        do l=1, npoints
          cosp_diag%cosp_calipso_mol_atb_mdl(list(l), i) &
          = cosp_out%calipso_beta_mol(l, i)
        end do
      end do
    end if

  end if ! 2340

  ! COSP: GBX-MEAN CSAT Ze MDL LEVELS (2353)
  if (associated(cosp_diag%cosp_cloudsat_gbxmean_ze_mdl)) then

    call cosp_gridbox_mean(npoints, ncolumns, nlevels, &
      cosp_column_in%surfelev, &
      cosp_column_in%hgt_matrix(:,nlevels:1:-1), &
      .false., &
      cosp_out%cloudsat_ze_tot, cloudsat_gbxmean_ze_mdl, &
      log_units=.true., sensitivity=0.001_wp)

    if (l_last) then
      do i=1, nlevels
        do l=1, npoints
          cosp_diag%cosp_cloudsat_gbxmean_ze_mdl(i, list(l)) &
          = cloudsat_gbxmean_ze_mdl(l, i)
        end do
      end do
    else
      do i=1, nlevels
        do l=1, npoints
          cosp_diag%cosp_cloudsat_gbxmean_ze_mdl(list(l), i) &
          = cloudsat_gbxmean_ze_mdl(l, i)
        end do
      end do
    end if

  end if ! 2353

  ! Required for 2354
  if (associated(cosp_diag%cosp_cloudsat_gbxmean_ze_40)) then

    call cosp_change_vertical_grid(npoints, ncolumns, nlevels, &
      cosp_column_in%hgt_matrix(:,nlevels:1:-1), &
      cosp_column_in%hgt_matrix_half(:,nlevels:1:-1), &
      cosp_out%cloudsat_ze_tot, &
      cosp_nlr, &
      vgrid_zl(cosp_nlr:1:-1), vgrid_zu(cosp_nlr:1:-1), &
      cloudsat_ze_tot_40(:,:,cosp_nlr:1:-1))

  end if
           
  ! COSP: GBX-MEAN CSAT Ze 40 LEVELS (was 2354)
  if (associated(cosp_diag%cosp_cloudsat_gbxmean_ze_40)) then

    call cosp_gridbox_mean(npoints, ncolumns, cosp_nlr, &
      cosp_column_in%surfelev, &
      vgrid_zu(cosp_nlr:1:-1), &
      .true., &
      cloudsat_ze_tot_40(:,:,cosp_nlr:1:-1), &
      cloudsat_gbxmean_ze_40(:,cosp_nlr:1:-1), &
      log_units=.true., sensitivity=0.001_wp)

    if (l_last) then
      do i=1, cosp_nlr
        do l=1, npoints
          cosp_diag%cosp_cloudsat_gbxmean_ze_40(i, list(l)) &
          = cloudsat_gbxmean_ze_40(l, i)
          end do
      end do
    else
      do i=1, cosp_nlr
        do l=1, npoints
          cosp_diag%cosp_cloudsat_gbxmean_ze_40(list(l), i) &
          = cloudsat_gbxmean_ze_40(l, i)
        end do
      end do
    end if

  end if ! 2354

  ! COSP: GBX-MEAN CALIPSO ATB MDL LEVELS (2355)
  if (associated(cosp_diag%cosp_calipso_gbxmean_atb_mdl)) then

    call cosp_gridbox_mean(npoints, ncolumns, nlevels, &
      cosp_column_in%hgt_matrix_half, cosp_column_in%surfelev, &
      .false., &
      cosp_out%calipso_beta_tot, calipso_gbxmean_atb_mdl)

    if (l_last) then
      do i=1, nlevels
        do l=1, npoints
          cosp_diag%cosp_calipso_gbxmean_atb_mdl(i, list(l)) &
          = calipso_gbxmean_atb_mdl(l, i)
        end do
      end do
    else
      do i=1, nlevels
        do l=1, npoints
          cosp_diag%cosp_calipso_gbxmean_atb_mdl(list(l), i) &
          = calipso_gbxmean_atb_mdl(l, i)
        end do
      end do
    end if

  end if ! 2355
 
  ! Required for 2356
  if (associated(cosp_diag%cosp_calipso_gbxmean_atb_40)) then
 
    call cosp_change_vertical_grid(npoints,ncolumns,nlevels, &
      cosp_column_in%hgt_matrix(:,nlevels:1:-1), &
      cosp_column_in%hgt_matrix_half(:,nlevels:1:-1), &
      cosp_out%calipso_beta_tot, &
      cosp_nlr, &
      vgrid_zl(cosp_nlr:1:-1), vgrid_zu(cosp_nlr:1:-1), &
      calipso_beta_tot_40(:,:,cosp_nlr:1:-1))
 
  end if
 
  ! COSP: GBX-MEAN CALIPSO ATB 40 LEVELS (was 2356)
  if (associated(cosp_diag%cosp_calipso_gbxmean_atb_40)) then 

    call cosp_gridbox_mean(npoints,ncolumns,nlevels, &
      cosp_column_in%surfelev, &
      vgrid_zu(cosp_nlr:1:-1), &
      .true., &
      calipso_beta_tot_40(:,:,cosp_nlr:1:-1), &
      calipso_gbxmean_atb_40(:,cosp_nlr:1:-1))

    if (l_last) then
      do i=1, cosp_nlr
        do l=1, npoints
          cosp_diag%cosp_calipso_gbxmean_atb_40(i, list(l)) &
          = calipso_gbxmean_atb_40(l, i)
        end do
      end do
    else
      do i=1, cosp_nlr
        do l=1, npoints
          cosp_diag%cosp_calipso_gbxmean_atb_40(list(l), i) &
          = calipso_gbxmean_atb_40(l, i)
        end do
      end do
    end if

  end if ! 2356

  ! Required for 2357, 2327 and 2359
  if (associated(cosp_diag%cosp_calipso_mol_atb_40) .or. &
      (associated(cosp_diag%cosp_calipso_cloudsat_40_cl_mask) .or. &
       associated(cosp_diag%cosp_calipso_cloudsat_40_cl))) then      

    call cosp_change_vertical_grid(npoints,1,nlevels, &
      cosp_column_in%hgt_matrix(:,nlevels:1:-1), &
      cosp_column_in%hgt_matrix_half(:,nlevels:1:-1), &
      cosp_out%calipso_beta_mol, &
      cosp_nlr, &
      vgrid_zl(cosp_nlr:1:-1),vgrid_zu(cosp_nlr:1:-1), &
      calipso_beta_mol_40(:,cosp_nlr:1:-1))

  endif

  ! COSP: CALIPSO MOLECULAR ATB 40 LVLS (was 2357)
  if (associated(cosp_diag%cosp_calipso_mol_atb_40)) then

    if (l_last) then
      do i=1, cosp_nlr
        do l=1, npoints
          cosp_diag%cosp_calipso_mol_atb_40(i, list(l)) &
          = calipso_beta_mol_40(l, i)
        end do
      end do
    else
      do i=1, cosp_nlr
        do l=1, npoints
          cosp_diag%cosp_calipso_mol_atb_40(list(l), i) &
          = calipso_beta_mol_40(l, i)
        end do
      end do
    end if

  end if ! 2357

  ! Required for 2326 and 2358
  if (associated(cosp_diag%cosp_calipso_cloudsat_mdl_cl_mask) .or. &
      associated(cosp_diag%cosp_calipso_cloudsat_mdl_cl)) then

    call cosp_radar_and_lidar_cloud_fraction( &
      npoints, ncolumns, nlevels, &
      cosp_out%calipso_beta_mol, cosp_out%calipso_beta_tot, &
      cosp_out%cloudsat_ze_tot, calipso_cloudsat_mdl_cl)

  end if

  ! COSP: MASK FOR CALIPSO/CLOUDSAT CLOUD 40 LEVELS (2358) (was 2326)
  if (associated(cosp_diag%cosp_calipso_cloudsat_mdl_cl_mask)) then

    if (l_last) then
      do i=1, nlevels
        do l=1, npoints
          call create_mask(r_undef, calipso_cloudsat_mdl_cl(l, i), &
          cosp_diag%cosp_calipso_cloudsat_mdl_cl_mask(i, list(l)))
        end do
      end do
    else
      do i=1, nlevels
        do l=1, npoints
          call create_mask(r_undef, calipso_cloudsat_mdl_cl(l, i), &
          cosp_diag%cosp_calipso_cloudsat_mdl_cl_mask(list(l), i))
        end do
      end do
    end if

  end if ! 2326

  ! COSP: CALIPSO/CLOUDSAT CLOUD MDL LEV (was 2358)
  if (associated(cosp_diag%cosp_calipso_cloudsat_mdl_cl)) then

    if (l_last) then
      do i=1, nlevels
        do l=1, npoints
          cosp_diag%cosp_calipso_cloudsat_mdl_cl(i, list(l)) &
          = calipso_cloudsat_mdl_cl(l, i)
        end do
      end do
    else
      do i=1, nlevels
        do l=1, npoints
          cosp_diag%cosp_calipso_cloudsat_mdl_cl(list(l), i) &
          = calipso_cloudsat_mdl_cl(l, i)
        end do
      end do
    end if

  end if ! 2358

  ! Required for 2327 and 2359
  if (associated(cosp_diag%cosp_calipso_cloudsat_40_cl_mask) .or. &
      associated(cosp_diag%cosp_calipso_cloudsat_40_cl)) then

    call cosp_radar_and_lidar_cloud_fraction( &
      npoints, ncolumns, cosp_nlr, &
      calipso_beta_mol_40,calipso_beta_tot_40, &
      cloudsat_ze_tot_40, calipso_cloudsat_40_cl)

  end if

  ! COSP: MASK FOR CALIPSO/CLOUDSAT CLOUD 40 LEVELS (2359) (was 2327)
  if (associated(cosp_diag%cosp_calipso_cloudsat_40_cl_mask)) then

    if (l_last) then
      do i=1, cosp_nlr
        do l=1, npoints
          call create_mask(r_undef, calipso_cloudsat_40_cl(l, i), &
          cosp_diag%cosp_calipso_cloudsat_40_cl_mask(i, list(l)))
        end do
      end do
    else
      do i=1, cosp_nlr
        do l=1, npoints
          call create_mask(r_undef, calipso_cloudsat_40_cl(l, i), &
          cosp_diag%cosp_calipso_cloudsat_40_cl_mask(list(l), i))
        end do
      end do
    end if

  end if ! 2327

  ! COSP: CALIPSO/CLOUDSAT CLOUD 40 LEV (was 2359)
  if (associated(cosp_diag%cosp_calipso_cloudsat_40_cl)) then

    if (l_last) then
      do i=1, cosp_nlr
        do l=1, npoints
          cosp_diag%cosp_calipso_cloudsat_40_cl(i, list(l)) &
          = calipso_cloudsat_40_cl(l, i)
        end do
      end do
    else
      do i=1, cosp_nlr
        do l=1, npoints
          cosp_diag%cosp_calipso_cloudsat_40_cl(list(l), i) &
          = calipso_cloudsat_40_cl(l, i)
        end do
      end do
    end if

  end if ! 2359

  ! COSP: CALIPSO CLOUD AREA 40 CSAT LEVELS (was 2371)
  if (associated(cosp_diag%cosp_calipso_cloud_area_40)) then

    if (l_last) then
      do i=1, cosp_nlr
        do l=1, npoints
          cosp_diag%cosp_calipso_cloud_area_40(i, list(l)) &
          = 0.01_RealExt * cosp_out%calipso_lidarcld(l, i)
        end do
      end do
    else
      do i=1, cosp_nlr
        do l=1, npoints
          cosp_diag%cosp_calipso_cloud_area_40(list(l), i) &
          = 0.01_RealExt * cosp_out%calipso_lidarcld(l, i)
        end do
      end do
    end if

  end if ! 2371 

  ! COSP: CLOUDSAT CFAD Ze 40 CSAT LVLS (was 2372)
  if (associated(cosp_diag%cosp_cloudsat_cfad_ze_40)) then

    if (l_last) then
      do i=1, cosp_nlr
        do j=1, n_backscatter_bins
          do l=1, npoints
            cosp_diag%cosp_cloudsat_cfad_ze_40(j, i, list(l)) &
            = cosp_out%cloudsat_cfad_ze(l, j, i)
          end do
        end do
      end do
    else
      do i=1, cosp_nlr
        do j=1, n_backscatter_bins
          do l=1, npoints
            cosp_diag%cosp_cloudsat_cfad_ze_40(list(l), j, i) &
            = cosp_out%cloudsat_cfad_ze(l, j, i)
          end do
        end do
      end do
    end if

  end if ! 2372


  if (.not.present(cosp_out_ext)) then
    call destroy_cosp_outputs(cosp_out)
  end if
  call cosp_cleanup
  call destroy_cosp_column_inputs(cosp_column_in)
  call destroy_cosp_optical_inputs(cosp_optical_in)
  call destroy_cosp_inputs_host_model(cosp_hmodel)

end subroutine cosp

end module cosp_mod
