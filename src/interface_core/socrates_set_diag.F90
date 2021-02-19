! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Set the diagnostics to be output from the Socrates runes interface
!
!------------------------------------------------------------------------------
module socrates_set_diag
implicit none
character(len=*), parameter, private :: ModuleName = 'SOCRATES_SET_DIAG'
contains

subroutine set_diag(control, dimen, spectrum, &
  atm, cld, mcica_data, aer, bound, radout, &
  n_profile, n_layer, n_tile, &
  layer_heat_capacity, layer_heat_capacity_1d, l_invert, &
  flux_direct, flux_down, flux_up, heating_rate, &
  flux_up_tile, flux_up_blue_tile, flux_direct_blue_surf, flux_down_blue_surf, &
  flux_direct_1d, flux_down_1d, flux_up_1d, heating_rate_1d, &
  flux_up_tile_1d, flux_up_blue_tile_1d, &
  total_cloud_cover, total_cloud_fraction, total_cloud_fraction_1d, &
  liq_frac_diag, ice_frac_diag, &
  liq_conv_frac_diag, ice_conv_frac_diag, &
  liq_incloud_mmr_diag, ice_incloud_mmr_diag, &
  liq_inconv_mmr_diag, ice_inconv_mmr_diag, &
  liq_dim_diag, ice_dim_diag, &
  liq_conv_dim_diag, ice_conv_dim_diag, &
  liq_frac_diag_1d, ice_frac_diag_1d, &
  liq_conv_frac_diag_1d, ice_conv_frac_diag_1d, &
  liq_incloud_mmr_diag_1d, ice_incloud_mmr_diag_1d, &
  liq_inconv_mmr_diag_1d, ice_inconv_mmr_diag_1d, &
  liq_dim_diag_1d, ice_dim_diag_1d, &
  liq_conv_dim_diag_1d, ice_conv_dim_diag_1d)

use def_control,  only: StrCtrl
use def_dimen,    only: StrDim
use def_spectrum, only: StrSpecData
use def_atm,      only: StrAtm
use def_cld,      only: StrCld
use def_mcica,    only: StrMcica
use def_aer,      only: StrAer
use def_bound,    only: StrBound
use def_out,      only: StrOut

use realtype_rd, only: RealK
use ereport_mod,  only: ereport
use errormessagelength_mod, only: errormessagelength
use rad_pcf, only: i_normal, i_err_fatal, ip_cloud_off, ip_mcica, &
                   ip_clcmp_st_water, ip_clcmp_st_ice, &
                   ip_clcmp_cnv_water, ip_clcmp_cnv_ice

implicit none

! Control options:
type(StrCtrl),      intent(in) :: control

! Dimensions:
type(StrDim),       intent(in) :: dimen

! Spectral data:
type (StrSpecData), intent(in) :: spectrum

! Atmospheric properties:
type(StrAtm),       intent(in) :: atm

! Cloud properties:
type(StrCld),       intent(in) :: cld

! MCICA data:
type(StrMcica),    intent(in) :: mcica_data

! Aerosol properties:
type(StrAer),       intent(in) :: aer

! Boundary conditions:
type(StrBound),     intent(in) :: bound

! Output fields from core radiation code:
type(StrOut),       intent(in) :: radout

integer, intent(in) :: n_profile
!   Number of columns to operate on
integer, intent(in) :: n_layer
!   Number of layers for radiation
integer, intent(in), optional :: n_tile
!   Number of surface tiles

real(RealK), intent(in), optional :: layer_heat_capacity(n_profile, n_layer)
real(RealK), intent(in), optional :: layer_heat_capacity_1d(n_layer)
!   Heat capacity of layer

logical, intent(in), optional :: l_invert
!   Flag to invert fields in the vertical


! Output fields:
real(RealK), intent(out), optional :: flux_direct(n_profile, 0:n_layer)
real(RealK), intent(out), optional :: flux_direct_1d(0:n_layer)
!   Direct (unscattered) downwards flux (Wm-2)
real(RealK), intent(out), optional :: flux_down(n_profile, 0:n_layer)
real(RealK), intent(out), optional :: flux_down_1d(0:n_layer)
!   Downwards flux (Wm-2)
real(RealK), intent(out), optional :: flux_up(n_profile, 0:n_layer)
real(RealK), intent(out), optional :: flux_up_1d(0:n_layer)
!   Upwards flux (Wm-2)
real(RealK), intent(out), optional :: heating_rate(n_profile, n_layer)
real(RealK), intent(out), optional :: heating_rate_1d(n_layer)
!   Heating rate (Ks-1)
real(RealK), intent(out), optional :: flux_up_tile(:, :) ! (n_profile, n_tile)
real(RealK), intent(out), optional :: flux_up_tile_1d(:) ! (n_tile)
!   Upwards flux on tiles (Wm-2)
real(RealK), intent(out), optional :: flux_up_blue_tile(:, :)
real(RealK), intent(out), optional :: flux_up_blue_tile_1d(:)
!   Upwards blue flux on tiles (Wm-2)
real(RealK), intent(out), optional :: flux_direct_blue_surf(n_profile)
!   Direct blue flux at the surface
real(RealK), intent(out), optional :: flux_down_blue_surf(n_profile)
!   Total downward blue flux at the surface
real(RealK), intent(out), optional :: total_cloud_cover(n_profile)
!   Total cloud cover
real(RealK), intent(out), optional :: total_cloud_fraction(n_profile, n_layer)
real(RealK), intent(out), optional :: total_cloud_fraction_1d(n_layer)
!   Total cloud fraction in layers
real(RealK), intent(out), dimension(n_profile, n_layer), optional :: &
  liq_frac_diag, ice_frac_diag, &
  liq_conv_frac_diag, ice_conv_frac_diag, &
  liq_incloud_mmr_diag, ice_incloud_mmr_diag, &
  liq_inconv_mmr_diag, ice_inconv_mmr_diag, &
  liq_dim_diag, ice_dim_diag, &
  liq_conv_dim_diag, ice_conv_dim_diag
real(RealK), intent(out), dimension(n_layer), optional :: &
  liq_frac_diag_1d, ice_frac_diag_1d, &
  liq_conv_frac_diag_1d, ice_conv_frac_diag_1d, &
  liq_incloud_mmr_diag_1d, ice_incloud_mmr_diag_1d, &
  liq_inconv_mmr_diag_1d, ice_inconv_mmr_diag_1d, &
  liq_dim_diag_1d, ice_dim_diag_1d, &
  liq_conv_dim_diag_1d, ice_conv_dim_diag_1d
!   Liquid and ice cloud fractions, in-cloud mean mixing ratios,
!   and effective dimension diagnostics


integer :: l, i, k
!   Loop variables
logical :: l_inv
!   Local logical for field inversion
real(RealK) :: flux_divergence(n_profile, n_layer)
!   Flux divergence across layer (Wm-2)

integer :: ierr = i_normal
character (len=errormessagelength) :: cmessage
character (len=*), parameter :: RoutineName = 'SET_DIAG'



if (present(l_invert)) then
  l_inv = l_invert
else
  l_inv = .false.
end if


!------------------------------------------------------------------------------
! Heating rates
!------------------------------------------------------------------------------
if (present(heating_rate).or.present(heating_rate_1d)) then
  if (l_inv) then
    do i=1, n_layer
      do l=1, n_profile
        flux_divergence(l, n_layer-i+1) = &
          sum(radout%flux_down(l, i-1, 1:control%n_channel)) - &
          sum(radout%flux_down(l, i,   1:control%n_channel)) + &
          sum(radout%flux_up(  l, i,   1:control%n_channel)) - &
          sum(radout%flux_up(  l, i-1, 1:control%n_channel))
      end do
    end do
  else
    do i=1, n_layer
      do l=1, n_profile
        flux_divergence(l, i) = &
          sum(radout%flux_down(l, i-1, 1:control%n_channel)) - &
          sum(radout%flux_down(l, i,   1:control%n_channel)) + &
          sum(radout%flux_up(  l, i,   1:control%n_channel)) - &
          sum(radout%flux_up(  l, i-1, 1:control%n_channel))
      end do
    end do
  end if
  if (present(heating_rate)) then
    if (present(layer_heat_capacity)) then
      heating_rate = flux_divergence / layer_heat_capacity
    else if (present(layer_heat_capacity_1d)) then
      do i=1, n_layer
        heating_rate(1:n_profile, i) = &
          flux_divergence(1:n_profile, i) / layer_heat_capacity_1d(i)
      end do
    else
      ! Just return the flux_divergence if no heat capacity supplied
      heating_rate = flux_divergence
    end if
  end if
  if (present(heating_rate_1d)) then
    if (n_profile == 1) then
      if (present(layer_heat_capacity)) then
        heating_rate_1d(1:n_layer) = &
          flux_divergence(1, 1:n_layer) / layer_heat_capacity(1, 1:n_layer)
      else if (present(layer_heat_capacity_1d)) then
        heating_rate_1d(1:n_layer) = &
          flux_divergence(1, 1:n_layer) / layer_heat_capacity_1d(1:n_layer)
      else
        heating_rate_1d(1:n_layer) = flux_divergence(1, 1:n_layer)
      end if
    else
      if (present(layer_heat_capacity)) then
        heating_rate_1d = &
          sum(flux_divergence / layer_heat_capacity, 1) &
          / real(n_profile, RealK)
      else if (present(layer_heat_capacity_1d)) then
        heating_rate_1d = &
          (sum(flux_divergence, 1) / layer_heat_capacity_1d) &
          / real(n_profile, RealK)
      else
        heating_rate_1d = sum(flux_divergence, 1) / real(n_profile, RealK)
      end if
    end if
  end if
end if


!------------------------------------------------------------------------------
! Fluxes
!------------------------------------------------------------------------------
call sum_flux_channels(flux_direct, flux_direct_1d, radout%flux_direct)
call sum_flux_channels(flux_down, flux_down_1d, radout%flux_down)
call sum_flux_channels(flux_up, flux_up_1d, radout%flux_up)
call sum_tile_channels(flux_up_tile, flux_up_tile_1d, radout%flux_up_tile)
call sum_tile_channels(flux_up_blue_tile, flux_up_blue_tile_1d, &
                       radout%flux_up_blue_tile)
if (present(flux_direct_blue_surf)) &
  flux_direct_blue_surf = radout%flux_direct_blue_surf(1:n_profile)
if (present(flux_down_blue_surf)) &
  flux_down_blue_surf = radout%flux_down_blue_surf(1:n_profile)


!------------------------------------------------------------------------------
! Cloud diagnostics
!------------------------------------------------------------------------------
if (present(total_cloud_cover)) then
  if (control%i_cloud_representation == ip_cloud_off) then
    total_cloud_cover = 0.0_RealK
  else
    if (control%i_inhom == ip_mcica) then
      total_cloud_cover = real(cld%n_subcol_cld(1:n_profile), RealK) &
                        / real(mcica_data%n_subcol_gen, RealK)
    else
      total_cloud_cover = radout%tot_cloud_cover(1:n_profile)
    end if
  end if
end if

call set_cloud_diag(total_cloud_fraction, total_cloud_fraction_1d, cld%w_cloud)

! Cloud component diagnostics
! Initially zero each diagnostic
if (present(liq_dim_diag))         liq_dim_diag         = 0.0_RealK
if (present(liq_incloud_mmr_diag)) liq_incloud_mmr_diag = 0.0_RealK
if (present(liq_frac_diag))        liq_frac_diag        = 0.0_RealK
if (present(ice_dim_diag))         ice_dim_diag         = 0.0_RealK
if (present(ice_incloud_mmr_diag)) ice_incloud_mmr_diag = 0.0_RealK
if (present(ice_frac_diag))        ice_frac_diag        = 0.0_RealK
if (present(liq_conv_dim_diag))    liq_conv_dim_diag    = 0.0_RealK
if (present(liq_inconv_mmr_diag))  liq_inconv_mmr_diag  = 0.0_RealK
if (present(liq_conv_frac_diag))   liq_conv_frac_diag   = 0.0_RealK
if (present(ice_conv_dim_diag))    ice_conv_dim_diag    = 0.0_RealK
if (present(ice_inconv_mmr_diag))  ice_inconv_mmr_diag  = 0.0_RealK
if (present(ice_conv_frac_diag))   ice_conv_frac_diag   = 0.0_RealK

if (present(liq_dim_diag_1d))         liq_dim_diag_1d         = 0.0_RealK
if (present(liq_incloud_mmr_diag_1d)) liq_incloud_mmr_diag_1d = 0.0_RealK
if (present(liq_frac_diag_1d))        liq_frac_diag_1d        = 0.0_RealK
if (present(ice_dim_diag_1d))         ice_dim_diag_1d         = 0.0_RealK
if (present(ice_incloud_mmr_diag_1d)) ice_incloud_mmr_diag_1d = 0.0_RealK
if (present(ice_frac_diag_1d))        ice_frac_diag_1d        = 0.0_RealK
if (present(liq_conv_dim_diag_1d))    liq_conv_dim_diag_1d    = 0.0_RealK
if (present(liq_inconv_mmr_diag_1d))  liq_inconv_mmr_diag_1d  = 0.0_RealK
if (present(liq_conv_frac_diag_1d))   liq_conv_frac_diag_1d   = 0.0_RealK
if (present(ice_conv_dim_diag_1d))    ice_conv_dim_diag_1d    = 0.0_RealK
if (present(ice_inconv_mmr_diag_1d))  ice_inconv_mmr_diag_1d  = 0.0_RealK
if (present(ice_conv_frac_diag_1d))   ice_conv_frac_diag_1d   = 0.0_RealK

! Update diagnostics where components are present
if (control%i_cloud_representation /= ip_cloud_off) then
  do k=1, cld%n_condensed
    select case (cld%type_condensed(k))
    case (ip_clcmp_st_water)
      call set_cloud_diag(liq_dim_diag, liq_dim_diag_1d, &
                          cld%condensed_dim_char(:, :, k))
      call set_cloud_diag(liq_incloud_mmr_diag, liq_incloud_mmr_diag_1d, &
                          cld%condensed_mix_ratio(:, :, k))
      call set_cloud_diag(liq_frac_diag, liq_frac_diag_1d, &
                          cld%w_cloud*cld%frac_cloud(:, :, cld%i_cloud_type(k)))
    case (ip_clcmp_st_ice)
      call set_cloud_diag(ice_dim_diag, ice_dim_diag_1d, &
                          cld%condensed_dim_char(:, :, k))
      call set_cloud_diag(ice_incloud_mmr_diag, ice_incloud_mmr_diag_1d, &
                          cld%condensed_mix_ratio(:, :, k))
      call set_cloud_diag(ice_frac_diag, ice_frac_diag_1d, &
                          cld%w_cloud*cld%frac_cloud(:, :, cld%i_cloud_type(k)))
    case (ip_clcmp_cnv_water)
      call set_cloud_diag(liq_conv_dim_diag, liq_conv_dim_diag_1d, &
                          cld%condensed_dim_char(:, :, k))
      call set_cloud_diag(liq_inconv_mmr_diag, liq_inconv_mmr_diag_1d, &
                          cld%condensed_mix_ratio(:, :, k))
      call set_cloud_diag(liq_conv_frac_diag, liq_conv_frac_diag_1d, &
                          cld%w_cloud*cld%frac_cloud(:, :, cld%i_cloud_type(k)))
    case (ip_clcmp_cnv_ice)
      call set_cloud_diag(ice_conv_dim_diag, ice_conv_dim_diag_1d, &
                          cld%condensed_dim_char(:, :, k))
      call set_cloud_diag(ice_inconv_mmr_diag, ice_inconv_mmr_diag_1d, &
                          cld%condensed_mix_ratio(:, :, k))
      call set_cloud_diag(ice_conv_frac_diag, ice_conv_frac_diag_1d, &
                          cld%w_cloud*cld%frac_cloud(:, :, cld%i_cloud_type(k)))
    end select
  end do
end if

contains


!------------------------------------------------------------------------------
subroutine sum_flux_channels(field, field_1d, field_channels)
  
  implicit none
  
  real(RealK), intent(out), optional :: field(n_profile, 0:n_layer)
  real(RealK), intent(out), optional :: field_1d(0:n_layer)
  real(RealK), intent(in) :: field_channels(:, 0:, :)
  
  if (present(field)) then
    if (l_inv) then
      do i=0, n_layer
        do l=1, n_profile
          field(l, n_layer-i) = &
            sum(field_channels(l, i, 1:control%n_channel))
        end do
      end do
    else
      do i=0, n_layer
        do l=1, n_profile
          field(l, i) = &
            sum(field_channels(l, i, 1:control%n_channel))
        end do
      end do
    end if
  end if
  if (present(field_1d)) then
    if (n_profile == 1) then
      if (l_inv) then
        do i=0, n_layer
          field_1d(n_layer-i) = &
            sum(field_channels(1, i, 1:control%n_channel))
        end do
      else
        do i=0, n_layer
          field_1d(i) = &
            sum(field_channels(1, i, 1:control%n_channel))
        end do
      end if
    else
      if (l_inv) then
        do i=0, n_layer
          field_1d(n_layer-i) = &
            sum(field_channels(1:n_profile, i, 1:control%n_channel)) &
            / real(n_profile, RealK)
        end do
      else
        do i=0, n_layer
          field_1d(i) = &
            sum(field_channels(1:n_profile, i, 1:control%n_channel)) &
            / real(n_profile, RealK)
        end do
      end if
    end if
  end if
  
end subroutine sum_flux_channels
  
  
!------------------------------------------------------------------------------
subroutine sum_tile_channels(field, field_1d, field_channels)
  
  implicit none
  
  real(RealK), intent(out), optional :: field(:, :)
  real(RealK), intent(out), optional :: field_1d(:)
  real(RealK), intent(in) :: field_channels(:, :, :)
  
  integer :: ll
  
  if (present(field)) then
    field(:, :) = 0.0_RealK
    if (present(n_tile).and.control%l_tile) then
      do i=1, n_tile
        do ll=1, bound%n_point_tile
          l = bound%list_tile(ll)
          field(l, i) = sum(field_channels(ll, i, 1:control%n_channel))
        end do
      end do
    end if
  end if
  if (present(field_1d)) then
    field_1d(:) = 0.0_RealK
    if (present(n_tile).and.control%l_tile) then
      if (bound%n_point_tile == 1) then
        do i=1, n_tile
          field_1d(i) = sum(field_channels(1, i, 1:control%n_channel))
        end do
      else if (bound%n_point_tile > 1) then
        do i=1, n_tile
          field_1d(i) = &
            sum(field_channels(1:bound%n_point_tile, i, 1:control%n_channel)) &
            / real(bound%n_point_tile, RealK)
        end do
      end if
    end if
  end if
  
end subroutine sum_tile_channels


!------------------------------------------------------------------------------
subroutine set_cloud_diag(field, field_1d, cld_field)

  implicit none

  real(RealK), intent(out), optional :: field(:, :)
  real(RealK), intent(out), optional :: field_1d(:)
  real(RealK), intent(in) :: cld_field(:, dimen%id_cloud_top:)

  if (present(field)) then
    if (control%i_cloud_representation == ip_cloud_off) then
      field = 0.0_RealK
    else
      if (l_inv) then
        do i=1, dimen%id_cloud_top-1
          do l=1, n_profile
            field(l, n_layer+1-i) = 0.0_RealK
          end do
        end do
        do i=dimen%id_cloud_top, n_layer
          do l=1, n_profile
            field(l, n_layer+1-i) = cld_field(l, i)
          end do
        end do
      else
        do i=1, dimen%id_cloud_top-1
          do l=1, n_profile
            field(l, i) = 0.0_RealK
          end do
        end do
        do i=dimen%id_cloud_top, n_layer
          do l=1, n_profile
            field(l, i) = cld_field(l, i)
          end do
        end do
      end if
    end if
  end if
  if (present(field_1d)) then
    if (control%i_cloud_representation == ip_cloud_off) then
      field_1d = 0.0_RealK
    else
      if (l_inv) then
        do i=1, dimen%id_cloud_top-1
          field_1d(n_layer+1-i) = 0.0_RealK
        end do
        if (n_profile == 1) then
          do i=dimen%id_cloud_top, n_layer
            field_1d(n_layer+1-i) = cld_field(1, i)
          end do
        else
          do i=dimen%id_cloud_top, n_layer
            field_1d(n_layer+1-i) = sum(cld_field(1:n_profile, i)) &
                                  / real(n_profile, RealK)
          end do
        end if
      else
        do i=1, dimen%id_cloud_top-1
          field_1d(i) = 0.0_RealK
        end do
        if (n_profile == 1) then
          do i=dimen%id_cloud_top, n_layer
            field_1d(i) = cld_field(1, i)
          end do
        else
          do i=dimen%id_cloud_top, n_layer
            field_1d(i) = sum(cld_field(1:n_profile, i)) &
                        / real(n_profile, RealK)
          end do
        end if
      end if
    end if
  end if

end subroutine set_cloud_diag

end subroutine set_diag
end module socrates_set_diag
