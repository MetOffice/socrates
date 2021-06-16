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

subroutine set_diag(diag, control, dimen, spectrum, &
  atm, cld, mcica_data, aer, bound, radout, &
  n_profile, n_layer, n_tile, &
  layer_heat_capacity, layer_heat_capacity_1d, l_invert)

use socrates_def_diag, only: StrDiag

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

! Output diagnostic fields
type(StrDiag),   intent(inout) :: diag

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
type(StrMcica),     intent(in) :: mcica_data

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


integer :: l, k
integer, target :: i, ii
integer, pointer :: i_p
!   Loop variables
real(RealK) :: flux_divergence(n_profile, n_layer)
!   Flux divergence across layer (Wm-2)

character (len=*), parameter :: RoutineName = 'SET_DIAG'



if (present(l_invert)) then
  if (l_invert) then
    ! The layer is indexed using an inverted loop counter
    i_p => ii
  else
    ! The layer is indexed in the standard order
    i_p => i
  end if
else
  i_p => i
end if


!------------------------------------------------------------------------------
! Heating rates
!------------------------------------------------------------------------------
if (associated(diag%heating_rate)) then
  do i=1, n_layer
    ii = n_layer-i+1
    do l=1, n_profile
      flux_divergence(l, i_p) = &
        sum(radout%flux_down(l, i-1, 1:control%n_channel)) - &
        sum(radout%flux_down(l, i,   1:control%n_channel)) + &
        sum(radout%flux_up(  l, i,   1:control%n_channel)) - &
        sum(radout%flux_up(  l, i-1, 1:control%n_channel))
    end do
  end do
  if (present(layer_heat_capacity)) then
    diag%heating_rate(1:n_profile, 1:n_layer) = &
      flux_divergence / layer_heat_capacity
  else if (present(layer_heat_capacity_1d)) then
    do i=1, n_layer
      diag%heating_rate(1:n_profile, i) = &
        flux_divergence(1:n_profile, i) / layer_heat_capacity_1d(i)
    end do
  else
    ! Just return the flux_divergence if no heat capacity supplied
    diag%heating_rate(1:n_profile, 1:n_layer) = flux_divergence
  end if
end if


!------------------------------------------------------------------------------
! Fluxes
!------------------------------------------------------------------------------
if (associated(diag%flux_direct)) then
  do i=0, n_layer
    ii = n_layer-i
    do l=1, n_profile
      diag%flux_direct(l, i_p) &
        = sum(radout%flux_direct(l, i, 1:control%n_channel))
    end do
  end do
end if
if (associated(diag%flux_down)) then
  do i=0, n_layer
    ii = n_layer-i
    do l=1, n_profile
      diag%flux_down(l, i_p) &
        = sum(radout%flux_down(l, i, 1:control%n_channel))
    end do
  end do
end if
if (associated(diag%flux_up)) then
  do i=0, n_layer
    ii = n_layer-i
    do l=1, n_profile
      diag%flux_up(l, i_p) &
        = sum(radout%flux_up(l, i, 1:control%n_channel))
    end do
  end do
end if

call sum_tile_channels(diag%flux_up_tile, radout%flux_up_tile)
call sum_tile_channels(diag%flux_up_blue_tile, radout%flux_up_blue_tile)
if (associated(diag%flux_direct_blue_surf)) &
  diag%flux_direct_blue_surf(1:n_profile) &
    = radout%flux_direct_blue_surf(1:n_profile)
if (associated(diag%flux_down_blue_surf)) &
  diag%flux_down_blue_surf(1:n_profile) &
    = radout%flux_down_blue_surf(1:n_profile)


!------------------------------------------------------------------------------
! Cloud diagnostics
!------------------------------------------------------------------------------
if (associated(diag%total_cloud_cover)) then
  if (control%i_cloud_representation == ip_cloud_off) then
    diag%total_cloud_cover(1:n_profile) = 0.0_RealK
  else
    if (control%i_inhom == ip_mcica) then
      diag%total_cloud_cover(1:n_profile) &
        = real(cld%n_subcol_cld(1:n_profile), RealK) &
        / real(mcica_data%n_subcol_gen, RealK)
    else
      diag%total_cloud_cover(1:n_profile) = radout%tot_cloud_cover(1:n_profile)
    end if
  end if
end if

if (associated(diag%total_cloud_fraction)) then
  if (control%i_cloud_representation == ip_cloud_off) then
    diag%total_cloud_fraction(1:n_profile, 1:n_layer) = 0.0_RealK
  else
    do i=1, dimen%id_cloud_top-1
      ii = n_layer+1-i
      do l=1, n_profile
        diag%total_cloud_fraction(l, i_p) = 0.0_RealK
      end do
    end do
    do i=dimen%id_cloud_top, n_layer
      ii = n_layer+1-i
      do l=1, n_profile
        diag%total_cloud_fraction(l, i_p) = cld%w_cloud(l, i)
      end do
    end do
  end if
end if

! Cloud component diagnostics
if (control%i_cloud_representation == ip_cloud_off) then
  if (associated(diag%liq_dim))         diag%liq_dim         = 0.0_RealK
  if (associated(diag%liq_incloud_mmr)) diag%liq_incloud_mmr = 0.0_RealK
  if (associated(diag%liq_frac))        diag%liq_frac        = 0.0_RealK
  if (associated(diag%ice_dim))         diag%ice_dim         = 0.0_RealK
  if (associated(diag%ice_incloud_mmr)) diag%ice_incloud_mmr = 0.0_RealK
  if (associated(diag%ice_frac))        diag%ice_frac        = 0.0_RealK
  if (associated(diag%liq_conv_dim))    diag%liq_conv_dim    = 0.0_RealK
  if (associated(diag%liq_inconv_mmr))  diag%liq_inconv_mmr  = 0.0_RealK
  if (associated(diag%liq_conv_frac))   diag%liq_conv_frac   = 0.0_RealK
  if (associated(diag%ice_conv_dim))    diag%ice_conv_dim    = 0.0_RealK
  if (associated(diag%ice_inconv_mmr))  diag%ice_inconv_mmr  = 0.0_RealK
  if (associated(diag%ice_conv_frac))   diag%ice_conv_frac   = 0.0_RealK
else
  do k=1, cld%n_condensed
    select case (cld%type_condensed(k))
    case (ip_clcmp_st_water)
      call set_cloud_dim(diag%liq_dim)
      call set_cloud_mmr(diag%liq_incloud_mmr)
      call set_cloud_frac(diag%liq_frac)
    case (ip_clcmp_st_ice)
      call set_cloud_dim(diag%ice_dim)
      call set_cloud_mmr(diag%ice_incloud_mmr)
      call set_cloud_frac(diag%ice_frac)
    case (ip_clcmp_cnv_water)
      call set_cloud_dim(diag%liq_conv_dim)
      call set_cloud_mmr(diag%liq_inconv_mmr)
      call set_cloud_frac(diag%liq_conv_frac)
    case (ip_clcmp_cnv_ice)
      call set_cloud_dim(diag%ice_conv_dim)
      call set_cloud_mmr(diag%ice_inconv_mmr)
      call set_cloud_frac(diag%ice_conv_frac)
    end select
  end do
end if


!------------------------------------------------------------------------------
! Aerosol diagnostics
!------------------------------------------------------------------------------
if (associated(diag%aerosol_optical_depth)) then
  if (control%l_aerosol_absorption_band .and. &
      control%l_aerosol_scattering_band) then
    do k=max(lbound(diag%aerosol_optical_depth,3), 1), &
         min(ubound(diag%aerosol_optical_depth,3), spectrum%basic%n_band)
      do i=max(lbound(diag%aerosol_optical_depth,2), 1), &
           min(ubound(diag%aerosol_optical_depth,2), n_layer)
        ii = n_layer+1-i
        do l=max(lbound(diag%aerosol_optical_depth,1), 1), &
             min(ubound(diag%aerosol_optical_depth,1), n_profile)
          diag%aerosol_optical_depth(l, i, k) &
            = (radout%aerosol_absorption_band(l, i_p, k) &
             + radout%aerosol_scattering_band(l, i_p, k)) * atm%mass(l, i_p)
        end do
      end do
    end do
  end if
end if
if (associated(diag%aerosol_scat_optical_depth)) then
  if (control%l_aerosol_scattering_band) then
    do k=max(lbound(diag%aerosol_scat_optical_depth,3), 1), &
         min(ubound(diag%aerosol_scat_optical_depth,3), spectrum%basic%n_band)
      do i=max(lbound(diag%aerosol_scat_optical_depth,2), 1), &
           min(ubound(diag%aerosol_scat_optical_depth,2), n_layer)
        ii = n_layer+1-i
        do l=max(lbound(diag%aerosol_scat_optical_depth,1), 1), &
             min(ubound(diag%aerosol_scat_optical_depth,1), n_profile)
          diag%aerosol_scat_optical_depth(l, i, k) &
            = radout%aerosol_scattering_band(l, i_p, k) * atm%mass(l, i_p)
        end do
      end do
    end do
  end if
end if
if (associated(diag%aerosol_asymmetry_scat)) then
  if (control%l_aerosol_asymmetry_band) then
    do k=max(lbound(diag%aerosol_asymmetry_scat,3), 1), &
         min(ubound(diag%aerosol_asymmetry_scat,3), spectrum%basic%n_band)
      do i=max(lbound(diag%aerosol_asymmetry_scat,2), 1), &
           min(ubound(diag%aerosol_asymmetry_scat,2), n_layer)
        ii = n_layer+1-i
        do l=max(lbound(diag%aerosol_asymmetry_scat,1), 1), &
             min(ubound(diag%aerosol_asymmetry_scat,1), n_profile)
          diag%aerosol_asymmetry_scat(l, i, k) &
            = radout%aerosol_asymmetry_band(l, i_p, k) * atm%mass(l, i_p)
        end do
      end do
    end do
  end if
end if


!------------------------------------------------------------------------------
contains
!------------------------------------------------------------------------------
subroutine sum_tile_channels(field, field_channels)
  
  implicit none
  
  real(RealK), intent(inout), pointer :: field(:, :)
  real(RealK), intent(in), allocatable :: field_channels(:, :, :)
  
  integer :: ll
  
  if (associated(field).and.present(n_tile)) then
    field = 0.0_RealK
    if (control%l_tile) then
      do i=1, n_tile
        do ll=1, bound%n_point_tile
          l = bound%list_tile(ll)
          field(l, i) = sum(field_channels(ll, i, 1:control%n_channel))
        end do
      end do
    end if
  end if
  
end subroutine sum_tile_channels


!------------------------------------------------------------------------------
subroutine set_cloud_dim(field)

  implicit none

  real(RealK), intent(inout), pointer :: field(:, :)
  integer :: i_lower, i_upper

  if (associated(field)) then
    if (associated(i_p, ii)) then
      ! Field output is inverted
      i_lower = n_layer + 1 - ubound(field, 2)
      i_upper = n_layer + 1 - lbound(field, 2)
    else
      i_lower = lbound(field, 2)
      i_upper = ubound(field, 2)
    end if
    ! Fill diagnostic between requested layers
    do i=max(i_lower, 1), min(i_upper, dimen%id_cloud_top-1)
      ii = n_layer+1-i
      do l=1, n_profile
        field(l, i_p) = 0.0_RealK
      end do
    end do
    do i=max(i_lower, dimen%id_cloud_top), min(i_upper, n_layer)
      ii = n_layer+1-i
      do l=1, n_profile
        field(l, i_p) = cld%condensed_dim_char(l, i, k)
      end do
    end do
  end if

end subroutine set_cloud_dim


!------------------------------------------------------------------------------
subroutine set_cloud_mmr(field)

  implicit none

  real(RealK), intent(inout), pointer :: field(:, :)
  integer :: i_lower, i_upper

  if (associated(field)) then
    if (associated(i_p, ii)) then
      ! Field output is inverted
      i_lower = n_layer + 1 - ubound(field, 2)
      i_upper = n_layer + 1 - lbound(field, 2)
    else
      i_lower = lbound(field, 2)
      i_upper = ubound(field, 2)
    end if
    ! Fill diagnostic between requested layers
    do i=max(i_lower, 1), min(i_upper, dimen%id_cloud_top-1)
      ii = n_layer+1-i
      do l=1, n_profile
        field(l, i_p) = 0.0_RealK
      end do
    end do
    do i=max(i_lower, dimen%id_cloud_top), min(i_upper, n_layer)
      ii = n_layer+1-i
      do l=1, n_profile
        field(l, i_p) = cld%condensed_mix_ratio(l, i, k)
      end do
    end do
  end if

end subroutine set_cloud_mmr


!------------------------------------------------------------------------------
subroutine set_cloud_frac(field)

  implicit none

  real(RealK), intent(inout), pointer :: field(:, :)
  integer :: i_lower, i_upper

  if (associated(field)) then
    if (associated(i_p, ii)) then
      ! Field output is inverted
      i_lower = n_layer + 1 - ubound(field, 2)
      i_upper = n_layer + 1 - lbound(field, 2)
    else
      i_lower = lbound(field, 2)
      i_upper = ubound(field, 2)
    end if
    ! Fill diagnostic between requested layers
    do i=max(i_lower, 1), min(i_upper, dimen%id_cloud_top-1)
      ii = n_layer+1-i
      do l=1, n_profile
        field(l, i_p) = 0.0_RealK
      end do
    end do
    do i=max(i_lower, dimen%id_cloud_top), min(i_upper, n_layer)
      ii = n_layer+1-i
      do l=1, n_profile
        field(l, i_p) = cld%w_cloud(l, i) * &
          cld%frac_cloud(l, i, cld%i_cloud_type(k))
      end do
    end do
  end if

end subroutine set_cloud_frac

end subroutine set_diag
end module socrates_set_diag
