! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
! @brief Update radiative fluxes using simple correction schemes

module socrates_bones

implicit none
character(len=*), parameter, private :: ModuleName = 'SOCRATES_BONES'
contains

! The bare "bones" of radiative transfer calculations. In ancient China,
! the future weather was predicted by reading the cracks that appeared
! when oracle bones were heated. Not particularly accurate, but simple
! and cheap.

subroutine bones(n_profile, n_layer, n_tile, &
  l_cos_zen_correction, cos_zen_rts, lit_frac_rts, cos_zen_mts, lit_frac_mts, &
  l_trans_zen_correction, flux_direct_toa_rts, l_orog_corr_rts, orog_corr_rts, &
  l_grey_emis_correction, grey_albedo_tile, t_tile, &
  grey_albedo_tile_1d, t_tile_1d, &
  l_debug, i_profile_debug, &
  flux_direct_rts, flux_down_rts, flux_up_rts, heating_rate_rts, &
  flux_up_tile_rts, flux_up_blue_tile_rts, &
  flux_direct_surf_rts, flux_down_surf_rts, &
  flux_direct_blue_surf_rts, flux_down_blue_surf_rts, &
  flux_direct_1d_rts, flux_down_1d_rts, flux_up_1d_rts, heating_rate_1d_rts, &
  flux_up_tile_1d_rts, flux_up_blue_tile_1d_rts, &
  flux_direct_mts, flux_down_mts, flux_up_mts, heating_rate_mts, &
  flux_up_tile_mts, flux_up_blue_tile_mts, &
  flux_direct_surf_mts, flux_down_surf_mts, &
  flux_direct_blue_surf_mts, flux_down_blue_surf_mts, &
  flux_direct_1d_mts, flux_down_1d_mts, flux_up_1d_mts, heating_rate_1d_mts, &
  flux_up_tile_1d_mts, flux_up_blue_tile_1d_mts)

use realtype_rd, only: RealK
use rad_ccf, only: stefan_boltzmann

implicit none

integer, intent(in) :: n_profile
!   Number of columns to operate on
integer, intent(in) :: n_layer
!   Number of layers for radiation
integer, intent(in), optional :: n_tile
!   Number of surface tiles

logical, intent(in), optional :: l_cos_zen_correction
!   Apply simple solar zenith angle correction
real(RealK), intent(in), optional :: cos_zen_rts(n_profile)
!   Mean cosine of solar zenith angle over lit fraction of radiation timestep
real(RealK), intent(in), optional :: lit_frac_rts(n_profile)
!   Lit fraction of radiation timestep
real(RealK), intent(in), optional :: cos_zen_mts(n_profile)
!   Mean cosine of solar zenith angle over lit fraction of model timestep
real(RealK), intent(in), optional :: lit_frac_mts(n_profile)
!   Lit fraction of model timestep

logical, intent(in), optional :: l_trans_zen_correction
!   Apply transmission based solar zenith angle correction, DOI 10.1002/qj.385
real(RealK), intent(in), optional :: flux_direct_toa_rts(n_profile)
!   Direct flux at top-of-atmosphere over radiation timestep
logical, intent(in), optional :: l_orog_corr_rts
!   Orographic correction applied for the radiation timestep
real(RealK), intent(in), optional :: orog_corr_rts(n_profile)
!   Orographic correction factor for the radiation timestep, DOI 10.1002/qj.956

logical, intent(in), optional :: l_grey_emis_correction
!   Apply surface temperature correction with grey emissivity per tile
real(RealK), intent(in), optional :: grey_albedo_tile(:, :)
!   Grey albedo of tiles (n_profile, n_tile)
real(RealK), intent(in), optional :: t_tile(:, :)
!   Tile temperatures (n_profile, n_tile)
real(RealK), intent(in), optional :: grey_albedo_tile_1d(:)
!   1d grey albedo of tiles (n_tile)
real(RealK), intent(in), optional :: t_tile_1d(:)
!   1d tile temperatures (n_tile)

logical, intent(in), optional :: l_debug
integer, intent(in), optional :: i_profile_debug
!   Options for outputting debugging information

! Input radiation timestep fields:
real(RealK), intent(in), optional :: flux_direct_rts(n_profile, 0:n_layer)
real(RealK), intent(in), optional :: flux_direct_1d_rts(0:n_layer)
!   Direct (unscattered) downwards flux (Wm-2)
real(RealK), intent(in), optional :: flux_down_rts(n_profile, 0:n_layer)
real(RealK), intent(in), optional :: flux_down_1d_rts(0:n_layer)
!   Downwards flux (Wm-2)
real(RealK), intent(in), optional :: flux_up_rts(n_profile, 0:n_layer)
real(RealK), intent(in), optional :: flux_up_1d_rts(0:n_layer)
!   Upwards flux (Wm-2)
real(RealK), intent(in), optional :: heating_rate_rts(n_profile, n_layer)
real(RealK), intent(in), optional :: heating_rate_1d_rts(n_layer)
!   Heating rate (Ks-1)
real(RealK), intent(in), optional :: flux_up_tile_rts(:, :)
real(RealK), intent(in), optional :: flux_up_tile_1d_rts(:)
!   Upwards flux on tiles (Wm-2) (n_profile, n_tile) and (n_tile)
real(RealK), intent(in), optional :: flux_up_blue_tile_rts(:, :)
real(RealK), intent(in), optional :: flux_up_blue_tile_1d_rts(:)
!   Upwards blue flux on tiles (Wm-2)
real(RealK), intent(in), optional :: flux_direct_surf_rts(n_profile)
!   Direct flux at the surface
real(RealK), intent(in), optional :: flux_down_surf_rts(n_profile)
!   Total downward flux at the surface
real(RealK), intent(in), optional :: flux_direct_blue_surf_rts(n_profile)
!   Direct blue flux at the surface
real(RealK), intent(in), optional :: flux_down_blue_surf_rts(n_profile)
!   Total downward blue flux at the surface

! Output model timestep fields:
real(RealK), intent(out), optional :: flux_direct_mts(n_profile, 0:n_layer)
real(RealK), intent(out), optional :: flux_direct_1d_mts(0:n_layer)
!   Direct (unscattered) downwards flux (Wm-2)
real(RealK), intent(out), optional :: flux_down_mts(n_profile, 0:n_layer)
real(RealK), intent(out), optional :: flux_down_1d_mts(0:n_layer)
!   Downwards flux (Wm-2)
real(RealK), intent(out), optional :: flux_up_mts(n_profile, 0:n_layer)
real(RealK), intent(out), optional :: flux_up_1d_mts(0:n_layer)
!   Upwards flux (Wm-2)
real(RealK), intent(out), optional :: heating_rate_mts(n_profile, n_layer)
real(RealK), intent(out), optional :: heating_rate_1d_mts(n_layer)
!   Heating rate (Ks-1)
real(RealK), intent(out), optional :: flux_up_tile_mts(:, :)
real(RealK), intent(out), optional :: flux_up_tile_1d_mts(:)
!   Upwards flux on tiles (Wm-2) (n_profile, n_tile) and (n_tile)
real(RealK), intent(out), optional :: flux_up_blue_tile_mts(:, :)
real(RealK), intent(out), optional :: flux_up_blue_tile_1d_mts(:)
!   Upwards blue flux on tiles (Wm-2)
real(RealK), intent(out), optional :: flux_direct_surf_mts(n_profile)
!   Direct flux at the surface
real(RealK), intent(out), optional :: flux_down_surf_mts(n_profile)
!   Total downward flux at the surface
real(RealK), intent(out), optional :: flux_direct_blue_surf_mts(n_profile)
!   Direct blue flux at the surface
real(RealK), intent(out), optional :: flux_down_blue_surf_mts(n_profile)
!   Total downward blue flux at the surface

! Local variables
integer :: l, i
real(RealK) :: trans_zen_correction(n_profile)
real(RealK) :: orog_corr(n_profile)
real(RealK) :: scaling(n_profile)
real(RealK) :: eps = epsilon(1.0_RealK)


if (present(l_orog_corr_rts)) then
  if (l_orog_corr_rts) then
    ! If the orographic correction has been applied to the surface fluxes
    ! this needs to be accounted for in the transmission-based solar zenith
    ! angle correction.
    orog_corr = orog_corr_rts
  else
    orog_corr = 1.0_RealK
  end if
else
  orog_corr = 1.0_RealK
end if


trans_zen_correction = 1.0_RealK
if (present(l_trans_zen_correction)) then
if (l_trans_zen_correction) then
  ! Transmission-based solar zenith angle correction for surface fluxes
  ! as described in Manners et al 2009, section 3.3 (DOI: 10.1002/qj.385).
  where (cos_zen_rts > eps .and. cos_zen_mts > eps .and. &
         flux_direct_surf_rts > eps .and. &
         orog_corr*flux_direct_toa_rts > flux_direct_surf_rts .and. &
         orog_corr > sqrt(eps))
    trans_zen_correction = 1.0_RealK &
      + (orog_corr - 0.5_RealK) * (flux_direct_toa_rts &
      * (flux_direct_surf_rts/(orog_corr*flux_direct_toa_rts)) &
      **(cos_zen_rts/cos_zen_mts) &
      - flux_direct_surf_rts/orog_corr) / flux_down_surf_rts
  end where
end if
end if


if (present(l_cos_zen_correction)) then
if (l_cos_zen_correction) then
  ! A simple solar zenith angle correction that scales the fluxes and
  ! heating rates by the change in the cosine of the solar zenith angle.
  where (cos_zen_rts*lit_frac_rts > eps)
    scaling = cos_zen_mts*lit_frac_mts / (cos_zen_rts*lit_frac_rts)
  elsewhere
    scaling = 0.0_RealK
  end where

  call scale_field( flux_direct_rts,  flux_direct_mts  )
  call scale_field( flux_down_rts,    flux_down_mts    )
  call scale_field( flux_up_rts,      flux_up_mts      )
  call scale_field( heating_rate_rts, heating_rate_mts )

  call scale_field_1d( flux_direct_1d_rts,  flux_direct_1d_mts  )
  call scale_field_1d( flux_down_1d_rts,    flux_down_1d_mts    )
  call scale_field_1d( flux_up_1d_rts,      flux_up_1d_mts      )
  call scale_field_1d( heating_rate_1d_rts, heating_rate_1d_mts )


  ! Surface fields may also be adjusted for the transmission-based solar
  ! zenith angle correction. Note: we apply the same correction to the
  ! total and direct fluxes here to maintain the ratio of direct to diffuse
  ! flux over the radiation timestep. Using the separate correction to the
  ! direct flux as outlined in Manners et al 2009 can in some cases reduce
  ! the accuracy of this ratio.
  scaling = scaling * trans_zen_correction

  call scale_field( flux_up_tile_rts,      flux_up_tile_mts      )
  call scale_field( flux_up_blue_tile_rts, flux_up_blue_tile_mts )

  call scale_field_1d( flux_up_tile_1d_rts,      flux_up_tile_1d_mts      )
  call scale_field_1d( flux_up_blue_tile_1d_rts, flux_up_blue_tile_1d_mts )

  call scale_field_surf( flux_direct_surf_rts,      flux_direct_surf_mts      )
  call scale_field_surf( flux_down_surf_rts,        flux_down_surf_mts        )
  call scale_field_surf( flux_direct_blue_surf_rts, flux_direct_blue_surf_mts )
  call scale_field_surf( flux_down_blue_surf_rts,   flux_down_blue_surf_mts   )

  if (present(l_debug)) then
    if (l_debug) then
      write(9000,'(A)') 'heating_rate_1d_rts, heating_rate_1d_mts'
      do i=1, n_layer
        write(9000,'(2(1pe16.8))') &
          heating_rate_1d_rts(i), heating_rate_1d_mts(i)
      end do
    end if
  end if

end if
end if


if (present(l_grey_emis_correction) .and. &
    present(flux_down_surf_rts)) then
if (l_grey_emis_correction) then
  ! A surface temperature correction with grey emissivity per tile.
  ! Only the upward tiled fluxes are corrected.
  scaling = 1.0_RealK

  call scale_field( flux_down_rts,    flux_down_mts    )
  call scale_field( heating_rate_rts, heating_rate_mts )

  call scale_field_1d( flux_down_1d_rts,    flux_down_1d_mts    )
  call scale_field_1d( heating_rate_1d_rts, heating_rate_1d_mts )

  call scale_field_surf( flux_down_surf_rts, flux_down_surf_mts )

  if (present(flux_up_tile_mts) .and. present(n_tile) .and. &
      present(grey_albedo_tile) .and. present(t_tile)) then
    do i=1, n_tile
      flux_up_tile_mts(1:n_profile, i) &
        = flux_down_surf_rts(:) * grey_albedo_tile(1:n_profile, i) &
        + (1.0_RealK - grey_albedo_tile(1:n_profile, i)) &
        * stefan_boltzmann * t_tile(1:n_profile, i)**4
    end do
  end if
  if (present(flux_up_tile_1d_mts) .and. present(n_tile) .and. &
      present(grey_albedo_tile_1d) .and. present(t_tile_1d)) then
    do i=1, n_tile
      flux_up_tile_1d_mts(i) &
        = flux_down_surf_rts(1) * grey_albedo_tile_1d(i) &
        + (1.0_RealK - grey_albedo_tile_1d(i)) &
        * stefan_boltzmann * t_tile_1d(i)**4
    end do
  end if
  if (present(flux_up_tile_1d_mts) .and. present(n_tile) .and. &
      present(grey_albedo_tile) .and. present(t_tile)) then
    do i=1, n_tile
      flux_up_tile_1d_mts(i) &
        = flux_down_surf_rts(1) * grey_albedo_tile(1, i) &
        + (1.0_RealK - grey_albedo_tile(1, i)) &
        * stefan_boltzmann * t_tile(1, i)**4
    end do
  end if
end if
end if

contains


  subroutine scale_field(field_rts, field_mts)

  implicit none
  
  real(RealK), intent(in), optional :: field_rts(:, :)
  real(RealK), intent(out), optional :: field_mts(:, :)

  if (present(field_rts).and.present(field_mts)) then
    do i=1, size(field_rts, 2)
      field_mts(:, i) = field_rts(:, i) * scaling(:)
    end do
  end if

  end subroutine scale_field


  subroutine scale_field_1d(field_rts, field_mts)

  implicit none
  
  real(RealK), intent(in), optional :: field_rts(:)
  real(RealK), intent(out), optional :: field_mts(:)

  if (present(field_rts).and.present(field_mts)) then
    field_mts(:) = field_rts(:) * scaling(1)
  end if

  end subroutine scale_field_1d


  subroutine scale_field_surf(field_rts, field_mts)

  implicit none
  
  real(RealK), intent(in), optional :: field_rts(:)
  real(RealK), intent(out), optional :: field_mts(:)

  if (present(field_rts).and.present(field_mts)) then
    field_mts(:) = field_rts(:) * scaling(:)
  end if

  end subroutine scale_field_surf


end subroutine bones
end module socrates_bones
