! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
! @brief Define the Socrates diagnostic output type

module socrates_def_diag

use realtype_rd, only: RealK

implicit none

type :: StrDiag

real(RealK), pointer :: heating_rate(:,:) => null()
! Heating rate, Ks-1 (n_profile, n_layer)

real(RealK), pointer :: flux_direct(:,:) => null()
! Direct (unscattered) downwards flux, Wm-2 (n_profile, 0:n_layer)

real(RealK), pointer :: flux_down(:,:) => null()
! Downwards flux, Wm-2 (n_profile, 0:n_layer)

real(RealK), pointer :: flux_up(:,:) => null()
! Upwards flux, Wm-2 (n_profile, 0:n_layer)

real(RealK), pointer :: flux_up_tile(:,:) => null()
! Upwards flux on tiles, Wm-2 (n_profile, n_tile)

real(RealK), pointer :: flux_up_blue_tile(:,:) => null()
! Upwards blue flux on tiles, Wm-2 (n_profile, n_tile)

real(RealK), pointer :: flux_direct_blue_surf(:) => null()
! Direct blue flux at the surface, Wm-2 (n_profile)

real(RealK), pointer :: flux_down_blue_surf(:) => null()
! Total downward blue flux at the surface, Wm-2 (n_profile)

real(RealK), pointer :: total_cloud_cover(:) => null()
! Total cloud cover (n_profile)

real(RealK), pointer :: total_cloud_fraction(:,:) => null()
! Total cloud fraction in layers (n_profile, n_layer)

real(RealK), pointer :: liq_frac(:,:) => null()
! Liquid cloud fraction (n_profile, n_layer)

real(RealK), pointer :: liq_conv_frac(:,:) => null()
! Liquid convective cloud fraction (n_profile, n_layer)

real(RealK), pointer :: liq_incloud_mmr(:,:) => null()
! Liquid in-cloud mean mixing ratio (n_profile, n_layer)

real(RealK), pointer :: liq_inconv_mmr(:,:) => null()
! Liquid convective in-cloud mean mixing ratio (n_profile, n_layer)

real(RealK), pointer :: liq_dim(:,:) => null()
! Cloud droplet effective dimension (n_profile, n_layer)

real(RealK), pointer :: liq_conv_dim(:,:) => null()
! Convective cloud droplet effective dimension (n_profile, n_layer)

real(RealK), pointer :: ice_frac(:,:) => null()
! Ice cloud fraction (n_profile, n_layer)

real(RealK), pointer :: ice_conv_frac(:,:) => null()
! Ice convective cloud fraction (n_profile, n_layer)

real(RealK), pointer :: ice_incloud_mmr(:,:) => null()
! Ice in-cloud mean mixing ratio (n_profile, n_layer)

real(RealK), pointer :: ice_inconv_mmr(:,:) => null()
! Ice convective in-cloud mean mixing ratio (n_profile, n_layer)

real(RealK), pointer :: ice_dim(:,:) => null()
! Cloud ice-crystal effective dimension (n_profile, n_layer)

real(RealK), pointer :: ice_conv_dim(:,:) => null()
! Convective cloud ice-crystal effective dimension (n_profile, n_layer)

real(RealK), pointer :: aerosol_optical_depth(:,:,:) => null()
! Total aerosol optical depth (n_profile, n_layer, n_band)

real(RealK), pointer :: aerosol_scat_optical_depth(:,:,:) => null()
! Total aerosol scattering optical depth (n_profile, n_layer, n_band)

real(RealK), pointer :: aerosol_asymmetry_scat(:,:,:) => null()
! Total aerosol asymmetry weighted by scattering optical depth

end type StrDiag

end module socrates_def_diag
