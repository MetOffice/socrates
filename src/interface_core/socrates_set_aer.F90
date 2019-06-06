! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Set the variables in the Socrates aerosol type
!
!------------------------------------------------------------------------------
module socrates_set_aer
implicit none
character(len=*), parameter, private :: ModuleName = 'SOCRATES_SET_AER'
contains

subroutine set_aer(aer, control, dimen, spectrum, &
  n_profile, n_layer, n_aer_mode, &
  aer_mix_ratio, aer_absorption, aer_scattering, aer_asymmetry)

use def_aer,      only: StrAer, allocate_aer, allocate_aer_prsc
use def_control,  only: StrCtrl
use def_dimen,    only: StrDim
use def_spectrum, only: StrSpecData
use realtype_rd,  only: RealK

implicit none


! Aerosol properties:
type(StrAer),      intent(out) :: aer

! Control options:
type(StrCtrl),     intent(in)  :: control

! Dimensions:
type(StrDim),      intent(in)  :: dimen

! Spectral data:
type(StrSpecData), intent(in)  :: spectrum

integer, intent(in) :: n_profile
integer, intent(in) :: n_layer

integer, intent(in), optional :: n_aer_mode
!   Number of aerosol modes

real(RealK), intent(in), optional :: aer_mix_ratio(:, :, :)
!   Mass-mixing ratio (n_profile, n_layer, n_mode)
real(RealK), intent(in), optional :: aer_absorption(:, :, :, :)
!   Aerosol absorption (n_profile, n_layer, n_mode, n_band)
real(RealK), intent(in), optional :: aer_scattering(:, :, :, :)
!   Aerosol scattering (n_profile, n_layer, n_mode, n_band)
real(RealK), intent(in), optional :: aer_asymmetry(:, :, :, :)
!   Aerosol asymmetry (n_profile, n_layer, n_mode, n_band)


! Allocate structure for the core radiation code interface
call allocate_aer(aer, dimen, spectrum)
call allocate_aer_prsc(aer, dimen, spectrum)

if (present(n_aer_mode)) aer%n_mode = n_aer_mode
if (present(aer_mix_ratio)) aer%mode_mix_ratio = aer_mix_ratio
if (present(aer_absorption)) aer%mode_absorption = aer_absorption
if (present(aer_scattering)) aer%mode_scattering = aer_scattering
if (present(aer_asymmetry)) aer%mode_asymmetry = aer_asymmetry

end subroutine set_aer
end module socrates_set_aer
