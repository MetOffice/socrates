! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
! @brief Set the spectral file data

module socrates_set_spectrum

use def_spectrum, only: StrSpecData

implicit none
private
public :: set_spectrum, compress_spectrum, set_weight_blue, get_spectrum, &
          spectrum_array_name, spectrum_array

integer, parameter :: specnamelength = 64
character(len=specnamelength), allocatable, save :: spectrum_array_name(:)
type(StrSpecData), allocatable, target, save :: spectrum_array(:)

character(len=*), parameter :: ModuleName='SOCRATES_SET_SPECTRUM'

contains

subroutine set_spectrum(n_instances, spectrum, spectrum_name, spectral_file, &
  l_h2o, l_co2, l_o3, l_o2, l_n2o, l_ch4, l_so2, l_cfc11, l_cfc12, &
  l_cfc113, l_cfc114, l_hcfc22, l_hfc125, l_hfc134a, l_co, l_nh3, &
  l_tio, l_vo, l_h2, l_he, l_na, l_k, l_li, l_rb, l_cs, &
  wavelength_blue)

use filenamelength_mod, only: filenamelength
use errormessagelength_mod, only: errormessagelength
use ereport_mod, only: ereport
use rad_pcf, only: i_normal, i_err_fatal
use realtype_rd, only: RealK
use missing_data_mod, only: rmdi
use yomhook,  only: lhook, dr_hook
use parkind1, only: jprb, jpim

implicit none


! Number of instances of the spectrum type (to allocate spectrum_array)
integer, intent(in), optional :: n_instances

! Spectral data:
type(StrSpecData), intent(inout), target, optional :: spectrum
character(len=*), intent(in), optional :: spectrum_name
character(len=filenamelength), intent(in), optional :: spectral_file

logical, intent(in), optional :: &
  l_h2o, l_co2, l_o3, l_o2, l_n2o, l_ch4, l_so2, l_cfc11, l_cfc12, &
  l_cfc113, l_cfc114, l_hcfc22, l_hfc125, l_hfc134a, l_co, l_nh3, &
  l_tio, l_vo, l_h2, l_he, l_na, l_k, l_li, l_rb, l_cs

real (RealK), intent(in), optional :: wavelength_blue

! Local variables
type(StrSpecData), pointer :: spec => null()
integer, parameter :: nd_instances = 2
integer :: id_spec
integer :: ierr = i_normal
character(len=errormessagelength) :: cmessage
character(len=*), parameter :: RoutineName='SET_SPECTRUM'

integer(kind=jpim), parameter :: zhook_in  = 0
integer(kind=jpim), parameter :: zhook_out = 1
real(kind=jprb)               :: zhook_handle


if (lhook) call dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

if (.not.allocated(spectrum_array)) then
  if (present(n_instances)) then
    allocate(spectrum_array(n_instances))
    allocate(spectrum_array_name(n_instances))
    spectrum_array_name = ''
  else
    allocate(spectrum_array(nd_instances))
    allocate(spectrum_array_name(nd_instances))
    spectrum_array_name = ''
  end if
end if

if (present(spectrum_name)) then
  do id_spec=1, size(spectrum_array)
    if (spectrum_array_name(id_spec) == spectrum_name) exit
    if (spectrum_array_name(id_spec) == '') then
      spectrum_array_name(id_spec) = spectrum_name
      exit
    end if
    if (id_spec == size(spectrum_array)) then
      cmessage = 'No more instances of spectrum type available.'
      ierr=i_err_fatal
      call ereport(ModuleName//':'//RoutineName, ierr, cmessage)
    end if
  end do
  spec => spectrum_array(id_spec)
else if (present(spectrum)) then
  spec => spectrum
end if

if (present(spectrum_name).and.present(spectrum)) then
  spectrum_array(id_spec) = spectrum
else if (present(spectrum_name).or.present(spectrum)) then
  ! DEPENDS ON: read_spectrum
  call read_spectrum(spectral_file, spec)
  if (spec%solar%weight_blue(1) == rmdi) then
    call set_weight_blue(spec, wavelength_blue)
  end if
  call compress_spectrum(spec, &
    l_h2o, l_co2, l_o3, l_o2, l_n2o, l_ch4, l_so2, l_cfc11, l_cfc12, &
    l_cfc113, l_cfc114, l_hcfc22, l_hfc125, l_hfc134a, l_co, l_nh3, &
    l_tio, l_vo, l_h2, l_he, l_na, l_k, l_li, l_rb, l_cs)
end if

if (lhook) call dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

end subroutine set_spectrum


subroutine compress_spectrum(spec, &
  l_h2o, l_co2, l_o3, l_o2, l_n2o, l_ch4, l_so2, l_cfc11, l_cfc12, &
  l_cfc113, l_cfc114, l_hcfc22, l_hfc125, l_hfc134a, l_co, l_nh3, &
  l_tio, l_vo, l_h2, l_he, l_na, l_k, l_li, l_rb, l_cs)

use gas_list_pcf, only: &
  ip_h2o, ip_co2, ip_o3, ip_o2, ip_n2o, ip_ch4, ip_so2, ip_cfc11, ip_cfc12, &
  ip_cfc113, ip_cfc114, ip_hcfc22, ip_hfc125, ip_hfc134a, ip_co, ip_nh3, &
  ip_tio, ip_vo, ip_h2, ip_he, ip_na, ip_k, ip_li, ip_rb, ip_cs

implicit none

type(StrSpecData), intent(inout) :: spec

logical, intent(in), optional :: &
  l_h2o, l_co2, l_o3, l_o2, l_n2o, l_ch4, l_so2, l_cfc11, l_cfc12, &
  l_cfc113, l_cfc114, l_hcfc22, l_hfc125, l_hfc134a, l_co, l_nh3, &
  l_tio, l_vo, l_h2, l_he, l_na, l_k, l_li, l_rb, l_cs

integer :: i, j, n_band_absorb
logical :: l_retain_absorb(spec%gas%n_absorb)


! Search the spectrum to find those gases to be retained.
l_retain_absorb=.false.
do i=1, spec%gas%n_absorb
  if (retain_absorber(ip_h2o,     l_h2o    ) .or. &
      retain_absorber(ip_co2,     l_co2    ) .or. &
      retain_absorber(ip_o3,      l_o3     ) .or. &
      retain_absorber(ip_o2,      l_o2     ) .or. &
      retain_absorber(ip_n2o,     l_n2o    ) .or. &
      retain_absorber(ip_ch4,     l_ch4    ) .or. &
      retain_absorber(ip_so2,     l_so2    ) .or. &
      retain_absorber(ip_cfc11,   l_cfc11  ) .or. &
      retain_absorber(ip_cfc12,   l_cfc12  ) .or. &
      retain_absorber(ip_cfc113,  l_cfc113 ) .or. &
      retain_absorber(ip_cfc114,  l_cfc114 ) .or. &
      retain_absorber(ip_hcfc22,  l_hcfc22 ) .or. &
      retain_absorber(ip_hfc125,  l_hfc125 ) .or. &
      retain_absorber(ip_hfc134a, l_hfc134a) .or. &
      retain_absorber(ip_co,      l_co     ) .or. &
      retain_absorber(ip_nh3,     l_nh3    ) .or. &
      retain_absorber(ip_tio,     l_tio    ) .or. &
      retain_absorber(ip_vo,      l_vo     ) .or. &
      retain_absorber(ip_h2,      l_h2     ) .or. &
      retain_absorber(ip_he,      l_he     ) .or. &
      retain_absorber(ip_na,      l_na     ) .or. &
      retain_absorber(ip_k,       l_k      ) .or. &
      retain_absorber(ip_li,      l_li     ) .or. &
      retain_absorber(ip_rb,      l_rb     ) .or. &
      retain_absorber(ip_cs,      l_cs     )) then
    l_retain_absorb(i)=.true.
  end if
end do

do i=1, spec%basic%n_band
  n_band_absorb=0
  do j=1, spec%gas%n_band_absorb(i)
    if (l_retain_absorb(spec%gas%index_absorb(j, i))) then
      n_band_absorb = n_band_absorb + 1
      spec%gas%index_absorb(n_band_absorb, i) = spec%gas%index_absorb(j, i)
    end if
  end do
  spec%gas%n_band_absorb(i)=n_band_absorb
end do

contains
  logical function retain_absorber(ip_absorber, l_absorber)
    implicit none
    integer, intent(in) :: ip_absorber
    logical, intent(in), optional :: l_absorber
  
    if (present(l_absorber)) then
      retain_absorber = (spec%gas%type_absorb(i) == ip_absorber) &
        .and. l_absorber
    else
      retain_absorber = .false.
    end if
  end function retain_absorber

end subroutine compress_spectrum


subroutine set_weight_blue(spec, wavelength_blue)

use realtype_rd, only: RealK

implicit none

type(StrSpecData), intent(inout) :: spec
real (RealK), intent(in), optional :: wavelength_blue

integer :: i, j, i_exclude
real (RealK) :: total_energy_range, blue_energy_range, wl_blue

if (present(wavelength_blue)) then
  wl_blue = wavelength_blue
else
  wl_blue = 6.9e-07_RealK
end if

do i=1, spec%basic%n_band
  if (spec%basic%wavelength_long(i) < wl_blue) then
    spec%solar%weight_blue(i) = 1.0_RealK
  else if (spec%basic%wavelength_short(i) > wl_blue) then
    spec%solar%weight_blue(i) = 0.0_RealK
  else
    blue_energy_range  = 1.0_RealK / spec%basic%wavelength_short(i) &
                       - 1.0_RealK / wl_blue
    total_energy_range = 1.0_RealK / spec%basic%wavelength_short(i) &
                       - 1.0_RealK / spec%basic%wavelength_long(i)
    if (spec%basic%l_present(14)) then
      ! Remove contributions from excluded bands.
      do j=1, spec%basic%n_band_exclude(i)
        i_exclude = spec%basic%index_exclude(j, i)
        if (spec%basic%wavelength_long(i_exclude) < wl_blue) then
          blue_energy_range = blue_energy_range &
            - 1.0_RealK / spec%basic%wavelength_short(i_exclude) &
            + 1.0_RealK / spec%basic%wavelength_long(i_exclude)
        else if (spec%basic%wavelength_short(i_exclude) < wl_blue) then
          blue_energy_range = blue_energy_range &
            - 1.0_RealK / spec%basic%wavelength_short(i_exclude) &
            + 1.0_RealK / wl_blue
        end if
        total_energy_range = total_energy_range &
          - 1.0_RealK / spec%basic%wavelength_short(i_exclude) &
          + 1.0_RealK / spec%basic%wavelength_long(i_exclude)
      end do
    end if
    spec%solar%weight_blue(i) = blue_energy_range / total_energy_range
  end if
end do

end subroutine set_weight_blue


subroutine get_spectrum(spectrum_name, spectrum, &
  n_band, wavelength_short, wavelength_long, weight_blue)

use realtype_rd, only: RealK
use errormessagelength_mod, only: errormessagelength
use ereport_mod, only: ereport
use rad_pcf, only: i_normal, i_err_fatal

implicit none

character(len=*), intent(in) :: spectrum_name
type (StrSpecData), intent(out), optional :: spectrum

integer, optional, intent(out) :: n_band
real (RealK), allocatable, optional, intent(out) :: wavelength_short(:)
real (RealK), allocatable, optional, intent(out) :: wavelength_long(:)
real (RealK), allocatable, optional, intent(out) :: weight_blue(:)

! Local variables
type(StrSpecData), pointer :: spec => null()
integer :: id_spec
integer :: ierr = i_normal
character(len=errormessagelength) :: cmessage
character(len=*), parameter :: RoutineName='GET_SPECTRUM'


do id_spec=1, size(spectrum_array)
  if (spectrum_array_name(id_spec) == spectrum_name) exit
  if (id_spec == size(spectrum_array)) then
    cmessage = 'Spectrum name not found.'
    ierr=i_err_fatal
    call ereport(ModuleName//':'//RoutineName, ierr, cmessage)
  end if
end do
spec => spectrum_array(id_spec)

if (present(spectrum)) spectrum = spec
if (present(n_band)) n_band = spec%basic%n_band
if (present(wavelength_short)) then
  if (allocated(wavelength_short)) deallocate(wavelength_short)
  allocate(wavelength_short(spec%basic%n_band))
  wavelength_short = spec%basic%wavelength_short
end if
if (present(wavelength_long)) then
  if (allocated(wavelength_long)) deallocate(wavelength_long)
  allocate(wavelength_long(spec%basic%n_band))
  wavelength_long = spec%basic%wavelength_long
end if
if (present(weight_blue)) then
  if (allocated(weight_blue)) deallocate(weight_blue)
  allocate(weight_blue(spec%basic%n_band))
  weight_blue = spec%solar%weight_blue
end if

end subroutine get_spectrum

end module socrates_set_spectrum
