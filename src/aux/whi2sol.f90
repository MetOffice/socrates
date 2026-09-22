! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Mean the WHI Reference spectra over the three observation periods
!
! Whole Heliosphere Interval (WHI) Reference Spectra data come from:
! Woods et al. (2009), GRL, 36, L01101, DOI:10.1029/2008GL036373
! https://lasp.colorado.edu/lisird/data/whi_ref_spectra
!------------------------------------------------------------------------------
program whi2sol

use realtype_rd, only: RealK
use def_solarspec, only: StrSolarSpec
use solar_intensity_nu_mod, only: solar_intensity_nu

implicit none

type (StrSolarSpec) :: sol(4)
! Stellar spectrum

character(len=256) :: line, input_file, output_file, solfile

integer :: ierr, ios
integer :: iu_input
integer :: i, k, n_points
real(RealK) :: source, wavelength
real(RealK) :: obs_period(3)=[5.0_RealK, 6.0_RealK, 7.0_RealK]

call get_command_argument(1, input_file)
call get_command_argument(2, output_file)

! Read header from the input WHI reference solar spectrum file
call get_free_unit(ierr, iu_input)
open(unit=iu_input, file=input_file, iostat=ios, status='UNKNOWN')
do
  read(iu_input, '(a)', IOSTAT=ios) line
  if (ios /= 0) stop 'Error reading WHI reference file'
  if (line(1:6) == 'FORMAT') then
    read(iu_input, '(i6,/)', IOSTAT=ios) n_points
    exit
  end if
end do

! Setup solar spectrum type
do k=1, 4
  sol(k)%n_points = n_points
  sol(k)%l_binned = .true.
  allocate(sol(k)%wavelength(n_points))
  allocate(sol(k)%irrad(n_points))
  allocate(sol(k)%bandsize(n_points))
  allocate(sol(k)%bandbnds(2, n_points))
  sol(k)%bandsize = 0.1e-9_RealK
end do

! Read WHI reference solar spectrum data
do i=1, n_points
  read(iu_input, '(F8.2,3E12.4,F4.0)', IOSTAT=ios) wavelength, &
    sol(1)%irrad(i), sol(2)%irrad(i), sol(3)%irrad(i), source
  ! Calculate mean spectrum weighting by observation period
  sol(4)%irrad(i) = ( sol(1)%irrad(i) * obs_period(1) &
                    + sol(2)%irrad(i) * obs_period(2) &
                    + sol(3)%irrad(i) * obs_period(3) ) &
                  / sum(obs_period)
  do k=1, 4
    sol(k)%wavelength(i) = wavelength * 1.0e-9_RealK
    sol(k)%irrad(i) = sol(k)%irrad(i) * 1.0e9_RealK
    sol(k)%bandbnds(1, i) = sol(k)%wavelength(i) - 0.5_RealK*sol(k)%bandsize(i)
    sol(k)%bandbnds(2, i) = sol(k)%wavelength(i) + 0.5_RealK*sol(k)%bandsize(i)
  end do
end do

! Set boundary of lowest wavelength bin to an arbitrary small value > 0
do k=1, 4
  sol(k)%bandbnds(1, 1) = 1.0e-12_RealK
end do

! Write out solar spectra for the three WHI periods
do k=1, 4
  if (k==1) then
    solfile = trim(output_file)//'_dark'
  else if (k==2) then
    solfile = trim(output_file)//'_bright'
  else if (k==3) then
    solfile = trim(output_file)//'_quiet'
  else
    solfile = trim(output_file)//'_mean'
  end if
  call write_solar_spectrum(trim(solfile), sol(k), ierr)
  deallocate(sol(k)%bandbnds)
  deallocate(sol(k)%bandsize)
  deallocate(sol(k)%irrad)
  deallocate(sol(k)%wavelength)
end do

end program whi2sol
