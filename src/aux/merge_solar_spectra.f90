! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Merge a high-resolution solar spectrum with a low-resolution spectrum
! keeping the same integrated flux as the low-resolution spectrum
!------------------------------------------------------------------------------
program merge_solar_spectra

use realtype_rd, only: RealK
use def_solarspec, only: StrSolarSpec

implicit none

type (StrSolarSpec) :: sol, sol_lr, sol_hr, sol_hr_in
! Stellar spectra

character(len=256) :: sol_lr_file, sol_hr_file, outfile, arg

logical :: l_sol_lr_file=.false., l_sol_hr_file=.false., l_outfile=.false.
logical :: l_single_scaling=.false.
logical :: l_hr_min=.false., l_hr_max=.false.

integer :: ierr, ios, iu_sol_lr, iu_sol_hr
integer :: i, j, jmin, jmax, ii, jj
integer, allocatable :: i_short(:), i_long(:)

real(RealK) :: sol_lr_binned, sol_hr_binned, scale_hr
real(RealK) :: hr_min, hr_max
real(RealK) :: tol=1.0e-6_RealK


i = 0
do
  i=i+1
  if (i > command_argument_count()) exit
  call get_command_argument(i, arg)
  select case (arg)
  case ('-s','--single_scaling')
    l_single_scaling=.true.
  case ('-l','--min_wavelength')
    i=i+1
    call get_command_argument(i, arg)
    read(arg, *) hr_min
    l_hr_min=.true.
  case ('-u','--max_wavelength')
    i=i+1
    call get_command_argument(i, arg)
    read(arg, *) hr_max
    l_hr_max=.true.
  case default
    if (l_sol_hr_file) then
      ! If the high-res solar file has been provided, the third file is
      ! the output solfile
      outfile = arg
      l_outfile = .true.
    else if (l_sol_lr_file) then
      ! If the low-res solar file has been provided, the second file is
      ! the high-res solfile
      sol_hr_file = arg
      l_sol_hr_file = .true.
    else
      ! First file given is the low-res solar file
      sol_lr_file = arg
      l_sol_lr_file = .true.
    end if
  end select
end do
if (.not.l_outfile) then
  write(*, '(a)') 'Usage: merge_solar_spectra sol_low_res sol_high_res outfile'
  write(*, '(27x,a)') '[--single_scaling]'
  write(*, '(27x,a)') '[--min_wavelength] <high_res min wavelength cutoff (m)>'
  write(*, '(27x,a)') '[--max_wavelength] <high_res max wavelength cutoff (m)>'
  stop
end if

call get_free_unit(ierr, iu_sol_lr)
open(iu_sol_lr, file=trim(sol_lr_file), iostat=ios, status='old')
call read_solar_spectrum_data(iu_sol_lr, sol_lr, ierr)

call get_free_unit(ierr, iu_sol_hr)
open(iu_sol_hr, file=trim(sol_hr_file), iostat=ios, status='old')
call read_solar_spectrum_data(iu_sol_hr, sol_hr_in, ierr)

if (.not.sol_lr%l_binned .or. .not.sol_hr_in%l_binned) then
  stop 'Requires input solar spectra to be in binned format'
end if

! Split high-res bins at the edges of low-res bins
sol_hr%n_points = sol_lr%n_points + sol_hr_in%n_points
sol_hr%l_binned = .true.
allocate(sol_hr%wavelength(sol_hr%n_points))
allocate(sol_hr%bandbnds(2, sol_hr%n_points))
allocate(sol_hr%bandsize(sol_hr%n_points))
allocate(sol_hr%irrad(sol_hr%n_points))
j=0
do jj=1, sol_hr_in%n_points
  j=j+1
  ii=0
  do i=1, sol_lr%n_points
    if ( sol_lr%bandbnds(1, i) > &
         sol_hr_in%bandbnds(1, jj) + tol*sol_hr_in%bandsize(jj) .and. &
         sol_lr%bandbnds(1, i) < &
         sol_hr_in%bandbnds(2, jj) - tol*sol_hr_in%bandsize(jj) ) then
      ii=i
      exit
    end if
  end do
  if (ii > 0) then
    sol_hr%bandbnds(1, j) = sol_hr_in%bandbnds(1, jj)
    sol_hr%bandbnds(2, j) = sol_lr%bandbnds(1, ii)
    sol_hr%bandsize(j) = sol_hr%bandbnds(2, j) - sol_hr%bandbnds(1, j)
    sol_hr%wavelength(j) = sol_hr%bandbnds(1, j) &
                         + sol_hr%bandsize(j) / 2.0_RealK
    sol_hr%irrad(j) = sol_hr_in%irrad(jj)
    j=j+1
    sol_hr%bandbnds(1, j) = sol_lr%bandbnds(1, ii)
    sol_hr%bandbnds(2, j) = sol_hr_in%bandbnds(2, jj)
    sol_hr%bandsize(j) = sol_hr%bandbnds(2, j) - sol_hr%bandbnds(1, j)
    sol_hr%wavelength(j) = sol_hr%bandbnds(1, j) &
                         + sol_hr%bandsize(j) / 2.0_RealK
    sol_hr%irrad(j) = sol_hr_in%irrad(jj)
  else
    sol_hr%wavelength(j) = sol_hr_in%wavelength(jj)
    sol_hr%bandbnds(:, j) = sol_hr_in%bandbnds(:, jj)
    sol_hr%bandsize(j) = sol_hr_in%bandsize(jj)
    sol_hr%irrad(j) = sol_hr_in%irrad(jj)
  end if
end do
sol_hr%n_points = j

! Find range of high-res spectrum that is within the specified cutoff
jmin=1
do j=1, sol_hr%n_points
  if (l_hr_min .and. sol_hr%wavelength(j) < hr_min) jmin=j+1
end do
jmax=sol_hr%n_points
do j=sol_hr%n_points, 1, -1
  if (l_hr_max .and. sol_hr%wavelength(j) > hr_max) jmax=j-1
end do

! Find high-res bins corresponding to each low-res bin
allocate(i_short(sol_lr%n_points))
allocate(i_long(sol_lr%n_points))
do i=1, sol_lr%n_points
  i_short(i)=0
  i_long(i)=0
  do j=jmin, jmax
    if ( abs( sol_lr%bandbnds(1, i) - sol_hr%bandbnds(1, j) ) &
         < tol*sol_hr%bandsize(j) ) i_short(i)=j
    if ( abs( sol_lr%bandbnds(2, i) - sol_hr%bandbnds(2, j) ) &
         < tol*sol_hr%bandsize(j) ) i_long(i)=j
  end do
  if (i_short(i) == i_long(i) .or. i_long(i) == 0) then
    i_short(i)=0
  end if
end do

! Allocate merged spectrum using maximum number of points
sol%n_points = sol_lr%n_points + sol_hr%n_points
sol%l_binned = .true.
allocate(sol%wavelength(sol%n_points))
allocate(sol%bandbnds(2, sol%n_points))
allocate(sol%bandsize(sol%n_points))
allocate(sol%irrad(sol%n_points))
sol%n_points = 0

! Fill short wavelengths with high-res values where low-res is not available
do j=jmin, jmax
  if ( sol_hr%bandbnds(2, j) &
     < sol_lr%bandbnds(1, 1) + tol*sol_hr%bandsize(j) ) then
    sol%n_points = sol%n_points + 1
    sol%wavelength(sol%n_points) = sol_hr%wavelength(j)
    sol%bandbnds(:, sol%n_points) = sol_hr%bandbnds(:, j)
    sol%bandsize(sol%n_points) = sol_hr%bandsize(j)
    sol%irrad(sol%n_points) = sol_hr%irrad(j)
  end if
end do

if (l_single_scaling) then
  ! If the high-res spectrum is to be scaled by a single factor then this
  ! is determined by matching the integrated flux over the low-res spectral
  ! bins that entirely overlap.
  sol_lr_binned=0.0_RealK
  sol_hr_binned=0.0_RealK
  do i=1, sol_lr%n_points
    if (i_short(i) /= 0) then
      do j=i_short(i), i_long(i)
        sol_lr_binned = sol_lr_binned + sol_lr%irrad(i)*sol_hr%bandsize(j)
        sol_hr_binned = sol_hr_binned + sol_hr%irrad(j)*sol_hr%bandsize(j)
      end do
    end if
  end do
  scale_hr = sol_lr_binned / sol_hr_binned
end if

! Fill the merged spectrum
do i=1, sol_lr%n_points
  if (i_short(i) == 0) then
    ! Use the low-res spectrum where the high-res does not cover the
    ! whole low-res bin
    sol%n_points = sol%n_points + 1
    sol%wavelength(sol%n_points) = sol_lr%wavelength(i)
    sol%bandbnds(:, sol%n_points) = sol_lr%bandbnds(:, i)
    sol%bandsize(sol%n_points) = sol_lr%bandsize(i)
    sol%irrad(sol%n_points) = sol_lr%irrad(i)
  else
    ! Scale the high-res spectrum values 
    if (.not.l_single_scaling) then
      ! Scale high-res irradiance per low-res bin
      scale_hr = sol_lr%irrad(i) * sum( sol_hr%bandsize(i_short(i):i_long(i)) ) &
                                 / sum( sol_hr%irrad(i_short(i):i_long(i)) &
                                      * sol_hr%bandsize(i_short(i):i_long(i)) )
    end if
    do j=i_short(i), i_long(i)
      sol%n_points = sol%n_points + 1
      sol%wavelength(sol%n_points) = sol_hr%wavelength(j)
      sol%bandbnds(:, sol%n_points) = sol_hr%bandbnds(:, j)
      sol%bandsize(sol%n_points) = sol_hr%bandsize(j)
      sol%irrad(sol%n_points) = sol_hr%irrad(j) * scale_hr
    end do
  end if
end do

call write_solar_spectrum(trim(outfile), sol, ierr)

end program merge_solar_spectra
