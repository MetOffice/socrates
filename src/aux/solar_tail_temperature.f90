! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Find an effective solar temperature for long wavelength extrapolation
! by fitting a Plankian to the tail of the spectrum
program solar_tail_temperature

use realtype_rd, only: RealK
use def_solarspec, only: StrSolarSpec
use solar_intensity_nu_mod, only: solar_intensity_nu

implicit none

type (StrSolarSpec) :: sol
! Stellar spectrum

integer :: ierr, ios, iu_solar, n_start, n_end, i
character(len=256) :: input_file, output_file
real(RealK) :: tail_irradiance, sol_irradiance

integer, parameter :: n_tail_bins = 3
! Number of spectrum bins in the tail over which to fit the Planck function
integer, parameter :: n_iter = 10000
! Max number of iterations to converge on effective tail temperature
real(RealK), parameter :: tol = 1.0e-6_RealK
! Tolerance on flux error

real(RealK), external :: planck_tail
real(RealK), external :: trapezoid

call get_command_argument(1, input_file)
call get_command_argument(2, output_file)

call get_free_unit(ierr, iu_solar)
open(iu_solar, file=trim(input_file), iostat=ios, status='old')
call read_solar_spectrum_data(iu_solar, sol, ierr)

do i=1, n_iter
  if (sol%l_binned) then
    n_start = sol%n_points - n_tail_bins + 1
    n_end = sol%n_points
    tail_irradiance = planck_tail(sol,sol%bandbnds(1, n_start))&
                    - planck_tail(sol,sol%bandbnds(2, n_end))
    sol_irradiance = sum(sol%irrad(n_start:n_end) * sol%bandsize(n_start:n_end))
  else
    n_start = sol%n_points - n_tail_bins
    n_end = sol%n_points
    tail_irradiance = planck_tail(sol,sol%wavelength(n_start)) &
                    - planck_tail(sol,sol%wavelength(n_end))
    sol_irradiance = trapezoid(n_tail_bins+1, sol%wavelength(n_start:n_end), &
                                              sol%irrad(n_start:n_end))
  end if
  if (abs(tail_irradiance - sol_irradiance) < sol_irradiance*tol) then
    write(*,'(a,i0,a)') ' Reached tolerance after ', i, ' iterations'
    write(*,'(a,1pe12.5,a)') ' Flux diff: ', &
      tail_irradiance-sol_irradiance, ' Wm-2'
    exit
  end if
  if (i == n_iter) then
    write(*,'(a,i0,a)') ' Failed to converge after ', i, ' iterations'
    write(*,'(a,1pe12.5,a)') ' Flux diff: ', &
      tail_irradiance-sol_irradiance, ' Wm-2'    
    exit
  end if
  sol%t_effective = sol%t_effective * sqrt(sol_irradiance / tail_irradiance)
end do

call write_solar_spectrum(trim(output_file), sol, ierr)

end program solar_tail_temperature
