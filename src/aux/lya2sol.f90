! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Calculate mean Lyman-alpha solar spectra for the WHI observation periods
!
! High resolution input spectra for this routine can be obtained from:
! Kretzschmar, Snow and Curdt (2018), GRL, 45, 2138. DOI:10.1002/2017GL076318
! https://lasp.colorado.edu/lisird/data/lyman_alpha_model_ssi
!------------------------------------------------------------------------------
program lya2sol

use realtype_rd, only: RealK
use def_solarspec, only: StrSolarSpec
use netcdf

implicit none

type (StrSolarSpec) :: sol(4)
! Stellar spectrum

character(len=256) :: input_file, output_file, solfile
character(len=15) :: wl_name='dim1_WAVELENGTH'
character(len=7) :: jd_name='dim1_JD'
integer :: ncid, dimid1, dimid2, varid, wl_len, jd_len
integer, parameter :: jd_start_WHI_sunspot_dark = 2454551 ! 25th March 2008
integer, parameter :: jd_end_WHI_sunspot_dark = 2454555 ! 29th March 2008
integer, parameter :: jd_start_WHI_faculae_bright = 2454556 ! 30th March 2008
integer, parameter :: jd_end_WHI_faculae_bright = 2454561 ! 4th April 2008
integer, parameter :: jd_start_WHI_quiet_sun = 2454567 ! 10th April 2008
integer, parameter :: jd_end_WHI_quiet_sun = 2454573 ! 16th April 2008
integer :: jd_start, jd_end
real(RealK) :: jd_total_length

integer :: ierr
integer :: i, j, k
integer, allocatable :: jd(:, :)
real (RealK), allocatable :: wl(:, :), irradiance(:, :, :)

call get_command_argument(1, input_file)
call get_command_argument(2, output_file)

! Open the file for reading
call nf(nf90_open(trim(input_file),NF90_NOWRITE,ncid))

! Get length of wavelength dimension
call nf(nf90_inq_dimid(ncid, wl_name, dimid1))
call nf(nf90_inquire_dimension(ncid, dimid1, wl_name, wl_len))
print*, 'Waelengths:', wl_len

! Get length of Julian Day dimension
call nf(nf90_inq_dimid(ncid, jd_name, dimid2))
call nf(nf90_inquire_dimension(ncid, dimid2, jd_name, jd_len))
print*, 'Julian days:', jd_len

! Allocate arrays
allocate(wl(wl_len, 1))
allocate(jd(jd_len, 1))
allocate(irradiance(wl_len, jd_len, 1))

! Read wavelengths
call nf(nf90_inq_varid(ncid, 'WAVELENGTH', varid))
call nf(nf90_get_var(ncid, varid, wl))

! Read Julian dates
call nf(nf90_inq_varid(ncid, 'JD', varid))
call nf(nf90_get_var(ncid, varid, jd))

! Read Irradaince
call nf(nf90_inq_varid(ncid, 'IRRADIANCE', varid))
call nf(nf90_get_var(ncid, varid, irradiance))

! Close netcdf file
call nf(nf90_close(ncid))

! Setup solar spectrum type
do k=1, 4
  sol(k)%n_points = wl_len
  sol(k)%l_binned = .true.
  allocate(sol(k)%wavelength(sol(k)%n_points))
  allocate(sol(k)%irrad(sol(k)%n_points))
  allocate(sol(k)%bandsize(sol(k)%n_points))
  allocate(sol(k)%bandbnds(2, sol(k)%n_points))
  sol(k)%wavelength = anint(wl(:,1)*10.0_RealK**3)/10.0_RealK**12
  sol(k)%bandsize = 0.001e-9_RealK
  sol(k)%bandbnds(1, :) = sol(k)%wavelength - 0.5_RealK*sol(k)%bandsize
  sol(k)%bandbnds(2, :) = sol(k)%wavelength + 0.5_RealK*sol(k)%bandsize
end do
sol(4)%irrad = 0.0_RealK
jd_total_length = 0.0_RealK

do k=1, 3
  ! Calculate mean irradiance for WHI periods:
  if (k==1) then
    jd_start = jd_start_WHI_sunspot_dark
    jd_end = jd_end_WHI_sunspot_dark
    solfile = trim(output_file)//'_dark'
  else if (k==2) then
    jd_start = jd_start_WHI_faculae_bright
    jd_end = jd_end_WHI_faculae_bright
    solfile = trim(output_file)//'_bright'
  else
    jd_start = jd_start_WHI_quiet_sun
    jd_end = jd_end_WHI_quiet_sun
    solfile = trim(output_file)//'_quiet'
  end if
  sol(k)%irrad = 0.0_RealK
  do i=1, jd_len
    if (jd(i, 1) >= jd_start .and. jd(i, 1) <= jd_end) then
      do j=1, wl_len
        sol(k)%irrad(j) = sol(k)%irrad(j) + irradiance(j, i, 1)*1.0e9_RealK
      end do
    end if
  end do
  sol(4)%irrad = sol(4)%irrad + sol(k)%irrad
  jd_total_length = jd_total_length + real(jd_end - jd_start + 1, RealK)

  sol(k)%irrad = sol(k)%irrad / real(jd_end - jd_start + 1, RealK)
  call write_solar_spectrum(trim(solfile), sol(k), ierr)
  deallocate(sol(k)%bandbnds)
  deallocate(sol(k)%bandsize)
  deallocate(sol(k)%irrad)
  deallocate(sol(k)%wavelength)
end do

! Calculate mean irradiance over all three periods
sol(4)%irrad = sol(4)%irrad / jd_total_length
solfile = trim(output_file)//'_mean'
call write_solar_spectrum(trim(solfile), sol(4), ierr)
deallocate(sol(4)%bandbnds)
deallocate(sol(4)%bandsize)
deallocate(sol(4)%irrad)
deallocate(sol(4)%wavelength)

contains

  subroutine nf(status)
    integer, intent(IN):: status
    if (status /= NF90_NOERR) then
       write(*,*) 'netCDF-ERROR: ',nf90_strerror(status)
       stop 'STOPPED!'
    end if
  end subroutine nf

end program lya2sol
