! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Mean the SOLARIS-HEPPA CMIP7 solar forcing spectra over a given time period
!
! The input CMIP7 solar forcing data come from:
! Funke et al. (2024), GMD, 17, DOI:10.5194/gmd-17-1217-2024
! https://www.solarisheppa.kit.edu/75.php
!------------------------------------------------------------------------------
program cmip2sol

use realtype_rd, only: RealK
use def_solarspec, only: StrSolarSpec
use def_std_io_icf, only: iu_err
use missing_data_mod, only: imdi
use netcdf

implicit none

type (StrSolarSpec) :: sol, solmean
! Stellar spectrum

logical :: l_cmipfile, l_solfile
character(len=256) :: cmipfile, solfile, arg
character(len=4) :: dim_name
character(len=3) :: frequency
integer :: ncid, dimid_time, time_len, dimid_wlen, wlen_len
integer :: varid
integer :: n_times=imdi, yearstart=imdi, monthstart=1, daystart=1
integer, allocatable :: calyear(:), calmonth(:), calday(:)
real(RealK), allocatable :: ssi(:,:), wbinsize(:), wbinbnds(:,:)

integer :: ios, ierr
integer :: i, j, k, jj

real (RealK) :: scale_wv, scale_irr
! Scaling for wavelength and irradiance to correct units


l_cmipfile = .false.
l_solfile = .false.
i = 0
do
  i=i+1
  if (i > command_argument_count()) exit
  call get_command_argument(i, arg)
  select case (arg)
  case ('-y','--yearstart')
    i=i+1
    call get_command_argument(i, arg)
    read(arg, *) yearstart
  case ('-m','--monthstart')
    i=i+1
    call get_command_argument(i, arg)
    read(arg, *) monthstart
  case ('-d','--daystart')
    i=i+1
    call get_command_argument(i, arg)
    read(arg, *) daystart
  case ('-n','--ntimes')
    i=i+1
    call get_command_argument(i, arg)
    read(arg, *) n_times
  case default
    if (l_cmipfile) then
      ! If the input CMIP file has been provided, the second file is
      ! the output solfile
      solfile = arg
      l_solfile = .true.
    else
      ! First file given is the input CMIP6/7 netcdf file
      cmipfile = arg
      l_cmipfile = .true.
    end if
  end select
end do
if (.not.l_solfile) then
  write(iu_err, '(a)') &
    'Usage: cmip2sol cmipfile solfile [-y <year> -m <month> -d <day> -n <ntimes>]'
  stop
end if

! Open the file for reading
call nf(nf90_open(trim(cmipfile),NF90_NOWRITE,ncid))

! Get number of times
dim_name = 'time'
call nf(nf90_inq_dimid(ncid, dim_name, dimid_time))
call nf(nf90_inquire_dimension(ncid, dimid_time, dim_name, time_len))
if (n_times == imdi) n_times = time_len

! Find the number of points in the spectrum.
dim_name = 'wlen'
call nf(nf90_inq_dimid(ncid, dim_name, dimid_wlen))
CALL nf(nf90_inquire_dimension(ncid, dimid_wlen, dim_name, wlen_len))
sol%n_points = wlen_len

allocate(sol%wavelength(sol%n_points))
allocate(sol%irrad(     sol%n_points))
allocate(sol%bandsize(  sol%n_points))
allocate(sol%bandbnds(  2, sol%n_points))

scale_wv=1.0E-09_RealK ! nm to m
scale_irr=1.0E+09_RealK ! Wm-2nm-1 to Wm-3

! Read the wavelength bin centres
call nf(nf90_inq_varid(ncid, 'wlen', varid))
call nf(nf90_get_var(ncid, varid, sol%wavelength))
! Scale the values to the correct units:
sol%wavelength = sol%wavelength * scale_wv

! Read the start time for each time bin
call nf(nf90_get_att(ncid, NF90_GLOBAL, 'frequency', frequency))
allocate(calyear(time_len))
ios = nf90_inq_varid(ncid, 'calyear', varid)
if (ios == NF90_NOERR) ios = nf90_get_var(ncid, varid, calyear)
if (ios /= NF90_NOERR) calyear(:) = 1850
allocate(calmonth(time_len))
ios = nf90_inq_varid(ncid, 'calmonth', varid)
if (ios == NF90_NOERR) ios = nf90_get_var(ncid, varid, calmonth)
if (ios /= NF90_NOERR) calmonth(:) = 1
allocate(calday(time_len))
ios = nf90_inq_varid(ncid, 'calday', varid)
if (ios == NF90_NOERR) ios = nf90_get_var(ncid, varid, calday)
if (ios /= NF90_NOERR) calday(:) = 1
      
! Read the wbinsize
allocate(wbinsize(wlen_len))
call nf(nf90_inq_varid(ncid, 'wlenbinsize', varid))
call nf(nf90_get_var(ncid, varid, wbinsize))
sol%bandsize = wbinsize * scale_wv

! Read the wbinbnds
allocate(wbinbnds(2,wlen_len))
call nf(nf90_inq_varid(ncid, 'wlen_bnds', varid))
call nf(nf90_get_var(ncid, varid, wbinbnds))
sol%bandbnds = wbinbnds * scale_wv

! Find the ssi variable id
allocate(ssi(1, wlen_len))
call nf(nf90_inq_varid(ncid, 'ssi', varid))

if (yearstart == imdi) then
  jj = 1
else
  do jj = 1, time_len
    if (calyear(jj) >  yearstart) exit
    if (calyear(jj) == yearstart) then
      if (calmonth(jj) >  monthstart) exit
      if (calmonth(jj) == monthstart) then
        if (calday(jj) >= daystart) exit
      end if
    end if
  end do
end if

do k = 1, n_times
  j = jj + k - 1

  ! Read the ssi for this time
  call nf(nf90_get_var(ncid, varid, ssi, &
    start=(/1, j/), count=(/wlen_len, 1/) ))
  ! Scale the values to the correct units:
  sol%irrad = ssi(1,:) * scale_irr

  if (k==1) then
    solmean%n_points    = sol%n_points
    solmean%l_binned    = .true.
    allocate(solmean%wavelength( solmean%n_points))
    allocate(solmean%irrad(      solmean%n_points))
    allocate(solmean%bandsize(   solmean%n_points))
    allocate(solmean%bandbnds(2, solmean%n_points))
    solmean%wavelength = sol%wavelength
    solmean%irrad = 0.0_RealK
    solmean%bandsize = sol%bandsize
    solmean%bandbnds = sol%bandbnds
  end if
  solmean%irrad = solmean%irrad + sol%irrad
  if (k==n_times) then
    solmean%irrad = solmean%irrad/real(n_times, RealK)
    call write_solar_spectrum(trim(solfile), solmean, ierr)
    deallocate(solmean%bandbnds)
    deallocate(solmean%bandsize)
    deallocate(solmean%irrad)
    deallocate(solmean%wavelength)
  end if
end do

call nf(nf90_close(ncid))
deallocate(ssi)
deallocate(wbinbnds)
deallocate(wbinsize)
deallocate(calday)
deallocate(calmonth)
deallocate(calyear)
deallocate(sol%bandbnds)
deallocate(sol%bandsize)
deallocate(sol%irrad)
deallocate(sol%wavelength)

contains

  subroutine nf(status)
    integer, intent(IN):: status
    if (status /= NF90_NOERR) then
       write(*,*) 'netCDF-ERROR: ',nf90_strerror(status)
       stop 'STOPPED!'
    end if
  end subroutine nf

end program cmip2sol
