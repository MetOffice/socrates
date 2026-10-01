! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Subroutine to read a solar spectrum.
!
SUBROUTINE read_solar_spectrum(SolarSpec, ierr)

! Description:
!   The solar spectrum is read as a file of irradiances against
!   wavelengths.

  USE def_solarspec, ONLY: StrSolarSpec
  USE rad_pcf, ONLY: i_normal

  IMPLICIT NONE


! Dummy arguments.
  INTEGER, Intent(INOUT) :: ierr
!   Error flag
  TYPE (StrSolarSpec), Intent(OUT) :: SolarSpec
!   Solar spectrum

! Local variables.
  INTEGER :: iu_solar
!   Unit number for the solar spectrum

  
! Obtain the file containing the solar spectrum.
  CALL get_free_unit(ierr, iu_solar)
  CALL open_file_in(ierr, iu_solar, & 
    'Enter the name of the file containing the solar irradiance data.')
  IF (ierr /= i_normal) RETURN

! Read the solar spectrum data
  CALL read_solar_spectrum_data(iu_solar, SolarSpec, ierr)

END SUBROUTINE read_solar_spectrum
