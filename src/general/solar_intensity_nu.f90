! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Subroutine to calculate the solar irradiance at given wavenumbers.
!
! Description:
!   This subroutine returns the solar irradiance per unit wavenumber
!   at given wavenumbers.
!
! Method:
!   The wavelength is bracketed between two points of the solar
!   spectrum and linear interpolation is used in the interval.
!   For binned solar spectra the bin value is used directly.
!   At long wavelengths a Rayleigh-Jeans tail is used.
!
!------------------------------------------------------------------------------
MODULE solar_intensity_nu_mod
IMPLICIT NONE
CONTAINS
SUBROUTINE solar_intensity_nu(nu, Sol, solar_intensity)

  USE realtype_rd, ONLY: RealK
  USE def_solarspec, ONLY: StrSolarSpec
  USE rad_ccf, ONLY: astronomical_unit
  USE planck_nu_mod, ONLY: planck_nu

  IMPLICIT NONE

! Dummy arguments
  REAL  (RealK), Intent(IN) :: nu(:)
!   Wavenumbers supplied to the routine
  TYPE (StrSolarSpec), Intent(IN) :: Sol
!   Spectral solar irradiance at the top of the atmosphere
  REAL  (RealK), Intent(OUT) :: solar_intensity(:)
!   Returned intensity

! Local variables.
  INTEGER :: n
!   Size of supplied array of wavenumbers
  INTEGER :: i
!   Loop variable
  INTEGER :: i_short
!   Array point just below lambda
  INTEGER :: i_long
!   Array point just above lambda
  REAL  (RealK) :: lambda
!   Wavelength
  REAL  (RealK) :: fraction
!   Fraction of interval covered


  n=SIZE(nu)

!$OMP PARALLEL DO PRIVATE(i, lambda, i_short, i_long, fraction)
  DO i=1, n
    ! Solar spectra are defined by wavelength.
    lambda=1.0_RealK/nu(i)

    IF (Sol%l_binned) THEN
      i_short = MINLOC(lambda - Sol%bandbnds(1, :), 1, &
                       lambda - Sol%bandbnds(1, :) >= 0.0_RealK)
      i_long = MINLOC(Sol%bandbnds(2, :) - lambda, 1, &
                      Sol%bandbnds(2, :) - lambda >= 0.0_RealK)
      IF (lambda < Sol%bandbnds(1, 1)) i_short = 0
      IF (lambda > Sol%bandbnds(2, Sol%n_points)) i_long = Sol%n_points + 1
    ELSE
      CALL point_bracket(lambda, Sol%n_points, Sol%wavelength, i_short, i_long)
    END IF

    IF (i_short == 0) THEN

      ! Lambda is at a wavelength shorter than any in the spectrum.
      solar_intensity(i) = 0.0_RealK

    ELSE IF (i_long > Sol%n_points) THEN

      ! Lambda is at a wavelength longer than any in the spectrum.
      ! Use a black body fit at the effective temperature.
      CALL planck_nu( nu(i:i), Sol%t_effective, solar_intensity(i:i) )
      solar_intensity(i) = solar_intensity(i) &
        * (Sol%radius / astronomical_unit)**2

    ELSE IF (i_short == i_long) THEN

      ! For a binned solar spectrum i_short and i_long will point to the
      ! same bin unless exactly on a bin boundary.
      solar_intensity(i) = Sol%irrad(i_short) * lambda**2

    ELSE

      ! Within the spectrum use linear interpolation.
      fraction = (lambda - Sol%wavelength(i_short)) &
               / (Sol%wavelength(i_long) - Sol%wavelength(i_short))
      solar_intensity(i) = ( fraction * Sol%irrad(i_long) + &
        (1.0_RealK - fraction) * Sol%irrad(i_short) ) * lambda**2

    END IF
  END DO
!$OMP END PARALLEL DO

END SUBROUTINE solar_intensity_nu
END MODULE solar_intensity_nu_mod
