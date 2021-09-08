! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

! Radiation Constant Configuration File

MODULE rad_ccf

! Description:
!   Module containing settings of standard constants used within the
!   radiation scheme. This replaces a number of separate constant
!   configuration files. The original file names are indicated at the
!   head of each section.

  USE realtype_rd

! astron_constants_ccf
! ------------------------------------------------------------------
! Module to set values of astronomical constants.
  REAL (RealK), PARAMETER :: astronomical_unit = 149597870700.0_RealK
!   Standard Astronomical Unit (mean Earth-Sun distance)
  REAL (RealK), PARAMETER :: solar_t_effective = 5.785E+03_RealK
!   Effective solar temperature
  REAL (RealK), PARAMETER :: solar_radius      = 6.957E+08_RealK
!   Radius of the Sun (IAU recommendation: https://arxiv.org/abs/1510.07674)
  REAL (RealK), PARAMETER :: earth_radius      = 6.37E+06_RealK
!   Radius of the Earth
  REAL (RealK), PARAMETER :: eccentricity      = 1.67E-02_RealK
!   Eccentricity of earth's orbit
  REAL (RealK), PARAMETER :: length_year       = 3.652422E+02_RealK
!   Number of days in year
  REAL (RealK), PARAMETER :: day_perihelion    = 3.71_RealK
!   Time of annual perihelion

! ------------------------------------------------------------------
! math_cnst_ccf
! ------------------------------------------------------------------
! Module to define mathematical constants.
  REAL (RealK), PARAMETER :: pi = 3.14159265358979323846e+00_RealK
!   Value of pi

! ------------------------------------------------------------------
! physical_constants_0_ccf, phycn03a
! ------------------------------------------------------------------
! Module setting physical constants for the Earth.
  REAL (RealK), PARAMETER :: mol_weight_air  = 28.966e-03_RealK
!   Molar weight of dry air
  REAL (RealK), PARAMETER :: seconds_per_day = 8.6400e+04_RealK
!   Number of seconds in a day
  REAL (RealK), PARAMETER :: n2_mass_frac    = 0.781e+00_RealK
!   Mass fraction of nitrogen

! ------------------------------------------------------------------
! physical_constants_1_ccf
! ------------------------------------------------------------------
  REAL (RealK), PARAMETER :: grav_acc           = 9.80665_RealK
!   Acceleration due to gravity
  REAL (RealK), PARAMETER :: r_gas              = 8.3143_RealK
!   Universal gas constant
  REAL (RealK), PARAMETER :: r_gas_dry          = 287.026_RealK
!   Gas constant for dry air
  REAL (RealK), PARAMETER :: cp_air_dry         = 1.005e+03_RealK
!   Specific heat of dry air
  REAL (RealK), PARAMETER :: ratio_molar_weight = 28.966_RealK /        &
                                                   18.0153_RealK
!   Molecular weight of dry air/ molecular weight of water
  REAL (RealK), PARAMETER :: r = r_gas_dry
  REAL (RealK), PARAMETER :: c_virtual = ratio_molar_weight - 1.0_RealK
  REAL (RealK), PARAMETER :: repsilon = 1.0_RealK / ratio_molar_weight

! ------------------------------------------------------------------
! physical_constants_pp_ccf
! ------------------------------------------------------------------
! Module setting physical constants.
  REAL (RealK), PARAMETER :: h_planck         = 6.626176e-34_RealK
!   Planck's constant (J s)
  REAL (RealK), PARAMETER :: c_light          = 2.9979245e+08_RealK
!   Speed of light in a vacuum (m s-1)
  REAL (RealK), PARAMETER :: k_boltzmann      = 1.380662e-23_RealK
!   Boltzmann's constant (J K-1)
  REAL (RealK), PARAMETER :: rho_n            = 2.79e-02_RealK
!   Depolarizing factor
  REAL (RealK), PARAMETER :: n_avogadro       = 6.022045e+23_RealK
!   Avogadro's number
  REAL (RealK), PARAMETER :: rho_air_stp      = 1.293125e+00_RealK
!   Density of dry air at standard temperature and pressure
  REAL (RealK), PARAMETER :: rho_water        = 1.0e+03_RealK
!   Density of pure water (kg/m3)
  REAL (RealK), PARAMETER :: stefan_boltzmann = 5.670374419e-08_RealK
!   Stefan-Boltzmann constant (W m-2 K-4)

! ------------------------------------------------------------------
! Exoplanet constants
! ------------------------------------------------------------------
! Physical constants for hot Jupiters.
  REAL (RealK), PARAMETER :: A_H             = 0.91183e+00_RealK
!   Number fraction of H
  REAL (RealK), PARAMETER :: A_He            = 1.0e+00_RealK - A_H
!   Number fraction of He
  REAL (RealK), PARAMETER :: mol_weight_h2he = 2.3376e-03_RealK
!   Mean molecular weight
  REAL (RealK), PARAMETER :: rho_n_h2he      = 2.00e-02_RealK
!   Depolarizing factor
  REAL (RealK), PARAMETER :: rho_h2he_stp    = 1.042921e-01_RealK
!   Density at standard temperature and pressure

END MODULE rad_ccf
