! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Function to calculate the integrated Planck flux after a given wavelength
!
! Method:
!   Uses the method from 'Integration of the Planck blackbody radiation
!   function', Widger and Woodall (1976) BAMS, 57, 10, 1217-1219
!   https://doi.org/10.1175/1520-0477(1976)057<1217:IOTPBR>2.0.CO;2
!
!------------------------------------------------------------------------------
FUNCTION planck_tail(Sol, lambda)

  USE realtype_rd, ONLY: RealK
  USE rad_ccf, ONLY: astronomical_unit, pi, h_planck, c_light, k_boltzmann
  USE def_solarspec, ONLY: StrSolarSpec

  IMPLICIT NONE


  TYPE (StrSolarSpec), INTENT(IN) :: Sol
!   Solar spectrum
  REAL (RealK), INTENT(IN) :: lambda
!   Initial wavelength
  REAL (RealK) :: planck_tail
!   Tail irradiance.
  REAL (RealK), PARAMETER :: cf1 = h_planck*c_light/k_boltzmann
  REAL (RealK), PARAMETER :: cf2 = 2.0_RealK*c_light*k_boltzmann/cf1**3
!   Folded constants
  REAL (RealK), PARAMETER :: tol = 1.0e-08_RealK
!   Tolerance used in calculating the Planckian

  REAL (RealK), EXTERNAL :: planck_cumul
!   Function to calculate the cumulative radiant Planck function

! Evaluate black body flux.
  planck_tail = pi * cf2 * Sol%t_effective**4 &
    * planck_cumul(cf1/(Sol%t_effective*lambda), tol)
! Scale to get the flux at 1 AU.
  planck_tail = planck_tail * (Sol%radius/astronomical_unit)**2

END FUNCTION planck_tail
