! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Subroutine to calculate the weightings for correlated-k.
!
! Description:
!   Weightings at the frequencies supplied are calculated. nu_wgt and wgt
!   should have the same lengths.
!
!------------------------------------------------------------------------------
SUBROUTINE rad_weight_90(i_weight, nu_wgt, SolarSpec, t, wgt)

  USE realtype_rd, ONLY: RealK
  USE def_solarspec, ONLY: StrSolarSpec
  USE weighting_pcf, ONLY: IP_weight_planck, IP_weight_d_planck, &
                           IP_weight_solar, &
                           IP_weight_uniform
  USE planck_nu_mod, ONLY: planck_nu
  USE d_planck_nu_mod, ONLY: d_planck_nu
  USE solar_intensity_nu_mod, ONLY: solar_intensity_nu

  IMPLICIT NONE

  INTEGER, Intent(IN) :: i_weight
!   Method of weighting
  TYPE (StrSolarSpec), Intent(IN) :: SolarSpec
!   Solar spectral irradiance data
  REAL  (RealK), Intent(IN) :: t
!   Temperatures for Planckian weighting
  REAL  (RealK), Intent(IN) :: nu_wgt(:)
!   Frequencies where weighting is applied
  REAL  (RealK), Intent(OUT) :: wgt(:)
!   Calculated weightings


  SELECT CASE (i_weight)
    CASE (IP_weight_planck)
      CALL planck_nu(nu_wgt, t, wgt)
    CASE (IP_weight_d_planck)
      CALL d_planck_nu(nu_wgt, t, wgt)
    CASE (IP_weight_solar)
      CALL solar_intensity_nu(nu_wgt, SolarSpec, wgt)
    CASE (IP_weight_uniform)
      wgt(:)=1.0_RealK
  END SELECT

END SUBROUTINE rad_weight_90
