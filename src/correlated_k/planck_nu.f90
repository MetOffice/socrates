! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Subroutine to calculate the Planckian function.
!
! Description:
!   This routine returns an array of values of the Planckian
!   radiance calculated at a range of supplied wavenumbers in
!   units of W m-2 sr-1 (m-1)-1
!
!------------------------------------------------------------------------------
MODULE planck_nu_mod
IMPLICIT NONE
CONTAINS
SUBROUTINE planck_nu(nu, t, b)

  USE realtype_rd, ONLY: RealK
  USE rad_ccf, ONLY: pi, h_planck, c_light, k_boltzmann

  IMPLICIT NONE

  REAL  (RealK), Intent(IN) :: nu(:)
!   Wavenumbers
  REAL  (RealK), Intent(IN) :: T
!   Temperature
  REAL  (RealK), Intent(OUT) :: b(:)
!   Calculated Planckian radiances


! Evaluate a negative exponential to ensure conditioning.
  b = EXP(-( h_planck * c_light * nu / ( k_boltzmann * T) ) )
  b = 2.0_RealK * h_planck * c_light**2 * b * (nu**3) / &
      (1.0_RealK - b)

END SUBROUTINE planck_nu
END MODULE planck_nu_mod
