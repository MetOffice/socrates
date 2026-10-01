! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Subroutine to calculate the derivative Planckian function.
!
! Description:
!   This routine returns an array of values of the derivative of
!   the Planckian radiance with respect to temperature calculated 
!   at a range of supplied wavenumbers.
!
!------------------------------------------------------------------------------
MODULE d_planck_nu_mod
IMPLICIT NONE
CONTAINS
SUBROUTINE d_planck_nu(nu, t, db)

  USE realtype_rd, ONLY: RealK
  USE rad_ccf, ONLY: pi, h_planck, c_light, k_boltzmann

  IMPLICIT NONE

  REAL  (RealK), Intent(IN) :: nu(:)
!   Wavenumbers
  REAL  (RealK), Intent(IN) :: T
!   Temperature
  REAL  (RealK), Intent(OUT) :: db(:)
!   Derivative of the Planckian function


! Evaluate a negative exponential to ensure conditioning.
  db = EXP(-( h_planck * c_light * nu / ( k_boltzmann * T) ) )
  db = 2.0_RealK * h_planck * c_light**2 * db * (nu**3) / &
      (1.0_RealK - db)**2

END SUBROUTINE d_planck_nu
END MODULE d_planck_nu_mod
