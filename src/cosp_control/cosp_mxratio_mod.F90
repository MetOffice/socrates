! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE cosp_mxratio_mod

USE parkind1, ONLY: jpim, jprb
USE yomhook,  ONLY: lhook, dr_hook
USE cosp_kinds, ONLY: wp

IMPLICIT NONE

! Description:
!   Routine that converts precipitation fluxes to mixing ratios from
!   the gridbox-mean inputs and microphysical constants.
!
! Method:
!   It uses the mixing ratio - flux relationship obtained from the PSD
!   and vertical fall speed, based on a gamma function PSD, and
!   a power-law terminal fall speed (i.e. h_x is zero). It does not
!   support Abel and Shipway (2007) fall speed.
!   It also computes the effective radius if required.
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: COSP
!
! Code description:
! Language: Fortran 95.
! This code is written to UMDP3 standards.

CHARACTER(LEN=*), PRIVATE, PARAMETER :: ModuleName='COSP_MXRATIO_MOD'

CONTAINS
SUBROUTINE cosp_mxratio(npoints,nlevels,p,t,n_ax,n_bx,alpha_x,c_x,d_x,g_x,     &
                        a_x,b_x,gamma1,gamma2,gamma3,gamma4,flux,mxratio,reff)
IMPLICIT NONE
! Input arguments, (IN)
INTEGER,INTENT(IN) :: Npoints,Nlevels
REAL(wp),INTENT(IN) :: p(Npoints,Nlevels),t(Npoints,Nlevels),                  &
                   flux(Npoints,Nlevels)
REAL(wp),INTENT(IN) :: n_ax,n_bx,alpha_x,c_x,d_x,g_x,a_x,b_x,gamma1,gamma2,    &
                   gamma3,gamma4
! Input arguments, (OUT)
REAL(wp),INTENT(OUT) :: mxratio(Npoints,Nlevels)
REAL(wp),INTENT(IN OUT) :: reff(Npoints,Nlevels)
! Local variables
INTEGER :: i,k
REAL(wp) :: sigma,one_over_xip1,xi,rho0,rho,lambda_x,gamma_4_3_2,delta
! Routine name and DrHook variables
CHARACTER(LEN=*),   PARAMETER :: RoutineName='COSP_MXRATIO'
INTEGER(KIND=jpim), PARAMETER :: zhook_in = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

mxratio = 0.0

! N_ax is used to control which hydrometeors need to be computed
IF (n_ax >= 0.0) THEN
  xi      = d_x/(alpha_x + b_x - n_bx + 1.0)
  rho0    = 1.29
  sigma   = (gamma2/(gamma1*c_x))*(n_ax*a_x*gamma2)**xi
  one_over_xip1 = 1.0/(xi + 1.0)
  gamma_4_3_2 = 0.5*gamma4/gamma3
  delta = (alpha_x + b_x + d_x - n_bx + 1.0)

!$OMP PARALLEL DEFAULT(NONE) PRIVATE(k, i, rho, lambda_x)                      &
!$OMP SHARED(Nlevels, Npoints, c_x, p, t, mxratio, rho0, g_x, sigma, a_x,      &
!$OMP   one_over_xip1, reff, flux, n_ax, gamma1, delta, gamma_4_3_2)
!$OMP DO SCHEDULE(STATIC)
  DO k=1,Nlevels
    DO i=1,Npoints
      IF (flux(i,k) > 0.0) THEN
        rho = p(i,k)/(287.05*t(i,k))
        mxratio(i,k)=(flux(i,k)*((rho/rho0)**g_x)*sigma)**one_over_xip1/rho
        ! Compute effective radius
        IF ((reff(i,k) <= 0.0) .AND. (flux(i,k) /= 0.0)) THEN
          lambda_x =                                                           &
             (a_x*c_x*((rho0/rho)**g_x)*n_ax*gamma1/flux(i,k))**(1.0/delta)
          reff(i,k) = gamma_4_3_2/lambda_x
        END IF
      END IF
    END DO
  END DO
!$OMP END DO
!$OMP END PARALLEL
END IF
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
END SUBROUTINE cosp_mxratio
END MODULE cosp_mxratio_mod
