! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE cosp_reff_mod
USE yomhook, ONLY: lhook, dr_hook
USE ereport_mod
USE cosp_kinds, ONLY: wp
USE parkind1, ONLY: jprb, jpim
IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='COSP_REFF_MOD'

! Description:
!   Routine that computes the hydrometeor effective radius of the precipitating
!   hydrometeors.
!
! Method:
!   Uses the analytical solution of the ith moment of the PSD to compute the
!   ratio between the 3rd and 2nd moments, divided by two. This is explained
!   in the COSP user's manual. It used the parameters that describe the PSD
!   in the UM microphysical settings (UMDP26).
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: COSP
!
! Code description:
! Language: Fortran 95.
! This code is written to UMDP3 standards.

CONTAINS
SUBROUTINE cosp_reff(flux,x1,x2,x3,x4,a,b,c,d,g,                               &
                npoints,model_levels,t,rho,mr,Reff)

IMPLICIT NONE
!----Input arguments
! Precipitation flux is used
LOGICAL,INTENT(IN) :: flux
! These are the constants that define the PSD in the microphysics
! scheme (UMDP 26).
! X1,X3: n_ax = X1*exp(-X3*T[degC])
! X2: n_bx
! X4: alpha_x
REAL(wp),INTENT(IN) :: x1
REAL(wp),INTENT(IN) :: x2
REAL(wp),INTENT(IN) :: x3
REAL(wp),INTENT(IN) :: x4
! These are the constants that define the mass-diameter relationship.
! M_x(D) = a_x*D^(b_x)
! A: a_x
! B: b_x
REAL(wp),INTENT(IN) :: a
REAL(wp),INTENT(IN) :: b
! These are the constants that define the terminal fall speed relationship.
! V_x(D) = c_x*D^(d_x)*exp(-h_x)*(rho_0/rho)^g_x
! C: c_x
! D: d_x
! G: g_x
! h_x is always 0
! Abel and Shipway (2007) is not supported
REAL(wp),INTENT(IN) :: c
REAL(wp),INTENT(IN) :: d
REAL(wp),INTENT(IN) :: g
! Dimensions
INTEGER,INTENT(IN) :: npoints
INTEGER,INTENT(IN) :: model_levels
! Air temperature [K]
REAL(wp),INTENT(IN) :: t(npoints,model_levels)
! Air density [kg/m^3]
REAL(wp),INTENT(IN) :: rho(npoints,model_levels)
! Hydrometeor mixing ratio [kg/kg]
REAL(wp),INTENT(IN) :: mr(npoints,model_levels)

!----Output arguments
! Effective radius [m]
REAL(wp),INTENT(OUT) :: Reff(npoints,model_levels)

!----Local variables
REAL(wp),PARAMETER :: rho_0 = 1.0_wp
REAL(wp),PARAMETER :: zerodegc = 273.15_wp
REAL(wp),PARAMETER :: t_agg_min = -45.0_wp
REAL(wp) :: gamma_a3,gamma_a4,gamma_ab1,gamma_abd1,frac_exp,gamma_ratio
REAL(wp) :: F_nax(npoints,model_levels)
CHARACTER(LEN=*), PARAMETER :: RoutineName='COSP_REFF'
INTEGER :: icode,i,k
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)
icode = 9

CALL gammafunc(3.0_wp+x4,gamma_a3)
CALL gammafunc(4.0_wp+x4,gamma_a4)
CALL gammafunc(1.0_wp+x4+b,gamma_ab1)
CALL gammafunc(1.0_wp+x4+b+d,gamma_abd1)


Reff = 0.0_wp

IF (a <= 0.0_wp) CALL Ereport(RoutineName, icode," A <= 0.0")

! Compute intercept as function of T, if needed
IF (x3 /= 0.0_wp) F_nax = EXP(-x3*MAX(t-zerodegc,t_agg_min))

! Compute the parameter lambda^-1 of the PSD. stored in variable Reff
IF (flux) THEN ! precipitation flux. Fall speed needed
  frac_exp = 1.0_wp/(x4+d-x2+4.0_wp)
  IF (c <= 0.0_wp) CALL Ereport(RoutineName, icode," C <= 0.0")
  IF (x3 /= 0.0_wp) THEN
    Reff = (mr/(a*c*((rho_0/rho)**g)*x1*F_nax*gamma_abd1))**frac_exp
  ELSE
    Reff = (mr/(a*c*((rho_0/rho)**g)*x1*gamma_abd1))**frac_exp
  END IF
ELSE ! mixing ratio
  frac_exp = 1.0_wp/(x4+b-x2+1.0_wp)
  IF (x3 /= 0.0_wp) THEN
    Reff = ((rho*mr)/(a*gamma_ab1*x1*F_nax))**frac_exp
  ELSE
    Reff = ((rho*mr)/(a*gamma_ab1*x1))**frac_exp
  END IF
END IF

! Compute radius and apply sanity check
gamma_ratio = 0.5_wp*(gamma_a4/gamma_a3)
!$OMP PARALLEL DEFAULT(NONE) PRIVATE(i,k)                                      &
!$OMP SHARED(model_levels, npoints, Reff, gamma_ratio)
!$OMP DO SCHEDULE(STATIC)
DO k = 1, model_levels
  DO i = 1, npoints
    IF (Reff(i,k) > 0.0_wp) Reff(i,k) = gamma_ratio*Reff(i,k)
    IF (Reff(i,k) < 0.0_wp) Reff(i,k) = 0.0_wp
  END DO
END DO
!$OMP END DO
!$OMP END PARALLEL

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
END SUBROUTINE cosp_reff

subroutine gammafunc(y_in,gam_out)
use yomhook, only: lhook, dr_hook
use parkind1, only: jprb, jpim
implicit none
real(wp) ::                                                      &
                            !, intent(in)
  y_in
real(wp) ::                                                      &
                            !, intent(out)
  gam_out
! Gamma function of Y

! LOCAL VARIABLE
integer :: i,m
real(wp) :: gg,g,pare,x

real(wp) :: y
real(wp) :: gam

integer(kind=jpim), parameter :: zhook_in  = 0
integer(kind=jpim), parameter :: zhook_out = 1
real(kind=jprb)               :: zhook_handle

character(len=*), parameter :: RoutineName='GAMMAF'

! --------------------------------------------------------------------
if (lhook) call dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

y=y_in
gg=1.0_wp
m=floor(y)
x=y-m
if (m > 1) then
  do i = 1, m-1
    g=y-i
    gg=gg*g
  end do
else if (m < 1) then
  do i = m, 0
    g=y-i
    gg=gg/g
  end do
end if
pare=-0.5748646_wp*x+0.9512363_wp*x*x-0.6998588_wp*x*x*x &
+0.4245549_wp*x*x*x*x-0.1010678_wp*x*x*x*x*x+1.0_wp
gam=pare*gg
gam_out=gam
if (lhook) call dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
return
end subroutine gammafunc

END MODULE cosp_reff_mod
