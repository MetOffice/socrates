! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE cosp_precip_mod
USE cosp_input_mod, ONLY: cosp_overlap, cosp_use_precipitation_fluxes
USE mod_cosp,       ONLY: cosp_column_inputs, cosp_optical_inputs
USE cosp2_types_mod, ONLY: cosp_inputs_host_model
USE cosp2_constants_mod, ONLY: i_lscliq, i_lscice, i_lsrain, i_lsiagg,         &
  i_cvcliq, i_cvcice, i_cvrain, i_cvsnow, i_lsgrpl, N_ax, N_bx, alpha_x, c_x,  &
  d_x, g_x, a_x, b_x, gamma_1, gamma_2, gamma_3, gamma_4, x_1, x_2, x_3, x_4
USE cosp_mxratio_mod, ONLY: cosp_mxratio
USE cosp_reff_mod, ONLY: cosp_reff
USE errormessagelength_mod, ONLY: errormessagelength
USE parkind1, ONLY: jpim, jprb
USE yomhook,  ONLY: lhook, dr_hook
USE cosp_kinds, ONLY: wp

IMPLICIT NONE
! Exponent that controls the temperature dependence of the intercept
! rainfall of the PSD (0.0 -> no dependence)
REAL(wp), PARAMETER :: x3r = 0.0_wp
REAL(wp), PARAMETER :: x3g = 0.0_wp
! Exponent of the normalised density in the terminal fall speed (UMDP26)
REAL(wp), PARAMETER :: gx = 0.4_wp
! Coefficients for the density distribution of rainfall (Homogeneous liquid
! spheres)
REAL(wp), PARAMETER :: ar = 523.6_wp
REAL(wp), PARAMETER :: br = 3.0_wp

REAL(wp), PARAMETER :: cr = 386.8_wp
REAL(wp), PARAMETER :: dr = 0.67_wp
REAL(wp), PARAMETER :: x1r = 2.2e-1_wp
REAL(wp), PARAMETER :: x2r = 2.2_wp
REAL(wp), PARAMETER :: x4r = 0.0_wp

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='COSP_PRECIP_MOD'

! Description:
!   Routine that populates the gridbox-mean precipitation variables.
!
! Method:
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: COSP
!
! Code description:
! Language: Fortran 95.
! This code is written to UMDP3 standards.

CONTAINS
SUBROUTINE cosp_gridbox_precip(cosp_column_in, cosp_hmodel)

IMPLICIT NONE
!----Input arguments
TYPE(cosp_column_inputs), INTENT(IN) :: cosp_column_in
!----Output arguments
TYPE(cosp_inputs_host_model), INTENT(IN OUT) :: cosp_hmodel
!----Local variables
INTEGER, PARAMETER :: nmax_precip_flux = 5
INTEGER :: i_convert_flux(nmax_precip_flux)
LOGICAL :: l_convert_flux(nmax_precip_flux)
INTEGER :: i,j,k,npoints,nlevels,ni,nj
REAL(wp), ALLOCATABLE :: aux2d(:,:)
REAL(wp), ALLOCATABLE :: rho(:,:)
REAL(wp), PARAMETER :: r_spec = 287.052874_wp
REAL(wp), PARAMETER :: repsilon = 0.62198_wp
LOGICAL, PARAMETER :: no_precip_flux = .FALSE.
CHARACTER(LEN=errormessagelength) :: cmessage = ' '
! Routine name and DrHook variables
CHARACTER(LEN=*),   PARAMETER :: RoutineName='COSP_GRIDBOX_PRECIP'
INTEGER(KIND=jpim), PARAMETER :: zhook_in = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

npoints = cosp_hmodel%npoints
nlevels = cosp_hmodel%nlevels

! Calculation of effective radius. aux2d is the layer density
ALLOCATE(aux2d(npoints, nlevels), rho(npoints, nlevels))
rho = cosp_column_in%pfull / (r_spec*cosp_column_in%at*(1.0 +                  &
  ((1.0 - repsilon)/repsilon)*cosp_column_in%qv -                              &
  cosp_hmodel%mr_gbx(:,:,i_lscliq) - cosp_hmodel%mr_gbx(:,:,i_lscice) -        &
  cosp_hmodel%mr_gbx(:,:,i_cvcliq) - cosp_hmodel%mr_gbx(:,:,i_cvcice)))

! Convert precipitation fluxes to mixing ratios
i_convert_flux = [ i_lsrain, i_lsiagg, i_lsgrpl, i_cvrain, i_cvsnow ]
l_convert_flux(1:3) = cosp_use_precipitation_fluxes
l_convert_flux(4:nmax_precip_flux) = .TRUE.
DO j = 1,nmax_precip_flux
  i = i_convert_flux(j)
  IF (l_convert_flux(j)) THEN
    aux2d = cosp_hmodel%mr_gbx(:,:,i)
    CALL cosp_mxratio(npoints, nlevels, cosp_column_in%pfull,                  &
      cosp_column_in%at, n_ax(i), n_bx(i), alpha_x(i), c_x(i), d_x(i),         &
      g_x(i), a_x(i), b_x(i), gamma_1(i), gamma_2(i), gamma_3(i),              &
      gamma_4(i), aux2d, cosp_hmodel%mr_gbx(:,:,i),                            &
      cosp_hmodel%reff_gbx(:,:,i))
  ELSE
    IF (a_x(i) > 0.0)                                                          &
     CALL cosp_reff(no_precip_flux, x_1(i), x_2(i), x_3(i), x_4(i), a_x(i),    &
      b_x(i), c_x(i), d_x(i) ,g_x(i), npoints, nlevels, cosp_column_in%at,     &
      rho, cosp_hmodel%mr_gbx(:,:,i), cosp_hmodel%reff_gbx(:,:,i))
  END IF
END DO
CALL cosp_reff(no_precip_flux,x1r,x2r,x3r,x4r,ar,br,cr,dr,gx,                  &
 npoints, nlevels, cosp_column_in%at, rho,                                     &
 cosp_hmodel%mr_gbx(:,:,i_lsrain), cosp_hmodel%reff_gbx(:,:,i_lsrain))

DEALLOCATE(aux2d, rho)
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
END SUBROUTINE cosp_gridbox_precip
END MODULE cosp_precip_mod
