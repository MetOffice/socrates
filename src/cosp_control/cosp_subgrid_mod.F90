! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE cosp_subgrid_mod
USE cosp_kinds,     ONLY: wp
USE cosp_input_mod, ONLY: cosp_overlap
USE mod_cosp,       ONLY: cosp_optical_inputs
USE cosp2_types_mod, ONLY: cosp_inputs_host_model
USE cosp2_constants_mod, ONLY: i_cvcliq, i_cvcice, i_lscliq, i_lscice,         &
  i_lsrain, i_lsiagg, i_cvrain, i_cvsnow, i_lsgrpl, i_lsc, i_cvc
USE mod_prec_scops, ONLY: prec_scops
USE parkind1, ONLY: jpim, jprb
USE yomhook,  ONLY: lhook, dr_hook

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='COSP_SUBGRID_MOD'

! Description:
!   Routines that populate the subgrid arrays with gridbox information.
!   COSP levels are from TOA to SFC.
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
SUBROUTINE cosp_subgrid_homogeneous_cloud(cosp_optical_in, cosp_hmodel)

IMPLICIT NONE
!----Input arguments
!----Output arguments
TYPE(cosp_optical_inputs),    INTENT(IN OUT) :: cosp_optical_in
TYPE(cosp_inputs_host_model), INTENT(IN OUT) :: cosp_hmodel
!----Local variables
INTEGER :: i,j,k,npoints,nlevels,ncolumns
REAL(wp) :: one_over_ncolumns
REAL(wp), ALLOCATABLE :: frac_ls(:,:)
REAL(wp), ALLOCATABLE :: frac_cv(:,:)
! Routine name and DrHook variables
CHARACTER(LEN=*),   PARAMETER :: RoutineName='COSP_SUBGRID_HOMOGENEOUS_CLOUD'
INTEGER(KIND=jpim), PARAMETER :: zhook_in = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

npoints = cosp_hmodel%npoints
nlevels = cosp_hmodel%nlevels
ncolumns = cosp_hmodel%ncolumns
one_over_ncolumns = 1.0_wp / REAL(ncolumns)


ALLOCATE(frac_ls(nPoints,nLevels), frac_cv(nPoints,nLevels))

! Compute L-S and CONV cloud fractions from the subcolumn arrays.
frac_ls = 0.0_wp
frac_cv = 0.0_wp
DO k=1,nlevels
  DO j=1,ncolumns
    DO i=1,npoints
      IF (cosp_optical_in%frac_out(i,j,k)  == i_lsc)                           &
        frac_ls(i,k) = frac_ls(i,k) + one_over_ncolumns
      IF (cosp_optical_in%frac_out(i,j,k)  == i_cvc)                           &
        frac_cv(i,k) = frac_cv(i,k) + one_over_ncolumns
    END DO
  END DO
END DO

! Fill in the subcolumn volumes, converting mixing ratios to in-cloud values
DO k=1,nlevels
  DO j=1,ncolumns
    DO i=1,npoints
      IF (cosp_optical_in%frac_out(i,j,k) == i_lsc) THEN
        !+++++++++++ LS clouds ++++++++
        IF (frac_ls(i,k) /= 0.0_wp) THEN
          cosp_hmodel%mr_hydro(i,j,k,i_lscliq)   =                             &
            cosp_hmodel%mr_gbx(i,k,i_lscliq)/frac_ls(i,k)
          cosp_hmodel%mr_hydro(i,j,k,i_lscice)   =                             &
            cosp_hmodel%mr_gbx(i,k,i_lscice)/frac_ls(i,k)
          cosp_hmodel%mr_hydro(i,j,k,i_lsiagg)   =                             &
            cosp_hmodel%mr_gbx(i,k,i_lsiagg)/frac_ls(i,k)
          cosp_hmodel%reff_hydro(i,j,k,i_lscliq) =                             &
            cosp_hmodel%reff_gbx(i,k,i_lscliq)
          cosp_hmodel%reff_hydro(i,j,k,i_lscice) =                             &
            cosp_hmodel%reff_gbx(i,k,i_lscice)
          cosp_hmodel%reff_hydro(i,j,k,i_lsiagg) =                             &
            cosp_hmodel%reff_gbx(i,k,i_lsiagg)
          cosp_optical_in%tau_067(i,j,k)  = cosp_hmodel%dtaus_gbx(i,k)
          cosp_optical_in%emiss_11(i,j,k) = cosp_hmodel%dems_gbx(i,k)
        END IF
      END IF
      !+++++++++++ CONV clouds ++++++
      IF (cosp_optical_in%frac_out(i,j,k) == i_cvc) THEN
        IF (frac_cv(i,k) /= 0.0_wp) THEN
          cosp_hmodel%mr_hydro(i,j,k,i_cvcliq)   =                             &
            cosp_hmodel%mr_gbx(i,k,i_cvcliq)/frac_cv(i,k)
          cosp_hmodel%mr_hydro(i,j,k,i_cvcice)   =                             &
            cosp_hmodel%mr_gbx(i,k,i_cvcice)/frac_cv(i,k)
          cosp_hmodel%reff_hydro(i,j,k,i_cvcliq) =                             &
            cosp_hmodel%reff_gbx(i,k,i_cvcliq)
          cosp_hmodel%reff_hydro(i,j,k,i_cvcice) =                             &
            cosp_hmodel%reff_gbx(i,k,i_cvcice)
          cosp_optical_in%tau_067(i,j,k)  = cosp_hmodel%dtauc_gbx(i,k)
          cosp_optical_in%emiss_11(i,j,k) = cosp_hmodel%demc_gbx(i,k)
        END IF
      END IF
    END DO
  END DO
END DO
! For consistency, set to zero gridbox-mean volumes where
! subcolumn cloud fraction is zero
WHERE (frac_ls == 0.0_wp)
  cosp_hmodel%mr_gbx(:,:,i_lscliq) = 0.0_wp
  cosp_hmodel%mr_gbx(:,:,i_lscice) = 0.0_wp
  cosp_hmodel%mr_gbx(:,:,i_lsiagg) = 0.0_wp
  cosp_hmodel%reff_gbx(:,:,i_lscliq) = 0.0_wp
  cosp_hmodel%reff_gbx(:,:,i_lscice) = 0.0_wp
  cosp_hmodel%reff_gbx(:,:,i_lsiagg) = 0.0_wp
  cosp_hmodel%dtaus_gbx = 0.0_wp
  cosp_hmodel%dems_gbx = 0.0_wp
END WHERE
WHERE (frac_cv == 0.0_wp)
  cosp_hmodel%mr_gbx(:,:,i_cvcliq) = 0.0_wp
  cosp_hmodel%mr_gbx(:,:,i_cvcice) = 0.0_wp
  cosp_hmodel%reff_gbx(:,:,i_cvcliq) = 0.0_wp
  cosp_hmodel%reff_gbx(:,:,i_cvcice) = 0.0_wp
  cosp_hmodel%dtauc_gbx = 0.0_wp
  cosp_hmodel%demc_gbx = 0.0_wp
END WHERE
DEALLOCATE(frac_ls, frac_cv)
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
END SUBROUTINE cosp_subgrid_homogeneous_cloud

SUBROUTINE cosp_subgrid_homogeneous_precip(cosp_optical_in, cosp_hmodel)

IMPLICIT NONE
!----Input arguments
!----Output arguments
TYPE(cosp_optical_inputs),    INTENT(IN OUT) :: cosp_optical_in
TYPE(cosp_inputs_host_model), INTENT(IN OUT) :: cosp_hmodel
!----Local variables
INTEGER, PARAMETER :: nmax_precip_flux = 5
INTEGER :: i,j,k,npoints,nlevels,ncolumns
INTEGER :: i_convert_flux(nmax_precip_flux)
LOGICAL :: l_convert_flux(nmax_precip_flux)
REAL(wp) :: one_over_ncolumns
REAL(wp) :: small_real
INTEGER, ALLOCATABLE :: prec_frac(:,:,:)
REAL(wp), ALLOCATABLE :: ls_p_rate(:,:)
REAL(wp), ALLOCATABLE :: cv_p_rate(:,:)
REAL(wp), ALLOCATABLE :: frac_ls(:,:)
REAL(wp), ALLOCATABLE :: frac_cv(:,:)
! Routine name and DrHook variables
CHARACTER(LEN=*),   PARAMETER :: RoutineName='COSP_SUBGRID_HOMOGENEOUS_PRECIP'
INTEGER(KIND=jpim), PARAMETER :: zhook_in = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

small_real = EPSILON(small_real)
npoints = cosp_hmodel%npoints
nlevels = cosp_hmodel%nlevels
ncolumns = cosp_hmodel%ncolumns
one_over_ncolumns = 1.0_wp / REAL(ncolumns)

ALLOCATE(frac_ls(npoints,nlevels), frac_cv(npoints,nlevels),                   &
         prec_frac(npoints,ncolumns,nlevels))
ALLOCATE(ls_p_rate(nPoints,nLevels), cv_p_rate(nPoints,Nlevels))
! PREC_SCOPS needs 2 precipitation arrays, large-scale and convective.
! Here we group all precipitation into 2 classes.
ls_p_rate = cosp_hmodel%mr_gbx(:,:,i_lsrain) +                                 &
            cosp_hmodel%mr_gbx(:,:,i_lsiagg) +                                 &
            cosp_hmodel%mr_gbx(:,:,i_lsgrpl)
cv_p_rate = cosp_hmodel%mr_gbx(:,:,i_cvrain) +                                 &
            cosp_hmodel%mr_gbx(:,:,i_cvsnow)
! Calculate the subcolumn array with precip distribution
CALL prec_scops(cosp_hmodel%npoints, cosp_hmodel%nlevels,                      &
  cosp_hmodel%ncolumns, ls_p_rate, cv_p_rate, cosp_optical_in%frac_out,        &
  prec_frac)
DEALLOCATE(ls_p_rate, cv_p_rate)
! Compute L-S and CONV precip fractions from the subcolumn arrays.
frac_ls = 0.0_wp
frac_cv = 0.0_wp
DO k=1,nlevels
  DO j=1,nColumns
    DO i=1,npoints
      IF (prec_frac(i,j,k) == 1)                                               &
        frac_ls(i,k) = frac_ls(i,k) + one_over_ncolumns
      IF (prec_frac(i,j,k) == 2)                                               &
        frac_cv(i,k) = frac_cv(i,k) + one_over_ncolumns
      IF (prec_frac(i,j,k) == 3) THEN
        frac_ls(i,k) = frac_ls(i,k) + one_over_ncolumns
        frac_cv(i,k) = frac_cv(i,k) + one_over_ncolumns
      END IF
    END DO
  END DO
END DO

! Fill in the subcolumn volumes, converting mixing ratios to in-cloud values
! ***number concentration??
DO k=1,nlevels
  DO j=1,ncolumns
    DO i=1,npoints
      IF ((prec_frac(i,j,k) == 1) .OR. (prec_frac(i,j,k) == 3)) THEN
        !+++++++++++ LS clouds ++++++++
        IF (frac_ls(i,k) >= small_real) THEN
          cosp_hmodel%mr_hydro(i,j,k,i_lsrain)   =                             &
            cosp_hmodel%mr_gbx(i,k,i_lsrain)/frac_ls(i,k)
          cosp_hmodel%mr_hydro(i,j,k,i_lsiagg)   =                             &
            cosp_hmodel%mr_gbx(i,k,i_lsiagg)/frac_ls(i,k)
          cosp_hmodel%mr_hydro(i,j,k,i_lsgrpl)   =                             &
            cosp_hmodel%mr_gbx(i,k,i_lsgrpl)/frac_ls(i,k)
          cosp_hmodel%reff_hydro(i,j,k,i_lsrain) =                             &
            cosp_hmodel%reff_gbx(i,k,i_lsrain)
          cosp_hmodel%reff_hydro(i,j,k,i_lsiagg) =                             &
            cosp_hmodel%reff_gbx(i,k,i_lsiagg)
          cosp_hmodel%reff_hydro(i,j,k,i_lsgrpl) =                             &
            cosp_hmodel%reff_gbx(i,k,i_lsgrpl)
        END IF
      END IF
      !+++++++++++ CONV clouds ++++++
      IF ((prec_frac(i,j,k) == 2) .OR. (prec_frac(i,j,k) == 3)) THEN
        IF (frac_cv(i,k) >= small_real) THEN
          cosp_hmodel%mr_hydro(i,j,k,i_cvrain)   =                             &
            cosp_hmodel%mr_gbx(i,k,i_cvrain)/frac_cv(i,k)
          cosp_hmodel%mr_hydro(i,j,k,i_cvsnow)   =                             &
            cosp_hmodel%mr_gbx(i,k,i_cvsnow)/frac_cv(i,k)
          cosp_hmodel%reff_hydro(i,j,k,i_cvrain) =                             &
            cosp_hmodel%reff_gbx(i,k,i_cvcliq)
          cosp_hmodel%reff_hydro(i,j,k,i_cvsnow) =                             &
            cosp_hmodel%reff_gbx(i,k,i_cvsnow)
        END IF
      END IF
    END DO
  END DO
END DO
! For consistency, set to zero gridbox-mean volumes where
! subcolumn precip fraction is zero
WHERE (frac_ls < small_real)
  cosp_hmodel%mr_gbx(:,:,i_lsrain) = 0.0_wp
  cosp_hmodel%mr_gbx(:,:,i_lsiagg) = 0.0_wp
  cosp_hmodel%mr_gbx(:,:,i_lsgrpl) = 0.0_wp
  cosp_hmodel%reff_gbx(:,:,i_lsrain) = 0.0_wp
  cosp_hmodel%reff_gbx(:,:,i_lsiagg) = 0.0_wp
  cosp_hmodel%reff_gbx(:,:,i_lsgrpl) = 0.0_wp
END WHERE
WHERE (frac_cv < small_real)
  cosp_hmodel%mr_gbx(:,:,i_cvrain) = 0.0_wp
  cosp_hmodel%mr_gbx(:,:,i_cvsnow) = 0.0_wp
  cosp_hmodel%reff_gbx(:,:,i_cvrain) = 0.0_wp
  cosp_hmodel%reff_gbx(:,:,i_cvsnow) = 0.0_wp
END WHERE
DEALLOCATE(prec_frac, frac_ls, frac_cv)
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
END SUBROUTINE cosp_subgrid_homogeneous_precip

END MODULE cosp_subgrid_mod
