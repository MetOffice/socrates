! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! This module defines a type (CospDiag) which holds a pointer for each
! COSP2 diagnostic
!
!----------------------------------------------------------------------

module cosp_def_diag

use realtype_rd, only: RealExt

implicit none

type :: cospdiag

! Priority COSP diagnostics

real(RealExt), pointer :: cosp_calipso_low_level_cl_mask(:) => null()
! COSP: MASK FOR CALIPSO LOW-LEVEL CF (was 2321)

real(RealExt), pointer :: cosp_calipso_mid_level_cl_mask(:) => null()
! COSP: MASK FOR CALIPSO MID-LEVEL CF (was 2322)

real(RealExt), pointer :: cosp_calipso_high_level_cl_mask(:) => null()
! COSP: MASK FOR CALIPSO HIGH-LEVEL CF (was 2323)

real(RealExt), pointer :: cosp_calipso_cf_40_mask(:,:) => null()
! COSP: MASK FOR CALIPSO CF 40 LVLS (was 2325)

real(RealExt), pointer :: cosp_cloud_weights(:) => null()
! COSP: ISCCP/MISR/MODIS CLOUD WEIGHTS (was 2330)

real(RealExt), pointer :: cosp_ctp_tau_histogram(:,:,:) => null()
! COSP: ISCCP CTP-TAU HISTOGRAM (was 2337)

real(RealExt), pointer :: cosp_calipso_tot_backscatter(:,:,:) => null()
! COSP: CALIPSO TOTAL BACKSCATTER (was 2341)

real(RealExt), pointer :: cosp_calipso_low_level_cl(:) => null()
! COSP: CALIPSO LOW-LEVEL CLOUD (was 2344)

real(RealExt), pointer :: cosp_calipso_mid_level_cl(:) => null()
! COSP: CALIPSO MID-LEVEL CLOUD (was 2345)

real(RealExt), pointer :: cosp_calipso_high_level_cl(:) => null()
! COSP: CALIPSO HIGH-LEVEL CLOUD (was 2346)

real(RealExt), pointer :: cosp_calipso_cfad_sr_40(:,:,:) => null()
! COSP: CALIPSO CFAD SR 40 CSAT LEVELS (was 2370)

real(RealExt), pointer :: cosp_calipso_cf_40_liq(:,:) => null()
! COSP: CALIPSO CF 40 LVLS (LIQ) (was 2473)

real(RealExt), pointer :: cosp_calipso_cf_40_ice(:,:) => null()
! COSP: CALIPSO CF 40 LVLS (ICE) (was 2474)

real(RealExt), pointer :: cosp_calipso_cf_40_undet(:,:) => null()
! COSP: CALIPSO CF 40 LVLS (UNDET) (was 2475)

! Secondary COSP diagnostics

real(RealExt), pointer :: cosp_calipso_cloudsat_mdl_cl_mask(:,:) => null()
! COSP: MASK FOR (CALIPSO/CLOUDSAT CLOUD MDL LEV (which was 2358)) (was 2326)

real(RealExt), pointer :: cosp_calipso_cloudsat_40_cl_mask(:,:) => null()
! COSP: MASK FOR (CALIPSO/CLOUDSAT CLOUD 40 LEV (which was 2359)) (was 2327)

real(RealExt), pointer :: cosp_weighted_cloud_albedo(:) => null()
! COSP: ISCCP WEIGHTED CLOUD ALBEDO (was 2331)

real(RealExt), pointer :: cosp_weighted_ctp(:) => null()
! COSP: ISCCP WEIGHTED CLOUD TOP PRESSURE (was 2333)

real(RealExt), pointer :: cosp_tot_cloud_area(:) => null()
! COSP: ISCCP TOTAL CLOUD AREA (was 2334)

real(RealExt), pointer :: cosp_calipso_mol_atb_mdl(:,:) => null()
! COSP: CALIPSO MOLECULAR BACKSCATTER (was 2340)

real(RealExt), pointer :: cosp_cloudsat_gbxmean_ze_mdl(:,:) => null()
! COSP: GBX-MEAN CSAT Ze MDL LEVELS (was 2353)

real(RealExt), pointer :: cosp_cloudsat_gbxmean_ze_40(:,:) => null()
! COSP: GBX-MEAN CSAT Ze 40 LEVELS (was 2354)

real(RealExt), pointer :: cosp_calipso_gbxmean_atb_mdl(:,:) => null()
! COSP: GBX-MEAN CALIPSO ATB MDL LVLS (was 2355)

real(RealExt), pointer :: cosp_calipso_gbxmean_atb_40(:,:) => null()
! COSP: GBX-MEAN CALIPSO ATB 40 LVLS (was 2356)

real(RealExt), pointer :: cosp_calipso_mol_atb_40(:,:) => null()
! COSP: CALIPSO MOLECULAR ATB 40 LVLS (was 2357)

real(RealExt), pointer :: cosp_calipso_cloudsat_mdl_cl(:,:) => null()
! COSP: CALIPSO/CLOUDSAT CLOUD 40 LEV (was 2358)

real(RealExt), pointer :: cosp_calipso_cloudsat_40_cl(:,:) => null()
! COSP: CALIPSO/CLOUDSAT CLOUD 40 LEV (was 2359)

real(RealExt), pointer :: cosp_calipso_cloud_area_40(:,:) => null()
! COSP: CALIPSO CLOUD AREA 40 CSAT LEVELS (was 2371)

real(RealExt), pointer :: cosp_cloudsat_cfad_ze_40(:,:,:) => null()
! COSP: CLOUDSAT CFAD Ze 40 CSAT LVLS (was 2372)

end type cospdiag

end module cosp_def_diag
