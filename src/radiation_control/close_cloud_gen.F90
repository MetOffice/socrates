! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

!  Subroutine to deallocate cloud sub-columns required for McICA

! Method:
!       Checks whether each of variables allocated in open_cloud_gen is
!       allocated, and if so deallocates it.

!

! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: Radiation Control

!- ---------------------------------------------------------------------
SUBROUTINE close_cloud_gen

  USE mcica_mod
  USE yomhook, ONLY: lhook, dr_hook
  USE parkind1, ONLY: jprb, jpim
  IMPLICIT NONE

  INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
  INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
  REAL(KIND=jprb)               :: zhook_handle

  CHARACTER(LEN=*), PARAMETER :: RoutineName='CLOSE_CLOUD_GEN'

  IF (lhook) CALL dr_hook(RoutineName,zhook_in,zhook_handle)
  IF (ALLOCATED(ncldy)) DEALLOCATE(ncldy)
  IF (ALLOCATED(frac_cloudy_full)) DEALLOCATE(frac_cloudy_full)
  IF (ALLOCATED(cic_sub_full)) DEALLOCATE(cic_sub_full)
  IF (ALLOCATED(clw_sub_full)) DEALLOCATE(clw_sub_full)
  IF (ALLOCATED(lw_subcol_reorder)) DEALLOCATE(lw_subcol_reorder)
  IF (ALLOCATED(sw_subcol_reorder)) DEALLOCATE(sw_subcol_reorder)

  IF (lhook) CALL dr_hook(RoutineName,zhook_out,zhook_handle)
  RETURN
END SUBROUTINE close_cloud_gen
