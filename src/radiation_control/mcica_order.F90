! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Calculate the first sub-grid cloud column to be sampled by each k-term
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: Radiation Control
!
!------------------------------------------------------------------------------
SUBROUTINE mcica_order(control, spectrum, cld)

USE def_control,  ONLY: StrCtrl
USE def_spectrum, ONLY: StrSpecData
USE def_cld,      ONLY: StrCld
USE yomhook,      ONLY: lhook, dr_hook
USE parkind1,     ONLY: jprb, jpim

IMPLICIT NONE


! Control options:
TYPE(StrCtrl),      INTENT(IN)    :: control

! Spectral data:
TYPE (StrSpecData), INTENT(IN)    :: spectrum

! Cloud properties:
TYPE(StrCld),       INTENT(INOUT) :: cld


! Local variables.
INTEGER :: i, j
!   Loop variables
INTEGER :: subcol_band(spectrum%dim%nd_band)
!   Number of subcolumns for each band (i.e. the sum of the number of
!   subcolumns for each k-term in that band)
INTEGER :: first_subcol_band(spectrum%dim%nd_band + 1)
!   The first subcolumn each band sees, required to prevent each
!   band seeing only the same first few columns.

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='MCICA_ORDER'


IF (lhook) CALL dr_hook(RoutineName,zhook_in,zhook_handle)

subcol_band                           = 0
first_subcol_band(control%first_band) = 1
cld%first_subcol_k                    = 0

! These values may be read in from the spectral files eventually, but
! until that functionality is added, we set them here.
DO i = control%first_band, control%last_band
  DO j = 1, spectrum%gas%i_band_k(i,spectrum%gas%index_absorb(1,i))
    subcol_band(i) = subcol_band(i) + cld%subcol_k(i,j)
  END DO

  first_subcol_band(i+1)  = first_subcol_band(i) + subcol_band(i)
  cld%first_subcol_k(i,1) = first_subcol_band(i)

  DO j = 1, spectrum%gas%i_band_k(i,spectrum%gas%index_absorb(1,i))
    cld%first_subcol_k(i,j+1) = cld%first_subcol_k(i,j) + cld%subcol_k(i,j)
  END DO
END DO

IF (lhook) CALL dr_hook(RoutineName,zhook_out,zhook_handle)

END SUBROUTINE mcica_order
