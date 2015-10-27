! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
!  Function to set number of source coefficients.
!
! Method:
!   The two-stream approximation is examined and the number
!   of coefficients is set accordingly.
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: Radiance Core
!
!- ---------------------------------------------------------------------
FUNCTION set_n_source_coeff(isolir, l_ir_source_quad)


  USE rad_pcf
  USE yomhook, ONLY: lhook, dr_hook
  USE parkind1, ONLY: jprb, jpim

  IMPLICIT NONE


! Dummy arguments.
  INTEGER, INTENT(IN) ::                                                &
      isolir
!       Spectral region
  LOGICAL, INTENT(IN) ::                                                &
      l_ir_source_quad
!       Flag for quadratic infra-red source

  INTEGER ::                                                            &
      set_n_source_coeff
!       Returned number of source coefficients

  INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
  INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
  REAL(KIND=jprb)               :: zhook_handle

  CHARACTER(LEN=*), PARAMETER :: RoutineName='SET_N_SOURCE_COEFF'


  IF (lhook) CALL dr_hook(RoutineName,zhook_in,zhook_handle)

  IF (isolir == ip_solar) THEN
    set_n_source_coeff=2
  ELSE
    IF (l_ir_source_quad) THEN
      set_n_source_coeff=2
    ELSE
      set_n_source_coeff=1
    END IF
  END IF


  IF (lhook) CALL dr_hook(RoutineName,zhook_out,zhook_handle)

END FUNCTION set_n_source_coeff
