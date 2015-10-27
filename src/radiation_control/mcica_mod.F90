! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
!+ Global data module for variables used in McICA scheme
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: Radiation Control
!
!- --------------------------------------------------------------------- 

MODULE mcica_mod

  USE realtype_rd

  IMPLICIT NONE
  SAVE


! Variables Required for using MCICA scheme.

  INTEGER :: index_subcol
!     The index of the current subcolumn

  INTEGER :: ipph
!     Plane-parallel homogeneous flag

  INTEGER :: ioverlap
!     Overlap flag

  INTEGER :: tot_subcol_gen=64
!     number of sub-columns to generate

  INTEGER,  ALLOCATABLE :: sw_subcol_k(:,:)
!     Number of subcolumns for each k_term in each band.

  INTEGER,  ALLOCATABLE :: lw_subcol_k(:,:)
!     Number of subcolumns for each k_term in each band.

  INTEGER,  ALLOCATABLE :: subcol_k(:,:)
!     Number of subcolumns for each k_term in each band.

  INTEGER, ALLOCATABLE :: first_subcol_k(:,:)
!     The first subcolumn each k-term sees,  required to prevent each
!     band seeing only the same first few columns.

  INTEGER, ALLOCATABLE :: subcol_band(:)
!     Number of subcolumns for each band (i.e. the sum of the number of 
!     subcolumns for each k-term in that band)

  INTEGER, ALLOCATABLE :: first_subcol_band(:)
!     The first subcolumn each band sees,  required to prevent each
!     band seeing only the same first few columns.

  REAL, ALLOCATABLE  ::  frac_cloudy_full(:)
!     fraction of the profile which is cloudy, full array

  REAL, ALLOCATABLE  ::  frac_cloudy(:)
!     fraction of the profile which is cloudy

  INTEGER, ALLOCATABLE  ::  ncldy(:)
!     number of cloudy sub-columns in each profile

  REAL, ALLOCATABLE  ::  clw_sub_full(:,:,:)
!     Value of cloud liquid water content for each sub-column

  REAL, ALLOCATABLE  ::  cic_sub_full(:,:,:)
!     Value of cloud ice water content for each sub-column

  REAL, ALLOCATABLE  ::  c_sub(:,:,:,:)
!     Value of condensate content for each sub-column

  INTEGER ::  subcol_need=59
!     Number of cloudy sub-columns required (i.e. MAX of SW and LW)
  INTEGER ::  subcol_need_single
!     Number of cloudy sub-columns required for single sampling
  INTEGER ::  subcol_need_optimal
!     Number of cloudy sub-columns required for optimal sampling

  INTEGER, ALLOCATABLE ::  sw_subcol_reorder(:)
!     Order of sub-columns points to either SW or LW. (Sub-columns are 
!     rearranged so that each sub-column is equivalently as important  
!     in the LW as in the SW

  INTEGER, ALLOCATABLE ::  lw_subcol_reorder(:)
!     LW order of sub-columns

  INTEGER, ALLOCATABLE ::  lw_subcol_reorder_single(:)
!     LW order of sub-columns (for single sampling)

  INTEGER, ALLOCATABLE ::  lw_subcol_reorder_optimal(:)
!     LW order of sub-columns (for optimal sampling)

  INTEGER, ALLOCATABLE ::  subcol_reorder(:)
!     SW order of sub-columns

  INTEGER, PARAMETER :: ip_mcica_full_sampling = 0
!     Each k-term "sees" every sub-column

  INTEGER, PARAMETER :: ip_mcica_single_sampling = 1
!     Each k-term "sees" a single sub-column

  INTEGER, PARAMETER :: ip_mcica_optimal_sampling = 2
!     Each k-term "sees" an optimal number of sub-columns
  
  REAL, ALLOCATABLE ::  rand_seed_x(:, :)
!     global array of random numbers for first level in cloud generator     

  REAL, ALLOCATABLE ::  rand_seed_y(:, :)
!     global array of random numbers for first level in cloud generator     

  REAL, PARAMETER :: cut  = 0.001
!     Cutoff for minimum cloud amount in a layer

  INTEGER :: n1, n2
!     Dimensions of xcw array:
!       Cumulative probability (n1)
!       Relative standard deviation (n2)

  REAL (RealK), ALLOCATABLE :: xcw(:,:)
!     Distribution of normalised condensate amount as a function of
!     cumulative probability and relative standard deviation.

  LOGICAL :: l_avg_phase_fnc = .FALSE.

!$OMP THREADPRIVATE (subcol_k, subcol_reorder, c_sub,                   &
!$OMP&               frac_cloudy, subcol_band,                          &
!$OMP&               first_subcol_band, first_subcol_k,index_subcol)

  CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='MCICA_MOD'

  CONTAINS

  SUBROUTINE read_mcica_data(mcica_data)

    USE dimensions_spec_ucf, ONLY: npd_band, npd_k_term
    USE yomhook, ONLY: lhook, dr_hook
    USE parkind1, ONLY: jprb, jpim
    USE ereport_mod, ONLY : ereport

    IMPLICIT NONE

! Intent IN arguments

    CHARACTER (LEN=200), INTENT(IN) :: MCICA_DATA
!     Path to McICA data file


! Local variables

    CHARACTER (LEN=*), PARAMETER :: RoutineName = 'READ_MCICA_DATA'
    INTEGER, PARAMETER :: iu_mcd = 80
    INTEGER            :: icode
    CHARACTER (LEN=80) :: cmessage
    CHARACTER (LEN=80) :: line
    INTEGER            :: band, k, subcol

    INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
    INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
    REAL(KIND=jprb)               :: zhook_handle


    IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

    OPEN(UNIT=iu_mcd, FILE=mcica_data, IOSTAT=icode, STATUS='OLD')
    IF (icode /= 0) THEN
      cmessage='McICA data file could not be opened.'
      GO TO 9999
    END IF

    DO
!     Read header until data block is found
      READ(iu_mcd, '(a80)', IOSTAT=icode) line
      IF (line(1:5) == '*DATA') EXIT
      IF (icode /= 0) THEN
        cmessage = 'No *DATA block present in McICA data file'
        GO TO 9999
      END IF
    END DO

    DO
!     Read variables from data block
      READ(iu_mcd, '(a80)', IOSTAT=icode) line
      IF (line(1:4) == '*END') EXIT

      SELECT CASE (line)

      CASE ('tot_subcol_gen')
        READ(iu_mcd, *, IOSTAT=icode) tot_subcol_gen
      CASE ('subcol_need_single')
        READ(iu_mcd, *, IOSTAT=icode) subcol_need_single
      CASE ('subcol_need_optimal')
        READ(iu_mcd, *, IOSTAT=icode) subcol_need_optimal
      CASE ('ipph')
        READ(iu_mcd, *, IOSTAT=icode) ipph
      CASE ('ioverlap')
        READ(iu_mcd, *, IOSTAT=icode) ioverlap

      CASE ('lw_subcol_reorder_single')
        ALLOCATE(lw_subcol_reorder_single(subcol_need_single),          &
          STAT=icode)
        IF (icode /= 0) THEN
          cmessage = 'Cannot allocate array: lw_subcol_reorder_single'
          GO TO 9999
        END IF
        READ(iu_mcd, *, IOSTAT=icode) lw_subcol_reorder_single

      CASE ('lw_subcol_reorder_optimal')
        ALLOCATE(lw_subcol_reorder_optimal(subcol_need_optimal),        &
          STAT=icode)
        IF (icode /= 0) THEN
          cmessage = 'Cannot allocate array: lw_subcol_reorder_optimal'
          GO TO 9999
        END IF
        READ(iu_mcd, *, IOSTAT=icode) lw_subcol_reorder_optimal

      CASE ('sw_subcol_k')
        ALLOCATE(sw_subcol_k(npd_band, npd_k_term), STAT=icode)
        IF (icode /= 0) THEN
          cmessage = 'Cannot allocate array: sw_subcol_k'
          GO TO 9999
        END IF
        sw_subcol_k=1
        DO
          READ(iu_mcd, '(3i4)', IOSTAT=icode) band, k, subcol
          IF (band == -99) EXIT
          sw_subcol_k(band,k)=subcol
          IF (icode /= 0) THEN
            cmessage = 'Error reading data for sw_subcol_k'
            GO TO 9999
          END IF
        END DO

      CASE ('lw_subcol_k')
        ALLOCATE(lw_subcol_k(npd_band, npd_k_term), STAT=icode)
        IF (icode /= 0) THEN
          cmessage = 'Cannot allocate array: lw_subcol_k'
          GO TO 9999
        END IF
        lw_subcol_k=1
        DO
          READ(iu_mcd, '(3i4)', IOSTAT=icode) band, k, subcol
          IF (band == -99) EXIT
          lw_subcol_k(band,k)=subcol
          IF (icode /= 0) THEN
            cmessage = 'Error reading data for lw_subcol_k'
            GO TO 9999
          END IF
        END DO

      CASE ('n1')
        READ(iu_mcd, *, IOSTAT=icode) n1
      CASE ('n2')
        READ(iu_mcd, *, IOSTAT=icode) n2
      CASE ('xcw')
        ALLOCATE(xcw(n1,n2), STAT=icode)
        IF (icode /= 0) THEN
          cmessage = 'Cannot allocate array: xcw'
          GO TO 9999
        END IF
        READ(iu_mcd, *, IOSTAT=icode) xcw

      END SELECT

      IF (icode /= 0) THEN
        cmessage = 'Error reading data from McICA data file'
        GO TO 9999
      END IF
    END DO

    CLOSE(iu_mcd)


! Check error condition
 9999 IF (icode /= 0) THEN
      CALL ereport(RoutineName, icode, cmessage)
    END IF
    IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
    RETURN

  END SUBROUTINE read_mcica_data

END MODULE mcica_mod
