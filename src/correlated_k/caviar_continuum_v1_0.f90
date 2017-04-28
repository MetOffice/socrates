! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Routines to calculate water vapour continuum absorption (CAVIAR)
!
MODULE caviar_continuum_v1_0

CONTAINS

  ! Self-broadened H2O continuum
  SUBROUTINE self_continuum ( &
       t,               & ! in
       p,               & ! in
       partial_p,       & ! in
       n,               & ! in
       wave_no,         & ! in
       l_return_m5pkg2, & ! in
       k_self           ) ! out

    ! Description:
    ! Calculates H2O continuum absorption using CAVIAR_1.0 coefficients
    ! Laboratory data where available supplied by Igor Ptashnik
    ! have been merged with MT_CKD-2.5 coefficients. Note that
    ! correction factors have been pre-applied.
    !
    ! If l_return_m5pkg2 == .TRUE., the continuum absorption is returned
    ! with the unit m5/kg2 (absorption per mass density of water vapour
    ! squared), which only depends on temperature (i.e. p and
    ! partial_p are not used). Otherwise the unit will be m2/kg,
    ! absorption per mass density of water vapour evaluated at the
    ! total pressure p and water vapour partial pressure partial_p.
    !
    ! Note: Uses ALLOCATE  on pointers!!!
    ! but DEALLOCATEs all variables before exiting.

    USE realtype_rd
    USE rad_ccf,         ONLY: r_gas
    USE gas_list_pcf,    ONLY: ip_h2o, molar_weight
    USE hitran_cnst
    USE ckd_extern_data   ! c_self_h2o_260, c_self_h2o_296,
                          ! and c_foreign_h2o_296 are set in here

    IMPLICIT NONE

    ! Scalar arguments with intent(in):

    REAL(RealK), Intent(IN) :: t
    REAL(RealK), Intent(IN) :: p
    REAL(RealK), Intent(IN) :: partial_p

    INTEGER, Intent(IN) :: n

    LOGICAL, Intent(IN) :: l_return_m5pkg2

    ! Array arguments with intent(in):

    REAL(RealK), Intent(IN) :: wave_no(n)

    ! Array arguments with intent(out):

    REAL(RealK), Intent(OUT), Dimension(n) :: k_self

    ! Local parameters:

    CHARACTER (LEN=*), Parameter :: routine_name = 'self_continuum'

    REAL  (RealK), Parameter :: tref = 296.0
    REAL  (RealK), Parameter :: pref = 1.013e+05

    ! Local scalars:

    REAL  (RealK) :: tfac
    REAL  (RealK) :: c_self

    REAL  (RealK) :: x
    REAL  (RealK) :: y2

    INTEGER :: allocation_status
    INTEGER :: i, ic

    INTEGER :: ierr  ! Error flag

    ! Local arrays:

    REAL  (RealK), Dimension(:), Allocatable :: cont_nu
    REAL  (RealK), Dimension(:), Allocatable :: d2y_s296
    REAL  (RealK), Dimension(:), Allocatable :: d2y_s260

    REAL  (RealK), Dimension(n) :: cs_296
    REAL  (RealK), Dimension(n) :: cs_260

    !- End of header

    tfac = (296.0 - t) / (296.0 - 260.0)

  ! Define the self-broadened continuum at 296K at the reference points.
    ALLOCATE(cont_nu(c_self_h2o_296%n_freq), STAT = allocation_status)
    IF (allocation_status .NE. 0 ) THEN
      PRINT *, "Couldn't allocate memory for cont_nu."
    ENDIF

    DO i = 1, c_self_h2o_296%n_freq
      cont_nu(i) = c_self_h2o_296%table_start + REAL(i-1, RealK) * &
        c_self_h2o_296%table_inc
    END DO

    ALLOCATE(d2y_s296(c_self_h2o_296%n_freq), STAT = allocation_status)
    IF (allocation_status .NE. 0 ) THEN
      PRINT *, "Couldn't allocate memory for d2y_s296."
    ENDIF

    CALL spline_fit(c_self_h2o_296%n_freq, &
      cont_nu, c_self_h2o_296%c, d2y_s296)
    DO i = 1, n
      x = 0.01_RealK * wave_no(i)
      CALL spline_evaluate(ierr, c_self_h2o_296%n_freq, &
        cont_nu, c_self_h2o_296%c, d2y_s296, x, cs_296(i))
    END DO

    DEALLOCATE(cont_nu,  STAT = allocation_status)
    IF (allocation_status .NE. 0 ) THEN
      PRINT *, "Deallocation problem for cont_nu."
    ENDIF
    DEALLOCATE(d2y_s296, STAT = allocation_status)
    IF (allocation_status .NE. 0 ) THEN
      PRINT *, "Deallocation problem for d2y_s296."
    ENDIF
  !
  !
  ! Repeat at 260 K.
    ALLOCATE(cont_nu(c_self_h2o_260%n_freq), STAT = allocation_status)
    IF (allocation_status .NE. 0 ) THEN
      PRINT *, "Couldn't allocate memory for cont_nu."
    ENDIF

    DO i = 1, c_self_h2o_260%n_freq
      cont_nu(i) = c_self_h2o_260%table_start + REAL(i-1, RealK) * &
        c_self_h2o_260%table_inc
    END DO

    ALLOCATE(d2y_s260(c_self_h2o_260%n_freq), STAT = allocation_status)
    IF (allocation_status .NE. 0 ) THEN
      PRINT *, "Couldn't allocate memory for d2y_s260."
    ENDIF

    CALL spline_fit(c_self_h2o_260%n_freq, &
      cont_nu, c_self_h2o_260%c, d2y_s260)
    DO i = 1, n
      x = 0.01 * wave_no(i)
      CALL spline_evaluate(ierr, c_self_h2o_260%n_freq, &
        cont_nu, c_self_h2o_260%c, d2y_s260, x, cs_260(i))
    END DO
    DEALLOCATE(cont_nu, STAT = allocation_status)
    IF (allocation_status .NE. 0 ) THEN
      PRINT *, "Deallocation problem for cont_nu."
    ENDIF
    DEALLOCATE(d2y_s260, STAT = allocation_status)
    IF (allocation_status .NE. 0 ) THEN
      PRINT *, "Deallocation problem for d2y_s260."
    ENDIF


    DO i = 1, n

       ! Clough et al temperature interpolation & conversion to SI

       c_self = 1.0e-20 &   ! data have been multiplied by 1e20
               *1.0e-06*avogadro_number*cs_296(i) &  ! convert to SI units
               *(cs_260(i)/cs_296(i))**tfac

       ! Radiation term

       c_self = c_self * wave_no(i) * TANH(wave_no(i)*c2 / (2.0*t))

       ! pressure/temperature scaling

       IF (l_return_m5pkg2) THEN
         k_self(i) = c_self*r_gas*tref/(pref &
                 *(molar_weight(ip_h2o)*1.0E-03_RealK)**2)
       ELSE
         k_self(i) = c_self &
                 *(tref/t)*(partial_p/pref) &
                 /(molar_weight(ip_h2o)*1.0E-03_RealK)
       END IF

    END DO

  END SUBROUTINE self_continuum


  ! foreign-broadened H2O continuum
  SUBROUTINE foreign_continuum ( &
       t,               & ! in
       p,               & ! in
       partial_p,       & ! in
       n,               & ! in
       wave_no,         & ! in
       l_return_m5pkg2, & ! in
       k_foreign        ) ! out

    ! Description:
    ! Calculates H2O continuum absorption using CAVIAR scheme.
    ! Laboratory data where available supplied by Igor Ptashnik
    ! have been merged with MT_CKD-2.5 coefficients. Note that
    ! correction factors have been pre-applied.
    !
    ! If l_return_m5pkg2 == .TRUE., the continuum absorption is returned
    ! with the unit m5/kg2 (absorption per mass density of water vapour
    ! per mass density of dry air), which only depends on temperature
    ! (i.e. p and partial_p are not used). Otherwise the unit will be m2/kg,
    ! absorption per mass density of water vapour evaluated at the
    ! total pressure p and water vapour partial pressure partial_p.

    USE realtype_rd
    USE hitran_cnst
    USE rad_ccf,         ONLY: r_gas
    USE gas_list_pcf,    ONLY: ip_h2o, ip_air, molar_weight
    USE ckd_extern_data

    IMPLICIT NONE

  ! Scalar arguments with intent(in):

    REAL(RealK), Intent(IN) :: t
    REAL(RealK), Intent(IN) :: p
    REAL(RealK), Intent(IN) :: partial_p

    INTEGER, Intent(IN) :: n

    LOGICAL, Intent(IN) :: l_return_m5pkg2

  ! Array arguments with intent(in):

    REAL(RealK), Intent(IN) :: wave_no(n)

  ! Array arguments with intent(out):

    REAL(RealK), Intent(OUT), Dimension(n) :: k_foreign

  ! Local parameters:

    CHARACTER (LEN=*), Parameter :: routine_name = 'foreign_continuum'

    REAL(RealK), Parameter :: tref = 296.0
    REAL(RealK), Parameter :: pref = 1.013e+05

  ! Local scalars:

    REAL(RealK) :: c_foreign
    REAL(RealK) :: x

    INTEGER :: allocation_status
    INTEGER :: i

    INTEGER :: ierr  ! Error flag

  ! Local arrays:

    REAL(RealK), Dimension(:), Allocatable :: cont_nu
    REAL(RealK), Dimension(:), Allocatable :: d2y_f296

    REAL(RealK), Dimension(n) :: cf_296

  !- End of header

  ! Set up frequencies in inverse metres: conversion of coefficients
  ! occurs later.
    ALLOCATE(cont_nu(c_foreign_h2o_296%n_freq), STAT = allocation_status)
    IF (allocation_status .NE. 0 ) THEN
      PRINT *, "Couldn't allocate memory for cont_nu."
    ENDIF

    DO i = 1, c_foreign_h2o_296%n_freq
      cont_nu(i) = 100.0 * (c_foreign_h2o_296%table_start + &
        REAL(i-1)*c_foreign_h2o_296%table_inc)
    END DO

    ALLOCATE(d2y_f296(c_foreign_h2o_296%n_freq), STAT = allocation_status)
    IF (allocation_status .NE. 0 ) THEN
      PRINT *, "Couldn't allocate memory for d2y_f296."
    ENDIF

    CALL spline_fit(c_foreign_h2o_296%n_freq, &
      cont_nu, c_foreign_h2o_296%c, d2y_f296)
    DO i = 1, n
      x = wave_no(i)
      CALL spline_evaluate(ierr, c_foreign_h2o_296%n_freq, &
        cont_nu, c_foreign_h2o_296%c, d2y_f296, x, cf_296(i))
    END DO
    DEALLOCATE(cont_nu, STAT = allocation_status)
    IF (allocation_status .NE. 0 ) THEN
      PRINT *, "Problem deallocating cont_nu."
    ENDIF
    DEALLOCATE(d2y_f296, STAT = allocation_status)
    IF (allocation_status .NE. 0 ) THEN
      PRINT *, "Problem deallocating cont_nu."
    ENDIF


    DO i = 1, n

       c_foreign = 1.0e-20 &   ! data have been multiplied by 1e20
                  *1.0e-06*avogadro_number*cf_296(i)  ! convert to SI units

       ! Radiation term

       c_foreign = c_foreign * wave_no(i) * TANH(wave_no(i)*c2 / (2.0*t))

       ! pressure/temperature scaling

       IF (l_return_m5pkg2) THEN
         k_foreign(i) = c_foreign*r_gas*tref/(pref &
              *molar_weight(ip_h2o)*1.0E-03_RealK &
              *molar_weight(ip_air)*1.0E-03_RealK)
       ELSE
         k_foreign(i) = c_foreign &
              *(tref/t)*((p-partial_p)/pref) &
              /(molar_weight(ip_h2o)*1.0E-03_RealK)
       END IF

    END DO

  END SUBROUTINE foreign_continuum


  ! calculates saturation vapour pressure
  FUNCTION sat_vap_press ( &
       t, &
       p  ) &

  RESULT(e_sat)

    ! Description:
    ! Uses formula given in Gill, Atmosphere-ocean Dynamics

    USE realtype_rd

    IMPLICIT NONE

    ! Scalar arguments with intent(in):

    REAL(RealK), Intent(IN) :: t
    REAL(RealK), Intent(IN) :: p

    REAL(RealK) :: e_sat


    e_sat = 1.0E+02*(1.0+1.0E-08*p &
          *(4.5E+00+6.0E-04*(t-273.15E+00)**2)) &
          *EXP((8.0061E-02*t-20.059E+00)/(4.12E-03*t-0.125378E+00))

  END FUNCTION sat_vap_press

END MODULE  caviar_continuum_v1_0
