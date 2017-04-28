! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Routines to calculate water vapour continuum absorption (Vn2.4)
!
MODULE ckd_continuum_v2_4

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
    ! Calculates H2O continuum absorption using version 2.4 of CKD method
    !
    ! If l_return_m5pkg2 == .TRUE., the continuum absorption is returned
    ! with the unit m5/kg2 (absorption per mass density of water vapour
    ! squared), which only depends on temperature (i.e. p and
    ! partial_p are not used). Otherwise the unit will be m2/kg,
    ! absorption per mass density of water vapour evaluated at the
    ! total pressure p and water vapour partial pressure partial_p.
    !
    ! Method:
    !   This is a direct implementation of the CKD scheme and is
    ! described in external documentation.
    !
    ! Note: Uses ALLOCATE  on pointers!!!
    ! but DEALLOCATEs all variables before exiting.

    USE realtype_rd
    USE rad_ccf,         ONLY: r_gas
    USE gas_list_pcf,    ONLY: ip_h2o, molar_weight
    USE hitran_cnst
    USE rad_ccf, ONLY: r_gas
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

    REAL  (RealK), Parameter :: factor_self = -0.2333e+00
    REAL  (RealK), Parameter :: alpha2_self =  4.00e+08
    REAL  (RealK), Parameter :: v0_self     =  1.05e+05

    REAL  (RealK), Parameter :: factor_self_2 = -0.15e+00
    REAL  (RealK), Parameter :: alpha2_self_2 =  1.44e+08
    REAL  (RealK), Parameter :: v0_self_2     =  1.31e+05
    REAL  (RealK), Parameter :: beta_0_self   =  5.0e-10

    REAL  (RealK), Parameter :: factor_self_3 =  0.688e+00
    REAL  (RealK), Parameter :: alpha2_self_3 =  1.00e+08
    REAL  (RealK), Parameter :: v0_self_3     =  0.0e+00
    REAL  (RealK), Parameter :: beta_0_self_3 =  1.0e-08

    REAL  (RealK), Parameter :: tref = 296.0
    REAL  (RealK), Parameter :: pref = 1.013e+05

    ! Local scalars:

    REAL  (RealK) :: tfac
    REAL  (RealK) :: c_self
    REAL  (RealK) :: vs2

    REAL  (RealK) :: x
    REAL  (RealK) :: y1, y2

    REAL  (RealK) :: fc, gc

    INTEGER :: allocation_status
    INTEGER :: i, ic

    INTEGER :: ierr  ! Error flag

    ! Local arrays:

    REAL  (RealK), Dimension(:), Allocatable :: cont_nu
    REAL  (RealK), Dimension(:), Allocatable :: d2y_s296
    REAL  (RealK), Dimension(:), Allocatable :: d2y_s260

    REAL  (RealK), Dimension(n) :: cs_296
    REAL  (RealK), Dimension(n) :: cs_260

    REAL  (RealK), Dimension(0:51) :: xfac = &
    ! CKD 2.n self correction factors between 700 and 1200 cm-1
         (/ 1.00000E+00, 1.01792E+00, 1.03767E+00, 1.05749E+00, &
            1.07730E+00, 1.09708E+00, 1.10489E+00, 1.11268E+00, &
            1.12047E+00, 1.12822E+00, 1.13597E+00, 1.14367E+00, &
            1.15135E+00, 1.15904E+00, 1.16669E+00, 1.17431E+00, &
            1.18786E+00, 1.20134E+00, 1.21479E+00, 1.22821E+00, &
            1.24158E+00, 1.26580E+00, 1.28991E+00, 1.28295E+00, &
            1.27600E+00, 1.26896E+00, 1.25550E+00, 1.24213E+00, &
            1.22879E+00, 1.21560E+00, 1.20230E+00, 1.18162E+00, &
            1.16112E+00, 1.14063E+00, 1.12016E+00, 1.10195E+00, &
            1.09207E+00, 1.08622E+00, 1.08105E+00, 1.07765E+00, &
            1.07398E+00, 1.06620E+00, 1.05791E+00, 1.04905E+00, &
            1.03976E+00, 1.02981E+00, 1.00985E+00, 1.00000E+00, &
            1.00000E+00, 1.00000E+00, 1.00000E+00, 1.00000E+00 /)


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


       !  calculate self modification factors, using xfac values given above,
       !  between 700 and 1200 cm-1

       IF (wave_no(i) >= 70000.0 .and. wave_no(i) <= 120000.0) THEN

          ic = INT(0.001*(wave_no(i) - 70000.0))
          fc = 70000.0 + ic*1000.0
          gc = (wave_no(i) - fc)/1000.0

          c_self = c_self * (xfac(ic) + gc*(xfac(ic+1)-xfac(ic)))

       END IF

       ! make the FASCODE correction of 0.7667 at 1050cm-1

       c_self = c_self*(1.0e+00 + factor_self*alpha2_self &
            /((wave_no(i)-v0_self)**2 + alpha2_self))

       ! make the secondary correction (1310 cm-1)

           vs2 = (wave_no(i) - v0_self_2)**2

           c_self = c_self*(1.0e+00 + factor_self_2*(alpha2_self_2 &
                /(alpha2_self_2 + vs2*(1.0e+00 &
                + beta_0_self*vs2))))

       ! make the self microwave patch correction (CKD2.2, revised CKD2.4)

           vs2 = (wave_no(i) - v0_self_3)**2

           c_self = c_self*(1.0e+00 + factor_self_3*(alpha2_self_3 &
                /(alpha2_self_3 + vs2*(1.0e+00 &
                + beta_0_self_3*vs2))))

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
    ! Calculates H2O continuum absorption using ckd method
    !
    ! If l_return_m5pkg2 == .TRUE., the continuum absorption is returned
    ! with the unit m5/kg2 (absorption per mass density of water vapour
    ! per mass density of dry air), which only depends on temperature
    ! (i.e. p and partial_p are not used). Otherwise the unit will be m2/kg,
    ! absorption per mass density of water vapour evaluated at the
    ! total pressure p and water vapour partial pressure partial_p.

    USE realtype_rd
    USE rad_ccf,         ONLY: r_gas
    USE gas_list_pcf,    ONLY: ip_h2o, ip_air, molar_weight
    USE hitran_cnst
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

    REAL  (RealK), Parameter ::  beta_0_frn =  8.0e-19
    REAL  (RealK), Parameter ::  alpha2_frn =  1.089e+09
    REAL  (RealK), Parameter ::  factor_frn = -0.97e+00
    REAL  (RealK), Parameter ::  v0_frn     =  1.130e+05

    REAL  (RealK), Parameter ::  beta_0_frn_2 =  5.0e-10
    REAL  (RealK), Parameter ::  alpha2_frn_2 =  6.25e+08
    REAL  (RealK), Parameter ::  factor_frn_2 = -0.65e+00
    REAL  (RealK), Parameter ::  v0_frn_2     =  1.975e+05

    REAL  (RealK), Parameter :: beta_0_frn_3 =  5.0e-17
    REAL  (RealK), Parameter :: alpha2_frn_3 =  4.0e+08
    REAL  (RealK), Parameter :: factor_frn_3 = -0.7e+00
    REAL  (RealK), Parameter :: v0_frn_3     =  3.5e+04

    REAL  (RealK), Parameter :: beta_0_frn_4 = 2.0e-16
    REAL  (RealK), Parameter :: alpha2_frn_4 = 4.225e+07
    REAL  (RealK), Parameter :: factor_frn_4 = 0.75e+00
    REAL  (RealK), Parameter :: v0_frn_4     = 6.3e+04

    REAL(RealK), Parameter :: tref = 296.0
    REAL(RealK), Parameter :: pref = 1.013e+05

    REAL(RealK), Parameter :: molar_weight_h2o = 18.0153E-03
    REAL(RealK), Parameter :: molar_weight_air = 28.9645E-03

  ! Local scalars:

    REAL(RealK) :: c_foreign
    REAL(RealK) :: vf2

    REAL(RealK) :: x
    REAL(RealK) :: y3

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


       ! make the first (1130 cm-1) FASCODE correction to the foreign continuum

       vf2 = (wave_no(i) - v0_frn)**2

       c_foreign = c_foreign &
                  *(1.0+factor_frn*alpha2_frn/(alpha2_frn+vf2 &
                  *(1.0+beta_0_frn*vf2**2)))

       ! make the second (1975 cm-1) FASCODE correction to the foreign continuum

       vf2 = (wave_no(i) - v0_frn_2)**2

       c_foreign = c_foreign &
            *(1.0e+00+factor_frn_2*alpha2_frn_2/ &
            (alpha2_frn_2+vf2 &
            *(1.0e+00+beta_0_frn_2*vf2)))

       ! make the third (350 cm-1) FASCODE correction to the foreign continuum

       vf2 = (wave_no(i) - v0_frn_3)**2

       c_foreign = c_foreign &
            *(1.0e+00+factor_frn_3*alpha2_frn_3/ &
            (alpha2_frn_3+vf2 &
            *(1.0e+00+beta_0_frn_3*vf2**2)))

       ! make the fourth (630 cm-1) FASCODE correction to the foreign continuum

       vf2 = (wave_no(i) - v0_frn_4)**2

       c_foreign = c_foreign &
            *(1.0e+00+factor_frn_4*alpha2_frn_4/ &
            (alpha2_frn_4+vf2 &
            *(1.0e+00+beta_0_frn_4*vf2**2)))

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

END MODULE  ckd_continuum_v2_4
