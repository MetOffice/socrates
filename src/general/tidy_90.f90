! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
!+ Program to tidy a spectral file.
!
PROGRAM tidy_90
!
! Description:
!   This program tidies a spectral file, removing undesirable features
!   and unnecessary contributions.
!
! Method:
!      A menu of operations on a spectral file is displayed. The
!      user selects those which are to be carried out. The
!      modified spectral file is written out.
!
! Modules used
  USE realtype_rd, ONLY: RealK
  USE def_spectrum, ONLY: StrSpecData
  USE def_std_io_icf, ONLY: iu_stdout, iu_stdin, iu_err
  USE socrates_ereport, ONLY: ereport, errormessagelength, &
    i_normal, i_err_exist, i_err_fatal, i_err_io, i_warning

  IMPLICIT NONE
!
!
!
  CHARACTER (LEN=132) :: file_spectral
!   Name of the spectral file
  CHARACTER (LEN=1)   :: char_qna
!   Character response variable
  CHARACTER (LEN=1)   :: char_yn
!   Character response variable
  INTEGER :: ierr = i_normal
!   Error flag
  INTEGER :: i_process
!   Process to be carried out
  INTEGER :: ios
!   Error flag
  INTEGER :: i
!   Loop variable
  INTEGER :: j
!   Loop variable
  INTEGER :: i_gas
!   Species of gas
  INTEGER :: i_cont
!   Species of continuum
  LOGICAL  :: l_exist
!   Existence flag
!
  TYPE(StrSpecData) :: Spectrum
!   The spectral configuration to be defined

  CHARACTER (LEN=errormessagelength) :: cmessage
  CHARACTER (LEN=*), PARAMETER :: RoutineName = 'TIDY_90'


! Read in the spectral file.
  WRITE(*, "(a)") "Enter the name of the spectral file."
  READ(*, "(a)") file_spectral
  CALL read_spectrum(file_spectral, Spectrum)
!
!
! Determine whether data are to be appended to the old file,
! or whether a new file is to be written.
  WRITE(iu_stdout, '(/a)') 'Type'
  WRITE(iu_stdout, '(6x, a)') &
    '"o" to overwrite the existing file;'
  WRITE(iu_stdout, '(6x, a)') &
    '"n" to create a new file'
  WRITE(iu_stdout, '(/)')

  READ(iu_stdin, '(a)') char_qna
  SELECT CASE(char_qna)
    CASE('o', 'O')
      ! File will be overwritten
    CASE('n', 'N')
      WRITE(iu_stdout, '(a)') 'Enter the name of the new file'
      READ(iu_stdin, '(a)') file_spectral
      ! Check for existence of file.
      INQUIRE(file=file_spectral, exist=l_exist)
      IF (l_exist) THEN
        cmessage = 'File exists: will not overwrite.'
        ierr = i_err_exist
        CALL ereport(RoutineName, ierr, cmessage)
      END IF
    CASE DEFAULT
      cmessage = 'Erroneous response'
      ierr = i_err_fatal
      CALL ereport(RoutineName, ierr, cmessage)
  END SELECT

! Now decide which processes are needed.
  loop: DO
!
    WRITE(iu_stdout, '(/a)') &
      'Select from the following list of operations.'
!
    WRITE(iu_stdout, '(/6x, a)') &
      '1.   Remove gases and continua from bands where they are weak.'
    WRITE(iu_stdout, '(6x, a)') &
      '2.   Force the sum of esft weights to be 1.'
    WRITE(iu_stdout, '(6x, a)') &
      '3.   Remove negative pressure scalings from esft data.'
!    WRITE(iu_stdout, '(6x, a)') &
!      '4.   Remove continua from bands where they are weak.'
    WRITE(iu_stdout, '(6x, a)') &
      '5.   Remove negative pressure scalings continuum data.'
    WRITE(iu_stdout, '(6x, a)') &
      '6.   Set the major gas and continuum in each band.'
    WRITE(iu_stdout, '(6x, a)') &
      '7.   Reorder the aerosols.'
    WRITE(iu_stdout, '(6x, a)') &
      '8.   Set overlap treatment for generalised continua.'
    WRITE(iu_stdout, '(6x, a)') &
      '9.   Scale extinction for ice-crystals.'
    WRITE(iu_stdout, '(5x, a)') &
      '10.   Ensure a single null absorber is present in each band.'
    WRITE(iu_stdout, '(5x, a)') &
      '11.   Set the scattering method for each k-term.'
    WRITE(iu_stdout, '(/5x, a//)') &
      '-1.  to finish.'

    READ(iu_stdin, *, iostat=ios) i_process
    IF (ios /= 0) THEN
      cmessage = 'Erroneous input'
      ierr = i_err_io
      CALL ereport(RoutineName, ierr, cmessage)
    END IF
!
!
!   For each valid process call the appropriate routine.
    SELECT CASE(i_process)
      CASE(-1)
!       Write out the spectral file.
        CALL out_spectrum(file_spectral, Spectrum, ierr)
        EXIT loop
      CASE(1)
        IF (Spectrum%Basic%l_present(5)) THEN
          CALL remove_weak
        ELSE
          WRITE(iu_err,'(/a)') &
            'The file contains no k-distribution data: the process ' &
            //'cannot be carried out.'
        ENDIF
      CASE(2)
        IF (Spectrum%Basic%l_present(5)) THEN
          DO i=1, Spectrum%Basic%n_band
            DO j=1, Spectrum%Gas%n_band_absorb(i)
              i_gas=Spectrum%Gas%index_absorb(j, i)
              CALL sum_unity(Spectrum%Gas%i_band_k(i, j), &
                Spectrum%Gas%w(1, i, j), Spectrum%Gas%k(1, i, j))
            ENDDO
          ENDDO
        ELSE
          WRITE(iu_err,'(/a)') &
            'The file contains no k-distribution data: the process ' &
            //'cannot be carried out.'
        ENDIF
      CASE(3)
        IF (Spectrum%Basic%l_present(5)) THEN
          CALL remove_negative_gas_90( &
            Spectrum%Dim%nd_band, &
            Spectrum%Dim%nd_species, &
            Spectrum%Dim%nd_k_term, &
            Spectrum%Dim%nd_scale_variable, &
            Spectrum%Basic%n_band, &
            Spectrum%Gas%n_band_absorb, Spectrum%Gas%index_absorb, &
            Spectrum%Gas%type_absorb, Spectrum%Gas%i_scale_fnc, &
            Spectrum%Gas%i_band_k, Spectrum%Gas%scale)
        ELSE
          WRITE(iu_err,'(/a)') &
            'The file contains no esft data: the process ' &
            //'cannot be carried out.'
        ENDIF
      CASE(4)
        IF (Spectrum%Basic%l_present(9)) THEN
          cmessage = 'remove_weak_cont not implemented'
          ierr = i_warning
        ELSE
          WRITE(iu_err,'(/a)') &
            'The file contains no continuum data: the process ' &
            //'cannot be carried out.'
        ENDIF
      CASE(5)
        IF (Spectrum%Basic%l_present(9)) THEN
          CALL remove_negative_cont_90( &
            Spectrum%Dim%nd_band, &
            Spectrum%Dim%nd_continuum, &
            Spectrum%Dim%nd_scale_variable, &
            Spectrum%Basic%n_band, &
            Spectrum%Cont%n_band_continuum, &
            Spectrum%Cont%index_continuum, &
            Spectrum%Cont%i_scale_fnc_cont, &
            Spectrum%Cont%scale_cont)
        ELSE
          WRITE(iu_err,'(/a)') &
            'The file contains no continuum data: the process ' &
            //'cannot be carried out.'
        ENDIF
      CASE(6)
        IF (Spectrum%Basic%l_present(5)) THEN
          CALL set_major_gas_cont
        ELSE
          WRITE(iu_err,'(/a)') &
            'The file contains no k-distribution data: the process ' &
            //'cannot be carried out.'
        ENDIF
      CASE(7)
        IF (Spectrum%Basic%l_present(11)) THEN
          CALL reorder_aerosols
        ELSE
          WRITE(iu_err,'(/a)') &
            'The file contains no aerosol data: the process ' &
            //'cannot be carried out.'
        ENDIF
      CASE(8)
        IF (Spectrum%Basic%l_present(19)) THEN
          CALL set_cont_overlap
        ELSE
          WRITE(iu_err,'(/a)') &
            'The file contains no generalised continuum data: the process ' &
            //'cannot be carried out.'
        END IF
      CASE(9)
        IF (Spectrum%Basic%l_present(12)) THEN
          CALL scale_ice
        ELSE
          WRITE(iu_err,'(/a)') &
            'The file contains no ice cloud data: the process ' &
            //'cannot be carried out.'
        ENDIF
      CASE(10)
        CALL set_null_absorber
      CASE(11)
        CALL set_scatter_method
      CASE DEFAULT
        cmessage = 'Invalid type of process'
        ierr = i_warning
    END SELECT
    CALL ereport(RoutineName, ierr, cmessage)

    WRITE(iu_stdout, '(/a/)') 'specify next process.'

  END DO loop

CONTAINS

!+ ---------------------------------------------------------------------
! Subroutine to remove weakly absorbing gases from bands.
!
! Method:
!   A tolerance for negelecting gaseous absorption is obtained.
!   A typical amount of each gas in a column is given. For each
!   band a test is made to see whether this amount of absorber
!   gives a transmission close to 1 as defined by the tolerance.
!   If so, the gas is removed from this spectral band.
!
!- ---------------------------------------------------------------------
  SUBROUTINE remove_weak

    USE gas_list_pcf

    LOGICAL, EXTERNAL :: lock_code
!     Logical to forbid interactive looping

    INTEGER :: k
!     Loop variables
    INTEGER :: index_gas_1, index_gas_2
!     Continuum gas indices
    INTEGER :: n_band_absorb_temp
!     Temporary number of absorbers
    INTEGER :: n_band_cont_temp
!     Temporary number of continua
    REAL (RealK) :: column_gas(Spectrum%Dim%nd_species+Spectrum%Dim%nd_cont)
!     Column amounts for gases
    REAL (RealK) :: column_cont(Spectrum%Dim%nd_cont)
!     Column amounts for continua
    REAL (RealK) :: trans_neglect
!     Transmission for neglecting
    REAL (RealK) :: trans_column
!     Transmission of column


!   Obtain column amounts of absorber.
    WRITE(iu_stdout, '(/A, /A)') &
      'For each absorber enter the amounts of the gas to test for', &
      'neglecting the gas in a band.'
    DO i=1, Spectrum%Gas%n_absorb
      WRITE(iu_stdout, '(4X, 2A)') 'Column amount for ', &
        name_absorb(Spectrum%Gas%type_absorb(i))
      DO
        READ(iu_stdin, *, IOSTAT=ios) column_gas(i)
        IF (ios.NE.0) THEN
          WRITE(iu_err, '(A)') '+++ Erroneous response:'
          IF (lock_code(.TRUE.)) THEN
            cmessage = 'Erroneous response'
            ierr = i_err_io
            CALL ereport(RoutineName, ierr, cmessage)
          ELSE
            WRITE(iu_stdout, '(A)') 'Please re-type.'
          ENDIF
        ELSE
          EXIT
        ENDIF
      END DO
    ENDDO
    DO i=1, Spectrum%ContGen%n_cont
      index_gas_1=Spectrum%ContGen%index_cont_gas_1(i)
      index_gas_2=Spectrum%ContGen%index_cont_gas_2(i)
      WRITE(iu_stdout, '(4X, A)') 'Column amount for ' // &
        TRIM(name_absorb(Spectrum%Gas%type_absorb(index_gas_1))) // &
        ' -- ' // &
        TRIM(name_absorb(Spectrum%Gas%type_absorb(index_gas_2))) // &
        ' continuum'
      DO
        READ(iu_stdin, *, IOSTAT=ios) column_cont(i)
        IF (ios.NE.0) THEN
          WRITE(iu_err, '(A)') '+++ Erroneous response:'
          IF (lock_code(.TRUE.)) THEN
            cmessage = 'Erroneous response'
            ierr = i_err_io
            CALL ereport(RoutineName, ierr, cmessage)
          ELSE
            WRITE(iu_stdout, '(A)') 'Please re-type.'
          ENDIF
        ELSE
          EXIT
        ENDIF
      END DO
    ENDDO
!
    WRITE(iu_stdout, '(/A)') &
      'Enter the transmission for neglecting gases and continua.'
    DO
      READ(iu_stdin, *, IOSTAT=ios) trans_neglect
      IF (ios.NE.0) THEN
        WRITE(iu_err, '(A)') '+++ Erroneous response:'
        IF (lock_code(.TRUE.)) THEN
          cmessage = 'Erroneous response'
          ierr = i_err_io
          CALL ereport(RoutineName, ierr, cmessage)
        ELSE
          WRITE(iu_stdout, '(A)') 'Please re-type.'
        ENDIF
      ELSE
        EXIT
      ENDIF
    END DO
!
!   Go through the bands removing gases which are too weak.
    DO i=1, Spectrum%Basic%n_band

!     Gases
      n_band_absorb_temp=Spectrum%Gas%n_band_absorb(i)
      Spectrum%Gas%n_band_absorb(i)=0
      DO j=1, n_band_absorb_temp
        i_gas=Spectrum%Gas%index_absorb(j, i)
        trans_column=0.0E+00_RealK
        DO k=1, Spectrum%Gas%i_band_k(i, i_gas)
          trans_column=trans_column + Spectrum%Gas%w(k, i, i_gas) &
            *EXP(-Spectrum%Gas%k(k, i, i_gas)*column_gas(i_gas))
        ENDDO
        IF (trans_column < trans_neglect) THEN
!         The gas is included.
          Spectrum%Gas%n_band_absorb(i) = &
            Spectrum%Gas%n_band_absorb(i)+1
          Spectrum%Gas%index_absorb( &
            Spectrum%Gas%n_band_absorb(i), i) = i_gas
        ENDIF
      ENDDO

!     Continua
      n_band_cont_temp=Spectrum%ContGen%n_band_cont(i)
      Spectrum%ContGen%n_band_cont(i)=0
      DO j=1, n_band_cont_temp
        i_cont=Spectrum%ContGen%index_cont(j, i)
        trans_column=0.0E+00_RealK
        DO k=1, Spectrum%ContGen%i_band_k_cont(i, i_cont)
          trans_column=trans_column + Spectrum%ContGen%w_cont(k, i, i_cont) &
            *EXP(-Spectrum%ContGen%k_cont(k, i, i_cont)*column_cont(i_cont))
        ENDDO
        IF (trans_column < trans_neglect) THEN
!         The continuum is included.
          Spectrum%ContGen%n_band_cont(i) = &
            Spectrum%ContGen%n_band_cont(i)+1
          Spectrum%ContGen%index_cont( &
            Spectrum%ContGen%n_band_cont(i), i) = i_cont
        ENDIF
      END DO
    ENDDO

  END SUBROUTINE remove_weak


!+ ---------------------------------------------------------------------
! Subroutine to set the major gas/continuum in each band.
!
! Method:
!      A typical amount of each gas and continuum in a column is given.
!      For each band the gases and continua are reordered by increasing
!      transmission.
!
!- ---------------------------------------------------------------------
  SUBROUTINE set_major_gas_cont

    USE gas_list_pcf

    LOGICAL, EXTERNAL :: lock_code
!     Logical to forbid interactive looping

    INTEGER :: i, j, i_band, i_gas, i_cont
!     Loop variables
    INTEGER :: index_gas_1, index_gas_2
!     Continuum gas indices
    INTEGER :: n_band_absorb, n_band_cont
!     Number of absorbers and continua in a band
    INTEGER :: n_k
!     Number of k-terms
    INTEGER :: n_column(Spectrum%Dim%nd_species), n_column_p
!     Number of column amounts
    REAL (RealK) :: column_gas(3, Spectrum%Dim%nd_species)
!     Specified column amounts for gases
    REAL (RealK) :: column_cont(Spectrum%Dim%nd_cont)
!     Specified column amounts for continua
    REAL (RealK) :: column_mass_gas(MAX(Spectrum%Gas%n_absorb, 1))
!     Column mass down to p_level for gases
    REAL (RealK) :: trans_column_gas(MAX(Spectrum%Gas%n_absorb, 1))
!     Transmission of column for all gases and perfectly correlated
!     continua
    REAL (RealK) :: column_mass_cont(MAX(Spectrum%ContGen%n_cont, 1))
!     Column mass down to p_level for continua
    REAL (RealK) :: trans_column_cont(MAX(Spectrum%ContGen%n_cont, 1))
!     Transmission of column for continua
    REAL (RealK) :: trans_major_gas
!     Transmission of major gas
    REAL (RealK) :: trans_major_cont
!     Transmission of major continuum
    REAL (RealK) :: trans_column_tot
!     Total transmission of all gases
    REAL (RealK) :: p_surf
!     Surface pressure
    REAL (RealK) :: p_level
!     Pressure at current level
    REAL (RealK) :: p_column(3)
!     Pressure for different gas cumulative column amounts
    REAL (RealK), PARAMETER :: p_toa = 0.001_RealK
!     Pressure at TOA
    REAL (RealK), PARAMETER :: p_inc = 10.0**(0.1_RealK)
!     Factor to increment pressure across a layer
    REAL (RealK), PARAMETER :: trans_def_major = EXP(-1.0_RealK)
!     Total transmission at which to define major absorber
    INTEGER :: map(MAX(Spectrum%Dim%nd_species, Spectrum%Dim%nd_cont))
!     Map sorting absorbers by increasing transmission
    CHARACTER(LEN=1) :: char_yn
!     Response to yes/no question
    CHARACTER(LEN=1024) :: column
!     Column amounts
    LOGICAL :: l_allow_cont_major
!     Flag for allowing continua to become the major absorber
    LOGICAL :: l_cont_added(Spectrum%Dim%nd_cont)
!     Flag for having added continuum k-distribution to that of a gas

  INTERFACE

    SUBROUTINE map_heap_func(a, map)
      USE realtype_rd
      REAL  (RealK), Intent(IN), Dimension(:) :: a
      INTEGER, Intent(OUT), Dimension(:) :: map
    END SUBROUTINE map_heap_func

  END INTERFACE

!   Obtain column amounts of absorber.
    WRITE(iu_stdout, '(/A, /A)') &
      'For each absorber enter the amounts of the gas to find', &
      'the major gas in each band.'
    n_column = 1
    DO i_gas=1, Spectrum%Gas%n_absorb
      WRITE(iu_stdout, '(4X, 2A)') &
        'Increasing cumulative column amounts (up to 3) for ', &
        name_absorb(Spectrum%Gas%type_absorb(i_gas))
      READ(iu_stdin, '(a)') column
      n_column(i_gas) = 3
      DO
        READ(column, *, IOSTAT=ios) column_gas(1:n_column(i_gas), i_gas)
        IF (ios /= 0) THEN
          IF (n_column(i_gas) == 1) THEN
            cmessage = 'Erroneous response'
            ierr = i_err_fatal
            CALL ereport(RoutineName, ierr, cmessage)
          ELSE
            n_column(i_gas) = n_column(i_gas) - 1
          END IF
        ELSE
          EXIT
        END IF
      END DO
    END DO
    DO i_cont=1, Spectrum%ContGen%n_cont
      index_gas_1=Spectrum%ContGen%index_cont_gas_1(i_cont)
      index_gas_2=Spectrum%ContGen%index_cont_gas_2(i_cont)
      WRITE(iu_stdout, '(4X, A)') 'Column amount for ' // &
        TRIM(name_absorb(Spectrum%Gas%type_absorb(index_gas_1))) // &
        ' -- ' // &
        TRIM(name_absorb(Spectrum%Gas%type_absorb(index_gas_2))) // &
        ' continuum'
      DO
        READ(iu_stdin, *, IOSTAT=ios) column_cont(i_cont)
        IF (ios.NE.0) THEN
          WRITE(iu_err, '(A)') '+++ Erroneous response:'
          IF (lock_code(.TRUE.)) THEN
            cmessage = 'Erroneous response'
            ierr = i_err_io
            CALL ereport(RoutineName, ierr, cmessage)
          ELSE
            WRITE(iu_stdout, '(A)') 'Please re-type.'
          ENDIF
        ELSE
          EXIT
        ENDIF
      END DO
    ENDDO

    n_column_p = MAXVAL(n_column)
    IF (Spectrum%ContGen%n_cont > 0 .OR. n_column_p > 1) THEN
      IF (n_column_p > 1) THEN
        WRITE(iu_stdout, '(/,A)') 'Enter the column pressures (low to high)'
        READ(iu_stdin, *, IOSTAT=ios) p_column(1:n_column_p)
        IF (ios /= 0 .OR. ANY(p_column(1:n_column_p) <= 0.0_RealK)) THEN
          cmessage = 'Erroneous response'
          ierr = i_err_io
          CALL ereport(RoutineName, ierr, cmessage)
        ELSE
          p_surf=MAXVAL(p_column(1:n_column_p))
        END IF
      ELSE
        WRITE(iu_stdout, '(/,A)') 'Enter the surface pressure.'
        READ(iu_stdin, *, IOSTAT=ios) p_surf
        IF (ios /= 0 .OR. p_surf <= 0.0_RealK) THEN
          cmessage = 'Erroneous response'
          ierr = i_err_io
          CALL ereport(RoutineName, ierr, cmessage)
        END IF
      END IF
    ELSE
!     If there are no continua the surface pressure is arbitrary
      p_surf=1.0E+05_RealK
    END IF

    IF (Spectrum%ContGen%n_cont > 0) THEN
      WRITE(iu_stdout, '(/,A)') 'Do you wish to allow continua to ' // &
          'become the major absorber? (Y/N)'
      DO
        READ(iu_stdin, '(A)') char_yn
        IF ( (char_yn == 'Y') .OR. (char_yn == 'y') ) THEN
          l_allow_cont_major=.TRUE.
          EXIT
        ELSE IF ( (char_yn == 'N') .OR. (char_yn == 'n') ) THEN
          l_allow_cont_major=.FALSE.
          EXIT
        ELSE
          WRITE(iu_err, '(A)') '+++ Erroneous response:'
          IF (lock_code(.TRUE.)) THEN
            cmessage = 'Erroneous response'
            ierr = i_err_fatal
            CALL ereport(RoutineName, ierr, cmessage)
          ELSE
            WRITE(iu_stdout, '(A)') 'Please re-type.'
          ENDIF
        ENDIF
      ENDDO
    ELSE
      l_allow_cont_major=.FALSE.
    END IF

    DO i_band=1,Spectrum%Basic%n_band
      n_band_absorb=Spectrum%Gas%n_band_absorb(i_band)
      n_band_cont=Spectrum%ContGen%n_band_cont(i_band)

!     Attempt to define major absorber from where the total optical depth
!     reaches 1
      p_level=p_toa
      DO WHILE (p_level <= p_surf)
        p_level=p_level*p_inc

!       Gaseous transmissions. Experience shows that the major absorber is
!       best determined if any continua that are perfectly correlated with
!       the gas are not included.
        trans_column_gas=1.0_RealK
        l_cont_added=.FALSE.
        DO i=1,n_band_absorb
          i_gas=Spectrum%Gas%index_absorb(i, i_band)
          IF (n_column(i_gas) == 3) THEN
            IF (p_level < p_column(1)) THEN
              column_mass_gas(i) = column_gas(1, i_gas)*p_level/p_column(1)
            ELSE IF (p_level < p_column(2)) THEN
              column_mass_gas(i) = column_gas(1, i_gas) &
                + ( column_gas(2, i_gas) - column_gas(1, i_gas) ) &
                * ( p_level - p_column(1) ) / ( p_column(2) - p_column(1) )
            ELSE
              column_mass_gas(i) = column_gas(2, i_gas) &
                + ( column_gas(3, i_gas) - column_gas(2, i_gas) ) &
                * ( p_level - p_column(2) ) / ( p_column(3) - p_column(2) )
            END IF
          ELSE IF (n_column(i_gas) == 2) THEN
            IF (p_level < p_column(1)) THEN
              column_mass_gas(i) = column_gas(1, i_gas)*p_level/p_column(1)
            ELSE
              column_mass_gas(i) = column_gas(1, i_gas) &
                + ( column_gas(2, i_gas) - column_gas(1, i_gas) ) &
                * ( p_level - p_column(1) ) / ( p_surf - p_column(1) )
            END IF
          ELSE
            column_mass_gas(i)=column_gas(1, i_gas)*p_level/p_surf
          END IF
          n_k=Spectrum%Gas%i_band_k(i_band, i_gas)
          trans_column_gas(i)=SUM(Spectrum%Gas%w(1:n_k, i_band, i_gas) &
            *EXP(-Spectrum%Gas%k(1:n_k, i_band, i_gas)*column_mass_gas(i)))
        END DO

!       Continuum transmissions.
        trans_column_cont=1.0_RealK
        DO j=1,n_band_cont
          i_cont=Spectrum%ContGen%index_cont(j, i_band)
!         Only consider continua whose overlap treatment is the same as that
!         for gases.
          IF (Spectrum%ContGen%i_cont_overlap_band(i_band, i_cont) == 0) THEN
            column_mass_cont(j)=column_cont(i_cont)*(p_level/p_surf)**2
            n_k=Spectrum%ContGen%i_band_k_cont(i_band, i_cont)
            trans_column_cont(j) &
                =SUM(Spectrum%ContGen%w_cont(1:n_k, i_band, i_cont) &
                *EXP(-Spectrum%ContGen%k_cont(1:n_k, i_band, i_cont) &
                *column_mass_cont(j)))
          END IF
        END DO

!       Assume random overlap to calculate total band transmission and
!       check if an optical depth of 1 has been reached
        trans_column_tot=PRODUCT(trans_column_gas)*PRODUCT(trans_column_cont)
        IF (trans_column_tot <= trans_def_major) EXIT
      END DO

!     Sort gases by increasing transmission
      IF (n_band_absorb > 0) THEN
        CALL map_heap_func(trans_column_gas(1:n_band_absorb), &
          map(1:n_band_absorb))
        Spectrum%Gas%index_absorb(1:n_band_absorb, i_band) = &
          Spectrum%Gas%index_absorb(map(1:n_band_absorb), i_band)
        trans_major_gas = trans_column_gas(map(1))
      ELSE
        trans_major_gas = 1.0_RealK
      END IF
      IF (n_band_absorb > 1) THEN
        IF (-2.0_RealK*LOG(trans_column_gas(map(2))) &
          > -LOG(trans_major_gas)) THEN
          Spectrum%Gas%i_overlap(i_band) = 3
        ELSE
          Spectrum%Gas%i_overlap(i_band) = 4
        END IF
      ELSE
        Spectrum%Gas%i_overlap(i_band) = 3
      END IF

!     Sort continua by increasing transmission
      IF (n_band_cont > 0) THEN
        CALL map_heap_func(trans_column_cont(1:n_band_cont), &
          map(1:n_band_cont))
        Spectrum%ContGen%index_cont(1:n_band_cont, i_band) &
          =Spectrum%ContGen%index_cont(map(1:n_band_cont), i_band)
        trans_major_cont=trans_column_cont(map(1))
      ELSE
        trans_major_cont = 1.0_RealK
      END IF

!     Determine if the a continuum is the major absorber
      IF (l_allow_cont_major .AND. &
          trans_major_cont < trans_major_gas) THEN
        Spectrum%ContGen%l_cont_major(i_band)=.TRUE.
      ELSE
        Spectrum%ContGen%l_cont_major(i_band)=.FALSE.
      END IF

    END DO

  END SUBROUTINE set_major_gas_cont


!+ ---------------------------------------------------------------------
! Subroutine to reorder the aerosol indices
!- ---------------------------------------------------------------------
  SUBROUTINE reorder_aerosols

    INTEGER :: type_aerosol(Spectrum%Aerosol%n_aerosol)
    INTEGER :: map(Spectrum%Aerosol%n_aerosol), i, n

    WRITE(*, '(a)') 'Enter aerosol type numbers in the new order:'
    READ(*, *, IOSTAT=IOS) type_aerosol

    n=Spectrum%Aerosol%n_aerosol

!   Map aerosol indices onto new order
    DO i = 1, n
      map(i)=MINLOC(ABS(Spectrum%Aerosol%type_aerosol-type_aerosol(i)),1)
    END DO
    Spectrum%Aerosol%l_aero_spec(1:n)=Spectrum%Aerosol%l_aero_spec(map)
    Spectrum%Aerosol%type_aerosol(1:n)=Spectrum%Aerosol%type_aerosol(map)
    Spectrum%Aerosol%i_aerosol_parm(1:n)=Spectrum%Aerosol%i_aerosol_parm(map)
    Spectrum%Aerosol%n_aerosol_phf_term(1:n) = &
      Spectrum%Aerosol%n_aerosol_phf_term(map)
    Spectrum%Aerosol%nhumidity(1:n)=Spectrum%Aerosol%nhumidity(map)
    Spectrum%Aerosol%abs(:,1:n,:)=Spectrum%Aerosol%abs(:,map,:)
    Spectrum%Aerosol%scat(:,1:n,:)=Spectrum%Aerosol%scat(:,map,:)
    Spectrum%Aerosol%phf_fnc(:,:,1:n,:)=Spectrum%Aerosol%phf_fnc(:,:,map,:)
    Spectrum%Aerosol%humidities(:,1:n)=Spectrum%Aerosol%humidities(:,map)

    IF (Spectrum%Basic%l_present(15)) THEN
      Spectrum%Aerosol%i_aod_type(1:n)=Spectrum%Aerosol%i_aod_type(map)
      Spectrum%Aerosol%aod_abs(:,1:n,:)=Spectrum%Aerosol%aod_abs(:,map,:)
      Spectrum%Aerosol%aod_scat(:,1:n,:)=Spectrum%Aerosol%aod_scat(:,map,:)
    END IF

  END SUBROUTINE reorder_aerosols


!+ ---------------------------------------------------------------------
! Subrouine to set the overlap treatment for each continuum in each band
!- ---------------------------------------------------------------------
  SUBROUTINE set_cont_overlap

    USE gas_list_pcf, ONLY: name_absorb

    INTEGER :: index_gas_1, index_gas_2
!     Continuum gas indices
    INTEGER :: index_gas
!     Index of gas
    INTEGER :: i_gas, i_cont
!     Loop indices
    INTEGER :: i_cont_overlap
!     Continuum overlap treatment

    LOGICAL, EXTERNAL :: lock_code
!     Logical to forbid interactive looping

    WRITE(*, '(a)') 'Enter the overlap treatment for generalised continua.'
    WRITE(*, '(a, i3, a)') 'Enter 0 to use the overlapping gaseous ' // &
        'absorption treatment,'
    WRITE(*, '(a)') 'or the type index of the gas for which the continuum ' // &
      'is perfectly correlated.'
    WRITE(*, '(a)') 'The type index of the continuum gases are given in ' // &
      'parentheses after gas name.'

    DO i_cont=1, Spectrum%ContGen%n_cont

      index_gas_1=Spectrum%ContGen%index_cont_gas_1(i_cont)
      index_gas_2=Spectrum%ContGen%index_cont_gas_2(i_cont)

      WRITE(*, '(4x, a, i3, a, i3, a)') 'Overlap treatment for ' // &
        TRIM(name_absorb(Spectrum%Gas%type_absorb(index_gas_1))) // &
        ' (', Spectrum%Gas%type_absorb(index_gas_1), ') -- ' // &
        TRIM(name_absorb(Spectrum%Gas%type_absorb(index_gas_2))) // &
        ' (', Spectrum%Gas%type_absorb(index_gas_2), ') continuum'

      DO
        READ(*, *, IOSTAT=ios) i_cont_overlap

!       Check that the input is valid
        IF (ios /= 0) THEN
          WRITE(iu_err, '(a)') '+++ Erroneous response:'
          IF (lock_code(.TRUE.)) THEN
            cmessage = 'Erroneous response'
            ierr = i_err_io
            CALL ereport(RoutineName, ierr, cmessage)
          ELSE
            WRITE(iu_stdout, '(a)') 'Please re-type.'
          ENDIF
        ELSE
!         Check that the selected overlap treatment is valid
          IF (i_cont_overlap == 0) THEN
            EXIT
          ELSE
            index_gas=0
            DO i_gas=1, Spectrum%Gas%n_absorb
              IF (Spectrum%Gas%type_absorb(i_gas) == i_cont_overlap) THEN
                index_gas=i_gas
                EXIT
              END IF
            END DO
            IF (index_gas == 0) THEN
              WRITE(iu_err, '(a)') '+++ Erroneous response:'
              IF (lock_code(.TRUE.)) THEN
                cmessage = 'Erroneous response'
                ierr = i_err_fatal
                CALL ereport(RoutineName, ierr, cmessage)
              ELSE
                WRITE(iu_stdout, '(a)') 'Please re-type.'
              ENDIF
            ELSE
              i_cont_overlap=index_gas
              EXIT
            END IF
          END IF
        END IF
      END DO

!     Set the overlap treatment for this continuum
      Spectrum%ContGen%i_cont_overlap_band(1:Spectrum%Basic%n_band, &
                                           i_cont) = i_cont_overlap
    END DO

  END SUBROUTINE set_cont_overlap


!+ ---------------------------------------------------------------------
! Subrouine to scale the ice extinction in each band for a given type
!- ---------------------------------------------------------------------
  SUBROUTINE scale_ice

    USE rad_pcf, only: ip_ice_baran, &
      ip_slingo_schrecker_ice, ip_slingo_schr_ice_phf, &
      ip_ice_pade_2_phf, ip_ice_fu_phf

    IMPLICIT NONE

    INTEGER :: type_ice
    REAL (RealK) :: scaling

    WRITE(*, '(a)') 'Enter ice type number to scale extinction:'
    READ(*, *, IOSTAT=IOS) type_ice

    IF (Spectrum%Ice%l_ice_type(type_ice)) THEN
      SELECT CASE (Spectrum%Ice%i_ice_parm(type_ice))
      CASE (ip_ice_baran)
        WRITE(*, '(a)') 'Enter scaling factor for ice extinction:'
        READ(*, *, IOSTAT=IOS) scaling
        DO j=1, Spectrum%Basic%n_band
          Spectrum%Ice%parm_list(1, j, type_ice) = scaling &
            * Spectrum%Ice%parm_list(1, j, type_ice)
        END DO
      CASE (ip_slingo_schrecker_ice, ip_slingo_schr_ice_phf)
        WRITE(*, '(a)') 'Enter scaling factor for ice extinction:'
        READ(*, *, IOSTAT=IOS) scaling
        DO j=1, Spectrum%Basic%n_band
          Spectrum%Ice%parm_list(1:2, j, type_ice) = scaling &
            * Spectrum%Ice%parm_list(1:2, j, type_ice)
        END DO
      CASE (ip_ice_pade_2_phf, ip_ice_fu_phf)
        WRITE(*, '(a)') 'Enter scaling factor for ice extinction:'
        READ(*, *, IOSTAT=IOS) scaling
        DO j=1, Spectrum%Basic%n_band
          Spectrum%Ice%parm_list(1:3, j, type_ice) = scaling &
            * Spectrum%Ice%parm_list(1:3, j, type_ice)
        END DO
      CASE DEFAULT
        WRITE(iu_err, '(a)') 'Scaling not implemented for this type'
      END SELECT
    ELSE
      WRITE(iu_err, '(a)') 'Ice type not present in spectral file'
    END IF

  END SUBROUTINE scale_ice

!+ ---------------------------------------------------------------------
! Subrouine to set at least one null absorber in each band
!- ---------------------------------------------------------------------
  SUBROUTINE set_null_absorber

    USE rad_pcf, ONLY: ip_scale_null, ip_scale_fnc_null
    IMPLICIT NONE

    DO i=1, Spectrum%Basic%n_band
      IF (Spectrum%Gas%n_band_absorb(i) == 0) THEN
        Spectrum%Gas%n_band_absorb(i) = 1
        Spectrum%Gas%index_absorb(1, i) = 1
        Spectrum%Gas%i_band_k(i, 1) = 1
        Spectrum%Gas%i_scale_k(i, 1)=ip_scale_null
        Spectrum%Gas%i_scale_fnc(i, 1)=ip_scale_fnc_null
        Spectrum%Gas%k(1, i, 1)=0.0_RealK
        Spectrum%Gas%w(1, i, 1)=1.0_RealK
        Spectrum%Gas%p_ref(1, i)=1.0_RealK
        Spectrum%Gas%t_ref(1, i)=200.0_RealK
      END IF
    END DO

  END SUBROUTINE set_null_absorber

!+ ---------------------------------------------------------------------
! Subroutine to set the scattering method per k-term 
!
! Method:
!      A typical amount of each gas in a column is given.
!      For each k-term the scattering method is set depending on the
!      column optical depth.
!      (For use with the Hybrid Scattering scheme.)
!
!- ---------------------------------------------------------------------
  SUBROUTINE set_scatter_method

    USE gas_list_pcf, ONLY: name_absorb
    USE rad_pcf, ONLY: ip_no_scatter_sw, ip_no_scatter_abs, ip_scatter_full
    IMPLICIT NONE

    INTEGER :: i, k, i_band, i_gas
!     Loop variables
    INTEGER :: n_k
!     Number of k-terms
    REAL (RealK) :: column_gas(Spectrum%Dim%nd_species)
!     Specified column amounts for gases
    REAL (RealK) :: tau
!     Optical depth of column
    REAL (RealK) :: tau_max
!     Threshold optical depth for neglecting scattering

!   Obtain column amounts of gas absorbers.
    WRITE(iu_stdout, '(/A, /A)') &
      'For each gas absorber enter the column amount (kg/m2)', &
      'to determine where scattering can be neglected.'
    DO i=1, Spectrum%Gas%n_absorb
      WRITE(iu_stdout, '(4X, 2A)') 'Column amount for ', &
        name_absorb(Spectrum%Gas%type_absorb(i))
      READ(iu_stdin, *, IOSTAT=ios) column_gas(i)
      IF (ios.NE.0) THEN
        cmessage = 'Erroneous response'
        ierr = i_err_io
        CALL ereport(RoutineName, ierr, cmessage)
      END IF
    END DO

!   Optical depth of column for which scattering is neglected
    WRITE(iu_stdout, '(/A)') &
      'Enter the column optical depth threshold for neglecting scattering.'
    READ(iu_stdin, *, IOSTAT=ios) tau_max
    IF (ios.NE.0) THEN
      cmessage = 'Erroneous response'
      ierr = i_err_io
      CALL ereport(RoutineName, ierr, cmessage)
    END IF

    DO i_band=1, Spectrum%Basic%n_band
      DO i=1, Spectrum%Gas%n_band_absorb(i_band)
        i_gas = Spectrum%Gas%index_absorb(i, i_band)
        n_k = Spectrum%Gas%i_band_k(i_band, i_gas)
        IF (column_gas(i_gas) > 0.0_RealK) THEN
          DO k=1, n_k
            tau = Spectrum%Gas%k(k, i_band, i_gas) * column_gas(i_gas)
            IF (tau > tau_max) THEN
              IF (Spectrum%Basic%l_present(2)) THEN
                ! SW spectral file
                Spectrum%Gas%i_scat(k, i_band, i_gas) = ip_no_scatter_sw
              ELSE
                ! LW spectral file
                Spectrum%Gas%i_scat(k, i_band, i_gas) = ip_no_scatter_abs
              END IF
            ELSE
              Spectrum%Gas%i_scat(k, i_band, i_gas) = ip_scatter_full
            END IF
          END DO
        ELSE
          Spectrum%Gas%i_scat(1:n_k, i_band, i_gas) = 0
        END IF
      END DO
    END DO

  END SUBROUTINE set_scatter_method

END PROGRAM tidy_90
