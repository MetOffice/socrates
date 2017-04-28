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
  USE realtype_rd
  USE def_spectrum
  USE def_std_io_icf
  USE error_pcf
!
!
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
  INTEGER :: ierr = 0
!   Error flag
  LOGICAL :: l_interactive
!   Flag for interactive operation
!
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
!
!
!
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
    '"n" to create a new file;'
  WRITE(iu_stdout, '(6x, a)') &
    '"q" to quit.'
  WRITE(iu_stdout, '(/)')
!
  DO
    READ(iu_stdin, '(a)') char_qna
    SELECT CASE(char_qna)
      CASE('o', 'O')
        EXIT
      CASE('n', 'N')
        WRITE(iu_stdout, '(a)') 'Enter the name of the new file'
        DO
          READ(iu_stdin, '(a)') file_spectral
!         Check for existence of file.
          INQUIRE(file=file_spectral, exist=l_exist)
          IF (l_exist) THEN
            IF (l_interactive) THEN
              WRITE(iu_stdout, '(/a)') &
                'This file already exists: do you wish to overwrite? (y/n)'
              DO
                READ(iu_stdin, '(a)') char_yn
                SELECT CASE(char_yn)
                  CASE('n', 'N')
                    STOP
                  CASE('y', 'Y')
                    EXIT
                END SELECT
              ENDDO
            ELSE
              STOP
            ENDIF
          ELSE
            EXIT
          ENDIF
        ENDDO
      CASE('q', 'Q')
        STOP
      CASE DEFAULT
        WRITE(iu_err, '(a)') '+++ erroneous response:'
        IF (.NOT.l_interactive) STOP
    END SELECT
    EXIT
  ENDDO
!
! Now decide which processes are needed.
  DO
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
    WRITE(iu_stdout, '(/5x, a//)') &
      '-1.  to finish.'
!
    DO
      READ(iu_stdin, *, iostat=ios) i_process
      IF (ios /= 0) THEN
        WRITE(iu_err, '(a)') '+++ Erroneous input:'
        IF (.NOT.l_interactive) THEN
          STOP
        ELSE
          WRITE(iu_stdout, '(a)') 'Please re-type.'
        ENDIF
      ELSE
        EXIT
      ENDIF
    ENDDO
!
!
!   For each valid process call the appropriate routine.
    SELECT CASE(i_process)
      CASE(-1)
!       Write out the spectral file.
        CALL out_spectrum(file_spectral, Spectrum, ierr)
        STOP
      CASE(1)
        IF (Spectrum%Basic%l_present(5)) THEN
          CALL remove_weak
          IF (ierr /= i_normal) STOP
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
!          CALL remove_weak_cont
          IF (ierr /= i_normal) STOP
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
          IF (ierr /= i_normal) STOP
        ELSE
          WRITE(iu_err,'(/a)') &
            'The file contains no k-distribution data: the process ' &
            //'cannot be carried out.'
        ENDIF
      CASE(7)
        IF (Spectrum%Basic%l_present(11)) THEN
          CALL reorder_aerosols
          IF (ierr /= i_normal) STOP
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
      CASE DEFAULT
        WRITE(iu_err, '(a)') '+++ Invalid type of process:'
        IF (l_interactive) THEN
          WRITE(iu_stdout, '(a)') 'Please re-type.'
        ELSE
          STOP
        ENDIF
    END SELECT
    IF (ierr /= i_normal) stop
!
    WRITE(iu_stdout, '(/a/)') 'specify next process.'
!
  ENDDO

CONTAINS

!+ ---------------------------------------------------------------------
! Subroutine to remove weakly absorbing gases from bands.
!
! Method:
!	A tolerance for negelecting gaseous absorption is obtained.
!	A typical amount of each gas in a column is given. For each
!	band a test is made to see whether this amount of absorber
!	gives a transmission close to 1 as defined by the tolerance.
!	If so, the gas is removed from this spectral band.
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
            STOP
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
            STOP
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
          STOP
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
!         The gas is included.
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
!	A typical amount of each gas in a column is given. For each
!	band the gases are reordered by increasing transmission.
!
!- ---------------------------------------------------------------------
  SUBROUTINE set_major_gas_cont

    USE gas_list_pcf

    LOGICAL, EXTERNAL :: lock_code
!     Logical to forbid interactive looping

    INTEGER :: k
!     Loop variable
    INTEGER :: index_gas_1, index_gas_2
!     Continuum gas indices
    INTEGER :: n_band_absorb, n_band_cont
!     Number of absorbers and continua in a band
    REAL (RealK) :: column_gas(Spectrum%Dim%nd_species)
!     Column amounts for gases
    REAL (RealK) :: column_cont(Spectrum%Dim%nd_cont)
!     Column amounts for continua
    REAL (RealK) :: trans_major_gas, trans_major_cont
!     Transmission for major gas, continuum
    REAL (RealK) :: trans_column(MAX(Spectrum%Dim%nd_species, &
                                     Spectrum%Dim%nd_cont))
!     Transmission of gas or continuum
    INTEGER :: map(MAX(Spectrum%Dim%nd_species, Spectrum%Dim%nd_cont))
!     Map sorting absorbers by increasing transmission
    CHARACTER(LEN=1) :: char_yn
!     Response to yes/no question
    LOGICAL :: l_allow_cont_major
!     Flag for allowing continua to become the major absorber

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
    DO i=1, Spectrum%Gas%n_absorb
      WRITE(iu_stdout, '(4X, 2A)') 'Column amount for ', &
        name_absorb(Spectrum%Gas%type_absorb(i))
      DO
        READ(iu_stdin, *, IOSTAT=ios) column_gas(i)
        IF (ios.NE.0) THEN
          WRITE(iu_err, '(A)') '+++ Erroneous response:'
          IF (lock_code(.TRUE.)) THEN
            STOP
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
            STOP
          ELSE
            WRITE(iu_stdout, '(A)') 'Please re-type.'
          ENDIF
        ELSE
          EXIT
        ENDIF
      END DO
    ENDDO

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
            STOP
          ELSE
            WRITE(iu_stdout, '(A)') 'Please re-type.'
          ENDIF
        ENDIF
      ENDDO
    END IF

!   Go through the bands to find the transmission for each gas
    DO i=1, Spectrum%Basic%n_band

!     Gases
      n_band_absorb = Spectrum%Gas%n_band_absorb(i)
      IF (n_band_absorb > 0) THEN
        DO j=1, n_band_absorb
          i_gas=Spectrum%Gas%index_absorb(j, i)
          trans_column(j)=0.0E+00_RealK
          DO k=1, Spectrum%Gas%i_band_k(i, i_gas)
            trans_column(j)=trans_column(j) + Spectrum%Gas%w(k, i, i_gas) &
              *EXP(-Spectrum%Gas%k(k, i, i_gas)*column_gas(i_gas))
          ENDDO
        END DO
        CALL map_heap_func(trans_column(1:n_band_absorb), map(1:n_band_absorb))
        Spectrum%Gas%index_absorb(1:n_band_absorb, i) = &
            Spectrum%Gas%index_absorb(map(1:n_band_absorb), i)
        trans_major_gas = trans_column(map(1))
      ELSE
        trans_major_gas = 1.0_RealK
      END IF

!     Continua
      n_band_cont = Spectrum%ContGen%n_band_cont(i)
      IF (n_band_cont > 0) THEN
        DO j=1, n_band_cont
          i_cont=Spectrum%ContGen%index_cont(j, i)
          trans_column(j)=0.0E+00_RealK
          DO k=1, Spectrum%ContGen%i_band_k_cont(i, i_cont)
            trans_column(j)=trans_column(j) &
              +Spectrum%ContGen%w_cont(k, i, i_cont) &
              *EXP(-Spectrum%ContGen%k_cont(k, i, i_cont)*column_cont(i_cont))
          ENDDO
        ENDDO
        CALL map_heap_func(trans_column(1:n_band_cont), map(1:n_band_cont))
        Spectrum%ContGen%index_cont(1:n_band_cont, i) = &
            Spectrum%ContGen%index_cont(map(1:n_band_cont), i)
        trans_major_cont = trans_column(map(1))
      ELSE
        trans_major_cont = 1.0_RealK
      END IF

!     Set flag for continuum beging major absorber
      IF (l_allow_cont_major .AND. trans_major_cont < trans_major_gas) THEN
        Spectrum%ContGen%l_cont_major(i)=.TRUE.
      ELSE
        Spectrum%ContGen%l_cont_major(i)=.FALSE.
      END IF
    ENDDO

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
        ' (', Spectrum%Gas%type_absorb(index_gas_1), ') continuum'

      DO
        READ(*, *, IOSTAT=ios) i_cont_overlap

!       Check that the input is valid
        IF (ios /= 0) THEN
          WRITE(iu_err, '(a)') '+++ Erroneous response:'
          IF (lock_code(.TRUE.)) THEN
            STOP
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
                STOP
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

END PROGRAM tidy_90
