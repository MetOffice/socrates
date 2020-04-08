! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
!+ Subroutine to make spectral blocks of type 5.
!
! Method:
!	Initialy, a transparent grey fit is set for each gas.
!	A file is opened and an ESFT fit is read from the file.
!
!- ---------------------------------------------------------------------
SUBROUTINE make_block_5(Spectrum, ierr)

  USE realtype_rd
  USE def_spectrum
  USE rad_pcf
  USE file_type_pcf

  IMPLICIT NONE

  TYPE (StrSpecData), Intent(INOUT), TARGET :: Spectrum
!   Spectral file to be assigned
  INTEGER, Intent(INOUT) :: ierr
!   Error flag

! Local arguments.
  CHARACTER (LEN=80) :: line
!   Input line
  INTEGER :: iu_esft
!   Unit number for file of ESFT/k-term data
  INTEGER :: ios
!   IO status
  INTEGER :: i_input_type
!   Type of input file
  INTEGER :: i_index
!   Index number of absorber
  INTEGER :: i_index_sb
!   Index number of absorber in self-broadening arrays
  INTEGER :: i_band
!   Number of band
  INTEGER :: i, j, k, l, ip, it, igf
!   Loop variables
  LOGICAL :: l_index_band(Spectrum%Dim%nd_band, Spectrum%Gas%n_absorb)
!   Absorbers present

  LOGICAL, EXTERNAL :: non_blank
!   Function to detect blank lines

! Pointers to dimensions: used to shorten declarations later
  INTEGER, POINTER :: nd_band
!   Size allocated for spectral bands
  INTEGER, POINTER :: nd_k_term
!   Size allocated for k-terms
  INTEGER, POINTER :: nd_species
!   Size allocated for gaseous species
  INTEGER, POINTER :: nd_scale_variable
!   Size allocated for scaling variables

  INTEGER :: nd_k_term_alloc
!   Previously allocated size for k-terms
  INTEGER, ALLOCATABLE :: arr_tmp_int_3d(:, :, :)
  REAL (RealK), ALLOCATABLE :: arr_tmp_real_3d(:, :, :)
  REAL (RealK), ALLOCATABLE :: arr_tmp_real_4d(:, :, :, :)
  REAL (RealK), ALLOCATABLE :: arr_tmp_real_5d(:, :, :, :, :)
  REAL (RealK), ALLOCATABLE :: arr_tmp_real_6d(:, :, :, :, :, :)
!   Temporary arrays used when resizing existing arrays

! Alias pointers to dimensions to the actual structure.
  nd_band            => Spectrum%Dim%nd_band
  nd_k_term          => Spectrum%Dim%nd_k_term
  nd_species         => Spectrum%Dim%nd_species
  nd_scale_variable  => Spectrum%Dim%nd_scale_variable

! If the block does not exist it is filled with grey null fits.
  IF (.NOT.Spectrum%Basic%l_present(5)) THEN
!   Allocate space for the arrays of k-terms.
    IF (ALLOCATED(Spectrum%Gas%i_band_k)) &
        DEALLOCATE(Spectrum%Gas%i_band_k)
    ALLOCATE(Spectrum%Gas%i_band_k(nd_band, nd_species))
    IF (ALLOCATED(Spectrum%Gas%i_scale_k)) &
        DEALLOCATE(Spectrum%Gas%i_scale_k)
    ALLOCATE(Spectrum%Gas%i_scale_k(nd_band, nd_species))
    IF (ALLOCATED(Spectrum%Gas%i_scale_fnc)) &
        DEALLOCATE(Spectrum%Gas%i_scale_fnc)
    ALLOCATE(Spectrum%Gas%i_scale_fnc(nd_band, nd_species))
    IF (ALLOCATED(Spectrum%Gas%k)) &
        DEALLOCATE(Spectrum%Gas%k)
    ALLOCATE(Spectrum%Gas%k(nd_k_term, nd_band, nd_species))
    IF (ALLOCATED(Spectrum%Gas%w)) &
        DEALLOCATE(Spectrum%Gas%w)
    ALLOCATE(Spectrum%Gas%w(nd_k_term, nd_band, nd_species))
    IF (ALLOCATED(Spectrum%Gas%p_ref)) &
        DEALLOCATE(Spectrum%Gas%p_ref)
    ALLOCATE(Spectrum%Gas%p_ref(nd_species, nd_band))
    IF (ALLOCATED(Spectrum%Gas%t_ref)) &
        DEALLOCATE(Spectrum%Gas%t_ref)
    ALLOCATE(Spectrum%Gas%t_ref(nd_species, nd_band))
    IF (ALLOCATED(Spectrum%Gas%scale)) &
        DEALLOCATE(Spectrum%Gas%scale)
    ALLOCATE(Spectrum%Gas%scale(nd_scale_variable, nd_k_term, &
      nd_band, nd_species))
    IF (ALLOCATED(Spectrum%Gas%i_scat)) &
        DEALLOCATE(Spectrum%Gas%i_scat)
    ALLOCATE(Spectrum%Gas%i_scat(nd_k_term, nd_band, nd_species))
    IF (ALLOCATED(Spectrum%Gas%num_ref_p)) &
        DEALLOCATE(Spectrum%Gas%num_ref_p)
    ALLOCATE(Spectrum%Gas%num_ref_p(nd_species, nd_band))
    IF (ALLOCATED(Spectrum%Gas%num_ref_t)) &
        DEALLOCATE(Spectrum%Gas%num_ref_t)
    ALLOCATE(Spectrum%Gas%num_ref_t(nd_species, nd_band))
    IF (ALLOCATED(Spectrum%Gas%l_self_broadening)) &
        DEALLOCATE(Spectrum%Gas%l_self_broadening)
    ALLOCATE(Spectrum%Gas%l_self_broadening(nd_species))
    IF (ALLOCATED(Spectrum%Gas%index_sb)) &
        DEALLOCATE(Spectrum%Gas%index_sb)
    ALLOCATE(Spectrum%Gas%index_sb(nd_species))
    Spectrum%Gas%i_scat=0
    Spectrum%Gas%num_ref_p=0
    Spectrum%Gas%num_ref_t=0
    Spectrum%Gas%l_self_broadening=.FALSE.
    Spectrum%Gas%index_sb=0
    DO i=1, Spectrum%Basic%n_band
      DO j=1, Spectrum%Gas%n_band_absorb(i)
        Spectrum%Gas%i_band_k(i, j)=1
        Spectrum%Gas%i_scale_k(i, j)=IP_scale_null
        Spectrum%Gas%i_scale_fnc(i, j)=IP_scale_fnc_null
        Spectrum%Gas%k(1, i, j)=0.0_RealK
        Spectrum%Gas%w(1, i, j)=1.0_RealK
        Spectrum%Gas%p_ref(i, j)=1.0_RealK
        Spectrum%Gas%t_ref(i, j)=200.0_RealK
      ENDDO
    ENDDO
  ENDIF
! Obtain the band data from the prepared file of ESFT terms.
  CALL get_free_unit(ierr, iu_esft)
  CALL open_file_in(ierr, iu_esft, &
    'enter the name of the file of esft data.')
  DO
    READ(iu_esft, '(A)', IOSTAT=ios) line
    IF (ios /= 0) THEN
      WRITE(*, '(/a)') '***error: file type not found.'
      ierr = i_err_fatal
      RETURN
    END IF
    IF (line(1:10) == '*FILE TYPE') THEN
      BACKSPACE iu_esft
      EXIT
    END IF
  END DO
  IF (ierr /= i_normal) RETURN

! Assemble the list of indexing numbers.
  DO i=1, Spectrum%Basic%n_band
    l_index_band(i,:)=.FALSE.
    DO j=1, Spectrum%Gas%n_band_absorb(i)
      l_index_band(i, Spectrum%Gas%index_absorb(j, i))=.TRUE.
    END DO
  END DO

  outer: DO
    inner: DO
      READ(iu_esft, '(A)', IOSTAT=ios) line
      IF (ios < 0) EXIT outer
      IF (line(1:10) == '*FILE TYPE') THEN
        BACKSPACE iu_esft
        EXIT inner
      END IF
    END DO inner
    READ(iu_esft, '(15x, i5, //)', IOSTAT=ios) i_input_type
    IF (ios < 0) EXIT
    IF (i_input_type /= it_file_line_fit .AND.                          &
        i_input_type /= it_file_line_fit_self) THEN
      WRITE(*, '(/a)') &
        '***error: the esft data have an invalid file type.'
      ierr=i_err_fatal
      RETURN
    END IF
    READ(iu_esft, '(14x, i5, 21x, i5)') i_band, i_index

!   Find the position of this datum in the array of gases.
    IF (.NOT.l_index_band(i_band, i_index)) THEN
      WRITE(*, '(/a, i5)') 'Adding gas to band',i_band
      l_index_band(i_band, i_index) = .TRUE.
      Spectrum%Gas%n_band_absorb(i_band) =                              &
        Spectrum%Gas%n_band_absorb(i_band)+1
      Spectrum%Gas%index_absorb(Spectrum%Gas%n_band_absorb(i_band),     &
        i_band) = i_index
    END IF

!   Set self-broadening flag
    IF (i_input_type == it_file_line_fit_self) THEN
!     Self-broadening is included
      IF (Spectrum%Gas%index_sb(i_index) == 0) THEN
!       Create new index for this gas for use in self-broadened arrays
        Spectrum%Gas%l_self_broadening(i_index) = .TRUE.
        i_index_sb =                                                    &
          MAXVAL(Spectrum%Gas%index_sb(1:Spectrum%Gas%n_absorb)) + 1
        Spectrum%Gas%index_sb(i_index) = i_index_sb
        Spectrum%Gas%n_absorb_sb = Spectrum%Gas%n_absorb_sb + 1
      ELSE
!       Get the index for this gas in self-broadened arrays
        i_index_sb = Spectrum%Gas%index_sb(i_index)
      END IF
    END IF

    READ(iu_esft, '(18x, 1pe10.3, 21x, 1pe10.3)')                       &
      Spectrum%Gas%p_ref(i_index, i_band),                              &
      Spectrum%Gas%t_ref(i_index, i_band)
    READ(iu_esft, '(//)')
!   Read over the transmission data.
    DO
      READ(iu_esft, '(a)') line
      IF (.NOT.non_blank(line)) EXIT
    END DO
    READ(iu_esft, '(/, 23x, i5, 20x, i5, 21x, i5, //)')                 &
      Spectrum%Gas%i_band_k(i_band, i_index),                           &
      Spectrum%Gas%i_scale_k(i_band, i_index),                          &
      Spectrum%Gas%i_scale_fnc(i_band, i_index)

!   Resize arrays if the number of k-terms is greater than that allocated
    nd_k_term_alloc = nd_k_term
    IF (Spectrum%Gas%i_band_k(i_band, i_index) > nd_k_term_alloc) THEN
      nd_k_term = Spectrum%Gas%i_band_k(i_band, i_index)

      ALLOCATE(arr_tmp_int_3d(nd_k_term_alloc, nd_band, nd_species))
      arr_tmp_int_3d = Spectrum%Gas%i_scat
      DEALLOCATE(Spectrum%Gas%i_scat)
      ALLOCATE(Spectrum%Gas%i_scat(nd_k_term, nd_band, nd_species))
      Spectrum%Gas%i_scat(1:nd_k_term_alloc,:,:) = arr_tmp_int_3d
      Spectrum%Gas%i_scat(nd_k_term_alloc+1:,:,:) = 0
      DEALLOCATE(arr_tmp_int_3d)

      ALLOCATE(arr_tmp_real_3d(nd_k_term_alloc, nd_band, nd_species))   
      arr_tmp_real_3d = Spectrum%Gas%k
      DEALLOCATE(Spectrum%Gas%k)
      ALLOCATE(Spectrum%Gas%k(nd_k_term, nd_band, nd_species))
      Spectrum%Gas%k(1:nd_k_term_alloc,:,:) = arr_tmp_real_3d
      arr_tmp_real_3d = Spectrum%Gas%w
      DEALLOCATE(Spectrum%Gas%w)
      ALLOCATE(Spectrum%Gas%w(nd_k_term, nd_band, nd_species))
      Spectrum%Gas%w(1:nd_k_term_alloc,:,:) = arr_tmp_real_3d
      DEALLOCATE(arr_tmp_real_3d)

      ALLOCATE(arr_tmp_real_4d(nd_scale_variable, nd_k_term_alloc, &
        nd_band, nd_species))
      arr_tmp_real_4d = Spectrum%Gas%scale
      DEALLOCATE(Spectrum%Gas%scale)
      ALLOCATE(Spectrum%Gas%scale(nd_scale_variable, nd_k_term, &
        nd_band, nd_species))
      Spectrum%Gas%scale(:,1:nd_k_term_alloc,:,:) = arr_tmp_real_4d
      DEALLOCATE(arr_tmp_real_4d)

      IF (ALLOCATED(Spectrum%Gas%k_lookup)) THEN
        ALLOCATE(arr_tmp_real_5d(Spectrum%Dim%nd_tmp, &
          Spectrum%Dim%nd_pre, nd_k_term_alloc, nd_species, nd_band))
        arr_tmp_real_5d = Spectrum%Gas%k_lookup
        DEALLOCATE(Spectrum%Gas%k_lookup)
        ALLOCATE(Spectrum%Gas%k_lookup( Spectrum%Dim%nd_tmp, &
          Spectrum%Dim%nd_pre, nd_k_term, nd_species, nd_band))
        Spectrum%Gas%k_lookup(:,:,1:nd_k_term_alloc,:,:) = arr_tmp_real_5d
        DEALLOCATE(arr_tmp_real_5d)
      END IF

      IF (ALLOCATED(Spectrum%Gas%k_lookup_sb)) THEN
        ALLOCATE(arr_tmp_real_6d(Spectrum%Dim%nd_tmp, &
          Spectrum%Dim%nd_pre, Spectrum%Dim%nd_gas_frac, &
          nd_k_term_alloc, nd_species, nd_band))
        arr_tmp_real_6d = Spectrum%Gas%k_lookup_sb
        DEALLOCATE(Spectrum%Gas%k_lookup_sb)
        ALLOCATE(Spectrum%Gas%k_lookup_sb(Spectrum%Dim%nd_tmp, &
          Spectrum%Dim%nd_pre, Spectrum%Dim%nd_gas_frac, &
          nd_k_term, nd_species, nd_band))
        Spectrum%Gas%k_lookup_sb(:,:,:,1:nd_k_term_alloc,:,:) = arr_tmp_real_6d
        DEALLOCATE(arr_tmp_real_6d)
      END IF
    END IF

    DO k=1, Spectrum%Gas%i_band_k(i_band, i_index)
      READ(iu_esft, '(2(3x, 1pe16.9), (t39, 2(3x, 1pe16.9)))')          &
        Spectrum%Gas%k(k, i_band, i_index),                             &
        Spectrum%Gas%w(k, i_band, i_index),                             &
        (Spectrum%Gas%scale(l, k, i_band, i_index),                     &
        l=1, n_scale_variable(Spectrum%Gas%i_scale_fnc(i_band, i_index)))
    END DO
    READ(iu_esft, '(/)')

    IF (Spectrum%Gas%i_scale_fnc(i_band,i_index) == ip_scale_lookup) THEN
!     Read in lookup table.
      READ(iu_esft, '(14x, i4, 12x, i4)')                               &
        Spectrum%Gas%num_ref_p(i_index,i_band),                         &
        Spectrum%Gas%num_ref_t(i_index,i_band)

      IF ( (MAXVAL(Spectrum%Gas%num_ref_p) > Spectrum%Dim%nd_pre) .OR.  &
           (MAXVAL(Spectrum%Gas%num_ref_t) > Spectrum%Dim%nd_tmp) ) THEN
        Spectrum%Dim%nd_pre = MAXVAL(Spectrum%Gas%num_ref_p)
        Spectrum%Dim%nd_tmp = MAXVAL(Spectrum%Gas%num_ref_t)
        IF (ALLOCATED(Spectrum%Gas%p_lookup)) &
            DEALLOCATE(Spectrum%Gas%p_lookup)
        ALLOCATE(Spectrum%Gas%p_lookup( Spectrum%Dim%nd_pre ))
        IF (ALLOCATED(Spectrum%Gas%t_lookup)) &
            DEALLOCATE(Spectrum%Gas%t_lookup)
        ALLOCATE(Spectrum%Gas%t_lookup( Spectrum%Dim%nd_tmp,            &
                                        Spectrum%Dim%nd_pre ))
        IF (ALLOCATED(Spectrum%Gas%k_lookup)) &
            DEALLOCATE(Spectrum%Gas%k_lookup)
        ALLOCATE(Spectrum%Gas%k_lookup( Spectrum%Dim%nd_tmp,            &
                                        Spectrum%Dim%nd_pre,            &
                                        nd_k_term, nd_species, nd_band ))
        IF (ALLOCATED(Spectrum%Gas%k_lookup_sb)) &
            DEALLOCATE(Spectrum%Gas%k_lookup_sb)
        ALLOCATE(Spectrum%Gas%k_lookup_sb(Spectrum%Dim%nd_tmp,          &
                                          Spectrum%Dim%nd_pre,          &
                                          Spectrum%Dim%nd_gas_frac,     &
                                          nd_k_term, nd_species,        &
                                          nd_band ))
      END IF

      DO ip=1, Spectrum%Dim%nd_pre
        READ(iu_esft, '(6(1PE13.6))', IOSTAT=ios)                       &
          Spectrum%Gas%p_lookup(ip), (Spectrum%Gas%t_lookup(it, ip),    &
                                      it=1, Spectrum%Dim%nd_tmp)
        IF (ios /= 0) THEN
          WRITE(*, '(/A/)') '*** Error in subroutine make_block_5'
          WRITE(*,'(a, i4)') 'P/T look-up table entry:', ip
          ierr=i_err_fatal
          RETURN
        END IF
        Spectrum%Gas%p_lookup(ip)=LOG(Spectrum%Gas%p_lookup(ip))
      END DO

      IF (Spectrum%Gas%l_self_broadening(i_index)) THEN
!       Read gas fraction lookup table.
        READ(iu_esft, '(/,14x, i4)') Spectrum%Gas%n_gas_frac

        IF (Spectrum%Gas%n_gas_frac > Spectrum%Dim%nd_gas_frac) THEN
          Spectrum%Dim%nd_gas_frac = Spectrum%Gas%n_gas_frac
          IF (ALLOCATED(Spectrum%Gas%gf_lookup))                        &
              DEALLOCATE(Spectrum%Gas%gf_lookup)
          ALLOCATE(Spectrum%Gas%gf_lookup( Spectrum%Dim%nd_gas_frac ))
          IF (ALLOCATED(Spectrum%Gas%k_lookup_sb))                      &
              DEALLOCATE(Spectrum%Gas%k_lookup_sb)
          ALLOCATE(Spectrum%Gas%k_lookup_sb(Spectrum%Dim%nd_tmp,        &
                                            Spectrum%Dim%nd_pre,        &
                                            Spectrum%Dim%nd_gas_frac,   &
                                            nd_k_term, nd_species,      &
                                            nd_band ))
        END IF

        READ(iu_esft, '(6(1PE13.6))', IOSTAT=ios)                       &
           (Spectrum%Gas%gf_lookup(igf),                                &
            igf=1, Spectrum%Dim%nd_gas_frac)
        IF (ios /= 0) THEN
          WRITE(*, '(/A/)') '*** Error in subroutine make_block_5'
          WRITE(*,'(a, i4)') 'Gas fraction look-up table is corrupt.'
          ierr=i_err_fatal
          RETURN
        END IF
      END IF

!     Skip over the headers.
      READ(iu_esft, '(/)')
      IF (Spectrum%Gas%l_self_broadening(i_index)) THEN
        DO k=1, Spectrum%Gas%i_band_k(i_band, i_index)
          DO igf=1, Spectrum%Gas%n_gas_frac
            DO ip=1, Spectrum%Dim%nd_pre
              READ(iu_esft, '(6(1PE13.6))', IOSTAT=ios)                 &
                (Spectrum%Gas%k_lookup_sb(it,ip,igf,k,                  &
                                          i_index_sb,i_band),           &
                 it=1, Spectrum%Dim%nd_tmp)
              IF (ios /= 0) THEN
                WRITE(*, '(/A/)') '*** Error in subroutine make_block_5'
                WRITE(*,'(a, 4i4)') 'Look-up table entry:',             &
                  i_band, k, i_index, ip
                ierr=i_err_fatal
                RETURN
              END IF
            END DO
          END DO
        END DO
      ELSE
        DO k=1, Spectrum%Gas%i_band_k(i_band, i_index)
          DO ip=1, Spectrum%Dim%nd_pre
            READ(iu_esft, '(6(1PE13.6))', IOSTAT=ios)                   &
              (Spectrum%Gas%k_lookup(it,ip,k,i_index,i_band),           &
               it=1, Spectrum%Dim%nd_tmp)
            IF (ios /= 0) THEN
              WRITE(*, '(/A/)') '*** Error in subroutine make_block_5'
              WRITE(*,'(a, 4i4)') 'Look-up table entry:',               &
                i_band, k, i_index, ip
              ierr=i_err_fatal
              RETURN
            END IF
          END DO
        END DO
      END IF
    END IF
  END DO outer

  Spectrum%Basic%l_present(5)=.TRUE.
  CLOSE(iu_esft)

END SUBROUTINE make_block_5
