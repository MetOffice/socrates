! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Adapted from a routine supplied by Environment Canada
!
!  Module containing subroutine to generate sub-columns for McICA
!
! Method:
!       See Raisanen et al, 2004, Stochastic generation of subgrid-scale 
!       cloudy columns for large-scale models,
!       Quart. J. Roy. Meteorol. Soc., 2004 , 130 , 2047-2068
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: Radiation Control
!
!- --------------------------------------------------------------------- 
MODULE cld_generator_mod

  IMPLICIT NONE

  CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='CLD_GENERATOR_MOD'

CONTAINS

  SUBROUTINE cld_generator(lay, cloud_top, ilg, nsubcol,                &
                           rlc_cf, rlc_cw, sigma_qcw, avg_cf,           &
                           c_cloud, c_ratio, ls_ratio, zf, il1, il2)

! --------------------------------------------------------------------
! --------------------------------------------------------------------
! As we use pressure coordinates in the UM, which decrease with height,
! I removed the minus sign from the calculation of alpha.

! Generate a ILG GCM column by LAY layer cloud field with NSUBCOL
! subcolumns in each GCM column. Method and model evaluation is 
! described in: Raisanen et al, 2004, Stochastic generation of 
! subgrid-scale cloudy columns for large-scale models,
! Quart. J. Roy. Meteorol. Soc., 2004 , 130 , 2047-2068
! Input profiles must be from top of the model to the bottom as is the 
! output. Note that all the cloudy subcolumns are placed at the "front".
! For example, if there are N cloudy subcolumns for a GCM column then 
! subcolumns 1 through N will contain the cloudy subcolumns while the 
! remaining subcolumns will not.
! -------------------------------------------------------------------- 
! This version generates the cloud overlap using 4 methods:
! The maximum-random overlap (MRO) in two methods:
! 1. MRO assumes that contiguous cloud layers are strictly maximum 
!    overlapped and non-contiguous layers are random overlap 
!    (IOVERLAP = 4)
! 2. MRO as described in Geleyn and Hollingsworth, 1979 (IOVERLAP=0)
! 3. "generalized overlap" approach of Hogan and Illingsworth, 
!    2000 (IOVERLAP = 2) 
! 4. "generalized overlap" approach of Hogan and Illingsworth, 
!    2000 with non-contiguous cloudy layer randomly overlapped 
!    (IOVERLAP = 3) 
!
! The cloud can be either horizontally homogeneous (IPPH=0) or 
! horizontally inhomogeneous (IPPH=1)
! The inhomogeneity can be described by either a Gamma distribution 
! or a Beta distribution
! The values describing the distribution are read in from a data file
! by the subroutine read_mcica_data, which is contained in the module
! mcica_mod. To switch between distributions, a different data file 
! is required. 

    USE realtype_rd, ONLY: RealK
    USE rand_no_mcica, ONLY: m,a,c
    USE mcica_mod
    USE yomhook, ONLY: lhook, dr_hook
    USE parkind1, ONLY: jprb, jpim
    USE ereport_mod, ONLY: ereport
    IMPLICIT NONE


! Note: LAY     => Number of layers in GCM
!       ILG     => Number of GCM columns
!       NSUBCOL => Number of sub-columns to generate


! INPUT DATA

    INTEGER, INTENT(IN) :: &
    il1,                   & ! Start GCM column
    il2,                   & ! End GCM column
    lay,                   &
    cloud_top,             &
    ilg,                   &
    nsubcol

    REAL(RealK), INTENT(IN) :: &
    zf(ilg,lay),               &  ! Full-level (layer midpoint) altitude (km)
    avg_cf(ilg,0:lay),         &  ! Cloud amount for each layer
    c_cloud(ilg,lay),          &  ! Convective cloud amount for each layer
    c_ratio(ilg,lay),          &  ! Convective condensate ratio
    ls_ratio(ilg,lay),         &  ! Large-scale condensate ratio
    sigma_qcw(ilg,lay),        &  ! Normalized cloud condensate std. dev. 
                                  ! (Std. dev. over mean)
    rlc_cf(ilg,lay),           &  ! Cloud fraction decorrelation length (km)
    rlc_cw(ilg,lay)               ! Cloud condensate decorrelation length (km)


! LOCAL DATA (ARRAYS)

    REAL(RealK) :: sigma_ccw(ilg,lay) ! Normalized cloud condensate std. dev. 
                                      ! for convective cloud

    REAL(RealK) ::  &
    int_x(ilg),     &
    int_y(ilg),     &    
    x(ilg),         & ! Random number vectors
    y(ilg),         &
    x1(ilg),        &
    y1(ilg),        &
    x2(ilg),        &
    y2(ilg),        &
    alpha(ilg,0:lay), & ! Fraction of maximum/random cloud overlap
    rcorr(ilg,0:lay)    ! Fraction of maximum/random cloud condensate
                        ! overlap

    INTEGER :: &
    i_loc(ilg) ! Index to place the new subcolumns into the arrays 
               ! starting from the front

    LOGICAL :: &
    l_cld(ilg) ! Flag that cloud has been generated in subcolumn


! LOCAL DATA (SCALARS)

    INTEGER :: &
    il,        & ! Counter over ILG GCM columns
    i,         & ! Counter
    k,         & ! Counter over LAY vertical layers
    k_top,     & ! Index of top most cloud layer
    k_base,    & ! Index of lowest cloud layer
    ind1,      & ! Index in variability calculation
    ind2,      & ! Index in variability calculation
    ErrorStatus  ! ErrorStatus for Ereport

    REAL(RealK) :: &
    rind1,         & ! Real index in variability calculation
    rind2,         & ! Real index in variability calculation
    zcw              ! Ratio of cloud condensate mixing ratio for
                     ! this cell to its layer cloud-mean value

    LOGICAL :: &
    l_top,     & ! Flag for cloud top
    l_bot        ! Flag for cloud bottom

    CHARACTER(LEN=*), PARAMETER :: RoutineName = 'CLD_GENERATOR'
    CHARACTER(LEN=80)           :: cmessage

    INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
    INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
    REAL(KIND=jprb)               :: zhook_handle

    REAL(RealK) :: temp, rm


    IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

!   Set convective cloud FSD equal to large-scale value for now.
    sigma_ccw=sigma_qcw

!   Initialize the arrays
    DO il = il1, il2
       i_loc(il) = 1
       l_cld(il) = .FALSE.
    END DO

    rm=1.0/REAL(m)

!   Find uppermost cloudy layer
    l_top = .FALSE.
    k_top = 0
    DO k=1,lay
       DO il = il1, il2
          k_top = k
          IF (avg_cf(il,k) > cut) l_top = .TRUE.
       END DO ! IL
       IF (l_top) EXIT
    END DO ! K

!   If no cloudy layers in any GCM column then exit
    IF (k_top == 0) THEN
      IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out, &
                              zhook_handle)
      RETURN
    END IF

!   Find lowermost cloudy layer
    l_bot = .FALSE.
    k_base = 0
    DO k=lay,1,-1
       DO il = il1, il2
          k_base = k
          IF (avg_cf(il,k) >= cut) l_bot = .TRUE.
       END DO ! IL
       IF (l_bot) EXIT
    END DO ! K

!   Calculate overlap factors ALPHA for cloud fraction and RCORR for cloud
!   condensate based on layer midpoint distances and decorrelation depths
    IF (ioverlap == 3) THEN   ! Using generalized overlap
       DO k = k_top-1, k_base-1
          DO il = il1, il2
             IF (avg_cf(il,k) > 0.0) THEN
                IF (rlc_cf(il,k) > 0.0) THEN
                   alpha(il,k) = EXP((zf(il,k) - zf(il,k+1))            &
                              / rlc_cf(il,k))
                ELSE
                   alpha(il,k) = 0.0
                END IF
                IF (rlc_cw(il,k) > 0.0) THEN
                  rcorr(il,k) = EXP((zf(il,k) - zf(il,k+1))             &
                              / rlc_cw(il,k))
                ELSE
                   rcorr(il,k) = 0.0
                END IF
             ELSE
                alpha(il,k) = 0.0
                rcorr(il,k) = 0.0
             ENDIF
          END DO ! IL
       END DO ! K
    END IF

    IF (ioverlap == 2) THEN   ! Using generalized overlap
       DO k = cloud_top-1, k_base-1
         IF (k == 0) THEN
           alpha(:,k) = 0.0
           rcorr(:,k) = 0.0
         ELSE
           DO il = il1, il2
             IF (rlc_cf(il,k) > 0.0) THEN
                alpha(il,k) = EXP((zf(il,k) - zf(il,k+1))               &
                           / rlc_cf(il,k))
             ELSE
                alpha(il,k) = 0.0
             END IF
             IF (rlc_cw(il,k) > 0.0) THEN
               rcorr(il,k) = EXP((zf(il,k) - zf(il,k+1))                &
                           / rlc_cw(il,k))
             ELSE
                rcorr(il,k) = 0.0
             END IF
           END DO ! IL
         END IF
       END DO ! K
    END IF

! Although it increases the size of the code I will use an if-then-else 
! appraoch to do the different version of cloud overlap
! It should make it easier for people to follow what is going on.
! IOVERLAP = 4, the overlap is maximum-random
! IOVERLAP = 0, the overlap is maximum-random following the definition 
! given in Geleyn and Hollingsworth
! IOVERLAP = 2, the overlap is generalized overlap
! IOVERLAP = 3, the overlap is generalized overlap with non-contiguous 
! cloud layers randomly overlapped.

    IF (ioverlap == 4) THEN

       DO i=1,nsubcol

!         Generate all subcolumns for latitude chain
          DO il = il1, il2
             int_x(il) = rand_seed_x(il,i)
             int_y(il) = rand_seed_y(il,i)
          ENDDO

          DO k = k_top, k_base

             DO il = il1, il2

                IF (avg_cf(il,k) > 0.0) THEN

                   IF (avg_cf(il,k-1) == 0.0) THEN !It is clear above
                      temp=int_x(il)*a+c
                      int_x(il)=temp-INT(temp*rm)*m
                      x(il)=(REAL(int_x(il))*rm) * (1.0 - avg_cf(il,k-1))

                      temp=int_y(il)*a+c
                      int_y(il)=temp-INT(temp*rm)*m
                      y(il)=REAL(int_y(il))*rm
                   ENDIF

!                  Treatment of cloudy cells
                   IF (x(il) < avg_cf(il,k)) THEN ! Generate cloud here

                      IF (ipph == 0) THEN ! Homogeneous clouds
                         zcw = 1.0
                      ELSE
! Horizontally variable clouds:
! Determine ZCW = ratio of cloud condensate mixing ratio QC for this cell 
! to its mean value for all cloudy cells in this layer.
! Use bilinear interpolation of ZCW tabulated in array XCW as a function 
! of
!    * cumulative probability Y
!    * relative standard deviation SIGMA
! Take care that the definition of RIND2 is consistent with subroutine
! TABULATE_XCW

                         rind1 = y(il) * (n1 - 1) + 1.0
                         ind1  = MAX(1, MIN(INT(rind1), n1-1))
                         rind1 = rind1 - ind1
                         rind2 = 40.0 * sigma_qcw(il,k) - 3.0
                         ind2  = MAX(1, MIN(INT(rind2), n2-1))
                         rind2 = rind2 - ind2

                         zcw = (1.0-rind1) * (1.0-rind2)* xcw(ind1,ind2)&
                             + (1.0-rind1) * rind2 * xcw(ind1,ind2+1)   &
                             + rind1 * (1.0-rind2) * xcw(ind1+1,ind2)   &
                             + rind1 * rind2 * xcw(ind1+1,ind2+1)
                      END IF

!                     A horizontally constant IWC/LWC ratio is assumed
!                     for each layer so far
                      l_cld(il)             = .TRUE.
                      clw_sub_full(il,k,i_loc(il)) = zcw
!                      cic_sub_full(il,k,i_loc(il)) = zcw
                   END IF

                ENDIF
             END DO           ! IL
          END DO              ! K

!         Need to check if a cloudy subcolumn was generated
          DO il = il1, il2
             IF (l_cld(il)) THEN
                i_loc(il) = i_loc(il) + 1
                l_cld(il) = .FALSE.
             END IF
          END DO
       END DO                 ! I
    ELSE IF (ioverlap == 0) THEN
       DO i=1,nsubcol

!         Generate all subcolumns for latitude chain
          DO il = il1, il2
             int_x(il) = rand_seed_x(il,i)
             int_y(il) = rand_seed_y(il,i)
          ENDDO

          DO k = k_top, k_base

             DO il = il1, il2

                IF (avg_cf(il,k) > 0.0) THEN

                   temp=int_x(il)*a+c
                   int_x(il)=temp-INT(temp*rm)*m
                   x(il)=REAL(int_x(il))*rm
 
                   temp=int_y(il)*a+c
                   int_y(il)=temp-INT(temp*rm)*m
                   y(il)=REAL(int_y(il))*rm
    
                   IF (x(il) < (1.0-avg_cf(il,k-1))) THEN !It is clear above
                      temp=int_x(il)*a+c
                      int_x(il)=temp-INT(temp*rm)*m
                      x1(il)=REAL(int_x(il))*rm

                      temp=int_y(il)*a+c
                      int_y(il)=temp-INT(temp*rm)*m
                      y1(il)=REAL(int_y(il))*rm

                      x(il)=x1(il) * (1.0 - avg_cf(il,k-1))
                      y(il)=y1(il)
                   END IF

!                  Treatment of cloudy cells
                   IF (x(il) < avg_cf(il,k)) THEN ! Generate cloud here

                      IF (ipph == 0) THEN ! Homogeneous clouds
                         zcw = 1.0
                      ELSE
! Horizontally variable clouds:
! Determine ZCW = ratio of cloud condensate mixing ratio QC for this cell 
! to its mean value for all cloudy cells in this layer.
! Use bilinear interpolation of ZCW tabulated in array XCW as a function 
! of
!    * cumulative probability Y
!    * relative standard deviation SIGMA
! Take care that the definition of RIND2 is consistent with subroutine
! TABULATE_XCW

                         rind1 = y(il) * (n1 - 1) + 1.0
                         ind1  = MAX(1, MIN(INT(rind1), n1-1))
                         rind1 = rind1 - ind1
                         rind2 = 40.0 * sigma_qcw(il,k) - 3.0
                         ind2  = MAX(1, MIN(INT(rind2), n2-1))
                         rind2 = rind2 - ind2

                         zcw = (1.0-rind1) * (1.0-rind2)* xcw(ind1,ind2)&
                             + (1.0-rind1) * rind2 * xcw(ind1,ind2+1)   &
                             + rind1 * (1.0-rind2) * xcw(ind1+1,ind2)   &
                             + rind1 * rind2 * xcw(ind1+1,ind2+1)
                      END IF

!                     A horizontally constant IWC/LWC ratio is assumed
!                     for each layer so far
                      l_cld(il)             = .TRUE.
                      clw_sub_full(il,k,i_loc(il)) = zcw
!                      cic_sub_full(il,k,i_loc(il)) = zcw
                   END IF

                ENDIF
             END DO           ! IL
          END DO              ! K

!         Need to check if a cloudy subcolumn was generated
          DO il = il1, il2
             IF (l_cld(il)) THEN
                i_loc(il) = i_loc(il) + 1
                l_cld(il) = .FALSE.
             END IF
          END DO
       END DO                 ! I
    ELSE IF (ioverlap == 2) THEN
       DO i=1,nsubcol

!         Generate all subcolumns for latitude chain
          DO il = il1, il2
             int_x(il)=rand_seed_x(il,i)
             int_y(il)=rand_seed_y(il,i)
          ENDDO

          DO k = cloud_top, k_base

             DO il = il1, il2

                IF (avg_cf(il,k) > 0.0) THEN
                   temp=int_x(il)*a+c
                   int_x(il)=temp-INT(temp*rm)*m
                   x1(il)=REAL(int_x(il))*rm

                   temp=int_y(il)*a+c
                   int_y(il)=temp-INT(temp*rm)*m
                   y1(il)=REAL(int_y(il))*rm

                   IF (x1(il) >= alpha(il,k-1)) THEN
                     temp=int_x(il)*a+c
                     int_x(il)=temp-INT(temp*rm)*m
                     x(il)=REAL(int_x(il))*rm
                   ENDIF
                   IF (y1(il) >= rcorr(il,k-1)) THEN
                     temp=int_y(il)*a+c
                     int_y(il)=temp-INT(temp*rm)*m
                     y(il)=REAL(int_y(il))*rm
                   ENDIF

!                  Treatment of cloudy cells
                   IF (x(il) < avg_cf(il,k)) THEN ! Generate cloud here

                      IF (ipph == 0) THEN ! Homogeneous clouds
                         zcw = 1.0
                      ELSE IF (x(il) < c_cloud(il,k)) THEN
                         rind1 = y(il) * (n1 - 1) + 1.0
                         ind1  = MAX(1, MIN(INT(rind1), n1-1))
                         rind1 = rind1 - ind1
                         rind2 = 40.0 * sigma_ccw(il,k) - 3.0
                         ind2  = MAX(1, MIN(INT(rind2), n2-1))
                         rind2 = rind2 - ind2

                         zcw = (1.0-rind1) * (1.0-rind2)* xcw(ind1,ind2)&
                             + (1.0-rind1) * rind2 * xcw(ind1,ind2+1)   &
                             + rind1 * (1.0-rind2) * xcw(ind1+1,ind2)   &
                             + rind1 * rind2 * xcw(ind1+1,ind2+1)

                         zcw = zcw*c_ratio(il,k)
                      ELSE
! Horizontally variable clouds:
! Determine ZCW = ratio of cloud condensate mixing ratio QC for this cell 
! to its mean value for all cloudy cells in this layer.
! Use bilinear interpolation of ZCW tabulated in array XCW as a function 
! of
!    * cumulative probability Y
!    * relative standard deviation SIGMA
! Take care that the definition of RIND2 is consistent with subroutine
! TABULATE_XCW

                         rind1 = y(il) * (n1 - 1) + 1.0
                         ind1  = MAX(1, MIN(INT(rind1), n1-1))
                         rind1 = rind1 - ind1
                         rind2 = 40.0 * sigma_qcw(il,k) - 3.0
                         ind2  = MAX(1, MIN(INT(rind2), n2-1))
                         rind2 = rind2 - ind2

                         zcw = (1.0-rind1) * (1.0-rind2)* xcw(ind1,ind2)&
                             + (1.0-rind1) * rind2 * xcw(ind1,ind2+1)   &
                             + rind1 * (1.0-rind2) * xcw(ind1+1,ind2)   &
                             + rind1 * rind2 * xcw(ind1+1,ind2+1)

                         zcw = zcw*ls_ratio(il,k)
                      END IF

!                     A horizontally constant IWC/LWC ratio is assumed
!                     for each layer so far
                      l_cld(il)             = .TRUE.
                      clw_sub_full(il,k,i_loc(il)) = zcw
!                      cic_sub_full(il,k,i_loc(il)) = zcw
                   END IF

                ENDIF
             END DO           ! IL
          END DO              ! K

!         Need to check if a cloudy subcolumn was generated
          DO il = il1, il2
             IF (l_cld(il)) THEN
                i_loc(il) = i_loc(il) + 1
                l_cld(il) = .FALSE.
             END IF
          END DO
       END DO                 ! I
    ELSE IF (ioverlap == 3) THEN
       DO i=1,nsubcol

!         Generate all subcolumns for latitude chain
          DO il = il1, il2
            int_x(il)=rand_seed_x(il,i)
            int_y(il)=rand_seed_y(il,i)
          END DO

          DO k = k_top, k_base

             DO il = il1, il2

                IF (avg_cf(il,k) > 0.0) THEN
                   temp=int_x(il)*a+c
                   int_x(il)=temp-INT(temp*rm)*m
                   x1(il)=REAL(int_x(il))*rm

                   temp=int_y(il)*a+c
                   int_y(il)=temp-INT(temp*rm)*m
                   y1(il)=REAL(int_y(il))*rm

                   IF (x1(il) >= alpha(il,k-1)) THEN
                     temp=int_x(il)*a+c
                     int_x(il)=temp-INT(temp*rm)*m
                     x(il)=REAL(int_x(il))*rm
                   ENDIF
                   IF (y1(il) >= rcorr(il,k-1)) THEN
                     temp=int_y(il)*a+c
                     int_y(il)=temp-INT(temp*rm)*m
                     y(il)=REAL(int_y(il))*rm
                   ENDIF

!                  Treatment of cloudy cells
                   IF (x(il) < avg_cf(il,k)) THEN ! Generate cloud here

                      IF (ipph == 0) THEN ! Homogeneous clouds
                         zcw = 1.0
                      ELSE IF (x(il) < c_cloud(il,k)) THEN
                         rind1 = y(il) * (n1 - 1) + 1.0
                         ind1  = MAX(1, MIN(INT(rind1), n1-1))
                         rind1 = rind1 - ind1
                         rind2 = 40.0 * sigma_ccw(il,k) - 3.0
                         ind2  = MAX(1, MIN(INT(rind2), n2-1))
                         rind2 = rind2 - ind2

                         zcw = (1.0-rind1) * (1.0-rind2)* xcw(ind1,ind2)&
                             + (1.0-rind1) * rind2 * xcw(ind1,ind2+1)   &
                             + rind1 * (1.0-rind2) * xcw(ind1+1,ind2)   &
                             + rind1 * rind2 * xcw(ind1+1,ind2+1)

                         zcw = zcw*c_ratio(il,k)
                      ELSE
! Horizontally variable clouds:
! Determine ZCW = ratio of cloud condensate mixing ratio QC for this cell 
! to its mean value for all cloudy cells in this layer.
! Use bilinear interpolation of ZCW tabulated in array XCW as a function 
! of
!    * cumulative probability Y
!    * relative standard deviation SIGMA
! Take care that the definition of RIND2 is consistent with subroutine
! TABULATE_XCW

                         rind1 = y(il) * (n1 - 1) + 1.0
                         ind1  = MAX(1, MIN(INT(rind1), n1-1))
                         rind1 = rind1 - ind1
                         rind2 = 40.0 * sigma_qcw(il,k) - 3.0
                         ind2  = MAX(1, MIN(INT(rind2), n2-1))
                         rind2 = rind2 - ind2

                         zcw = (1.0-rind1) * (1.0-rind2)* xcw(ind1,ind2)&
                             + (1.0-rind1) * rind2 * xcw(ind1,ind2+1)   &
                             + rind1 * (1.0-rind2) * xcw(ind1+1,ind2)   &
                             + rind1 * rind2 * xcw(ind1+1,ind2+1)

                         zcw = zcw*ls_ratio(il,k)
                      END IF

!                     A horizontally constant IWC/LWC ratio is assumed
!                     for each layer so far
                      l_cld(il)             = .TRUE.
                      clw_sub_full(il,k,i_loc(il)) = zcw
!                      cic_sub_full(il,k,i_loc(il)) = zcw
                   END IF

                ENDIF
             END DO           ! IL
          END DO              ! K

!         Need to check if a cloudy subcolumn was generated
          DO il = il1, il2
             IF (l_cld(il)) THEN
                i_loc(il) = i_loc(il) + 1
                l_cld(il) = .FALSE.
             END IF
          END DO
       END DO                 ! I
    ELSE
       cmessage = 'Choice of overlap not compatible with cloud generator'
       ErrorStatus = 1
       CALL ereport(RoutineName, ErrorStatus, cmessage)
    END IF ! IOVERLAP


!   Record the number of cloudy subcolumns generated
    DO il = il1, il2
       ncldy(il) = i_loc(il)-1
    END DO ! IL
    IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
    RETURN
  END SUBROUTINE cld_generator
END MODULE cld_generator_mod
