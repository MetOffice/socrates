! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

!  Subroutine to generate sub-columns for McICA

! Method:
!       Allocates arrays to store sub-columns, rearranges cloud fields
!       so that they are in suitable order for generator and calls
!       generator which fills these arrays. Copies cloudy sub-columns 
!       if more are required

! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: Radiation Control

!- ---------------------------------------------------------------------
SUBROUTINE open_cloud_gen(                                              &
!                       Model Dimensions
  n_layer, npd_layer, n_profile, npd_profile, p_temp                    &
!                       Properties of clouds
, w_cloud1, dp_corr_strat                                               &
!                       McICA input parameters
, rad_mcica_sampling, rad_mcica_sigma                                   &
!                       error information
, ierr)

  USE realtype_rd
  USE rad_pcf
  USE rand_no_mcica
  USE mcica_mod
  USE cld_generator_mod
  USE yomhook, ONLY: lhook, dr_hook
  USE parkind1, ONLY: jprb, jpim
  IMPLICIT NONE




!                       Dummy arguments.
  INTEGER, INTENT(INOUT) ::                                             &
    ierr
!           Error flag
!
!
!                       Dimensions of arrays
  INTEGER, INTENT(IN) ::                                                &
    n_profile                                                           &
!           Size allocated for atmospheric profiles
  , npd_profile                                                         &
!           Size allocated for atmospheric profiles
  , n_layer                                                             &
!           Size allocated for atmospheric layers
  , npd_layer
!           number of potentially cloudy layers
!
!                                                        
  REAL(RealK), INTENT(IN) ::                                            &
    p_temp(npd_profile, npd_layer)                                      &
!           Pressure
  , w_cloud1(npd_profile, npd_layer)
!           Amount of cloud
!
!
!                       Properties of clouds
  REAL(RealK), INTENT(IN) ::                                            &
    dp_corr_strat 
!           Decorrelation pressure scale for large scale cloud!

!                       McICA input parameters
  INTEGER, INTENT(IN) ::                                                &
    rad_mcica_sampling
!           Level of optimisation of sampling
  REAL(RealK), INTENT(IN) ::                                            &
    rad_mcica_sigma
!           FSD of cloud water content

  INTEGER ::                                                            &
    cloud_levels
!           Number of global cloud levels

!                       Local variables.
  INTEGER ::                                                            &
    i                                                                   &
!           Loop variable
  , j                                                                   &
!           Loop variable
  , k                                                                   &
!           Loop variable
  , l                                                                   &
!           Loop variable
  , info                                                                &
!           Loop variable
  , random_dummy_init                                                   &
!           Seed for generating seeds for random numbers used in the
!           generator          
  , random_dummy(n_profile)
!           Seed for generating seeds for random numbers used in the
!           generator

  REAL(RealK) ::                                                        &
    p(n_profile, n_layer)                                               &
!           Pressure
  , eps                                                                 &
!           small number to prevent division by zero.
  , cloud_scale                                                         &
!           cloud fraction times gridbox size
  , thickness_part(n_profile, n_layer)                                  &
!           part of FSD param related to layer thickness
  , dp_corr_cloud(n_profile,n_layer)                                    &
!           Cloud fraction decorrelation length
  , dp_corr_cond(n_profile,n_layer)                                     &
!           Cloud condensate decorrelation length
  , sigma_qcw(n_profile,n_layer)                                        &
!           Normalized cloud condensate std. dev
  , w_cloud(n_profile, 0:n_layer)                                       &
!           Amount of cloud
  , c_cloud(n_profile, n_layer)                                         &
!           Amount of convective cloud
  , c_ratio(n_profile, n_layer)                                         &
!           Ratio of convective cloud condensate to mean condensate
  , ls_ratio(n_profile, n_layer)
!           Ratio of large-scale cloud condensate to mean condensate

  REAL(RealK), PARAMETER ::                                             &
    one_third = 1.0/3.0                                                 
!           Constant for use in FSD parametrization

  REAL(RealK), PARAMETER ::                                             &
    root_two = SQRT(2.0)                                                
!           Constant for use in FSD parametrization

  LOGICAL ::                                                            &
    l_layer_clear
!           Flag for layer free of clouds

  REAL(RealK), ALLOCATABLE :: temp_rand(:,:,:)
  INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
  INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
  REAL(KIND=jprb)               :: zhook_handle

  CHARACTER(LEN=*), PARAMETER :: RoutineName='OPEN_CLOUD_GEN'

  IF (lhook) CALL dr_hook(RoutineName,zhook_in,zhook_handle)

  w_cloud=0.0_RealK
  c_cloud=0.0_RealK
  c_ratio=0.0_RealK
  ls_ratio=1.0_RealK

  DO j=1,npd_layer
    DO i=1,n_profile
      IF (w_cloud1(i,j) .gt. cut) THEN
        w_cloud(i,j)=w_cloud1(i,j)
      END IF
    END DO
  END DO

  DO j=1, n_layer
    DO i=1, n_profile
      sigma_qcw(i,j)=rad_mcica_sigma
    END DO
  END DO


  cloud_levels=1
  DO j=1,npd_layer
    DO i=1,n_profile
      IF (w_cloud1(i,j) .gt. cut) THEN
        cloud_levels=j
      END IF
    END DO
  END DO

  eps=EPSILON(eps)

! Set the SW and LW values of subcol_k (the number of sub-columns 
! each k-term "sees") and subcol_reorder (a reordering of the 
! sub-columns so that each sub-column is equivalently important in 
! the SW and LW).
!
  SELECT CASE (rad_mcica_sampling)

  CASE (ip_mcica_full_sampling)
    subcol_need=tot_subcol_gen
    sw_subcol_k=tot_subcol_gen
    lw_subcol_k=tot_subcol_gen

    ALLOCATE(lw_subcol_reorder(subcol_need))
    DO i=1,subcol_need
      lw_subcol_reorder(i)=i
    END DO

  CASE (ip_mcica_single_sampling)
    subcol_need=subcol_need_single
    sw_subcol_k=1
    lw_subcol_k=1

    ALLOCATE(lw_subcol_reorder(subcol_need))
    DO i=1,subcol_need
      lw_subcol_reorder(i) =                                        &
        MOD(lw_subcol_reorder_single(i),tot_subcol_gen)
      IF (lw_subcol_reorder(i) == 0) THEN
        lw_subcol_reorder(i) = tot_subcol_gen
      END IF
    END DO

  CASE (ip_mcica_optimal_sampling)
    subcol_need=subcol_need_optimal
!          sw_subcol_k and lw_subcol_k have been read from data file

    ALLOCATE(lw_subcol_reorder(subcol_need))
    DO i=1,subcol_need
      lw_subcol_reorder(i) =                                        &
        MOD(lw_subcol_reorder_optimal(i),tot_subcol_gen)
      IF (lw_subcol_reorder(i) == 0) THEN
        lw_subcol_reorder(i) = tot_subcol_gen
      END IF
    END DO
     
  END SELECT

  ALLOCATE(sw_subcol_reorder(subcol_need))
  DO i=1,subcol_need
    sw_subcol_reorder(i) = MOD(i, tot_subcol_gen)
    IF (sw_subcol_reorder(i) == 0) THEN
      sw_subcol_reorder(i) = tot_subcol_gen
    END IF
  END DO

  DO j=1,n_layer
    DO i=1,n_profile
      p(i,j)=p_temp(i,j)
    END DO
  END DO

  DO j=1, n_layer
    DO i=1, n_profile
      dp_corr_cloud(i,j)=dp_corr_strat
      dp_corr_cond(i,j)=dp_corr_cloud(i,j)*0.5
    END DO
  END DO
 
  ALLOCATE(clw_sub_full(n_profile,1:n_layer,tot_subcol_gen))
!        ALLOCATE(cic_sub_full(n_profile,1:n_layer,tot_subcol_gen))
  ALLOCATE(temp_rand(n_profile,1,tot_subcol_gen))
  ALLOCATE(frac_cloudy_full(n_profile))
  ALLOCATE(ncldy(n_profile))

  random_dummy_init=10
 
  DO i=1,n_profile
    random_dummy(i)=random_dummy_init+i
  END DO

! The initial values of random dummy are successive integers, which 
! result in random numbers that are close to each other.This first 
! call to mcica_rand_no is to ensure that the seed for each 
! profile is itself random.

  CALL mcica_rand_no(random_dummy, temp_rand,n_profile              &
    ,tot_subcol_gen)

  ALLOCATE(rand_seed_x(n_profile,tot_subcol_gen))

  DO i=1,tot_subcol_gen
    CALL mcica_rand_no(random_dummy, temp_rand,n_profile,1)
    DO j=1, n_profile
      rand_seed_x(j,i)=random_dummy(j)
    END DO
  END DO
  
  ALLOCATE(rand_seed_y(n_profile,tot_subcol_gen))

  DO i=1,tot_subcol_gen
    CALL mcica_rand_no(random_dummy, temp_rand,n_profile,1)
    DO j=1, n_profile
      rand_seed_y(j,i)=random_dummy(j)
    END DO
  END DO

! Zero out fields
  DO i = 1,n_profile
    ncldy(i)  = 0
  END DO ! i

  DO k = 1, tot_subcol_gen
    DO j = 1, n_layer
      DO i = 1, n_profile
!              cic_sub_full(i,j,k) = 0.0
        clw_sub_full(i,j,k) = 0.0
      END DO
    END DO
  END DO

! Set the overlap used in the cloud generator
!  ioverlap=sw_control(1)%i_overlap
  ioverlap=2

  CALL cld_generator(n_layer, 1, n_profile                         &
  , tot_subcol_gen, dp_corr_cloud, dp_corr_cond, sigma_qcw         &
  , w_cloud, c_cloud, c_ratio, ls_ratio, p, 1, n_profile)

  IF (ALLOCATED(rand_seed_y)) DEALLOCATE(rand_seed_y)     
  IF (ALLOCATED(rand_seed_x)) DEALLOCATE(rand_seed_x)     
  IF (ALLOCATED(temp_rand)) DEALLOCATE(temp_rand)     

  IF (rad_mcica_sampling /= ip_mcica_full_sampling) THEN
    DO i=1,n_profile
      frac_cloudy_full(i)=REAL(ncldy(i))/REAL(tot_subcol_gen)
    END DO
  ELSE IF (rad_mcica_sampling == ip_mcica_full_sampling) THEN
! In this case we treat the clear sub-columns as cloudy sub-columns
! so the clear-sky fraction is implicit in the summing of the 
! "cloudy" sub-columns
    DO i=1,n_profile
      frac_cloudy_full(i)=1.0
    END DO
  END IF

  IF (rad_mcica_sampling /= ip_mcica_full_sampling) THEN
! For the case where are less cloudy subcolumns than required, 
! copy cloudy values to ensure enough cloudy subcolumns
    DO i=1, n_profile
      IF (ncldy(i) < subcol_need .AND. ncldy(i) > 0) THEN
        DO j=ncldy(i)+1,MIN(subcol_need,tot_subcol_gen)
          DO k=1,n_layer
            l=j-ncldy(i)
            clw_sub_full(i,k,j)=clw_sub_full(i,k,l)
!                  cic_sub_full(i,k,j)=cic_sub_full(i,k,l)
          END DO
        END DO
      END IF
    END DO
  END IF

  IF (lhook) CALL dr_hook(RoutineName,zhook_out,zhook_handle)

END SUBROUTINE open_cloud_gen
