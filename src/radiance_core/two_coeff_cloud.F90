! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
!  Subroutine to calculate cloudy two-stream coefficients.
!
! Method:
!   The coeffients for each type of cloud are determined and
!   averaged.
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: Radiance Core
!
!- ---------------------------------------------------------------------
SUBROUTINE two_coeff_cloud(ierr                                         &
     , n_profile, i_layer_first, i_layer_last                           &
     , i_2stream, l_ir_source_quad, n_source_coeff                      &
     , n_cloud_type, frac_cloud                                         &
     , phase_fnc_cloud, omega_cloud, tau_cloud                          &
     , isolir, sec_0                                                    &
     , trans_cloud, reflect_cloud, trans_0_cloud                        &
     , source_coeff_cloud                                               &
     , nd_profile, nd_layer, id_ct, nd_max_order                        &
     , nd_source_coeff, nd_cloud_type                                   &
     )


  USE realtype_rd, ONLY: RealK
  USE rad_pcf
  USE yomhook, ONLY: lhook, dr_hook
  USE parkind1, ONLY: jprb, jpim

  IMPLICIT NONE


! Sizes of dummy arrays.
  INTEGER, INTENT(IN) ::                                                &
      nd_profile                                                        &
!       Maximum number of profiles
    , nd_layer                                                          &
!       Maximum number of layers
    , id_ct                                                             &
!       Topmost declared potentially cloudy layer
    , nd_max_order                                                      &
!       Size allocated for orders of spherical harmonics
    , nd_source_coeff                                                   &
!       Size allocated for source coefficients
    , nd_cloud_type
!       Maximum number of types of cloud


! Dummy arguments.
  INTEGER, INTENT(INOUT) ::                                             &
      ierr
!       Error flag
  INTEGER, INTENT(IN) ::                                                &
      n_profile                                                         &
!       Number of profiles
    , i_layer_first                                                     &
!       First layer to consider
    , i_layer_last                                                      &
!       Last layer to consider
    , isolir                                                            &
!       Spectral region
    , n_cloud_type                                                      &
!       Number of types of clouds
    , i_2stream                                                         &
!       Two stream scheme
    , n_source_coeff
!       Number of source coefficients

  LOGICAL, INTENT(IN) ::                                                &
      l_ir_source_quad
!       Use a quadratic source in the infra-red

! Optical properties of layer:
  REAL (RealK), INTENT(IN) ::                                           &
      frac_cloud(nd_profile, id_ct: nd_layer, nd_cloud_type)            &
!       Fractions of different types of clouds
    , phase_fnc_cloud(nd_profile, id_ct: nd_layer                       &
        , nd_max_order, nd_cloud_type)                                  &
!       Phase functions
    , omega_cloud(nd_profile, id_ct: nd_layer, nd_cloud_type)           &
!       Albedo of single scattering
    , tau_cloud(nd_profile, id_ct: nd_layer, nd_cloud_type)
!       Optical depth

! Solar beam
  REAL (RealK), INTENT(IN) ::                                           &
      sec_0(nd_profile)
!       Secant of zenith angle


! Coefficients in the two-stream equations:
  REAL (RealK), INTENT(OUT) ::                                          &
      trans_cloud(nd_profile, nd_layer)                                 &
!       Mean diffuse transmission coefficient
    , reflect_cloud(nd_profile, nd_layer)                               &
!       Mean diffuse reflection coefficient
    , trans_0_cloud(nd_profile, nd_layer)                               &
!       Mean direct transmission coefficient
    , source_coeff_cloud(nd_profile, nd_layer, nd_source_coeff)
!       Mean source coefficients in two-stream equations


! Local variables.
  INTEGER                                                               &
      i                                                                 &
!       Loop variable
    , j                                                                 &
!       Loop variable
    , k                                                                 &
!       Loop variable
    , l
!       Loop variable

! Coefficients in the two-stream equations:
  REAL (RealK) ::                                                       &
      trans_temp(nd_profile, 1)                                         &
!       Temporary diffuse transmission coefficient
    , reflect_temp(nd_profile, 1)                                       &
!       Temporary diffuse reflection coefficient
    , trans_0_temp(nd_profile, 1)                                       &
!       Temporary direct transmission coefficient
    , source_coeff_temp(nd_profile, 1, nd_source_coeff)
!       Temporary source coefficients in two-stream equations

! Variables for gathering:
  INTEGER                                                               &
      n_list                                                            &
!       Number of points in list
    , l_list(nd_profile)                                                &
!       List of collected points
    , ll
!       Loop variable
  REAL (RealK) ::                                                       &
      tau_gathered(nd_profile, 1)                                       &
!       Gathered optical depth
    , omega_gathered(nd_profile, 1)                                     &
!       Gathered alebdo of single scattering
    , asymmetry_gathered(nd_profile, 1)                                 &
!       Gathered asymmetry
    , sec_0_gathered(nd_profile)
!       Gathered asymmetry

  INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
  INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
  REAL(KIND=jprb)               :: zhook_handle

  CHARACTER(LEN=*), PARAMETER :: RoutineName='TWO_COEFF_CLOUD'


  IF (lhook) CALL dr_hook(RoutineName,zhook_in,zhook_handle)

! Initialize the full arrays.
  DO i=i_layer_first, i_layer_last
    DO l=1, n_profile
      trans_cloud(l, i)=0.0e+00_RealK
      reflect_cloud(l, i)=0.0e+00_RealK
    END DO
  END DO
  DO j=1, n_source_coeff
    DO i=i_layer_first, i_layer_last
      DO l=1, n_profile
        source_coeff_cloud(l, i, j)=0.0e+00_RealK
      END DO
    END DO
  END DO

  IF (isolir == ip_solar) THEN
    DO i=i_layer_first, i_layer_last
      DO l=1, n_profile
        trans_0_cloud(l, i)=0.0e+00_RealK
      END DO
    END DO
  END IF


! Calculate the transmission and reflection coefficients for
! each type of cloud and increment the totals, weighting with
! the cloud fraction.

  DO k=1, n_cloud_type

    DO i=i_layer_first, i_layer_last

!     Determine where cloud of the current type exists
!     in this row and gather the points.
      n_list=0
      DO l=1, n_profile
        IF (frac_cloud(l, i, k) >  0.0e+00_RealK) THEN
          n_list=n_list+1
          l_list(n_list)=l
        END IF
      END DO


      IF (n_list >  0) THEN

!       Gather the optical properties.
!       Here we must consider one layer at a time. To reduce
!       storage the temporary arrays are only one layer thick,
!       but they will be past to the subroutine where they
!       will be declared as running from the Ith to the Ith layer
!       to make the code more readable at the lower level.

        DO l=1, n_list
          tau_gathered(l, 1)                                            &
            =tau_cloud(l_list(l), i, k)
          omega_gathered(l, 1)                                          &
            =omega_cloud(l_list(l), i, k)
          asymmetry_gathered(l, 1)                                      &
            =phase_fnc_cloud(l_list(l), i, 1, k)
        END DO
        IF (isolir == ip_solar) THEN
          DO l=1, n_list
            sec_0_gathered(l)=sec_0(l_list(l))
          END DO
        END IF


! DEPENDS ON: two_coeff
        CALL two_coeff(ierr                                             &
          , n_list, i, i                                                &
          , i_2stream, l_ir_source_quad                                 &
          , asymmetry_gathered, omega_gathered                          &
          , tau_gathered                                                &
          , isolir, sec_0_gathered                                      &
          , trans_temp, reflect_temp, trans_0_temp                      &
          , source_coeff_temp                                           &
          , nd_profile, i, i, i, i, nd_source_coeff                     &
          )

        DO l=1, n_list
          ll=l_list(l)
          trans_cloud(ll, i)=trans_cloud(ll, i)                         &
            +frac_cloud(ll, i, k)*trans_temp(l, 1)
          reflect_cloud(ll, i)=reflect_cloud(ll, i)                     &
            +frac_cloud(ll, i, k)*reflect_temp(l, 1)
        END DO
        DO j=1, n_source_coeff
          DO l=1, n_list
            ll=l_list(l)
            source_coeff_cloud(ll, i, j)                                &
              =source_coeff_cloud(ll, i, j)                             &
              +frac_cloud(ll, i, k)*source_coeff_temp(l, 1, j)
          END DO
        END DO
        IF (isolir == ip_solar) THEN
          DO l=1, n_list
            ll=l_list(l)
            trans_0_cloud(ll, i)=trans_0_cloud(ll, i)                   &
               +frac_cloud(ll, i, k)*trans_0_temp(l, 1)
          END DO
        END IF
      END IF

    END DO
  END DO


  IF (lhook) CALL dr_hook(RoutineName,zhook_out,zhook_handle)

END SUBROUTINE two_coeff_cloud
