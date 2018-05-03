! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
!  Subroutine to calculate two-stream coefficients in the regions.
!
! Method:
!   The coeffients for each region are determined and
!   averaged.
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: Radiance Core
!
!- ---------------------------------------------------------------------
SUBROUTINE two_coeff_region(ierr, control                               &
     , n_profile, n_layer, n_cloud_top                                  &
     , i_2stream, l_ir_source_quad, n_source_coeff                      &
     , n_cloud_type, frac_cloud                                         &
     , n_region, i_region_cloud, frac_region                            &
     , phase_fnc_clr, omega_clr                                         &
     , tau_clr_noscal, tau_clr                                          &
     , phase_fnc, omega                                                 &
     , tau_noscal, tau                                                  &
     , isolir, sec_0, sph                                               &
     , trans, reflect, trans_0_noscal, trans_0, source_coeff            &
     , nd_profile, nd_layer, nd_layer_clr, id_ct                        &
     , nd_max_order, nd_source_coeff                                    &
     , nd_cloud_type, nd_region                                         &
     )


  USE realtype_rd, ONLY: RealK
  USE def_spherical_geometry, ONLY: StrSphGeo
  USE rad_pcf
  USE yomhook, ONLY: lhook, dr_hook
  USE parkind1, ONLY: jprb, jpim
  USE def_control, ONLY: StrCtrl

  IMPLICIT NONE


! Control options:
  TYPE(StrCtrl),      INTENT(IN)    :: control

! Sizes of dummy arrays.
  INTEGER, INTENT(IN) ::                                                &
      nd_profile                                                        &
!       Size allocated for atmospheric profiles
    , nd_layer                                                          &
!       Size allocated for atmospheric layers
    , nd_layer_clr                                                      &
!       Size allocated for completely clear atmospheric layers
    , id_ct                                                             &
!       Topmost declared cloudy layer
    , nd_max_order                                                      &
!       Size allocated for orders of spherical harmonics
    , nd_source_coeff                                                   &
!       Size allocated for source coefficients
    , nd_cloud_type                                                     &
!       Maximum number of types of cloud
    , nd_region
!       Maximum number of cloudy regions


! Dummy arguments.
  INTEGER, INTENT(INOUT) ::                                             &
      ierr
!       Error flag
  INTEGER, INTENT(IN) ::                                                &
      n_profile                                                         &
!       Number of profiles
    , n_layer                                                           &
!       Number of layers
    , n_cloud_top                                                       &
!       Topmost cloudy layer
    , isolir                                                            &
!       Spectral region
    , n_cloud_type                                                      &
!       Number of types of clouds
    , i_2stream                                                         &
!       Two stream scheme
    , n_source_coeff
!       Number of source coefficients

  INTEGER, INTENT(IN) ::                                                &
      n_region                                                          &
!       Number of cloudy regions
    , i_region_cloud(nd_cloud_type)
!       Regions in which types of clouds fall

  LOGICAL, INTENT(IN) ::                                                &
      l_ir_source_quad
!       Use a quadratic source in the infra-red

! Optical properties of layer:
  REAL (RealK), INTENT(IN) ::                                           &
      frac_cloud(nd_profile, id_ct: nd_layer, nd_cloud_type)            &
!       Fractions of different types of clouds
    , frac_region(nd_profile, id_ct: nd_layer, nd_region)               &
!       Fractions of total cloud occupied by each region
    , phase_fnc_clr(nd_profile, nd_layer_clr, nd_max_order)             &
!       Phase function in clear-sky
    , omega_clr(nd_profile, nd_layer_clr)                               &
!       Clear-sky albedo of single scattering
    , tau_clr(nd_profile, nd_layer_clr)                                 &
!       Clear-sky optical depth
    , tau_clr_noscal(nd_profile, nd_layer_clr)                          &
!       Unscaled Clear-sky optical depth
    , tau(nd_profile, id_ct: nd_layer, 0: nd_cloud_type)                &
!       Optical depth
    , tau_noscal(nd_profile, id_ct: nd_layer, 0: nd_cloud_type)         &
!       Unscaled Optical depth
    , omega(nd_profile, id_ct: nd_layer, 0: nd_cloud_type)              &
!       Albedo of single scattering
    , phase_fnc(nd_profile, id_ct: nd_layer                             &
        , nd_max_order, 0: nd_cloud_type)
!       Phase function

! Solar beam
  REAL (RealK), INTENT(IN) ::                                           &
      sec_0(nd_profile)
!       Secant of zenith angle

  TYPE(StrSphGeo), INTENT(INOUT) :: sph
!       Spherical geometry fields


! Coefficients in the two-stream equations:
  REAL (RealK), INTENT(OUT) ::                                          &
      trans(nd_profile, nd_layer, nd_region)                            &
!       Diffuse transmission coefficient
    , reflect(nd_profile, nd_layer, nd_region)                          &
!       Diffuse reflection coefficient
    , trans_0(nd_profile, nd_layer, nd_region)                          &
!       Direct transmission coefficient
    , trans_0_noscal(nd_profile, nd_layer, nd_region)                   &
!       Unscaled Direct transmission coefficient
    , source_coeff(nd_profile, nd_layer                                 &
      , nd_source_coeff, nd_region)
!       Source coefficients in two-stream equations

! Local variables.
  INTEGER                                                               &
      i                                                                 &
!       Loop variable
    , j                                                                 &
!       Loop variable
    , k                                                                 &
!       Loop variable
    , l                                                                 &
!       Loop variable
    , i_region
!       Loop variable over regions

! Coefficients in the two-stream equations:
  REAL (RealK) ::                                                       &
      trans_temp(nd_profile, 1)                                         &
!       Temporary diffuse transmission coefficient
    , reflect_temp(nd_profile, 1)                                       &
!       Temporary diffuse reflection coefficient
    , trans_0_temp(nd_profile, 1)                                       &
!       Temporary direct transmission coefficient
    , trans_0_temp_noscal(nd_profile, 1)                                &
!       Unscaled Temporary direct transmission coefficient
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
    , tau_gathered_noscal(nd_profile, 1)                                &
!       Unscaled Gathered optical depth
    , omega_gathered(nd_profile, 1)                                     &
!       Gathered alebdo of single scattering
    , asymmetry_gathered(nd_profile, 1)                                 &
!       Gathered asymmetry
    , sec_0_gathered(nd_profile)                                        &
!       Gathered secant of the solar zenith angle
    , path_div_gathered(nd_profile, 1)                                  &
!       Gathered path scaling for calculating direct flux divergence
    , tmp_inv(nd_profile)
!       Temporary work array

  REAL (RealK), PARAMETER :: tiny_frac=TINY(frac_region)

  INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
  INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
  REAL(KIND=jprb)               :: zhook_handle

  CHARACTER(LEN=*), PARAMETER :: RoutineName='TWO_COEFF_REGION'


  IF (lhook) CALL dr_hook(RoutineName,zhook_in,zhook_handle)

! Determine the optical properties of the clear-sky regions of
! the layers.


! DEPENDS ON: two_coeff
  CALL two_coeff(ierr, control                                          &
    , n_profile, 1, n_cloud_top-1                                       &
    , i_2stream, l_ir_source_quad                                       &
    , phase_fnc_clr, omega_clr, tau_clr_noscal, tau_clr                 &
    , isolir, sec_0, sph%common%path_div                                &
    , trans(1, 1, ip_region_clear)                                      &
    , reflect(1, 1, ip_region_clear)                                    &
    , trans_0_noscal(1, 1, ip_region_clear)                             &
    , trans_0(1, 1, ip_region_clear)                                    &
    , source_coeff(1, 1, 1, ip_region_clear)                            &
    , nd_profile, 1, nd_layer_clr, 1, nd_layer, nd_source_coeff         &
    )
! DEPENDS ON: two_coeff
  CALL two_coeff(ierr, control                                          &
    , n_profile, n_cloud_top, n_layer                                   &
    , i_2stream, l_ir_source_quad                                       &
    , phase_fnc, omega, tau_noscal, tau                                 &
    , isolir, sec_0, sph%common%path_div                                &
    , trans(1, 1, ip_region_clear)                                      &
    , reflect(1, 1, ip_region_clear)                                    &
    , trans_0_noscal(1, 1, ip_region_clear)                             &
    , trans_0(1, 1, ip_region_clear)                                    &
    , source_coeff(1, 1, 1, ip_region_clear)                            &
    , nd_profile, id_ct, nd_layer, 1, nd_layer                          &
    , nd_source_coeff                                                   &
    )


! Now deal with clouds.

! Initialize the full arrays for cloudy regions.

  DO i_region=1, n_region
    IF (i_region /= ip_region_clear) THEN
      IF (isolir == ip_solar) THEN
        DO i=n_cloud_top, n_layer
          DO l=1, n_profile
            trans(l, i, i_region)=0.0e+00_RealK
            reflect(l, i, i_region)=0.0e+00_RealK
            trans_0(l, i, i_region)=0.0e+00_RealK
            trans_0_noscal(l, i, i_region)=0.0e+00_RealK
          END DO
        END DO
      ELSE
        DO i=n_cloud_top, n_layer
          DO l=1, n_profile
            trans(l, i, i_region)=0.0e+00_RealK
            reflect(l, i, i_region)=0.0e+00_RealK
          END DO
        END DO
      END IF
      DO j=1, n_source_coeff
        DO i=n_cloud_top, n_layer
          DO l=1, n_profile
            source_coeff(l, i, j, i_region)=0.0e+00_RealK
          END DO
        END DO
      END DO
    END IF
  END DO



! Consider each type of cloud in turn, checking which region it
! contrubutes to and form weighted sums of cloud properties.

  DO k=1, n_cloud_type


!   Set the region in which clouds of this type are included.
    i_region=i_region_cloud(k)

    DO i=n_cloud_top, n_layer

!     Form a list of points where cloud of this type exists
!     on this row for gathering.
      n_list=0
      DO l=1, n_profile
        IF (frac_cloud(l, i, k) > tiny_frac) THEN
          n_list=n_list+1
          l_list(n_list)=l
        END IF
      END DO


      IF (n_list >  0) THEN

!       Gather the optical properties. Though we consider only
!       one layer at a time the lower routines will operate on
!       arrays with vertical structure, so the gathered arrays
!       are two-dimensional, however, it is only necessary to
!       have one layer in the temporary arrays.

        DO l=1, n_list
          tau_gathered(l, 1)                                            &
            =tau(l_list(l), i, k)
          omega_gathered(l, 1)                                          &
            =omega(l_list(l), i, k)
          asymmetry_gathered(l, 1)                                      &
            =phase_fnc(l_list(l), i, 1, k)
        END DO
        IF (control%l_noscal_tau) THEN
          DO l=1, n_list
          tau_gathered_noscal(l, 1)                                     &
            =tau_noscal(l_list(l), i, k)
          END DO
        END IF
        IF (isolir == ip_solar) THEN
          IF (control%l_spherical_solar) THEN
            DO l=1, n_list
              path_div_gathered(l,1)=sph%common%path_div(l_list(l),i)
            END DO
          ELSE
            DO l=1, n_list
              sec_0_gathered(l)=sec_0(l_list(l))
            END DO
          END IF
        END IF


! DEPENDS ON: two_coeff
        CALL two_coeff(ierr, control                                    &
          , n_list, i, i                                                &
          , i_2stream, l_ir_source_quad                                 &
          , asymmetry_gathered, omega_gathered                          &
          , tau_gathered_noscal, tau_gathered                           &
          , isolir, sec_0_gathered, path_div_gathered                   &
          , trans_temp, reflect_temp                                    &
          , trans_0_temp_noscal, trans_0_temp                           &
          , source_coeff_temp                                           &
          , nd_profile, i, i, i, i, nd_source_coeff                     &
          )


!CDIR NODEP
        DO l=1, n_list
          ll=l_list(l)
          trans(ll, i, i_region)=trans(ll, i, i_region)                 &
            +frac_cloud(ll, i, k)*trans_temp(l, 1)
          reflect(ll, i, i_region)=reflect(ll, i, i_region)             &
            +frac_cloud(ll, i, k)*reflect_temp(l, 1)
        END DO
        DO j=1, n_source_coeff
!CDIR NODEP
          DO l=1, n_list
            ll=l_list(l)
            source_coeff(ll, i, j, i_region)                            &
              =source_coeff(ll, i, j, i_region)                         &
              +frac_cloud(ll, i, k)                                     &
              *source_coeff_temp(l, 1, j)
          END DO
        END DO
        IF (isolir == ip_solar) THEN
!CDIR NODEP
          DO l=1, n_list
            ll=l_list(l)
            trans_0(ll, i, i_region)=trans_0(ll, i, i_region)           &
              +frac_cloud(ll, i, k)*trans_0_temp(l, 1)
          END DO
!
          IF (control%l_noscal_tau) THEN
            DO l=1, n_list
              ll=l_list(l)
              trans_0_noscal(ll, i, i_region)                           &
                =trans_0_noscal(ll, i, i_region)                        &
                +frac_cloud(ll, i, k)*trans_0_temp_noscal(l, 1)
            END DO
          END IF
!
        END IF

      END IF

    END DO
  END DO


! Finally, scale the weighted sums by the cloud fractions.
  DO i_region=1, n_region
    IF (i_region /= ip_region_clear) THEN
      DO i=n_cloud_top, n_layer

!       Gather points within this region.
        n_list=0
        DO l=1,n_profile
          IF (frac_region(l, i, i_region) > tiny_frac) THEN
            n_list=n_list+1
            l_list(n_list)=l
          END IF
        END DO
        IF (isolir == ip_solar) THEN
!CDIR NODEP
          DO l=1, n_list
            ll=l_list(l)
            tmp_inv(l)=1.0_RealK/frac_region(ll, i, i_region)
            trans(ll, i, i_region)=trans(ll, i, i_region)               &
              *tmp_inv(l)
            reflect(ll, i, i_region)=reflect(ll, i, i_region)           &
              *tmp_inv(l)
            trans_0(ll, i, i_region)=trans_0(ll, i, i_region)           &
              *tmp_inv(l)
          END DO
!
          IF (control%l_noscal_tau) THEN
            DO l=1, n_list
              ll=l_list(l)
              trans_0_noscal(ll, i, i_region)                           &
                =trans_0_noscal(ll, i, i_region)*tmp_inv(l)
            END DO
          END IF
        ELSE
!CDIR NODEP
          DO l=1, n_list
            ll=l_list(l)
            tmp_inv(l)=1.0_RealK/frac_region(ll, i, i_region)
            trans(ll, i, i_region)=trans(ll, i, i_region)               &
              *tmp_inv(l)
            reflect(ll, i, i_region)=reflect(ll, i, i_region)           &
              *tmp_inv(l)
          END DO
        END IF
        DO j=1, n_source_coeff
!CDIR NODEP
          DO l=1, n_list
            ll=l_list(l)
            source_coeff(ll, i, j, i_region)                            &
              =source_coeff(ll, i, j, i_region)*tmp_inv(l)
          END DO
        END DO
      END DO
    END IF
  END DO


  IF (lhook) CALL dr_hook(RoutineName,zhook_out,zhook_handle)

END SUBROUTINE two_coeff_region
