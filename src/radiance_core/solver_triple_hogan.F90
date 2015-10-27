! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
!  Subroutine to solve for mixed fluxes scattering without a matrix.
!
! Method:
!   Gaussian elimination in an upward direction is employed to
!   determine effective albedos for lower levels of the atmosphere.
!   This allows a downward pass of back-substitution to be carried
!   out to determine the upward and downward fluxes.
!
!   This version has been modified by Robin Hogan to fix asymmetry
!   between stratiform and convective clouds, and to allow
!   shadowing, as documented in Shonk & Hogan, 2007, J. Climate.
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: Radiance Core
!
!- ---------------------------------------------------------------------
SUBROUTINE solver_triple_hogan(n_profile, n_layer, n_cloud_top          &
     , t, r, s_down, s_up                                               &
     , t_strat, r_strat, s_down_strat, s_up_strat                       &
     , t_conv, r_conv, s_down_conv, s_up_conv                           &
     , v11, v12, v13, v21, v22, v23, v31, v32, v33                      &
     , u11, u12, u13, u21, u22, u23, u31, u32, u33                      &
     , flux_inc_down                                                    &
     , source_ground_free, source_ground_strat                          &
     , source_ground_conv, albedo_surface_diff                          &
     , flux_total                                                       &
     , nd_profile, nd_layer, id_ct                                      &
     )


  USE realtype_rd, ONLY: RealK
  USE yomhook, ONLY: lhook, dr_hook
  USE parkind1, ONLY: jprb, jpim

  IMPLICIT NONE


! Sizes of dummy arrays.
  INTEGER, INTENT(IN) ::                                                &
      nd_profile                                                        &
!       Size allocated for atmospheric profiles
    , nd_layer                                                          &
!       Size allocated for atmospheric layers
    , id_ct
!       Topmost declared cloudy layer

! Dummy arguments.
  INTEGER, INTENT(IN) ::                                                &
      n_profile                                                         &
!       Number of profiles
    , n_layer                                                           &
!       Number of layers
    , n_cloud_top
!       Topmost cloudy layer
  REAL (RealK), INTENT(IN) ::                                           &
      t(nd_profile, nd_layer)                                           &
!       Clear-sky transmission
    , r(nd_profile, nd_layer)                                           &
!       Clear-sky reflection
    , s_down(nd_profile, nd_layer)                                      &
!       Clear-sky downward source function
    , s_up(nd_profile, nd_layer)                                        &
!       Clear-sky upward source function
    , t_strat(nd_profile, nd_layer)                                     &
!       Stratfiform transmission
    , r_strat(nd_profile, nd_layer)                                     &
!       Stratfiform reflection
    , s_down_strat(nd_profile, nd_layer)                                &
!       Downward stratfiform source function
    , s_up_strat(nd_profile, nd_layer)                                  &
!       Upward stratfiform source function
    , t_conv(nd_profile, nd_layer)                                      &
!       Convective transmission
    , r_conv(nd_profile, nd_layer)                                      &
!       Convective reflection
    , s_down_conv(nd_profile, nd_layer)                                 &
!       Downward convective source function
    , s_up_conv(nd_profile, nd_layer)
!       Upward convective source function
  REAL (RealK), INTENT(IN) ::                                           &
      v11(nd_profile, id_ct-1: nd_layer)                                &
!       Energy transfer coefficient for downward radiation
    , v12(nd_profile, id_ct-1: nd_layer)                                &
!       Energy transfer coefficient for downward radiation
    , v13(nd_profile, id_ct-1: nd_layer)                                &
!       Energy transfer coefficient for downward radiation
    , v21(nd_profile, id_ct-1: nd_layer)                                &
!       Energy transfer coefficient for downward radiation
    , v22(nd_profile, id_ct-1: nd_layer)                                &
!       Energy transfer coefficient for downward radiation
    , v23(nd_profile, id_ct-1: nd_layer)                                &
!       Energy transfer coefficient for downward radiation
    , v31(nd_profile, id_ct-1: nd_layer)                                &
!       Energy transfer coefficient for downward radiation
    , v32(nd_profile, id_ct-1: nd_layer)                                &
!       Energy transfer coefficient for downward radiation
    , v33(nd_profile, id_ct-1: nd_layer)
!       Energy transfer coefficient for downward radiation
  REAL (RealK) ::                                                       &
      u11(nd_profile, id_ct-1: nd_layer)                                &
!       Energy transfer coefficient for upward radiation
    , u12(nd_profile, id_ct-1: nd_layer)                                &
!       Energy transfer coefficient for upward radiation
    , u13(nd_profile, id_ct-1: nd_layer)                                &
!       Energy transfer coefficient for upward radiation
    , u21(nd_profile, id_ct-1: nd_layer)                                &
!       Energy transfer coefficient for upward radiation
    , u22(nd_profile, id_ct-1: nd_layer)                                &
!       Energy transfer coefficient for upward radiation
    , u23(nd_profile, id_ct-1: nd_layer)                                &
!       Energy transfer coefficient for upward radiation
    , u31(nd_profile, id_ct-1: nd_layer)                                &
!       Energy transfer coefficient for upward radiation
    , u32(nd_profile, id_ct-1: nd_layer)                                &
!       Energy transfer coefficient for upward radiation
    , u33(nd_profile, id_ct-1: nd_layer)
!       Energy transfer coefficient for upward radiation
  REAL (RealK), INTENT(IN) ::                                           &
      flux_inc_down(nd_profile)                                         &
!       Incident flux
    , source_ground_free(nd_profile)                                    &
!       Source from ground (clear sky)
    , source_ground_strat(nd_profile)                                   &
!       Source from ground (cloudy region)
    , source_ground_conv(nd_profile)                                    &
!       Source from ground (cloudy region)
    , albedo_surface_diff(nd_profile)
!       Diffuse albedo
  REAL (RealK), INTENT(OUT) ::                                          &
      flux_total(nd_profile, 2*nd_layer+2)
!       Total flux

! Local variables.
  INTEGER                                                               &
      i                                                                 &
!       Loop variable
    , l
!       Loop variable

! Effective coupling albedos and source functions:
  REAL (RealK) ::                                                       &
      alpha11(nd_profile, nd_layer+1)                                   &
    , alpha22(nd_profile, nd_layer+1)                                   &
    , alpha33(nd_profile, nd_layer+1)                                   &
    , g1(nd_profile, nd_layer+1)                                        &
    , g2(nd_profile, nd_layer+1)                                        &
    , g3(nd_profile, nd_layer+1)
! Terms for downward propagation:
  REAL (RealK) ::                                                       &
      gamma11(nd_profile, nd_layer)                                     &
    , gamma22(nd_profile, nd_layer)                                     &
    , gamma33(nd_profile, nd_layer)                                     &
    , beta11_inv(nd_profile, nd_layer)                                  &
    , beta22_inv(nd_profile, nd_layer)                                  &
    , beta33_inv(nd_profile, nd_layer)                                  &
    , h1(nd_profile, nd_layer)                                          &
    , h2(nd_profile, nd_layer)                                          &
    , h3(nd_profile, nd_layer)

! Auxilairy numerical variables required only in the current layer:
  REAL (RealK) ::                                                       &
      theta11                                                           &
    , theta22                                                           &
    , theta33                                                           &
    , lambda3                                                           &
    , lambda2                                                           &
    , lambda1                                                           &
    , lambda

! Temporary fluxes
  REAL (RealK) ::                                                       &
      flux_down_1(nd_profile, 0: nd_layer)                              &
!       Downward fluxes outside clouds just below i'th level
    , flux_down_2(nd_profile, 0: nd_layer)                              &
!       Downward fluxes inside clouds just below i'th level
    , flux_down_3(nd_profile, 0: nd_layer)                              &
!       Downward fluxes inside clouds just below i'th level
    , flux_up_1(nd_profile, 0: nd_layer)                                &
!       Upward fluxes outside clouds just above i'th level
    , flux_up_2(nd_profile, 0: nd_layer)                                &
!       Upward fluxes inside clouds just above i'th level
    , flux_up_3(nd_profile, 0: nd_layer)
!       Upward fluxes inside clouds just above i'th level

  INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
  INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
  REAL(KIND=jprb)               :: zhook_handle

  CHARACTER(LEN=*), PARAMETER :: RoutineName='SOLVER_TRIPLE_HOGAN'


  IF (lhook) CALL dr_hook(RoutineName,zhook_in,zhook_handle)

! This routine is specific to cases of three regions and it is
! assumed that 1 represents clear skies, 2 represents startiform
! clouds and 3 represents convective cloud.

! Initialize at the bottom of the column for upward elimination.
  DO l=1, n_profile
    alpha11(l, n_layer+1)=albedo_surface_diff(l)
    alpha22(l, n_layer+1)=albedo_surface_diff(l)
    alpha33(l, n_layer+1)=albedo_surface_diff(l)
    g1(l, n_layer+1)=source_ground_free(l)
    g2(l, n_layer+1)=source_ground_strat(l)
    g3(l, n_layer+1)=source_ground_conv(l)
  END DO

! Upward elimination through the cloudy layers.
  DO i=n_layer, n_cloud_top, -1
    DO l=1, n_profile

      theta11=alpha11(l, i+1)*v11(l, i)+alpha22(l, i+1)*v21(l, i)       &
        +alpha33(l, i+1)*v31(l, i)
      theta22=alpha11(l, i+1)*v12(l, i)+alpha22(l, i+1)*v22(l, i)       &
        +alpha33(l, i+1)*v32(l, i)
      theta33=alpha11(l, i+1)*v13(l, i)+alpha22(l, i+1)*v23(l, i)       &
        +alpha33(l, i+1)*v33(l, i)

      beta11_inv(l, i)=1.0e+00_RealK                                    &
        /(1.0e+00_RealK-theta11*r(l, i))
      gamma11(l, i)=theta11*t(l, i)
      h1(l, i)=g1(l, i+1)+theta11*s_down(l, i)

      beta22_inv(l, i)=1.0e+00_RealK                                    &
        /(1.0e+00_RealK-theta22*r_strat(l, i))
      gamma22(l, i)=theta22*t_strat(l, i)
      h2(l, i)=g2(l, i+1)+theta22*s_down_strat(l, i)

      beta33_inv(l, i)=1.0e+00_RealK                                    &
        /(1.0e+00_RealK-theta33*r_conv(l, i))
      gamma33(l, i)=theta33*t_conv(l, i)
      h3(l, i)=g3(l, i+1)+theta33*s_down_conv(l, i)

      lambda1 = s_up(l, i)+h1(l, i)*t(l, i)*beta11_inv(l, i)
      lambda2 = s_up_strat(l, i)+h2(l, i)*t_strat(l, i)                 &
        *beta22_inv(l, i)
      lambda3 = s_up_conv(l, i)+h3(l, i)*t_conv(l, i)                   &
        *beta33_inv(l, i)

      alpha11(l, i)=r(l, i)                                             &
        + theta11*t(l, i)*t(l, i)*beta11_inv(l, i)
      g1(l, i)=u11(l, i-1)*lambda1                                      &
        + u12(l, i-1)*lambda2 + u13(l, i-1)*lambda3

      alpha22(l, i)=r_strat(l, i)                                       &
        + theta22*t_strat(l, i)*t_strat(l, i)*beta22_inv(l, i)
      g2(l, i)=u21(l, i-1)*lambda1                                      &
        + u22(l, i-1)*lambda2 + u23(l, i-1)*lambda3

      alpha33(l, i)=r_conv(l, i)                                        &
        + theta33*t_conv(l, i)*t_conv(l, i)*beta33_inv(l, i)

      g3(l, i)=u31(l, i-1)*lambda1                                      &
        + u32(l, i-1)*lambda2 + u33(l, i-1)*lambda3

    END DO
  END DO

! The layer above the cloud, if the cloud does not reach to the top
! of the column: only one set of alphas is now needed.

  IF (n_cloud_top > 1) THEN

    i=n_cloud_top-1
    DO l=1, n_profile

      IF (n_cloud_top < n_layer) THEN
!       If there is no cloud in the column the V's will not be
!       assigned so an IF-test is required.
        theta11=alpha11(l, i+1)*v11(l, i)+alpha22(l, i+1)*v21(l, i)     &
           +alpha33(l, i+1)*v31(l, i)
      ELSE
        theta11=alpha11(l, i+1)
      END IF

      beta11_inv(l, i)=1.0e+00_RealK                                    &
        /(1.0e+00_RealK-theta11*r(l, i))
      gamma11(l, i)=theta11*t(l, i)
      h1(l, i)=g1(l, i+1)+theta11*s_down(l, i)

      lambda=t(l, i)*beta11_inv(l, i)
      alpha11(l, i)=r(l, i)+lambda*gamma11(l, i)
      g1(l, i)=s_up(l, i)+lambda*h1(l, i)

    END DO

  END IF


! Continue through the cloud free region: if there is no such
! region the DO-loop will not be executed.
  DO i=n_cloud_top-2, 1, -1
    DO l=1, n_profile

      beta11_inv(l, i)=1.0e+00_RealK                                    &
        /(1.0e+00_RealK-alpha11(l, i+1)*r(l, i))
      gamma11(l, i)=alpha11(l, i+1)*t(l, i)
      h1(l, i)=g1(l, i+1)+alpha11(l, i+1)*s_down(l, i)

      lambda1=t(l, i)*beta11_inv(l, i)
      alpha11(l, i)=r(l, i)+lambda1*gamma11(l, i)
      g1(l, i)=s_up(l, i)+lambda1*h1(l, i)

    END DO
  END DO


! Initialize for downward back-substitution.
  DO l=1, n_profile
    flux_total(l, 2)=flux_inc_down(l)
  END DO
  IF (n_cloud_top > 1) THEN
    DO l=1, n_profile
      flux_total(l, 1)=alpha11(l, 1)*flux_total(l, 2)+g1(l, 1)
    END DO
  ELSE
    DO l=1, n_profile
      flux_total(l, 1)=g1(l, 1)+flux_inc_down(l)                        &
        *(v11(l, 0)*alpha11(l, 1)+v21(l, 0)*alpha22(l, 1)               &
        + v31(l, 0)*alpha33(l, 1))
    END DO
  END IF

! Sweep downward through the clear-sky region, finding the downward
! flux at the top of the layer and the upward flux at the bottom.
  DO i=1, n_cloud_top-1
    DO l=1, n_profile
      flux_total(l, 2*i+1)=(gamma11(l, i)*flux_total(l, 2*i)            &
        +h1(l, i))*beta11_inv(l, i)
      flux_total(l, 2*i+2)=t(l, i)*flux_total(l, 2*i)                   &
        +r(l, i)*flux_total(l, 2*i+1)+s_down(l, i)
    END DO
  END DO

! Pass into the top cloudy layer. use flux_down_[1,2,3] to hold,
! provisionally, the downward fluxes just below the top of the
! layer, then calculate the upward fluxes at the bottom and
! finally the downward fluxes at the bottom of the layer.
  IF (n_cloud_top <= n_layer) THEN
!   If there are no clouds n_cloud_top may be out-of-bounds for
!   these arrays so an if test is required.
    i=n_cloud_top
    DO l=1, n_profile
      flux_down_1(l, i)=v11(l, i-1)*flux_total(l, 2*i)
      flux_down_2(l, i)=v21(l, i-1)*flux_total(l, 2*i)
      flux_down_3(l, i)=v31(l, i-1)*flux_total(l, 2*i)

      flux_up_1(l, i)=(gamma11(l, i)*flux_down_1(l, i)                  &
        +h1(l, i))*beta11_inv(l, i)
      flux_up_2(l, i)=(gamma22(l, i)*flux_down_2(l, i)                  &
        +h2(l, i))*beta22_inv(l, i)
      flux_up_3(l, i)=(gamma33(l, i)*flux_down_3(l, i)                  &
        +h3(l, i))*beta33_inv(l, i)

      flux_down_1(l, i)=t(l, i)*flux_down_1(l, i)                       &
        +r(l, i)*flux_up_1(l, i)+s_down(l, i)
      flux_down_2(l, i)=t_strat(l, i)*flux_down_2(l, i)                 &
        +r_strat(l, i)*flux_up_2(l, i)+s_down_strat(l, i)
      flux_down_3(l, i)=t_conv(l, i)*flux_down_3(l, i)                  &
        +r_conv(l, i)*flux_up_3(l, i)+s_down_conv(l, i)
    END DO
  END IF

! The main loop of back-substitution. the provisional use of the
! downward fluxes is as above.
  DO i=n_cloud_top+1, n_layer
    DO l=1, n_profile
      flux_down_1(l, i)=v11(l, i-1)*flux_down_1(l, i-1)                 &
        +v12(l, i-1)*flux_down_2(l, i-1)                                &
        +v13(l, i-1)*flux_down_3(l, i-1)
      flux_down_2(l, i)=v21(l, i-1)*flux_down_1(l, i-1)                 &
        +v22(l, i-1)*flux_down_2(l, i-1)                                &
        +v23(l, i-1)*flux_down_3(l, i-1)
      flux_down_3(l, i)=v31(l, i-1)*flux_down_1(l, i-1)                 &
        +v32(l, i-1)*flux_down_2(l, i-1)                                &
        +v33(l, i-1)*flux_down_3(l, i-1)

      flux_up_1(l, i)=(gamma11(l, i)*flux_down_1(l, i)                  &
        +h1(l, i))*beta11_inv(l, i)
      flux_up_2(l, i)=(gamma22(l, i)*flux_down_2(l, i)                  &
        +h2(l, i))*beta22_inv(l, i)
      flux_up_3(l, i)=(gamma33(l, i)*flux_down_3(l, i)                  &
        +h3(l, i))*beta33_inv(l, i)

      flux_down_1(l, i)=t(l, i)*flux_down_1(l, i)                       &
        +r(l, i)*flux_up_1(l, i)+s_down(l, i)
      flux_down_2(l, i)=t_strat(l, i)*flux_down_2(l, i)                 &
        +r_strat(l, i)*flux_up_2(l, i)+s_down_strat(l, i)
      flux_down_3(l, i)=t_conv(l, i)*flux_down_3(l, i)                  &
        +r_conv(l, i)*flux_up_3(l, i)+s_down_conv(l, i)
    END DO
  END DO


! Calculate the overall flux.
  DO i=n_cloud_top, n_layer
    DO l=1, n_profile
      flux_total(l, 2*i+1)=flux_up_1(l, i)+flux_up_2(l, i)              &
        +flux_up_3(l, i)
      flux_total(l, 2*i+2)=flux_down_1(l, i)+flux_down_2(l, i)          &
        +flux_down_3(l, i)
    END DO
  END DO


  IF (lhook) CALL dr_hook(RoutineName,zhook_out,zhook_handle)

END SUBROUTINE solver_triple_hogan
