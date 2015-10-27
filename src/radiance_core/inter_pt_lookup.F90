! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Calculate pressure and temperature interpolation factor for k-terms
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: Radiation Control
!
!-----------------------------------------------------------------------

SUBROUTINE inter_pt_lookup(nd_profile, nd_layer, nd_pre, nd_tmp         &
     , n_profile, n_layer, p, t, p_lookup, t_lookup                     &
     , fac00, fac01, fac10, fac11, jp, jt, jtt )

  USE realtype_rd, ONLY: RealK
  USE yomhook, ONLY: lhook, dr_hook
  USE parkind1, ONLY: jprb, jpim

  IMPLICIT NONE


  INTEGER, INTENT(IN) ::                                                &
       nd_profile                                                       &
!       Max number of profile
     , nd_layer                                                         &
!       Max number of layer
     , nd_pre                                                           &
!       Number of lookup pressures
     , nd_tmp                                                           &
!       Number of lookup temperatures
     , n_layer                                                          &
!       Number of layers
     , n_profile
!       Number of profiles

  REAL (RealK), INTENT(IN) ::                                           &
       p(nd_profile, nd_layer)                                          &
!          Actual pressure
     , t(nd_profile, nd_layer)                                          &
!          Layer temperature
     , p_lookup(nd_pre)                                                 &
!          Lookup table pressures
     , t_lookup(nd_tmp, nd_pre)
!          Lookup table temperatures


  REAL (RealK), INTENT(OUT) ::                                          &
       fac00(nd_profile, nd_layer)                                      &
     , fac01(nd_profile, nd_layer)                                      &
     , fac10(nd_profile, nd_layer)                                      &
     , fac11(nd_profile, nd_layer)
!          Multiplication factors for P & T interpolation

  INTEGER, INTENT(OUT) ::                                               &
       jp(nd_profile, nd_layer)                                         &
!       Index of reference pressure level such that the actual
!       pressure is between JP and JP+1
     , jt(nd_profile, nd_layer)                                         &
!       Index of reference temperature at level I such that the actual
!       temperature is between JT and JT+1
     , jtt(nd_profile, nd_layer)
!       Index of reference temperature at level I+1 such that the actual
!       temperature is between JTT and JTT+1

! Local variables
  REAL (RealK) ::                                                       &
       fp, ft, ftt, compfp                                              &
!          Fraction factor for P & T interpolation
     , p_layer, t_layer                                                 &
!          Inter-medium variable
     , p_lookup_min, p_lookup_max
!          Minimum and maximum values for look-up pressure

  INTEGER ::                                                            &
       i, l
!       Vertical and horizontal loop index

  REAL, PARAMETER :: eps = EPSILON(1.0_RealK)

  INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
  INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
  REAL(KIND=jprb)               :: zhook_handle

  CHARACTER(LEN=*), PARAMETER :: RoutineName='INTER_PT_LOOKUP'

  IF (lhook) CALL dr_hook(RoutineName,zhook_in,zhook_handle)

  p_lookup_min = p_lookup(1)      + MAX(ABS(p_lookup(1)     )*eps, eps)
  p_lookup_max = p_lookup(nd_pre) - MAX(ABS(p_lookup(nd_pre))*eps, eps)

  DO i=1, n_layer
    DO l=1, n_profile
!     Find the reference pressure on the lower side of the layer
!     pressure and store in JP. Store in FP the fraction
!     of the difference (in ln(pressure)) between JP and JP+1
!     that the layer pressure lies for gas absorption
!     coefficient interpolation.
      p_layer = MIN( MAX( LOG(p(l,i)), p_lookup_min ), p_lookup_max )

      jp(l,i) = MINLOC( p_layer - p_lookup, 1, &
                        p_layer - p_lookup >= 0.0_RealK)

      fp = ( p_layer             - p_lookup(jp(l,i)) ) &
         / ( p_lookup(jp(l,i)+1) - p_lookup(jp(l,i)) )

!     Find the reference temperature on the lower side of the
!     layer temperature for each of the layers JP and JP+1. Store
!     these indices in JT and JTT, resp.
!     Store in FT (resp. FTT) the fraction of the way between JT
!     (JTT) and the next highest reference temperature that the
!     layer temperature falls.
      t_layer=MIN( MAX( t(l,i), t_lookup(1,jp(l,i))*(1.0_RealK+eps) ), &
                   t_lookup(nd_tmp,jp(l,i))*(1.0_RealK-eps) )

      jt(l,i) = MINLOC( t_layer - t_lookup(:,jp(l,i)), 1, &
                        t_layer - t_lookup(:,jp(l,i)) >= 0.0_RealK)

      ft = ( t_layer                     - t_lookup(jt(l,i),jp(l,i)) ) &
         / ( t_lookup(jt(l,i)+1,jp(l,i)) - t_lookup(jt(l,i),jp(l,i)) )

      t_layer=MIN( MAX( t(l,i), t_lookup(1,jp(l,i)+1)*(1.0_RealK+eps) ), &
                   t_lookup(nd_tmp,jp(l,i)+1)*(1.0_RealK-eps) )

      jtt(l,i) = MINLOC( t_layer - t_lookup(:,jp(l,i)+1), 1, &
                         t_layer - t_lookup(:,jp(l,i)+1) >= 0.0_RealK)

      ftt=(t_layer                       -t_lookup(jtt(l,i),jp(l,i)+1)) &
        / (t_lookup(jtt(l,i)+1,jp(l,i)+1)-t_lookup(jtt(l,i),jp(l,i)+1))

!     Multiply the pressure fraction with the appropriate temperature
!     fraction for use in the interpolations
      compfp=1.0_RealK-fp
      fac00(l,i)=compfp*(1.0_RealK-ft)
      fac10(l,i)=fp*(1.0_RealK-ftt)
      fac01(l,i)=compfp*ft
      fac11(l,i)=fp*ftt
    END DO
  END DO

  IF (lhook) CALL dr_hook(RoutineName,zhook_out,zhook_handle)
END SUBROUTINE inter_pt_lookup
