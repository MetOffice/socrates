! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Subroutine to solve for the solar transmission with no scattering.
!
! Method:
!   The Beer-Lambert law is used for transmission of direct flux with
!   scattering and surface reflection neglected.
!
!-----------------------------------------------------------------------
MODULE solar_no_scat_mod
IMPLICIT NONE
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName = 'SOLAR_NO_SCAT_MOD'
CONTAINS
SUBROUTINE solar_no_scat( &
  control, n_profile, n_layer, &
  l_scale_solar, adjust_solar_ke, &
  flux_inc_direct, sec_0, &
  sph, tau, &
  flux_direct, flux_total, &
  nd_profile, nd_layer)

  USE realtype_rd, ONLY: RealK
  USE def_control, ONLY: StrCtrl
  USE def_spherical_geometry, ONLY: StrSphGeo
  USE vectlib_mod, ONLY : exp_v
  USE yomhook, ONLY: lhook, dr_hook
  USE parkind1, ONLY: jprb, jpim

  IMPLICIT NONE


! Control options:
  TYPE(StrCtrl), INTENT(IN) :: control

! Sizes of dummy arrays.
  INTEGER, INTENT(IN) :: &
      nd_profile &
!       Size allocated for profiles
    , nd_layer
!       Size allocated for layers

! Dummy variables.
  INTEGER, INTENT(IN) :: &
      n_profile &
!       Number of profiles
    , n_layer
!       Number of layers
  LOGICAL, INTENT(IN) :: &
      l_scale_solar
!       Scaling applied to solar flux
  REAL (RealK), INTENT(IN) :: &
      tau(nd_profile, nd_layer) &
!       Optical depth
    , sec_0(nd_profile) &
!       Secants of solar zenith angles
    , flux_inc_direct(nd_profile) &
!       Incident direct flux
    , adjust_solar_ke(nd_profile, nd_layer)
!       Adjustment of solar beam with equivalent extinction
  REAL (RealK), INTENT(OUT) :: &
      flux_direct(nd_profile, 0: nd_layer) &
!       Direct flux
    , flux_total(nd_profile, 2*nd_layer+2)
!       Total fluxes
  TYPE(StrSphGeo), INTENT(INOUT) :: sph
!       Spherical geometry fields


! Local variables.
  INTEGER :: i, l
!       Loop variables
  REAL (RealK) :: trans_0(nd_profile, nd_layer)
!       Direct transmittance
  REAL (RealK) :: temp(nd_profile)
!       Working variable to optimise exponential

  INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
  INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
  REAL(KIND=jprb)               :: zhook_handle
  CHARACTER (LEN=*), PARAMETER  :: RoutineName = 'SOLAR_NO_SCAT'


  IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

  IF (control%l_spherical_solar) THEN
    ! Spherical shell geometry
    DO i=1, n_layer
      ! Calculate the direct transmission coefficient
      DO l=1, n_profile
        temp(l) = -tau(l,i)*sph%common%path_div(l,i)
      END DO
      CALL exp_v(n_profile,temp,trans_0(1,i))
    END DO
    IF (l_scale_solar) THEN
      ! The solar flux may be multiplied by a scaling factor if an
      ! equivalent extinction is used.
      DO i=0, n_layer+1
        DO l=1, n_profile
          ! Direct flux arriving at each layer through spherical shells
          sph%allsky%flux_direct(l, i) = sph%common%flux_inc_direct(l, i) &
            * sph%allsky%trans_0(l, i) * sph%common%adjust_solar_ke(l, i)
        END DO
      END DO
      DO i=1, n_layer
        DO l=1, n_profile
          ! Direct flux divergence across the layer
          sph%allsky%flux_direct_div(l, i) = sph%allsky%flux_direct(l, i) &
            * ( 1.0_RealK - trans_0(l, i)*adjust_solar_ke(l, i) )
          ! Convert flux to Watts per square metre normal to the beam
          ! for diagnostic output
          sph%allsky%flux_direct(l, i) &
            = sph%allsky%flux_direct(l, i) * sph%common%path_div(l, i)
        END DO
      END DO
    ELSE
      DO i=0, n_layer+1
        DO l=1, n_profile
          sph%allsky%flux_direct(l, i) = sph%common%flux_inc_direct(l, i) &
            * sph%allsky%trans_0(l, i)
        END DO
      END DO
      DO i=1, n_layer
        DO l=1, n_profile
          sph%allsky%flux_direct_div(l, i) = sph%allsky%flux_direct(l, i) &
            * ( 1.0_RealK - trans_0(l, i) )
          sph%allsky%flux_direct(l, i) &
            = sph%allsky%flux_direct(l, i) * sph%common%path_div(l, i)
        END DO
      END DO
    END IF
    DO i=1, 2*n_layer+2
      DO l=1, n_profile
        ! No scattering or surface reflection so diffuse fluxes are zero
        flux_total(l, i) = 0.0_RealK
      END DO
    END DO
  ELSE
    ! Plane parallel geometry
    DO i=1, n_layer
      DO l=1, n_profile
        temp(l) = -tau(l,i)*sec_0(l)
      END DO
      CALL exp_v(n_profile,temp,trans_0(1,i))
    END DO
    DO l=1, n_profile
      flux_direct(l, 0)=flux_inc_direct(l)
    END DO
    IF (l_scale_solar) THEN
      DO i=1, n_layer
        DO l=1, n_profile
          flux_direct(l, i) = flux_direct(l, i-1) * trans_0(l, i) &
            * adjust_solar_ke(l, i)
        END DO
      END DO    
    ELSE
      DO i=1, n_layer
        DO l=1, n_profile
          flux_direct(l, i) = flux_direct(l, i-1) * trans_0(l, i)
        END DO
      END DO
    END IF
    DO i=0, n_layer
      DO l=1, n_profile
        ! Upward fluxes are zero
        flux_total(l, 2*i+1) = 0.0_RealK
        ! Total downward flux contains direct component for plane-parallel
        flux_total(l, 2*i+2) = flux_direct(l, i)
      END DO
    END DO
  END IF
  
  IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

END SUBROUTINE solar_no_scat
END MODULE solar_no_scat_mod
