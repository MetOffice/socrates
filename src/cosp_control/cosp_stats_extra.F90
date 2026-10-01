
MODULE mod_cosp_stats_extra
USE mod_cosp_config, ONLY: r_undef, r_ground
USE cosp_input_mod, ONLY: cosp_sr_cloud
USE cosp_kinds, ONLY: wp
USE yomhook, ONLY: lhook, dr_hook
USE parkind1, ONLY: jpim, jprb
IMPLICIT NONE

REAL(wp),PARAMETER :: R_LogUnitMin = -3000.0 ! Min value for log unit conversion
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='MOD_COSP_STATS'

CONTAINS

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!---------- SUBROUTINE COSP_GRIDBOX_MEAN ----------------
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
SUBROUTINE cosp_gridbox_mean(Npoints,Ncolumns,Nlevels,zhalf,zu,lstdgrid,x,r,   &
                             log_units,sensitivity)
IMPLICIT NONE
! Input arguments
INTEGER,INTENT(IN) :: Npoints  !# of grid points
INTEGER,INTENT(IN) :: Nlevels  !# of grid levels
INTEGER,INTENT(IN) :: Ncolumns !# of columns
REAL(wp), INTENT(IN) :: zhalf(Npoints) ! Height at bottom half level [m]
REAL(wp), INTENT(IN) :: zu(Nlevels)    ! Upper boundary of new levels  [m]
LOGICAL,INTENT(IN) :: lstdgrid ! Using standard grid (40 cloudsat levels)?
REAL(wp), INTENT(IN) :: x(Npoints,Ncolumns,Nlevels)
                                        !x -  Input variable to be averaged
LOGICAL,OPTIONAL,INTENT(IN) :: log_units ! Need to convert to linear units
REAL(wp),OPTIONAL,INTENT(IN) :: sensitivity ! Minimum value used
! Output
REAL(wp), INTENT(OUT) :: r(Npoints,Nlevels)
                                        ! Variable averaged over subcolumns
! Local variables
INTEGER :: i,j,k,n
LOGICAL :: lunits
REAL(wp) :: t,lsensitivity
CHARACTER (LEN=* ), PARAMETER :: RoutineName='COSP_GRIDBOX_MEAN'
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)
lunits=.FALSE.
IF (PRESENT(log_units)) lunits=log_units
lsensitivity=0.0
IF (PRESENT(sensitivity)) lsensitivity=sensitivity

r = 0.0
DO k=1,Nlevels
  DO i=1,Npoints
    IF (lstdgrid .AND. (zu(k) <= zhalf(i))) THEN
       ! Level of standard grid below model bottom level
      r(i,k) = r_undef
    ELSE
      n = 0 ! Counter of valid points
      IF (lunits) THEN
        DO j=1,Ncolumns
          t = x(i,j,k)
          IF (t /= r_undef) THEN
            IF (t >= R_LogUnitMin) THEN
              t = 10.0**(t/10.0)
            ELSE
              t = 0.0
            END IF
            n = n+1
          ELSE
            t = 0.0
          END IF
          r(i,k) = r(i,k) + t
        END DO
        ! Divide and output in linear units
        IF (n /=0 ) r(i,k) = r(i,k)/n
      ELSE ! lunits=.false
        DO j=1,Ncolumns
          t = x(i,j,k)
          IF (t /= r_undef) THEN
            IF (t >= lsensitivity) r(i,k) = r(i,k) + t
            n = n+1
          END IF
        END DO
        ! Compute mean
        IF (n /=0 ) THEN
          r(i,k) = r(i,k)/n
        END IF
      END IF ! LUNITS
    END IF
  END DO
END DO
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

END SUBROUTINE cosp_gridbox_mean


!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!---------- SUBROUTINE COSP_RADAR_AND_LIDAR_CLOUD_FRACTION ----------------
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
SUBROUTINE cosp_radar_and_lidar_cloud_fraction(Npoints,Ncolumns,Nlevels,       &
              atb_mol,atb_tot,ze_tot,cllidarradar)
IMPLICIT NONE
! Input arguments
INTEGER,INTENT(IN) :: Npoints  !# of grid points
INTEGER,INTENT(IN) :: Ncolumns !# of columns
INTEGER,INTENT(IN) :: Nlevels  !# of grid levels
REAL(wp),INTENT(IN) :: atb_mol(Npoints,Nlevels) ! Molecular backscatter
REAL(wp),INTENT(IN) :: atb_tot(Npoints,Ncolumns,Nlevels)
                                               ! Total backscattered signal
REAL(wp),INTENT(IN) :: ze_tot(Npoints,Ncolumns,Nlevels)
                                               ! Radar reflectivity
! Output arguments
REAL(wp),INTENT(OUT) :: cllidarradar(Npoints,Nlevels)

! Local variables
LOGICAL :: radar_detect ! Radar detects cloud in volume
LOGICAL :: lidar_detect ! Lidar detects cloud in volume
LOGICAL :: valid_volume ! Volume with non-missing data or attenuated
REAL(wp) :: lidar_sr ! Lidar scattering ratio
REAL(wp) :: sr_cld ! Threshold for lidar cloud detection
REAL(wp),PARAMETER :: sr_att = 0.01 ! Threshold for attenuation
REAL(wp),PARAMETER :: ze_cld = -30.0 ! Threshold for radar cloud detection
REAL(wp) :: Nvol ! Number of valid volumes
REAL(wp) :: Ncloud ! Number of cloudy volumes
INTEGER :: ip,ic,il ! Loop indices
CHARACTER (LEN=* ), PARAMETER :: RoutineName=                                  &
    'COSP_RADAR_AND_LIDAR_CLOUD_FRACTION'
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)
sr_cld = cosp_sr_cloud

DO il=1,Nlevels
  DO ip=1,Npoints
    Nvol = 0.0
    Ncloud = 0.0
    DO ic=1,Ncolumns
      ! Initialise volume logicals and scattering ratio
      radar_detect = .FALSE.
      lidar_detect = .FALSE.
      valid_volume = .FALSE.
      lidar_sr = r_undef
      ! Lidar detection
      IF (atb_mol(ip,il) > 0.0) THEN
        lidar_sr = atb_tot(ip,ic,il)/atb_mol(ip,il)
        IF (lidar_sr >= sr_att) THEN
          Nvol = Nvol + 1.0 ! Valid volume
          valid_volume = .TRUE.
          IF (lidar_sr >= sr_cld) lidar_detect = .TRUE.
        END IF
      END IF
      ! Radar detection
      IF ((ze_tot(ip,ic,il) /= r_undef) .AND.                                  &
          (ze_tot(ip,ic,il) /= r_ground)) THEN
        IF (.NOT. valid_volume) Nvol = Nvol + 1.0
        IF (ze_tot(ip,ic,il) >= ze_cld) radar_detect = .TRUE.
      END IF
      ! Accumulate cloudy volumes in layer
      IF (lidar_detect .OR. radar_detect) Ncloud = Ncloud + 1.0
    END DO
    ! Calculate cloud fraction
    IF (Nvol > 0.0) THEN
      cllidarradar(ip,il) = Ncloud/Nvol
    ELSE
      cllidarradar(ip,il) = r_undef
    END IF
  END DO
END DO
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)

END SUBROUTINE cosp_radar_and_lidar_cloud_fraction

END MODULE mod_cosp_stats_extra
