! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Set the variables in the Socrates boundary conditions type
!
!------------------------------------------------------------------------------
module socrates_set_bound
implicit none
character(len=*), parameter, private :: ModuleName = 'SOCRATES_SET_BOUND'
contains

subroutine set_bound(bound, control, dimen, spectrum, &
  n_profile, n_tile, &
  t_ground, cos_zenith_angle, solar_irrad, orog_corr, &
  l_grey_albedo, grey_albedo, albedo_diff, albedo_dir, &
  albedo_diff_1d, albedo_dir_1d, &
  frac_tile, t_tile, albedo_diff_tile, albedo_dir_tile, &
  frac_tile_1d, t_tile_1d, albedo_diff_tile_1d, albedo_dir_tile_1d, &
  l_debug, i_profile_debug)

use def_bound,    only: StrBound, allocate_bound
use def_control,  only: StrCtrl
use def_dimen,    only: StrDim
use def_spectrum, only: StrSpecData
use realtype_rd,  only: RealK
use rad_pcf,      only: &
  ip_solar, ip_infra_red, ip_surf_alb_diff, ip_surf_alb_dir

implicit none


! Boundary properties:
type(StrBound),    intent(out) :: bound

! Control options:
type(StrCtrl),     intent(in)  :: control

! Dimensions:
type(StrDim),      intent(in)  :: dimen

! Spectral data:
type(StrSpecData), intent(in)  :: spectrum

integer, intent(in) :: n_profile
!   Number of atmospheric profiles for radiation calculations
integer, intent(in), optional :: n_tile
!   Number of surface tiles for radiation calculations

real(RealK), intent(in), optional :: t_ground(n_profile)
!   Effective radiative temperature over whole grid-box
real(RealK), intent(in), optional :: cos_zenith_angle(n_profile)
!   Cosine of solar zenith angle
real(RealK), intent(in), optional :: solar_irrad(n_profile)
!   Solar irradiance at top-of-atmosphere (mean over timestep)
real(RealK), intent(in), optional :: orog_corr(n_profile)
!   Orographic correction factor

logical, intent(in), optional :: l_grey_albedo
!   Set a single grey albedo for the surface
real(RealK), intent(in), optional :: grey_albedo
!   Grey surface albedo

real(RealK), intent(in), dimension(:, :), optional :: &
  albedo_diff, albedo_dir
!   Diffuse, direct albedo (n_profile, n_band)

real(RealK), intent(in), dimension(:), optional :: &
  albedo_diff_1d, albedo_dir_1d
!   1d diffuse, direct albedo (n_band)

real(RealK), intent(in), dimension(:, :), optional :: &
  frac_tile, t_tile
!   Tile fraction, temperature (n_profile, n_tile)

real(RealK), intent(in), dimension(:, :, :), optional :: &
  albedo_diff_tile, albedo_dir_tile
!   Tile albedos (n_profile, n_tile, n_band)

real(RealK), intent(in), dimension(:), optional :: &
  frac_tile_1d, t_tile_1d
!   1d tile fraction, temperature (n_tile)

real(RealK), intent(in), dimension(:), optional :: &
  albedo_diff_tile_1d, albedo_dir_tile_1d
!   1d tile albedos (n_tile*n_band)

logical, intent(in), optional :: l_debug
integer, intent(in), optional :: i_profile_debug
!   Options for outputting debugging information

! Local variables.
integer :: l, ll, i_band, i_tile
logical :: l_grey_alb

! Allocate structure for the core radiation code interface
call allocate_bound(bound, dimen, spectrum)

if (present(l_grey_albedo)) then
  l_grey_alb = l_grey_albedo
else
  l_grey_alb = .false.
end if

! Surface albedo
if (l_grey_alb .and. present(grey_albedo)) then
  do i_band=1, spectrum%basic%n_band
    do l=1, n_profile
      bound%rho_alb(l, ip_surf_alb_diff, i_band) = grey_albedo
    end do
    do l=1, n_profile
      bound%rho_alb(l, ip_surf_alb_dir,  i_band) = grey_albedo
    end do
  end do
else
  if (present(albedo_diff)) then
    do i_band=1, spectrum%basic%n_band
      do l=1, n_profile
        bound%rho_alb(l, ip_surf_alb_diff, i_band) = albedo_diff(l, i_band)
      end do
    end do
  else if (present(albedo_diff_1d)) then
    do i_band=1, spectrum%basic%n_band
      do l=1, n_profile
        bound%rho_alb(l, ip_surf_alb_diff, i_band) = albedo_diff_1d(i_band)
      end do
    end do
  else
    do i_band=1, spectrum%basic%n_band
      do l=1, n_profile
        bound%rho_alb(l, ip_surf_alb_diff, i_band) = 0.0_RealK
      end do
    end do
  end if
  if (present(albedo_dir)) then
    do i_band=1, spectrum%basic%n_band
      do l=1, n_profile
        bound%rho_alb(l, ip_surf_alb_dir, i_band) = albedo_dir(l, i_band)
      end do
    end do
  else if (present(albedo_dir_1d)) then
    do i_band=1, spectrum%basic%n_band
      do l=1, n_profile
        bound%rho_alb(l, ip_surf_alb_dir, i_band) = albedo_dir_1d(i_band)
      end do
    end do
  else
    do i_band=1, spectrum%basic%n_band
      do l=1, n_profile
        bound%rho_alb(l, ip_surf_alb_dir, i_band) = 0.0_RealK
      end do
    end do
  end if
end if

! Surface temperature
if (present(t_ground)) then
  do l=1, n_profile
    bound%t_ground(l) = t_ground(l)
  end do
else
  do l=1, n_profile
    bound%t_ground(l) = 0.0_RealK
  end do
end if

bound%n_point_tile=0
bound%n_tile=1
if (control%l_tile .and. present(n_tile) .and. &
    (present(frac_tile) .or. present(frac_tile_1d))) then

  ! Set up the surface tiling variables
  ! Treat all points as tiled when l_tile is true
  bound%n_tile=n_tile
  bound%n_point_tile = n_profile
  do l=1, n_profile
    bound%list_tile(l) = l
  end do

  if (present(frac_tile)) then
    do i_tile=1, n_tile
      do l=1, n_profile
        bound%frac_tile(l, i_tile) = frac_tile(l, i_tile)
      end do
    end do
  else
    do i_tile=1, n_tile
      do l=1, n_profile
        bound%frac_tile(l, i_tile) = frac_tile_1d(i_tile)
      end do
    end do
  end if

  ! Diffuse tile albedos
  if (present(albedo_diff_tile).and..not.l_grey_alb) then
    do i_band=1, spectrum%basic%n_band
      do i_tile=1, n_tile
        do l=1, n_profile
          bound%rho_alb_tile(l, ip_surf_alb_diff, i_tile, i_band) &
            = albedo_diff_tile(l, i_tile, i_band)
        end do
      end do
    end do
    ! Ensure the total albedo is consistent with the tile albedos
    do i_band=1, spectrum%basic%n_band
      do l=1, n_profile
        bound%rho_alb(l, ip_surf_alb_diff, i_band) = 0.0_RealK
      end do
      do i_tile=1, n_tile
        do l=1, n_profile
          bound%rho_alb(l, ip_surf_alb_diff, i_band) &
            = bound%rho_alb(l, ip_surf_alb_diff, i_band) &
            + albedo_diff_tile(l, i_tile, i_band) * bound%frac_tile(l, i_tile)
        end do
      end do
    end do
  else if (present(albedo_diff_tile_1d).and..not.l_grey_alb) then
    do i_band=1, spectrum%basic%n_band
      do i_tile=1, n_tile
        do l=1, n_profile
          bound%rho_alb_tile(l, ip_surf_alb_diff, i_tile, i_band) &
            = albedo_diff_tile_1d( (i_band-1)*n_tile + i_tile )
        end do
      end do
    end do
    ! Ensure the total albedo is consistent with the tile albedos
    do i_band=1, spectrum%basic%n_band
      do l=1, n_profile
        bound%rho_alb(l, ip_surf_alb_diff, i_band) = 0.0_RealK
      end do
      do i_tile=1, n_tile
        do l=1, n_profile
          bound%rho_alb(l, ip_surf_alb_diff, i_band) &
            = bound%rho_alb(l, ip_surf_alb_diff, i_band) &
            + albedo_diff_tile_1d( (i_band-1)*n_tile + i_tile ) &
            * bound%frac_tile(l, i_tile)
        end do
      end do
    end do
  else
    ! When not present just use the gridbox mean diffuse albedo 
    do i_band=1, spectrum%basic%n_band
      do i_tile=1, n_tile
        do l=1, n_profile
          bound%rho_alb_tile(l, ip_surf_alb_diff, i_tile, i_band) &
            = bound%rho_alb(l, ip_surf_alb_diff, i_band)
        end do
      end do
    end do
  end if

  ! Direct tile albedos
  if (present(albedo_dir_tile).and..not.l_grey_alb) then
    do i_band=1, spectrum%basic%n_band
      do i_tile=1, n_tile
        do l=1, n_profile
          bound%rho_alb_tile(l, ip_surf_alb_dir, i_tile, i_band) &
            = albedo_dir_tile(l, i_tile, i_band)
        end do
      end do
    end do
    ! Ensure the total albedo is consistent with the tile albedos
    do i_band=1, spectrum%basic%n_band
      do l=1, n_profile
        bound%rho_alb(l, ip_surf_alb_dir, i_band) = 0.0_RealK
      end do
      do i_tile=1, n_tile
        do l=1, n_profile
          bound%rho_alb(l, ip_surf_alb_dir, i_band) &
            = bound%rho_alb(l, ip_surf_alb_dir, i_band) &
            + albedo_dir_tile(l, i_tile, i_band) * bound%frac_tile(l, i_tile)
        end do
      end do
    end do
  else if (present(albedo_dir_tile_1d).and..not.l_grey_alb) then
    do i_band=1, spectrum%basic%n_band
      do i_tile=1, n_tile
        do l=1, n_profile
          bound%rho_alb_tile(l, ip_surf_alb_dir, i_tile, i_band) &
            = albedo_dir_tile_1d( (i_band-1)*n_tile + i_tile )
        end do
      end do
    end do
    ! Ensure the total albedo is consistent with the tile albedos
    do i_band=1, spectrum%basic%n_band
      do l=1, n_profile
        bound%rho_alb(l, ip_surf_alb_dir, i_band) = 0.0_RealK
      end do
      do i_tile=1, n_tile
        do l=1, n_profile
          bound%rho_alb(l, ip_surf_alb_dir, i_band) &
            = bound%rho_alb(l, ip_surf_alb_dir, i_band) &
            + albedo_dir_tile_1d( (i_band-1)*n_tile + i_tile ) &
            * bound%frac_tile(l, i_tile)
        end do
      end do
    end do
  else
    ! When not present just use the gridbox mean direct albedo 
    do i_band=1, spectrum%basic%n_band
      do i_tile=1, n_tile
        do l=1, n_profile
          bound%rho_alb_tile(l, ip_surf_alb_dir, i_tile, i_band) &
            = bound%rho_alb(l, ip_surf_alb_dir, i_band)
        end do
      end do
    end do
  end if

  if (present(t_tile)) then
    ! Set the tile temperatures (t_ground will not be used on these points)
    do i_tile=1, n_tile
      do l=1, n_profile
        bound%t_tile(l, i_tile) = t_tile(l, i_tile)
      end do
    end do
  else if (present(t_tile_1d)) then
    ! Set the tile temperatures from 1d input
    do i_tile=1, n_tile
      do l=1, n_profile
        bound%t_tile(l, i_tile) = t_tile_1d(i_tile)
      end do
    end do
  else
    ! When not present just use the gridbox mean surface temperature
    do i_tile=1, n_tile
      do l=1, n_profile
        bound%t_tile(l, i_tile) = bound%t_ground(l)
      end do
    end do
  end if
end if


! Set the surface basis functions for a Lambertian surface.
bound%n_brdf_basis_fnc=1
! By defining F_{1,0,0,0} to be 4, rho_alb becomes equal to the diffuse albedo.
bound%f_brdf(1, 0, 0, 0)=4.0_RealK


! Orographic correction factor
if (present(orog_corr)) then
  do l=1, n_profile
    bound%orog_corr(l)=orog_corr(l)
  end do
end if


! Incident solar flux
if (present(cos_zenith_angle) .and. present(solar_irrad)) then
  do l=1, n_profile
    if (cos_zenith_angle(l) > 0.0_RealK) then
      bound%solar_irrad(l)=solar_irrad(l)
      bound%zen_0(l)=1.0_RealK/cos_zenith_angle(l)
    else
      bound%solar_irrad(l)=0.0_RealK
      bound%zen_0(l)=1.0_RealK
    end if
  end do
end if


if (present(l_debug)) then
  if (l_debug) then
    if (present(i_profile_debug)) then
      l = i_profile_debug
    else
      l = 1
    end if
    write(2000+l,'(A)') 'TEMP(K) SOLAR_IRRAD(WM-2) ZEN_0 OROG_CORR'
    write(2000+l,'(4(1pe16.8))') bound%t_ground(l), &
      bound%solar_irrad(l), bound%zen_0(l), bound%orog_corr(l)
    write(2000+l,'(A)') 'BAND DIFFUSE_ALBEDO DIRECT_ALBEDO'
    do i_band=1, spectrum%basic%n_band
      write(2000+l,'(i0, 2(1pe16.8))') i_band, &
        bound%rho_alb(l, ip_surf_alb_diff, i_band), &
        bound%rho_alb(l, ip_surf_alb_dir, i_band)
    end do
    if (control%l_tile .and. present(n_tile)) then
!      ll = findloc(bound%list_tile, l, 1)
      do ll=1, bound%n_point_tile
        if (bound%list_tile(ll) == l) then
          write(2000+l,'(A)') 'TILE FRAC TEMP(K)'
          do i_tile=1, n_tile
            write(2000+l,'(i0, 2(1pe16.8))') i_tile, &
              bound%frac_tile(ll, i_tile), bound%t_tile(ll, i_tile)
          end do
        end if
      end do
    end if
  end if
end if


end subroutine set_bound
end module socrates_set_bound
