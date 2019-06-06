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
  frac_tile, t_tile, albedo_diff_tile, albedo_dir_tile, &
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

real(RealK), intent(in), optional :: &
  albedo_diff(n_profile, spectrum%dim%nd_band), &
  albedo_dir(n_profile, spectrum%dim%nd_band)

real(RealK), intent(in), optional :: &
  frac_tile(n_profile, dimen%nd_tile), &
  albedo_diff_tile(n_profile, dimen%nd_tile, spectrum%dim%nd_band), &
  albedo_dir_tile(n_profile, dimen%nd_tile, spectrum%dim%nd_band), &
  t_tile(n_profile, dimen%nd_tile)

logical, intent(in), optional :: l_debug
integer, intent(in), optional :: i_profile_debug
!   Options for outputting debugging information

! Local variables.
integer :: l, ll, i_band, i_tile

real(RealK) :: frac_tile_tolerance = 1.0_RealK - epsilon(1.0_RealK)


! Allocate structure for the core radiation code interface
call allocate_bound(bound, dimen, spectrum)

! Surface albedo
if (l_grey_albedo) then
  bound%rho_alb(1:n_profile, ip_surf_alb_diff, 1:spectrum%basic%n_band) &
    = grey_albedo
  bound%rho_alb(1:n_profile, ip_surf_alb_dir,  1:spectrum%basic%n_band) &
    = grey_albedo
else
  if (present(albedo_diff)) then
    bound%rho_alb(1:n_profile, ip_surf_alb_diff, 1:spectrum%basic%n_band) &
      = albedo_diff(1:n_profile, 1:spectrum%basic%n_band)
  else
    bound%rho_alb(1:n_profile, ip_surf_alb_diff, 1:spectrum%basic%n_band) &
      = 0.0_RealK
  end if
  if (present(albedo_dir)) then
    bound%rho_alb(1:n_profile, ip_surf_alb_dir,  1:spectrum%basic%n_band) &
      = albedo_dir(1:n_profile, 1:spectrum%basic%n_band)
  else
    bound%rho_alb(1:n_profile, ip_surf_alb_dir,  1:spectrum%basic%n_band) &
      = 0.0_RealK
  end if
end if

! Surface temperature
if (present(t_ground)) then
  bound%t_ground(1:n_profile) = t_ground(1:n_profile)
else
  bound%t_ground(1:n_profile) = 0.0_RealK
end if

bound%n_point_tile=0
bound%n_tile=1
if (control%l_tile .and. present(n_tile) .and. present(frac_tile)) then
  ! Set up the surface tiling variables.
  bound%n_tile=n_tile
  do l=1, n_profile
    if (maxval(frac_tile(l, 1:n_tile)) < frac_tile_tolerance) then
      ! More than one tile present
      bound%n_point_tile = bound%n_point_tile + 1
      bound%list_tile(bound%n_point_tile) = l
      bound%frac_tile(bound%n_point_tile, 1:n_tile) = frac_tile(l, 1:n_tile)
      if (present(albedo_diff_tile).and..not.l_grey_albedo) then
        ! Set the diffuse tile albedos
        bound%rho_alb_tile(bound%n_point_tile, ip_surf_alb_diff, 1:n_tile, &
                           1:spectrum%basic%n_band) &
          = albedo_diff_tile(l, 1:n_tile, 1:spectrum%basic%n_band)
        ! Ensure the total albedo is consistent with the tile albedos
        do i_band=1, spectrum%basic%n_band
          bound%rho_alb(l, ip_surf_alb_diff, i_band) &
            = sum(albedo_diff_tile(l, 1:n_tile, i_band)*frac_tile(l, 1:n_tile))
        end do
      else
        ! When not present just use the gridbox mean diffuse albedo 
        do i_band=1, spectrum%basic%n_band
          bound%rho_alb_tile(bound%n_point_tile, ip_surf_alb_diff, &
                             1:n_tile, i_band) &
            = bound%rho_alb(l, ip_surf_alb_diff, i_band)
        end do
      end if
      if (present(albedo_dir_tile).and..not.l_grey_albedo) then
        ! Set the direct tile albedos
        bound%rho_alb_tile(bound%n_point_tile, ip_surf_alb_dir, 1:n_tile, &
                           1:spectrum%basic%n_band) &
          = albedo_dir_tile(l, 1:n_tile, 1:spectrum%basic%n_band)
        ! Ensure the total albedo is consistent with the tile albedos
        do i_band=1, spectrum%basic%n_band
          bound%rho_alb(l, ip_surf_alb_dir, i_band) &
            = sum(albedo_dir_tile(l, 1:n_tile, i_band)*frac_tile(l, 1:n_tile))
        end do
      else
        ! When not present just use the gridbox mean direct albedo 
        do i_band=1, spectrum%basic%n_band
          bound%rho_alb_tile(bound%n_point_tile, ip_surf_alb_dir, &
                             1:n_tile, i_band) &
            = bound%rho_alb(l, ip_surf_alb_dir, i_band)
        end do
      end if
      if (present(t_tile)) then
        ! Set the tile temperatures (t_ground will not be used on these points)
        bound%t_tile(bound%n_point_tile, 1:n_tile) &
          = t_tile(l, 1:n_tile)
      else
        ! When not present just use the gridbox mean surface temperature
        bound%t_tile(bound%n_point_tile, 1:n_tile) &
          = bound%t_ground(l)
      end if
    else
      ! One tile has a fraction of 1.0 so consider point to be untiled
      i_tile = maxloc(frac_tile(l, 1:n_tile), 1)
      ! If present use the tile albedos and surface temperature.
      ! Otherwise the gridbox mean values (already set) will be used
      if (present(albedo_diff_tile).and..not.l_grey_albedo) then
        bound%rho_alb(l, ip_surf_alb_diff, 1:spectrum%basic%n_band) &
          = albedo_diff_tile(l, i_tile, 1:spectrum%basic%n_band)
      end if
      if (present(albedo_dir_tile).and..not.l_grey_albedo) then
        bound%rho_alb(l, ip_surf_alb_dir, 1:spectrum%basic%n_band) &
          = albedo_dir_tile(l, i_tile, 1:spectrum%basic%n_band)
      end if
      if (present(t_tile)) then
        bound%t_ground(l) = t_tile(l, i_tile)
      end if      
    end if
  end do
end if


! Set the surface basis functions for a Lambertian surface.
bound%n_brdf_basis_fnc=1
! By defining F_{1,0,0,0} to be 4, rho_alb becomes equal to the diffuse albedo.
bound%f_brdf(1, 0, 0, 0)=4.0_RealK


! Orographic correction factor
if (present(orog_corr)) then
  bound%orog_corr(1:n_profile)=orog_corr(1:n_profile)
end if


! Incident solar flux
if (present(cos_zenith_angle) .and. present(solar_irrad)) then
  do l=1, n_profile
    if (cos_zenith_angle(l) > 0.0) then
      bound%solar_irrad(l)=solar_irrad(l)
      bound%zen_0(l)=1.0/cos_zenith_angle(l)
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
    if (control%l_tile .and. present(n_tile) .and. present(frac_tile)) then
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
