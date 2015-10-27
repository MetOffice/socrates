! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
!  Subroutine to calculate the absorptive extinctions of gases.
!
! Method:
!   Straightforward.
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: Radiance Core
!
!- ---------------------------------------------------------------------
SUBROUTINE gas_optical_properties(n_profile, n_layer                    &
     , n_gas, i_gas_pointer, k_esft_mono, gas_mix_ratio                 &
     , k_gas_abs                                                        &
     , nd_profile, nd_layer, nd_species                                 &
     )


  USE realtype_rd, ONLY: RealK
  USE yomhook, ONLY: lhook, dr_hook
  USE parkind1, ONLY: jprb, jpim

  IMPLICIT NONE

! Sizes of dummy arrays.
  INTEGER, INTENT(IN) ::                                                &
       nd_profile                                                       &
!       Size allocated for atmospheric profiles
    , nd_layer                                                          &
!       Size allocated for atmospheric layers
    , nd_species
!       Size allocated for gaseous species

! Dummy variables.
  INTEGER, INTENT(IN) ::                                                &
      n_profile                                                         &
!       Number of profiles
    , n_layer                                                           &
!       Number of layers
    , n_gas                                                             &
!       Number of gases
    , i_gas_pointer(nd_species)
!       Pointers to active gases
  REAL (RealK), INTENT(IN) ::                                           &
      k_esft_mono(nd_species)                                           &
!       ESFT exponents for each gas
    , gas_mix_ratio(nd_profile, nd_layer, nd_species)
!       Gas mixing ratios
  REAL (RealK), INTENT(OUT) ::                                          &
      k_gas_abs(nd_profile, nd_layer)
!       Clear absorptive extinction

! Local variables.
  INTEGER                                                               &
      i_gas                                                             &
!       Temporary gas `index'
    , l                                                                 &
!       Loop variable
    , i                                                                 &
!       Loop variable
    , j
!       Loop variable

  INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
  INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
  REAL(KIND=jprb)               :: zhook_handle

  CHARACTER(LEN=*), PARAMETER :: RoutineName='GAS_OPTICAL_PROPERTIES'


  IF (lhook) CALL dr_hook(RoutineName,zhook_in,zhook_handle)

! Calculate the absorption for the first gas and add on the rest.
  i_gas=i_gas_pointer(1)
  DO j=1, n_layer
    DO l=1, n_profile
      k_gas_abs(l, j)                                                   &
        =k_esft_mono(i_gas)*gas_mix_ratio(l, j, i_gas)
    END DO
  END DO
  DO i=2, n_gas
  i_gas=i_gas_pointer(i)
    DO j=1, n_layer
      DO l=1, n_profile
        k_gas_abs(l, j)=k_gas_abs(l, j)                                 &
          +k_esft_mono(i_gas)*gas_mix_ratio(l, j, i_gas)
      END DO
    END DO
  END DO


  IF (lhook) CALL dr_hook(RoutineName,zhook_out,zhook_handle)

END SUBROUTINE gas_optical_properties
