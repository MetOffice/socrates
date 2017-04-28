! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
!  Module to set indexing numbers of gaseous absorbing species.
!
! Description:
!   This module defines the identifiers defining the physical types
!   of each molecular absorbing species.
!   The numbering 1-12 agrees with HITRAN.
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: Radiation Control
!
!- ---------------------------------------------------------------------
MODULE gas_list_pcf

USE realtype_rd

IMPLICIT NONE

INTEGER, PARAMETER :: npd_gases = 38
!   Number of indexed gases

INTEGER, PARAMETER :: IP_h2o = 1
!   Identifier for water vapour
INTEGER, PARAMETER :: IP_co2 = 2
!   Identifier for carbon dioxide
INTEGER, PARAMETER :: IP_o3 = 3
!   Identifier for ozone
INTEGER, PARAMETER :: IP_n2o = 4
!   Identifier for dinitrogen oxide
INTEGER, PARAMETER :: IP_co = 5
!   Identifier for carbon monoxide
INTEGER, PARAMETER :: IP_ch4 = 6
!   Identifier for methane
INTEGER, PARAMETER :: IP_o2 = 7
!   Identifier for oxygen
INTEGER, PARAMETER :: IP_no = 8
!   Identifier for nitrogen monoxide
INTEGER, PARAMETER :: IP_so2 = 9
!   Identifier for sulphur dioxide
INTEGER, PARAMETER :: IP_no2 = 10
!   Identifier for nitrogen dioxide
INTEGER, PARAMETER :: IP_nh3 = 11
!   Identifier for ammonia
INTEGER, PARAMETER :: IP_hno3 = 12
!   Identifier for nitric acid
INTEGER, PARAMETER :: IP_n2 = 13
!   Identifier for nitrogen
INTEGER, PARAMETER :: IP_cfc11 = 14
!   Identifier for CFC11 (CFCl3)
INTEGER, PARAMETER :: IP_cfc12 = 15
!   Identifier for CFC12 (CF2Cl2)
INTEGER, PARAMETER :: IP_cfc113 = 16
!   Identifier for CFC113 (CF2ClCFCl2)
INTEGER, PARAMETER :: IP_hcfc22 = 17
!   Identifier for HCFC22 (CHF2Cl)
INTEGER, PARAMETER :: IP_hfc125 = 18
!   Identifier for HFC125 (C2HF5)
INTEGER, PARAMETER :: IP_hfc134a = 19
!   Identifier for HFC134A (CF3CFH2)
INTEGER, PARAMETER :: IP_cfc114 = 20
!   Identifier for CFC114 (C2Cl2F4)
INTEGER, PARAMETER :: IP_tio = 21
!   Identifier for TiO
INTEGER, PARAMETER :: IP_vo = 22
!   Identifier for VO
INTEGER, PARAMETER :: IP_h2 = 23
!   Identifier for hydrogen
INTEGER, PARAMETER :: IP_he = 24
!   Identifier for helium
INTEGER, PARAMETER :: IP_ocs = 25
!   Identifier for carbonyl sulphide
INTEGER, PARAMETER :: IP_na = 26
!   Identifier for sodium
INTEGER, PARAMETER :: IP_k = 27
!   Identifier for potassium
INTEGER, PARAMETER :: IP_feh = 28
!   Identifier for iron hydride
INTEGER, PARAMETER :: IP_crh = 29
!   Identifier for chromium hydride
INTEGER, PARAMETER :: IP_li = 30
!   Identifier for lithium
INTEGER, PARAMETER :: IP_rb = 31
!   Identifier for rubidium
INTEGER, PARAMETER :: IP_cs = 32
!   Identifier for cesium
INTEGER, PARAMETER :: IP_ph3 = 33
!   Identifier for phosphine
INTEGER, PARAMETER :: IP_c2h2 = 34
!   Identifier for acetylene
INTEGER, PARAMETER :: IP_hcn = 35
!   Identifier for hydrogen cyanide
INTEGER, PARAMETER :: IP_h2s = 36
!   Identifier for hydrogen sulphide
INTEGER, PARAMETER :: IP_ar = 37
!   Identifier for argon
INTEGER, PARAMETER :: IP_air = 38
!   Identifier for all other gases, used by generalised continuum

CHARACTER (LEN=20), PARAMETER :: name_absorb(npd_gases) = (/ &
                                   "Water Vapour        ", &
                                   "Carbon Dioxide      ", &
                                   "Ozone               ", &
                                   "Dinitrogen Oxide    ", &
                                   "Carbon monoxide     ", &
                                   "Methane             ", &
                                   "Oxygen              ", &
                                   "Nitrogen monoxide   ", &
                                   "Sulphur dioxide     ", &
                                   "Nitrogen dioxide    ", &
                                   "Ammonia             ", &
                                   "Nitric acid         ", &
                                   "Nitrogen            ", &
                                   "CFC11               ", &
                                   "CFC12               ", &
                                   "CFC113              ", &
                                   "HCFC22              ", &
                                   "HFC125              ", &
                                   "HFC134A             ", &
                                   "CFC114              ", &
                                   "Titanium oxide      ", &
                                   "Vanadium oxide      ", &
                                   "Hydrogen            ", &
                                   "Helium              ", &
                                   "Carbonyl sulphide   ", &
                                   "Sodium              ", &
                                   "Potassium           ", &
                                   "Iron hydride        ", &
                                   "Chromium hydride    ", &
                                   "Lithium             ", &
                                   "Rubidium            ", &
                                   "Cesium              ", &
                                   "Phosphine           ", &
                                   "Acetylene           ", &
                                   "Hydrogen cyanide    ", &
                                   "Hydrogen sulphide   ", &
                                   "Argon               ", &
                                   "Dry air             " /)


! Molecular weights taken from "General Inorganic Chemistry"
! by J. A. Duffy (1970), Longmans (except where stated).
REAL (RealK), PARAMETER :: molar_weight(npd_gases) = (/ &
  18.0153_RealK,     & ! H2O
  44.0100_RealK,     & ! CO2
  47.9982_RealK,     & ! O3
  44.0128_RealK,     & ! N2O
  28.0106_RealK,     & ! CO
  16.0430_RealK,     & ! CH4
  31.9988_RealK,     & ! O2
  30.0061_RealK,     & ! NO
  64.0628_RealK,     & ! SO2
  46.0055_RealK,     & ! NO2
  17.0306_RealK,     & ! NH3
  63.0129_RealK,     & ! HNO3
  28.0134_RealK,     & ! N2
  137.3686_RealK,    & ! CFC11
  120.9140_RealK,    & ! CFC12
  187.3765_RealK,    & ! CFC113
  86.46892_RealK,    & ! HCFC22
  120.02227_RealK,   & ! HFC125
  102.03184_RealK,   & ! HFC134a
  170.921_RealK,     & ! CFC114 (from NIST)
  63.866_RealK,      & ! TiO (from NIST)
  66.9409_RealK,     & ! VO (from NIST)
  2.01588_RealK,     & ! H2 (from NIST)
  4.002602_RealK,    & ! He (from NIST)
  60.075_RealK,      & ! OCS
  22.98976928_RealK, & ! Na (from NIST)
  39.0983_RealK,     & ! K (from NIST)
  56.853_RealK,      & ! FeH (from NIST)
  53.004_RealK,      & ! CrH (from NIST)
  6.941_RealK,       & ! Li (from NIST)
  85.4678_RealK,     & ! Rb (from NIST)
  132.9054519_RealK, & ! Cs (from NIST)
  33.99758_RealK,    & ! PH3 (from NIST)
  26.0373_RealK,     & ! C2H2 (from NIST)
  27.0253_RealK,     & ! HCN (from NIST)
  34.081_RealK,      & ! H2S (from NIST)
  39.948_RealK,      & ! Ar (from NIST)
  28.966_RealK      /) ! Dry air


! Array of identifiers in HITRAN for each gas in the radiation code.
INTEGER, PARAMETER :: hitran_number(npd_gases) = (/ &
  1,   & ! H2O
  2,   & ! CO2
  3,   & ! O3
  4,   & ! N2O
  5,   & ! CO
  6,   & ! CH4
  7,   & ! O2
  8,   & ! NO
  9,   & ! SO2
  10,  & ! NO2
  11,  & ! NH3
  12,  & ! HNO3
  22,  & ! N2
  0,   & ! CFC11
  0,   & ! CFC12
  0,   & ! CFC113
  0,   & ! HCFC22
  0,   & ! HFC125
  0,   & ! HFC134a
  0,   & ! CFC114
  0,   & ! TiO
  0,   & ! VO
  0,   & ! H2
  0,   & ! He
  19,  & ! OCS
  0,   & ! Na
  0,   & ! K
  0,   & ! FeH
  0,   & ! CrH
  0,   & ! Li
  0,   & ! Rb
  0,   & ! Cs
  28,  & ! PH3
  26,  & ! C2H2
  23,  & ! HCN
  31,  & ! H2S
  0,   & ! Ar
  0   /) ! Dry air

! Depolarization factors used to compute the Rayleigh scattering coefficients
REAL (RealK), PARAMETER :: depolarization_factor(npd_gases) = (/ &
  0.0_RealK,     & ! H2O
  0.0922_RealK,  & ! CO2 (Parthasarathy, Indian J. Phys. 25, 21 (1951))
  0.0_RealK,     & ! O3
  0.1197_RealK,  & ! N2O (Parthasarathy, Indian J. Phys. 25, 21 (1951))
  0.08_RealK,    & ! CO (Parthasarathy, Indian J. Phys. 25, 21 (1951))
  0.0_RealK,     & ! CH4
  0.06_RealK,    & ! O2 (Parthasarathy, Indian J. Phys. 25, 21 (1951))
  0.0218_RealK,  & ! NO (Parthasarathy, Indian J. Phys. 25, 21 (1951))
  0.0_RealK,     & ! SO2
  0.0_RealK,     & ! NO2
  0.0_RealK,     & ! NH3
  0.0_RealK,     & ! HNO3
  0.0305_RealK,  & ! N2 (Parthasarathy, Indian J. Phys. 25, 21 (1951))
  0.0_RealK,     & ! CFC11
  0.0_RealK,     & ! CFC12
  0.0_RealK,     & ! CFC113
  0.0_RealK,     & ! HCFC22
  0.0_RealK,     & ! HFC125
  0.0_RealK,     & ! HFC134a
  0.0_RealK,     & ! CFC114
  0.0_RealK,     & ! TiO
  0.0_RealK,     & ! VO
  0.0221_RealK,  & ! H2 (Parthasarathy, Indian J. Phys. 25, 21 (1951))
  0.025_RealK,   & ! He (Parthasarathy, Indian J. Phys. 25, 21 (1951))
  0.0_RealK,     & ! OCS
  0.0_RealK,     & ! Na
  0.0_RealK,     & ! K
  0.0_RealK,     & ! FeH
  0.0_RealK,     & ! CrH
  0.0_RealK,     & ! Li
  0.0_RealK,     & ! Rb
  0.0_RealK,     & ! Cs
  0.0_RealK,     & ! PH3
  0.0_RealK,     & ! C2H2
  0.0_RealK,     & ! HCN
  0.0_RealK,     & ! H2S
  0.0006_RealK,  & ! Ar (Parthasarathy, Indian J. Phys. 25, 21 (1951))
  0.0279_RealK  /) ! Dry air

END MODULE gas_list_pcf
