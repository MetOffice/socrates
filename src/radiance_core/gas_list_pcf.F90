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
!- ---------------------------------------------------------------------
MODULE gas_list_pcf

USE realtype_rd, ONLY: RealK

IMPLICIT NONE

INTEGER, PRIVATE :: i

INTEGER, PARAMETER :: npd_gases = 110
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
INTEGER, PARAMETER :: IP_o = 39
!   Identifier for atomic oxygen
INTEGER, PARAMETER :: IP_n = 40
!   Identifier for atomic nitrogen
INTEGER, PARAMETER :: IP_no3  = 41
!   Identifier for nitrate radical
INTEGER, PARAMETER :: IP_n2o5 = 42
!   Identifier for dinitrogen pentoxide
INTEGER, PARAMETER :: IP_hono = 43
!   Identifier for nitrous acid
INTEGER, PARAMETER :: IP_ho2no2 = 44
!   Identifier for peroxynitric acid
INTEGER, PARAMETER :: IP_h2o2 = 45
!   Identifier for hydrogen peroxide
INTEGER, PARAMETER :: IP_c2h6 = 46
!   Identifier for ethane
INTEGER, PARAMETER :: IP_ch3 = 47
!   Identifier for methyl radical
INTEGER, PARAMETER :: IP_h2co = 48
!   Identifier for formaldehyde
INTEGER, PARAMETER :: IP_ho2 = 49
!   Identifier for hydroperoxy radical
INTEGER, PARAMETER :: IP_hdo = 50
!   Identifier for semiheavy water
INTEGER, PARAMETER :: IP_hcl = 51
!   Identifier for hydrogen chloride
INTEGER, PARAMETER :: IP_hf = 52
!   Identifier for hydrogen fluoride
INTEGER, PARAMETER :: IP_cosso = 53
!   Identifier for cis-OSSO
INTEGER, PARAMETER :: IP_tosso = 54
!   Identifier for trans-OSSO
INTEGER, PARAMETER :: IP_yosos = 55
!   Identifier for OSO-S
INTEGER, PARAMETER :: IP_ch3cho = 56
!   Identifier for acetaldehyde
INTEGER, PARAMETER :: IP_ch3ooh = 57
!   Identifier for methylhydroperoxide
INTEGER, PARAMETER :: IP_ch3coch3 = 58
!   Identifier for acetone
INTEGER, PARAMETER :: IP_ch3cocho = 59
!   Identifier for methylglyoxal (MGLY)
INTEGER, PARAMETER :: IP_chocho = 60
!   Identifier for glyoxal
INTEGER, PARAMETER :: IP_c2h5cho = 61
!   Identifier for propanal
INTEGER, PARAMETER :: IP_hoch2cho = 62
!   Identifier for glycolaldehyde
INTEGER, PARAMETER :: IP_c2h5coch3 = 63
!   Identifier for methyl ethyl ketone (MEK)
INTEGER, PARAMETER :: IP_mvk = 64
!   Identifier for methyl vinyl ketone (MVK)
INTEGER, PARAMETER :: IP_macr = 65
!   Identifier for methacrolein (MACR)
INTEGER, PARAMETER :: IP_pan = 66
!   Identifier for peroxyacetyl nitrate (PAN)
INTEGER, PARAMETER :: IP_ch3ono2 = 67
!   Identifier for methylnitrate
INTEGER, PARAMETER :: IP_c2h3 = 68
!   Identifier for vinyl radical
INTEGER, PARAMETER :: IP_c2h4 = 69
!   Identifier for ethylene
INTEGER, PARAMETER :: IP_oh = 70
!   Identifier for hydroxyl radical
INTEGER, PARAMETER :: IP_hco = 71
!   Identifier for formyl radical
INTEGER, PARAMETER :: IP_n2o4 = 72
!   Identifier for dinitrogen tetroxide
INTEGER, PARAMETER :: IP_c2n2 = 73
!   Identifier for cyanogen
INTEGER, PARAMETER :: IP_n2h4 = 74
!   Identifier for hydrazine
INTEGER, PARAMETER :: IP_n2o3 = 75
!   Identifier for dinitrogen trioxide
INTEGER, PARAMETER :: IP_si = 76
!   Identifier for silicon
INTEGER, PARAMETER :: IP_sio = 77
!   Identifier for silicon monoxide
INTEGER, PARAMETER :: IP_sio2 = 78
!   Identifier for silicon dioxide
INTEGER, PARAMETER :: IP_mg = 79
!   Identifier for atomic magnesium
INTEGER, PARAMETER :: IP_mg2 = 80
!   Identifier for magnesium dimer
INTEGER, PARAMETER :: IP_mgo = 81
!   Identifier for magnesium oxide
INTEGER, PARAMETER :: IP_tio2 = 82
!   Identifier for titanium dioxide
INTEGER, PARAMETER :: IP_fe = 83
!   Identifier for atomic iron
INTEGER, PARAMETER :: IP_feo = 84
!   Identifier for iron(ii) oxide
INTEGER, PARAMETER :: IP_ca = 85
!   Identifier for calcium
INTEGER, PARAMETER :: IP_cao = 86
!   Identifier for calcium oxide
INTEGER, PARAMETER :: IP_alo = 87
!   Identifier for aluminium monoxide
INTEGER, PARAMETER :: IP_na2 = 88
!   Identifier for disodium
INTEGER, PARAMETER :: IP_nao = 89
!   Identifier for sodium oxide
INTEGER, PARAMETER :: IP_naoh = 90
!   Identifier for sodium hydroxide
INTEGER, PARAMETER :: IP_koh = 91
!   Identifier for potassium hydroxide
INTEGER, PARAMETER :: IP_hminus = 92
!   Identifier for hydride anion
INTEGER, PARAMETER :: IP_ps = 93
!   Identifier for phosphorus sulfide
INTEGER, PARAMETER :: IP_po = 94
!   Identifier for phosphorus monoxide
INTEGER, PARAMETER :: IP_pn = 95
!   Identifier for phosphorus nitride
INTEGER, PARAMETER :: IP_ch3sh = 96
!   Identifier for methanethiol
INTEGER, PARAMETER :: IP_ch3s = 97
!   Identifier for methylthiyl radical
INTEGER, PARAMETER :: IP_c2h6s = 98
!   Identifier for dimethyl sulfide
INTEGER, PARAMETER :: IP_c2h6s2 = 99
!   Identifier for dimethyl disulfide
INTEGER, PARAMETER :: IP_c3h4 = 100
!   Identifier for propyne
INTEGER, PARAMETER :: IP_c4h3 = 101
!   Identifier for butadiynyl radical
INTEGER, PARAMETER :: IP_sih4 = 102
!   Identifier for silane
INTEGER, PARAMETER :: IP_s2 = 103
!   Identifier for disulfur
INTEGER, PARAMETER :: IP_sf6 = 104
!   Identifier for sulfur hexafluoride
INTEGER, PARAMETER :: IP_cs2 = 105
!   Identifier for carbon disulfide
INTEGER, PARAMETER :: IP_s8 = 106
!   Identifier for octasulfur
INTEGER, PARAMETER :: IP_cn = 107
!   Identifier for cyanogen radical
INTEGER, PARAMETER :: IP_ch3cl = 108
!   Identifier for methyl chloride
INTEGER, PARAMETER :: IP_ch3f = 109
!   Identifier for methyl fluoride
INTEGER, PARAMETER :: IP_ch3br = 110
!   Identifier for methyl bromide


! Column headers for reading data in raw_input
CHARACTER (LEN=10), PARAMETER :: header_gas(npd_gases) = (/ &
    'H2O       ', 'CO2       ', 'O3        ', 'N2O       ', &
    'CO        ', 'CH4       ', 'O2        ', 'NO        ', &
    'SO2       ', 'NO2       ', 'NH3       ', 'HNO3      ', &
    'N2        ', 'CFC11     ', 'CFC12     ', 'CFC113    ', &
    'HCFC22    ', 'HFC125    ', 'HFC134A   ', 'CFC114    ', &
    'TiO       ', 'VO        ', 'H2        ', 'He        ', &
    'OCS       ', 'Na        ', 'K         ', 'FeH       ', &
    'CrH       ', 'Li        ', 'Rb        ', 'Cs        ', &
    'PH3       ', 'C2H2      ', 'HCN       ', 'H2S       ', &
    'Ar        ', 'AIR       ', 'O         ', 'N         ', &
    'NO3       ', 'N2O5      ', 'HONO      ', 'HO2NO2    ', &
    'H2O2      ', 'C2H6      ', 'CH3       ', 'H2CO      ', &
    'HO2       ', 'HDO       ', 'HCl       ', 'HF        ', &
    'cOSSO     ', 'tOSSO     ', 'yOSOS     ', 'CH3CHO    ', &
    'CH3OOH    ', 'CH3COCH3  ', 'CH3COCHO  ', 'CHOCHO    ', &
    'C2H5CHO   ', 'HOCH2CHO  ', 'C2H5COCH3 ', 'MVK       ', &
    'MACR      ', 'PAN       ', 'CH3ONO2   ', 'C2H3      ', &
    'C2H4      ', 'OH        ', 'HCO       ', 'N2O4      ', &
    'C2N2      ', 'N2H4      ', 'N2O3      ', 'Si        ', &
    'SiO       ', 'SiO2      ', 'Mg        ', 'Mg2       ', &
    'MgO       ', 'TiO2      ', 'Fe        ', 'FeO       ', &
    'Ca        ', 'CaO       ', 'AlO       ', 'Na2       ', &
    'NaO       ', 'NaOH      ', 'KOH       ', 'H-        ', &
    'PS        ', 'PO        ', 'PN        ', 'CH3SH     ', &
    'CH3S      ', 'C2H6S     ', 'C2H6S2    ', 'C3H4      ', &
    'C4H3      ', 'SiH4      ', 'S2        ', 'SF6       ', &
    'CS2       ', 'S8        ', 'CN        ', 'CH3Cl     ', &
    'CH3F      ', 'CH3Br     ' /)

! File suffixes
CHARACTER (LEN=12), PARAMETER :: gas_suffix(npd_gases) = (/ &
    'q           ', 'co2         ', 'o3          ', 'n2o         ', &
    'co          ', 'ch4         ', 'o2          ', 'no          ', &
    'so2         ', 'no2         ', 'nh3         ', 'hno3        ', &
    'n2          ', 'cfc11       ', 'cfc12       ', 'cfc113      ', &
    'hcfc22      ', 'hfc125      ', 'hfc134a     ', 'cfc114      ', &
    'tio         ', 'vo          ', 'h2          ', 'he          ', &
    'ocs         ', 'na          ', 'k           ', 'feh         ', &
    'crh         ', 'li          ', 'rb          ', 'cs          ', &
    'ph3         ', 'c2h2        ', 'hcn         ', 'h2s         ', &
    'ar          ', 'air         ', 'o           ', 'n           ', &
    'no3         ', 'n2o5        ', 'hono        ', 'ho2no2      ', &
    'h2o2        ', 'c2h6        ', 'ch3         ', 'h2co        ', &
    'ho2         ', 'hdo         ', 'hcl         ', 'hf          ', &
    'cosso       ', 'tosso       ', 'yosos       ', 'ch3cho      ', &
    'ch3ooh      ', 'ch3coch3    ', 'ch3cocho    ', 'chocho      ', &
    'c2h5cho     ', 'hoch2cho    ', 'c2h5coch3   ', 'mvk         ', &
    'macr        ', 'pan         ', 'ch3ono2     ', 'c2h3        ', &
    'c2h4        ', 'oh          ', 'hco         ', 'n2o4        ', &
    'c2n2        ', 'n2h4        ', 'n2o3        ', 'si          ', &
    'sio         ', 'sio2        ', 'mg          ', 'mg2         ', &
    'mgo         ', 'tio2        ', 'fe          ', 'feo         ', &
    'ca          ', 'cao         ', 'alo         ', 'na2         ', &
    'nao         ', 'naoh        ', 'koh         ', 'hminus      ', &
    'ps          ', 'po          ', 'pn          ', 'ch3sh       ', &
    'ch3s        ', 'c2h6s       ', 'c2h6s2      ', 'c3h4        ', &
    'c4h3        ', 'sih4        ', 's2          ', 'sf6         ', &
    'cs2         ', 's8          ', 'cn          ', 'ch3cl       ', &
    'ch3f        ', 'ch3br       ' /)

! Long names
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
                                   "Dry air             ", &
                                   "Atomic oxygen       ", &
                                   "Atomic nitrogen     ", &
                                   "Nitrate radical     ", &
                                   "Dinitrogen pentoxide", &
                                   "Nitrous acid        ", &
                                   "Peroxynitric acid   ", &
                                   "Hydrogen peroxide   ", &
                                   "Ethane              ", &
                                   "Methyl radical      ", &
                                   "Formaldehyde        ", &
                                   "Hydroperoxy radical ", &
                                   "Semiheavy water     ", &
                                   "Hydrogen chloride   ", &
                                   "Hydrogen fluoride   ", &
                                   "cis-OSSO            ", &
                                   "trans-OSSO          ", &
                                   "OSO-S               ", &
                                   "Acetaldehyde        ", &
                                   "Methylhydroperoxide ", &
                                   "Acetone             ", &
                                   "Methylglyoxal       ", &
                                   "Glyoxal             ", &
                                   "Propanal            ", &
                                   "Glycolaldehyde      ", &
                                   "Methyl ethyl ketone ", &
                                   "Methyl vinyl ketone ", &
                                   "Methacrolein        ", &
                                   "Peroxyacetyl nitrate", &
                                   "Methylnitrate       ", &
                                   "Vinyl radical       ", &
                                   "Ethylene            ", &
                                   "Hydroxyl radical    ", &
                                   "Formyl radical      ", &
                                   "Dinitrogen tetroxide", &
                                   "Cyanogen            ", &
                                   "Hydrazine           ", &
                                   "Dinitrogen trioxide ", &
                                   "Silicon             ", &
                                   "Silicon monoxide    ", &
                                   "Silicon dioxide     ", &
                                   "Atomic magnesium    ", &
                                   "Magnesium dimer     ", &
                                   "Magnesium oxide     ", &
                                   "Titanium dioxide    ", &
                                   "Iron                ", &
                                   "Iron(II) oxide      ", &
                                   "Calcium             ", &
                                   "Calcium oxide       ", &
                                   "Aluminium monoxide  ", &
                                   "Disodium            ", &
                                   "Sodium oxide        ", &
                                   "Sodium hydroxide    ", &
                                   "Potassium hydroxide ", &
                                   "Hydride anion       ", &
                                   "Phosphorus sulfide  ", &
                                   "Phosphorus monoxide ", &
                                   "Phosphorus nitride  ", &
                                   "Methanethiol        ", &
                                   "Methylthiyl radical ", &
                                   "Dimethyl sulfide    ", &
                                   "Dimethyl disulfide  ", &
                                   "Propyne             ", &
                                   "Butadiynyl radical  ", &
                                   "Silane              ", &
                                   "Disulfur            ", &
                                   "Sulfur hexafluoride ", &
                                   "Carbon disulfide    ", &
                                   "Octasulfur          ", &
                                   "Cyanogen radical    ", &
                                   "Methyl chloride     ", &
                                   "Methyl fluoride     ", &
                                   "Methyl bromide      " /)


! Molecular weights taken from "General Inorganic Chemistry"
! by J. A. Duffy (1970), Longmans (except where stated).
REAL (RealK), PARAMETER :: molar_weight(npd_gases) = (/ &
  18.0153_RealK,     & !  1: H2O
  44.0100_RealK,     & !  2: CO2
  47.9982_RealK,     & !  3: O3
  44.0128_RealK,     & !  4: N2O
  28.0106_RealK,     & !  5: CO
  16.0430_RealK,     & !  6: CH4
  31.9988_RealK,     & !  7: O2
  30.0061_RealK,     & !  8: NO
  64.0628_RealK,     & !  9: SO2
  46.0055_RealK,     & ! 10: NO2
  17.0306_RealK,     & ! 11: NH3
  63.0129_RealK,     & ! 12: HNO3
  28.0134_RealK,     & ! 13: N2
  137.3686_RealK,    & ! 14: CFC11
  120.9140_RealK,    & ! 15: CFC12
  187.3765_RealK,    & ! 16: CFC113
  86.46892_RealK,    & ! 17: HCFC22
  120.02227_RealK,   & ! 18: HFC125
  102.03184_RealK,   & ! 19: HFC134a
  170.921_RealK,     & ! 20: CFC114 (from NIST)
  63.866_RealK,      & ! 21: TiO (from NIST)
  66.9409_RealK,     & ! 22: VO (from NIST)
  2.01588_RealK,     & ! 23: H2 (from NIST)
  4.002602_RealK,    & ! 24: He (from NIST)
  60.075_RealK,      & ! 25: OCS
  22.98976928_RealK, & ! 26: Na (from NIST)
  39.0983_RealK,     & ! 27: K (from NIST)
  56.853_RealK,      & ! 28: FeH (from NIST)
  53.004_RealK,      & ! 29: CrH (from NIST)
  6.941_RealK,       & ! 30: Li (from NIST)
  85.4678_RealK,     & ! 31: Rb (from NIST)
  132.9054519_RealK, & ! 32: Cs (from NIST)
  33.99758_RealK,    & ! 33: PH3 (from NIST)
  26.0373_RealK,     & ! 34: C2H2 (from NIST)
  27.0253_RealK,     & ! 35: HCN (from NIST)
  34.081_RealK,      & ! 36: H2S (from NIST)
  39.948_RealK,      & ! 37: Ar (from NIST)
  28.966_RealK,      & ! 38: Dry air
  15.9994_RealK,     & ! 39: O (from NIST)
  14.00674_RealK,    & ! 40: N (from NIST)
  63.0128_RealK,     & ! 41: NO3 (from NIST)
  108.0104_RealK,    & ! 42: N2O5 (from NIST)
  47.0134_RealK,     & ! 43: HONO (from NIST)
  79.0122_RealK,     & ! 44: HO2NO2 (from NIST)
  34.0147_RealK,     & ! 45: H2O2 (from NIST)
  30.0690_RealK,     & ! 46: C2H6 (from NIST)
  15.0345_RealK,     & ! 47: CH3  (from NIST)
  30.0260_RealK,     & ! 48: H2CO (from NIST
  33.0067_RealK,     & ! 49: HO2 (from NIST)
  19.0214_RealK,     & ! 50: HDO (from NIST)
  36.461_RealK,      & ! 51: HCl (from NIST)
  20.00689_RealK,    & ! 52: HF (from NIST)
  96.129_RealK,      & ! 53: cis-OSSO (from NIST)
  96.129_RealK,      & ! 54: trans-OSSO (from NIST)
  96.129_RealK,      & ! 55: OSO-S (from NIST)
  44.0526_RealK,     & ! 56: CH3CHO (from NIST)
  48.0413_RealK,     & ! 57: CH3OOH (from NIST)
  58.0791_RealK,     & ! 58: CH3COCH3 (from NIST)
  72.0627_RealK,     & ! 59: CH3COCHO (from NIST)
  58.0361_RealK,     & ! 60: CHOCHO (from NIST)
  58.0791_RealK,     & ! 61: C2H5CHO (from NIST)
  60.0520_RealK,     & ! 62: HOCH2CHO (from NIST)
  72.1057_RealK,     & ! 63: C2H5COCH3 (from NIST)
  70.0898_RealK,     & ! 64: MVK (from NIST)
  70.0898_RealK,     & ! 65: MACR (from NIST)
  121.0489_RealK,    & ! 66: PAN (from NIST)
  77.0394_RealK,     & ! 67: CH3ONO2 (from NIST)
  27.0452_RealK,     & ! 68: C2H3 (from NIST)
  28.0532_RealK,     & ! 69: C2H4 (from NIST)
  17.0073_RealK,     & ! 70: OH (from NIST)
  29.0180_RealK,     & ! 71: HCO (from NIST)
  91.0110_RealK,     & ! 72: N2O4 (from NIST)
  52.0348_RealK,     & ! 73: C2N2 (from NIST)
  32.0452_RealK,     & ! 74: N2H4 (from NIST)
  76.0116_RealK,     & ! 75: N2O3 (from NIST)
  28.0855_RealK,     & !  76: Si (from NIST)
  44.0849_RealK,     & !  77: SiO (from NIST)
  60.0843_RealK,     & !  78: SiO2 (from NIST)
  24.3050_RealK,     & !  79: Mg (from NIST)
  48.6100_RealK,     & !  80: Mg2 (from NIST)
  40.3044_RealK,     & !  81: MgO (from NIST)
  79.8660_RealK,     & !  82: TiO2 (from NIST)
  55.8450_RealK,     & !  83: Fe (from NIST)
  71.8440_RealK,     & !  84: FeO (from NIST)
  40.0780_RealK,     & !  85: Ca (from NIST)
  56.0770_RealK,     & !  86: CaO (from NIST)
  42.9809_RealK,     & !  87: AlO (from NIST)
  45.9795_RealK,     & !  88: Na2 (from NIST)
  38.9892_RealK,     & !  89: NaO (from NIST)
  39.9971_RealK,     & !  90: NaOH (from NIST)
  56.1056_RealK,     & !  91: KOH (from NIST)
   1.0085_RealK,     & !  92: H- (from NIST)
  63.0390_RealK,     & !  93: PS (from NIST)
  46.9732_RealK,     & !  94: PO (from NIST)
  44.9805_RealK,     & !  95: PN (from NIST)
  48.1070_RealK,     & !  96: CH3SH (from NIST)
  47.1000_RealK,     & !  97: CH3S (from NIST)
  62.1340_RealK,     & !  98: C2H6S (from NIST)
  94.1990_RealK,     & !  99: C2H6S2 (from NIST)
  40.0638_RealK,     & ! 100: C3H4 (from NIST)
  51.0680_RealK,     & ! 101: C4H3 (4*C + 3*H)
  32.1173_RealK,     & ! 102: SiH4 (from NIST)
  64.1300_RealK,     & ! 103: S2 (from NIST)
  146.0550_RealK,    & ! 104: SF6 (from NIST)
  76.1410_RealK,     & ! 105: CS2 (from NIST)
  256.5200_RealK,    & ! 106: S8 (from NIST)
  26.0174_RealK,     & ! 107: CN (from NIST)
  50.4880_RealK,     & ! 108: CH3Cl (from NIST)
  34.0329_RealK,     & ! 109: CH3F (from NIST)
  94.9390_RealK     /) ! 110: CH3Br (from NIST)


! Array of identifiers in HITRAN for each gas in the radiation code.
INTEGER, PARAMETER :: hitran_number(npd_gases) = (/ &
  1,   & !  1: H2O
  2,   & !  2: CO2
  3,   & !  3: O3
  4,   & !  4: N2O
  5,   & !  5: CO
  6,   & !  6: CH4
  7,   & !  7: O2
  8,   & !  8: NO
  9,   & !  9: SO2
  10,  & ! 10: NO2
  11,  & ! 11: NH3
  12,  & ! 12: HNO3
  22,  & ! 13: N2
  0,   & ! 14: CFC11
  0,   & ! 15: CFC12
  0,   & ! 16: CFC113
  0,   & ! 17: HCFC22
  0,   & ! 18: HFC125
  0,   & ! 19: HFC134a
  0,   & ! 20: CFC114
  0,   & ! 21: TiO
  0,   & ! 22: VO
  45,  & ! 23: H2
  0,   & ! 24: He
  19,  & ! 25: OCS
  0,   & ! 26: Na
  0,   & ! 27: K
  0,   & ! 28: FeH
  0,   & ! 29: CrH
  0,   & ! 30: Li
  0,   & ! 31: Rb
  0,   & ! 32: Cs
  28,  & ! 33: PH3
  26,  & ! 34: C2H2
  23,  & ! 35: HCN
  31,  & ! 36: H2S
  0,   & ! 37: Ar
  0,   & ! 38: Dry air
  34,  & ! 39: O
  0,   & ! 40: N
  0,   & ! 41: NO3
  0,   & ! 42: N2O5
  60,  & ! 43: HONO
  0,   & ! 44: HO2NO2
  25,  & ! 45: H2O2
  27,  & ! 46: C2H6
  57,  & ! 47: CH3
  20,  & ! 48: H2CO
  33,  & ! 49: HO2
  1,   & ! 50: HDO
  15,  & ! 51: HCl
  14,  & ! 52: HF
  0,   & ! 53: cis-OSSO
  0,   & ! 54: trans-OSSO
  0,   & ! 55: OSO-S
  0,   & ! 56: CH3CHO
  0,   & ! 57: CH3OOH
  0,   & ! 58: CH3COCH3
  0,   & ! 59: CH3COCHO
  0,   & ! 60: CHOCHO
  0,   & ! 61: C2H5CHO
  0,   & ! 62: HOCH2CHO
  0,   & ! 63: C2H5COCH3
  0,   & ! 64: MVK
  0,   & ! 65: MACR
  0,   & ! 66: PAN
  0,   & ! 67: CH3ONO2
  0,   & ! 68: C2H3
  38,  & ! 69: C2H4
  13,  & ! 70: OH
  0,   & ! 71: HCO
  0,   & ! 72: N2O4
  48,  & ! 73: C2N2
  0 ,  & ! 74: N2H4
  0,   & ! 75: N2H3
  0  , & !  76: Si
  0  , & !  77: SiO
  0  , & !  78: SiO2
  0  , & !  79: Mg
  0  , & !  80: Mg2
  0  , & !  81: MgO
  0  , & !  82: TiO2
  0  , & !  83: Fe
  0  , & !  84: FeO
  0  , & !  85: Ca
  0  , & !  86: CaO
  0  , & !  87: AlO
  0  , & !  88: Na2
  0  , & !  89: NaO
  0  , & !  90: NaOH
  0  , & !  91: KOH
  0  , & !  92: H-
  0  , & !  93: PS
  0  , & !  94: PO
  0  , & !  95: PN
  0  , & !  96: CH3SH
  0  , & !  97: CH3S
  0  , & !  98: C2H6S
  0  , & !  99: C2H6S2
  0  , & ! 100: C3H4
  0  , & ! 101: C4H3
  0  , & ! 102: SiH4
  58 , & ! 103: S2
  30 , & ! 104: SF6
  53 , & ! 105: CS2
  0  , & ! 106: S8
  0  , & ! 107: CN
  24 , & ! 108: CH3Cl
  51 , & ! 109: CH3F
  40  /) ! 110: CH3Br

! Maximum number of specified HITRAN isotopes for a given absorber
INTEGER, PARAMETER :: npd_isotopes = 3

! List of HITRAN isotopes for each absorber (0 for all isotopes)
INTEGER, PARAMETER :: hitran_isotopes(npd_isotopes, npd_gases) &
  = RESHAPE ( [INTEGER :: &
  (0, i=1, npd_isotopes*ip_ho2),            & ! H2O -> HO2
  4, 5, 6, (0, i=1, npd_isotopes-3),        & ! HDO: HD16O, HD18O, HD17O
  (0, i=1, npd_isotopes*(npd_gases-ip_hdo)) & ! HCl -> last gas
  ], shape=[npd_isotopes, npd_gases] )

! Depolarization factors used to compute the Rayleigh scattering coefficients
REAL (RealK), PARAMETER :: depolarization_factor(npd_gases) = (/ &
  0.0_RealK,     & !  1: H2O
  0.0922_RealK,  & !  2: CO2 (Parthasarathy, Indian J. Phys. 25, 21 (1951))
  0.0_RealK,     & !  3: O3
  0.1197_RealK,  & !  4: N2O (Parthasarathy, Indian J. Phys. 25, 21 (1951))
  0.08_RealK,    & !  5: CO (Parthasarathy, Indian J. Phys. 25, 21 (1951))
  0.0_RealK,     & !  6: CH4
  0.06_RealK,    & !  7: O2 (Parthasarathy, Indian J. Phys. 25, 21 (1951))
  0.0218_RealK,  & !  8: NO (Parthasarathy, Indian J. Phys. 25, 21 (1951))
  0.0_RealK,     & !  9: SO2
  0.0_RealK,     & ! 10: NO2
  0.0_RealK,     & ! 11: NH3
  0.0_RealK,     & ! 12: HNO3
  0.0305_RealK,  & ! 22: N2 (Parthasarathy, Indian J. Phys. 25, 21 (1951))
  0.0_RealK,     & ! 14: CFC11
  0.0_RealK,     & ! 15: CFC12
  0.0_RealK,     & ! 16: CFC113
  0.0_RealK,     & ! 17: HCFC22
  0.0_RealK,     & ! 18: HFC125
  0.0_RealK,     & ! 19: HFC134a
  0.0_RealK,     & ! 20: CFC114
  0.0_RealK,     & ! 21: TiO
  0.0_RealK,     & ! 22: VO
  0.0221_RealK,  & ! 23: H2 (Parthasarathy, Indian J. Phys. 25, 21 (1951))
  0.025_RealK,   & ! 24: He (Parthasarathy, Indian J. Phys. 25, 21 (1951))
  0.0_RealK,     & ! 25: OCS
  0.0_RealK,     & ! 26: Na
  0.0_RealK,     & ! 27: K
  0.0_RealK,     & ! 28: FeH
  0.0_RealK,     & ! 29: CrH
  0.0_RealK,     & ! 30: Li
  0.0_RealK,     & ! 31: Rb
  0.0_RealK,     & ! 32: Cs
  0.0_RealK,     & ! 33: PH3
  0.0_RealK,     & ! 34: C2H2
  0.0_RealK,     & ! 35: HCN
  0.0_RealK,     & ! 36: H2S
  0.0006_RealK,  & ! 37: Ar (Parthasarathy, Indian J. Phys. 25, 21 (1951))
  0.0279_RealK,  & ! 38: Dry air
  0.0_RealK,     & ! 39: O
  0.0_RealK,     & ! 40: N
  0.0_RealK,     & ! 41: NO3
  0.0_RealK,     & ! 42: N2O5
  0.0_RealK,     & ! 43: HONO
  0.0_RealK,     & ! 44: HO2NO2
  0.0_RealK,     & ! 45: H2O2
  0.0_RealK,     & ! 46: C2H6
  0.0_RealK,     & ! 47: CH3
  0.0_RealK,     & ! 48: H2CO
  0.0_RealK,     & ! 49: HO2
  0.0_RealK,     & ! 50: HDO
  0.0_RealK,     & ! 51: HCl
  0.0_RealK,     & ! 52: HF
  0.0_RealK,     & ! 53: cis-OSSO
  0.0_RealK,     & ! 54: trans-OSSO
  0.0_RealK,     & ! 55: OSO-S
  0.0_RealK,     & ! 56: CH3CHO
  0.0_RealK,     & ! 57: CH3OOH
  0.0_RealK,     & ! 58: CH3COCH3
  0.0_RealK,     & ! 59: CH3COCHO
  0.0_RealK,     & ! 60: CHOCHO
  0.0_RealK,     & ! 61: C2H5CHO
  0.0_RealK,     & ! 62: HOCH2CHO
  0.0_RealK,     & ! 63: C2H5COCH3
  0.0_RealK,     & ! 64: MVK
  0.0_RealK,     & ! 65: MACR
  0.0_RealK,     & ! 66: PAN
  0.0_RealK,     & ! 67: CH3ONO2
  0.0_RealK,     & ! 68: C2H3
  0.0_RealK,     & ! 69: C2H4
  0.0_RealK,     & ! 70: OH
  0.0_RealK,     & ! 71: HCO
  0.0_RealK,     & ! 72: N2O4
  0.0_RealK,     & ! 73: C2N2
  0.0_RealK,     & ! 74: N2H4
  0.0_RealK,     & ! 75: N2H3
  0.0_RealK,     & ! 76: Si
  0.0_RealK,     & ! 77: SiO
  0.0_RealK,     & ! 78: SiO2
  0.0_RealK,     & ! 79: Mg
  0.0_RealK,     & ! 80: Mg2
  0.0_RealK,     & ! 81: MgO
  0.0_RealK,     & ! 82: TiO2
  0.0_RealK,     & ! 83: Fe
  0.0_RealK,     & ! 84: FeO
  0.0_RealK,     & ! 85: Ca
  0.0_RealK,     & ! 86: CaO
  0.0_RealK,     & ! 87: AlO
  0.0_RealK,     & ! 88: Na2
  0.0_RealK,     & ! 89: NaO
  0.0_RealK,     & ! 90: NaOH
  0.0_RealK,     & ! 91: KOH
  0.0_RealK,     & ! 92: H-
  0.0_RealK,     & ! 93: PS
  0.0_RealK,     & ! 94: PO
  0.0_RealK,     & ! 95: PN
  0.0_RealK,     & ! 96: CH3SH
  0.0_RealK,     & ! 97: CH3S
  0.0_RealK,     & ! 98: C2H6S
  0.0_RealK,     & ! 99: C2H6S2
  0.0_RealK,     & ! 100: C3H4
  0.0_RealK,     & ! 101: C4H3
  0.0_RealK,     & ! 102: SiH4
  0.0_RealK,     & ! 103: S2
  0.0_RealK,     & ! 104: SF6
  0.0_RealK,     & ! 105: CS2
  0.0_RealK,     & ! 106: S8
  0.0_RealK,     & ! 107: CN
  0.0_RealK,     & ! 108: CH3Cl
  0.0_RealK,     & ! 109: CH3F
  0.0_RealK     /) ! 110: CH3Br

! Minimum wavelength to consider Rayleigh scattering
REAL (RealK), PARAMETER :: rayleigh_cutoff(npd_gases) = (/ &
  175.0E-09_RealK, & !  1: H2O ( threshold for H2O -> O(1D) + H2 )
  167.1E-09_RealK, & !  2: CO2 ( threshold for CO2 -> CO + O(1D) )
  0.0_RealK,       & !  3: O3
  0.0_RealK,       & !  4: N2O
  0.0_RealK,       & !  5: CO
  0.0_RealK,       & !  6: CH4
  175.0E-09_RealK, & !  7: O2 ( threshold for O2 -> O(3P) + O(1D) )
  0.0_RealK,       & !  8: NO
  0.0_RealK,       & !  9: SO2
  0.0_RealK,       & ! 10: NO2
  0.0_RealK,       & ! 11: NH3
  0.0_RealK,       & ! 12: HNO3
  79.8E-09_RealK,  & ! 13: N2 ( threshold for N2 -> N2+ )
  0.0_RealK,       & ! 14: CFC11
  0.0_RealK,       & ! 15: CFC12
  0.0_RealK,       & ! 16: CFC113
  0.0_RealK,       & ! 17: HCFC22
  0.0_RealK,       & ! 18: HFC125
  0.0_RealK,       & ! 19: HFC134a
  0.0_RealK,       & ! 20: CFC114
  0.0_RealK,       & ! 21: TiO
  0.0_RealK,       & ! 22: VO
  0.0_RealK,       & ! 23: H2
  0.0_RealK,       & ! 24: He
  0.0_RealK,       & ! 25: OCS
  0.0_RealK,       & ! 26: Na
  0.0_RealK,       & ! 27: K
  0.0_RealK,       & ! 28: FeH
  0.0_RealK,       & ! 29: CrH
  0.0_RealK,       & ! 30: Li
  0.0_RealK,       & ! 31: Rb
  0.0_RealK,       & ! 32: Cs
  0.0_RealK,       & ! 33: PH3
  0.0_RealK,       & ! 34: C2H2
  0.0_RealK,       & ! 35: HCN
  0.0_RealK,       & ! 36: H2S
  0.0_RealK,       & ! 37: Ar
  0.0_RealK,       & ! 38: Dry air
  0.0_RealK,       & ! 39: O
  0.0_RealK,       & ! 40: N
  0.0_RealK,       & ! 41: NO3
  0.0_RealK,       & ! 42: N2O5
  0.0_RealK,       & ! 43: HONO
  0.0_RealK,       & ! 44: HO2NO2
  0.0_RealK,       & ! 45: H2O2
  0.0_RealK,       & ! 46: C2H6
  0.0_RealK,       & ! 47: CH3
  0.0_RealK,       & ! 48: H2CO
  0.0_RealK,       & ! 49: HO2
  0.0_RealK,       & ! 50: HDO
  0.0_RealK,       & ! 51: HCl
  0.0_RealK,       & ! 52: HF
  0.0_RealK,       & ! 53: cis-OSSO
  0.0_RealK,       & ! 54: trans-OSSO
  0.0_RealK,       & ! 55: OSO-S
  0.0_RealK,       & ! 56: CH3CHO
  0.0_RealK,       & ! 57: CH3OOH
  0.0_RealK,       & ! 58: CH3COCH3
  0.0_RealK,       & ! 59: CH3COCHO
  0.0_RealK,       & ! 60: CHOCHO
  0.0_RealK,       & ! 61: C2H5CHO
  0.0_RealK,       & ! 62: HOCH2CHO
  0.0_RealK,       & ! 63: C2H5COCH3
  0.0_RealK,       & ! 64: MVK
  0.0_RealK,       & ! 65: MACR
  0.0_RealK,       & ! 66: PAN
  0.0_RealK,       & ! 67: CH3ONO2
  0.0_RealK,       & ! 68: C2H3
  0.0_RealK,       & ! 69: C2H4
  0.0_RealK,       & ! 70: OH
  0.0_RealK,       & ! 71: HCO
  0.0_RealK,       & ! 72: N2O4
  0.0_RealK,       & ! 73: C2N2
  0.0_RealK,       & ! 74: N2H4
  0.0_RealK,       & ! 75: N2H3
  0.0_RealK,       & ! 76: Si
  0.0_RealK,       & ! 77: SiO
  0.0_RealK,       & ! 78: SiO2
  0.0_RealK,       & ! 79: Mg
  0.0_RealK,       & ! 80: Mg2
  0.0_RealK,       & ! 81: MgO
  0.0_RealK,       & ! 82: TiO2
  0.0_RealK,       & ! 83: Fe
  0.0_RealK,       & ! 84: FeO
  0.0_RealK,       & ! 85: Ca
  0.0_RealK,       & ! 86: CaO
  0.0_RealK,       & ! 87: AlO
  0.0_RealK,       & ! 88: Na2
  0.0_RealK,       & ! 89: NaO
  0.0_RealK,       & ! 90: NaOH
  0.0_RealK,       & ! 91: KOH
  0.0_RealK,       & ! 92: H-
  0.0_RealK,       & ! 93: PS
  0.0_RealK,       & ! 94: PO
  0.0_RealK,       & ! 95: PN
  0.0_RealK,       & ! 96: CH3SH
  0.0_RealK,       & ! 97: CH3S
  0.0_RealK,       & ! 98: C2H6S
  0.0_RealK,       & ! 99: C2H6S2
  0.0_RealK,       & ! 100: C3H4
  0.0_RealK,       & ! 101: C4H3
  0.0_RealK,       & ! 102: SiH4
  0.0_RealK,       & ! 103: S2
  0.0_RealK,       & ! 104: SF6
  0.0_RealK,       & ! 105: CS2
  0.0_RealK,       & ! 106: S8
  0.0_RealK,       & ! 107: CN
  0.0_RealK,       & ! 108: CH3Cl
  0.0_RealK,       & ! 109: CH3F
  0.0_RealK       /) ! 110: CH3Br

! Maximum number of photolysis products for a given absorber
INTEGER, PARAMETER :: npd_products = 9
INTEGER, PARAMETER :: npd_pathway = npd_gases * (npd_products + 1)

CHARACTER(LEN=56), PARAMETER :: blank = ""

! Description of photolysis products
! 1: H2O
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_h2o(npd_products) &
  = [CHARACTER(LEN=56) ::            &
  "H2O -> O(3P) + H2             ",  &
  "H2O -> OH(X2Pi) + H           ",  &
  "H2O -> O(1D) + H2             ",  &
  "H2O -> OH(A2Sigma+) + H       ",  &
  "H2O -> O(3P) + H + H          ",  &
  (blank, i=1, npd_products-5)]

REAL (RealK), PARAMETER :: threshold_wavelength_h2o(npd_products) &
  = [REAL(RealK) ::                 &
  246.0E-09_RealK,                  & ! H2O -> O(3P) + H2
  242.0E-09_RealK,                  & ! H2O -> OH(X2Pi) + H
  175.0E-09_RealK,                  & ! H2O -> O(1D) + H2
  134.0E-09_RealK,                  & ! H2O -> OH(A2Sigma+) + H
  129.0E-09_RealK,                  & ! H2O -> O(3P) + H + H
  (0.0_RealK, i=1, npd_products-5)]

! 2: CO2
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_co2(npd_products) &
  = [CHARACTER(LEN=56) ::            &
  "CO2 -> CO + O(3P)             ",  &
  "CO2 -> CO + O(1D)             ",  &
  "CO2 -> CO + O(1S)             ",  &
  "CO2 -> CO(a3Pi) + O(3P)       ",  &
  "CO2 -> CO2+                   ",  &
  "CO2 -> CO + O+                ",  &
  "CO2 -> CO+ + O(3P)            ",  &
  "CO2 -> O2 + C+                ",  &
  (blank, i=1, npd_products-8)]

REAL (RealK), PARAMETER :: threshold_wavelength_co2(npd_products) &
  = [REAL(RealK) ::                 &
  227.5E-09_RealK,                  & ! CO2 -> CO + O(3P)
  167.1E-09_RealK,                  & ! CO2 -> CO + O(1D)
  128.6E-09_RealK,                  & ! CO2 -> CO + O(1S)
  108.2E-09_RealK,                  & ! CO2 -> CO(a3Pi) + O(3P)
  89.922E-09_RealK,                 & ! CO2 -> CO2+
  65.026E-09_RealK,                 & ! CO2 -> CO + O+
  63.693E-09_RealK,                 & ! CO2 -> CO+ + O
  54.655E-09_RealK,                 & ! CO2 -> O2 + C+
  (0.0_RealK, i=1, npd_products-8)]   ! Heubner 92

! 3: O3
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_o3(npd_products) &
  = [CHARACTER(LEN=56) ::            &
  "O3 -> O(3P) + O2(X3Sigmag-)   ",  &
  "O3 -> O(3P) + O2(a1Deltag)    ",  &
  "O3 -> O(3P) + O2(b1Sigmag+)   ",  &
  "O3 -> O(1D) + O2(X3Sigmag-)   ",  &
  "O3 -> O(1D) + O2(a1Deltag)    ",  &
  "O3 -> O(1D) + O2(b1Sigmag+)   ",  &
  "O3 -> 3 O(3P)                 ",  &
  "O3 -> O(1S) + O2(a1Deltag)    ",  &
  (blank, i=1, npd_products-8)]

REAL (RealK), PARAMETER :: threshold_wavelength_o3(npd_products) &
  = [REAL(RealK) ::                 &
  1180.0E-09_RealK,                 & ! O3 -> O(3P) + O2(X3Sigmag-)
   612.0E-09_RealK,                 & ! O3 -> O(3P) + O2(a1Deltag)
   463.0E-09_RealK,                 & ! O3 -> O(3P) + O2(b1Sigmag+)
   411.0E-09_RealK,                 & ! O3 -> O(1D) + O2(X3Sigmag-)
   310.0E-09_RealK,                 & ! O3 -> O(1D) + O2(a1Deltag)
   267.0E-09_RealK,                 & ! O3 -> O(1D) + O2(b1Sigmag+)
   201.0E-09_RealK,                 & ! O3 -> 3 O(3P)
   196.0E-09_RealK,                 & ! O3 -> O(1S) + O2(a1Deltag)
  (0.0_RealK, i=1, npd_products-8)]

! 4: N2O
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_n2o(npd_products) &
  = [CHARACTER(LEN=56) ::            &
  "N2O -> N2 + O(1D)             ",  &
  "N2O -> N2 + O(3P)             ",  &
  "N2O -> N(4S) + NO(2Pi)        ",  &
  "N2O -> N2 + O(1S)             ",  &
  (blank, i=1, npd_products-4)]

REAL (RealK), PARAMETER :: threshold_wavelength_n2o(npd_products) &
  = [REAL(RealK) ::                 &
  336.0E-09_RealK,                  & ! N2O -> N2 + O(1D)
  713.0E-09_RealK,                  & ! N2O -> N2 + O(3P)
  248.0E-09_RealK,                  & ! N2O -> N(4S) + NO(2Pi)
  210.0E-09_RealK,                  & ! N2O -> N2 + O(1S)
  (0.0_RealK, i=1, npd_products-4)]

! 5: CO
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_co(npd_products) &
  = [CHARACTER(LEN=56) ::            &
  "CO -> C + O(3P)               ",  &
  (blank, i=1, npd_products-1)]

REAL (RealK), PARAMETER :: threshold_wavelength_co(npd_products) &
  = [REAL(RealK) ::                 &
  111.78E-09_RealK,                 & ! CO -> CO + O(3P)
  (0.0_RealK, i=1, npd_products-1)]   ! Huebner 92

! 6: CH4
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_ch4(npd_products) &
  = [CHARACTER(LEN=56) ::            &
  "CH4 -> CH3 + H                ",  &
  "CH4 -> CH2(1) + H2            ",  &
  "CH4 -> CH2(3) + H + H         ",  &
  "CH4 -> CH + H2 + H            ",  &
  (blank, i=1, npd_products-4)]

REAL (RealK), PARAMETER :: threshold_wavelength_ch4(npd_products) &
  = [REAL(RealK) ::                 &
  277.0E-09_RealK,                  & ! CH4 -> CH3 + H
  237.3E-09_RealK,                  & ! CH4 -> CH2(1) + H2
  132.2E-09_RealK,                  & ! CH4 -> CH2(3) + H + H
  137.0E-09_RealK,                  & ! CH4 -> CH + H2 + H
  (0.0_RealK, i=1, npd_products-4)]   ! Huebner 92

! 7: O2
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_o2(npd_products) &
  = [CHARACTER(LEN=56) ::            &
  "O2 -> O(3P) + O(3P)           ",  &
  "O2 -> O(3P) + O(1D)           ",  &
  "O2 -> O(1D) + O(1D)           ",  &
  "O2 -> O(3P) + O(1S)           ",  &
  "O2 -> O(1D) + O(1S)           ",  &
  "O2 -> O2+                     ",  &
  "O2 -> O+ + O                  ",  &
  (blank, i=1, npd_products-7)]

REAL (RealK), PARAMETER :: threshold_wavelength_o2(npd_products) &
  = [REAL(RealK) ::                 &
  242.3E-09_RealK,                  & ! O2 -> O(3P) + O(3P)
  175.0E-09_RealK,                  & ! O2 -> O(3P) + O(1D)
  137.0E-09_RealK,                  & ! O2 -> O(1D) + O(1D)
  132.0E-09_RealK,                  & ! O2 -> O(3P) + O(1S)
  110.0E-09_RealK,                  & ! O2 -> O(1D) + O(1S)
  102.78E-09_RealK,                 & ! O2 -> O2+
   66.2E-09_RealK,                  & ! O2 -> O+ + O
  (0.0_RealK, i=1, npd_products-7)]

! 8: NO
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_no(npd_products) &
  = [CHARACTER(LEN=56) ::            &
  "NO -> O(3P) + N(4S)           ",  &
  (blank, i=1, npd_products-1)]

REAL (RealK), PARAMETER :: threshold_wavelength_no(npd_products) &
  = [REAL(RealK) ::                 &
  191.0E-09_RealK,                  & ! NO -> O(3P) + N(4S)
  (0.0_RealK, i=1, npd_products-1)]   ! Huebner 92

! 9: SO2
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_so2(npd_products) &
  = [CHARACTER(LEN=56) ::            &
  "SO2 -> SO + O(3P)             ",  &
  (blank, i=1, npd_products-1)]

REAL (RealK), PARAMETER :: threshold_wavelength_so2(npd_products) &
  = [REAL(RealK) ::                 &
  218.7E-09_RealK,                  & ! SO2 -> SO + O(3P)
  (0.0_RealK, i=1, npd_products-1)]   ! Becker 95

! 10: NO2
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_no2(npd_products) &
  = [CHARACTER(LEN=56) ::            &
  "NO2 -> NO + O(3P)             ",  &
  "NO2 -> NO + O(1D)             ",  &
  (blank, i=1, npd_products-2)]

REAL (RealK), PARAMETER :: threshold_wavelength_no2(npd_products) &
  = [REAL(RealK) ::                 &
  398.0E-09_RealK,                  & ! NO2 -> NO + O(3P)
  244.0E-09_RealK,                  & ! NO2 -> NO + O(1D)
  (0.0_RealK, i=1, npd_products-2)]

! 11: NH3
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_nh3(npd_products) &
  = [CHARACTER(LEN=56) ::            &
  "NH3 -> NH2 + H                ",  &
  (blank, i=1, npd_products-1)]

REAL (RealK), PARAMETER :: threshold_wavelength_nh3(npd_products) &
  = [REAL(RealK) ::                 &
  279.8E-09_RealK,                  & ! NH3 -> NH2 + H
  (0.0_RealK, i=1, npd_products-1)]   ! Huebner 92

! 12: HNO3
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_hno3(npd_products) &
  = [CHARACTER(LEN=56) ::            &
  "HNO3 -> OH + NO2              ",  &
  "HNO3 -> HONO + O(3P)          ",  &
  "HNO3 -> H + NO3               ",  &
  "HNO3 -> OH + NO2*(12B2)       ",  &
  "HNO3 -> HONO + O(1D)          ",  &
  "HNO3 -> HONO (a3A)+ O(3P)     ",  &
  (blank, i=1, npd_products-6)]

REAL (RealK), PARAMETER :: threshold_wavelength_hno3(npd_products) &
  = [REAL(RealK) ::                 &
  604.0E-09_RealK,                  & ! HNO3 -> OH + NO2
  393.0E-09_RealK,                  & ! HNO3 -> HONO + O(3P)
  278.0E-09_RealK,                  & ! HNO3 -> H + NO3
  381.0E-09_RealK,                  & ! HNO3 -> OH + NO2*(12B2)
  242.0E-09_RealK,                  & ! HNO3 -> HONO + O(1D)
  216.0E-09_RealK,                  & ! HNO3 -> HONO (a3A)+ O(3P)
  (0.0_RealK, i=1, npd_products-6)]   ! IUPAC PNOx2

! 13: N2
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_n2(npd_products) &
  = [CHARACTER(LEN=56) ::            &
  "N2 -> N + N                   ",  &
  "N2 -> N2+                     ",  &
  "N2 -> N+ + N                  ",  &
  (blank, i=1, npd_products-3)]

REAL (RealK), PARAMETER :: threshold_wavelength_n2(npd_products) &
  = [REAL(RealK) ::                 &
   98.6E-09_RealK,                  & ! N2 -> N + N
   79.8E-09_RealK,                  & ! N2 -> N2+
   51.0E-09_RealK,                  & ! N2 -> N+ + N
  (0.0_RealK, i=1, npd_products-3)]

! 23: H2
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_h2(npd_products) &
  = [CHARACTER(LEN=56) ::            &
  "H2 -> H + H                   ",  &
  (blank, i=1, npd_products-1)]

REAL (RealK), PARAMETER :: threshold_wavelength_h2(npd_products) &
  = [REAL(RealK) ::                 &
  276.885E-09_RealK,                & ! H2 -> H + H
  (0.0_RealK, i=1, npd_products-1)]   ! Huebner 92

! 25: OCS
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_ocs(npd_products) &
  = [CHARACTER(LEN=56) ::            &
  "OCS -> CO + S(3P)             ",  &
  "OCS -> CO + S(1D)             ",  &
  "OCS -> CO + S(1S)             ",  &
  (blank, i=1, npd_products-3)]

REAL (RealK), PARAMETER :: threshold_wavelength_ocs(npd_products) &
  = [REAL(RealK) ::                 &
  388.0E-09_RealK,                  & ! OCS -> CO + S(3P)
  285.0E-09_RealK,                  & ! OCS -> CO + S(1D)
  209.0E-09_RealK,                  & ! OCS -> CO + S(1S)
  (0.0_RealK, i=1, npd_products-3)]

! 34: C2H2
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_c2h2(npd_products) &
  = [CHARACTER(LEN=56) ::            &
  "C2H2 -> C2H + H               ",  &
  (blank, i=1, npd_products-1)]

REAL (RealK), PARAMETER :: threshold_wavelength_c2h2(npd_products) &
  = [REAL(RealK) ::                 &
  230.6E-09_RealK,                  & ! C2H2 -> C2H + H
  (0.0_RealK, i=1, npd_products-1)]   ! Huebner 92

! 35: HCN
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_hcn(npd_products) &
  = [CHARACTER(LEN=56) ::            &
  "HCN -> CN + H                 ",  &
  (blank, i=1, npd_products-1)]

REAL (RealK), PARAMETER :: threshold_wavelength_hcn(npd_products) &
  = [REAL(RealK) ::                 &
  195.0E-09_RealK,                  & ! HCN -> CN + H
  (0.0_RealK, i=1, npd_products-1)]   ! Huebner 92

! 39: O
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_o(npd_products) &
  = [CHARACTER(LEN=56) ::            &
  "O -> O+(4S)                   ",  &
  "O -> O+(2D)                   ",  &
  "O -> O+(2P)                   ",  &
  "O -> O+(4Pe)                  ",  &
  "O -> O+(2Pe)                  ",  &
  "O -> O++                      ",  &
  "O -> O+++                     ",  &
  (blank, i=1, npd_products-7)]

REAL (RealK), PARAMETER :: threshold_wavelength_o(npd_products) &
  = [REAL(RealK) ::                 &
   91.25E-09_RealK,                 & ! O -> O+(4S)
   73.18E-09_RealK,                 & ! O -> O+(2D)
   66.58E-09_RealK,                 & ! O -> O+(2P)
   43.50E-09_RealK,                 & ! O -> O+(4Pe)
   31.00E-09_RealK,                 & ! O -> O+(2Pe)
   24.80E-09_RealK,                 & ! O -> O++
   12.179E-09_RealK,                & ! O -> O+++
  (0.0_RealK, i=1, npd_products-7)]

! 40: N
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_n(npd_products) &
  = [CHARACTER(LEN=56) ::            &
  "N -> N+                       ",  &
  "N -> N++                      ",  &
  (blank, i=1, npd_products-2)]

REAL (RealK), PARAMETER :: threshold_wavelength_n(npd_products) &
  = [REAL(RealK) ::                 &
   85.92E-09_RealK,                 & ! N -> N+
   28.00E-09_RealK,                 & ! N -> N++
  (0.0_RealK, i=1, npd_products-2)]

! 41: NO3
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_no3(npd_products) &
  = [CHARACTER(LEN=56) ::            &
  "NO3 -> NO + O2                ",  &
  "NO3 -> NO2 + O(3P)            ",  &
  (blank, i=1, npd_products-2)]

REAL (RealK), PARAMETER :: threshold_wavelength_no3(npd_products) &
  = [REAL(RealK) ::                 &
   7320.0E-09_RealK,                & ! NO3 -> NO + O2
   574.0E-09_RealK,                 & ! NO3 -> NO2 + O(3P)
  (0.0_RealK, i=1, npd_products-2)]   ! JPL 19-5

! 42: N2O5
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_n2o5(npd_products) &
  = [CHARACTER(LEN=56) ::            &
  "N2O5 -> NO3 + NO2             ",  &
  "N2O5 -> NO3 + NO + O(3P)      ",  &
  (blank, i=1, npd_products-2)]

REAL (RealK), PARAMETER :: threshold_wavelength_n2o5(npd_products) &
  = [REAL(RealK) ::                 &
   1255.0E-09_RealK,                & ! N2O5 -> NO3 + NO2
   298.0E-09_RealK,                 & ! N2O5 -> NO3 + NO + O(3P)
  (0.0_RealK, i=1, npd_products-2)]   ! JPL 19-5

! 43: HONO
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_hono(npd_products) &
  = [CHARACTER(LEN=56) ::            &
  "HONO -> OH + NO               ",  &
  "HONO -> H + NO2               ",  &
  (blank, i=1, npd_products-2)]

REAL (RealK), PARAMETER :: threshold_wavelength_hono(npd_products) &
  = [REAL(RealK) ::                 &
   579.0E-09_RealK,                 & ! HONO -> OH + NO
   362.0E-09_RealK,                 & ! HONO -> H + NO2
  (0.0_RealK, i=1, npd_products-2)]   ! JPL 19-5

! 44: HO2NO2
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_ho2no2(npd_products) &
  = [CHARACTER(LEN=56) ::            &
  "HO2NO2 -> HO2 + NO2           ",  &
  "HO2NO2 -> OH + NO3            ",  &
  "HO2NO2 -> O(3P) + HNO3        ",  &
  "HO2NO2 -> H + NO2 + O2        ",  &
  "HO2NO2 -> HO2 + NO + O(3P)    ",  &
  "HO2NO2 -> OH + NO2 + O(3P)    ",  &
  "HO2NO2 -> H + O(3P) + NO3     ",  &
  "HO2NO2 -> HONO + O2(1Sigma)   ",  &
  "HO2NO2 -> HONO + O2(1Lambda)  ",  &
  (blank, i=1, npd_products-9)]

REAL (RealK), PARAMETER :: threshold_wavelength_ho2no2(npd_products) &
  = [REAL(RealK) ::                 &
  1207.0E-09_RealK,                 & ! HO2NO2 -> HO2 + NO2
   726.0E-09_RealK,                 & ! HO2NO2 -> OH + NO3
   713.0E-09_RealK,                 & ! HO2NO2 -> O(3P) + HNO3
   393.0E-09_RealK,                 & ! HO2NO2 -> H + NO2 + O2
   339.0E-09_RealK,                 & ! HO2NO2 -> HO2 + NO + O(3P)
   321.0E-09_RealK,                 & ! HO2NO2 -> OH + NO2 + O(3P)
   201.0E-09_RealK,                 & ! HO2NO2 -> H + O(3P) + NO3
   911.0E-09_RealK,                 & ! HO2NO2 -> HONO + O2(1Sigma)
   1744.0E-09_RealK,                & ! HO2NO2 -> HONO + O2(1Lambda)
  (0.0_RealK, i=1, npd_products-9)]   ! JPL 19-5

! 45: H2O2
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_h2o2(npd_products) &
  = [CHARACTER(LEN=56) ::            &
  "H2O2 -> OH + OH               ",  &
  "H2O2 -> H2O + O(1D)           ",  &
  "H2O2 -> H + HO2               ",  &
  (blank, i=1, npd_products-3)]

REAL (RealK), PARAMETER :: threshold_wavelength_h2o2(npd_products) &
  = [REAL(RealK) ::                 &
   557.0E-09_RealK,                 & ! H2O2 -> OH + OH
   359.0E-09_RealK,                 & ! H2O2 -> H2O + O(1D)
   324.0E-09_RealK,                 & ! H2O2 -> H + HO2
  (0.0_RealK, i=1, npd_products-3)]   ! JPL 19-5

! 46: C2H6
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_c2h6(npd_products) &
  = [CHARACTER(LEN=56) ::            &
  "C2H6 -> C2H4 + H2             ",  &
  "C2H6 -> C2H4 + H + H          ",  &
  "C2H6 -> C2H2 + H2 + H2        ",  &
  "C2H6 -> CH4 + CH2(1)          ",  &
  "C2H6 -> CH3 + CH3             ",  &
  (blank, i=1, npd_products-5)]

REAL (RealK), PARAMETER :: threshold_wavelength_c2h6(npd_products) &
  = [REAL(RealK) ::                 &
   874.3E-09_RealK,                 & ! C2H6 -> C2H4 + H2 : Huebner 92
   290.0E-09_RealK,                 & ! C2H6 -> C2H4 + H + H :Huebner 92/Lias 70
   428.7E-09_RealK,                 & ! C2H6 -> C2H2 + H2 + H2 :Chang 2020
   272.6E-09_RealK,                 & ! C2H6 -> CH4 + CH2(1) :Huebner 92
   322.0E-09_RealK,                 & ! C2H6 -> CH3 + CH3 :Huebner 92
  (0.0_RealK, i=1, npd_products-5)]

! 47: CH3
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_ch3(npd_products) &
  = [CHARACTER(LEN=56) ::            &
  "CH3 -> CH2(1) + H             ",  &
  (blank, i=1, npd_products-1)]

REAL (RealK), PARAMETER :: threshold_wavelength_ch3(npd_products) &
  = [REAL(RealK) ::                 &
   216.0E-09_RealK,                 & ! CH3 -> CH2(1) + H
  (0.0_RealK, i=1, npd_products-1)]   ! Venot 2012-highest non zero qy

! 48: H2CO
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_h2co(npd_products) &
  = [CHARACTER(LEN=56) ::            &
  "H2CO -> H + HCO               ",  &
  "H2CO -> H2 + CO               ",  &
  "H2CO -> H + H + CO            ",  &
  (blank, i=1, npd_products-3)]

REAL (RealK), PARAMETER :: threshold_wavelength_h2co(npd_products) &
  = [REAL(RealK) ::                 &
   330.0E-09_RealK,                 & ! H2CO -> H + HCO
   361.0E-09_RealK,                 & ! H2CO -> H2 + CO
   283.0E-09_RealK,                 & ! H2CO -> H + H + CO
  (0.0_RealK, i=1, npd_products-3)]   ! JPL 19-5

! 49: HO2
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_ho2(npd_products) &
  = [CHARACTER(LEN=56) ::            &
  "HO2 -> OH + O(3P)             ",  &
  "HO2 -> OH + O(1D)             ",  &
  (blank, i=1, npd_products-2)]

REAL (RealK), PARAMETER :: threshold_wavelength_ho2(npd_products) &
  = [REAL(RealK) ::                 &
   438.0E-09_RealK,                 & ! HO2 -> OH + O(3P)
   259.0E-09_RealK,                 & ! HO2 -> OH + O(1D)
  (0.0_RealK, i=1, npd_products-2)]   ! JPL 19-5

! 56: CH3CHO
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_ch3cho(npd_products) &
  = [CHARACTER(LEN=56) ::            &
  "CH3CHO -> CH3 + HCO           ",  &
  "CH3CHO -> CH4 + CO            ",  &
  "CH3CHO -> CH3CO + H           ",  &
  (blank, i=1, npd_products-3)]

REAL (RealK), PARAMETER :: threshold_wavelength_ch3cho(npd_products) &
  = [REAL(RealK) ::                 &
   335.0E-09_RealK,                 & ! CH3CHO -> CH3 + HCO
   1.0_RealK,                       & ! CH3CHO -> CH4 + CO
   320.0E-09_RealK,                 & ! CH3CHO -> CH3CO + H
  (0.0_RealK, i=1, npd_products-3)]   ! JPL 19-5

! 57: CH3OOH
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_ch3ooh(npd_products) &
  = [CHARACTER(LEN=56) ::            &
  "CH3OOH -> CH3O + OH           ",  &
  "CH3OOH -> CH3 + HO2           ",  &
  "CH3OOH -> CH3O2 + H           ",  &
  "CH3OOH -> CH3OH + O(3P)       ",  &
  (blank, i=1, npd_products-4)]

REAL (RealK), PARAMETER :: threshold_wavelength_ch3ooh(npd_products) &
  = [REAL(RealK) ::                 &
   645.0E-09_RealK,                 & ! CH3OOH -> CH3O + OH
   410.0E-09_RealK,                 & ! CH3OOH -> CH3 + HO2
   334.0E-09_RealK,                 & ! CH3OOH -> CH3O2 + H
   670.0E-09_RealK,                 & ! CH3OOH -> CH3OH + O(3P)
  (0.0_RealK, i=1, npd_products-4)]   ! JPL 19-5

! 58: CH3COCH3
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_ch3coch3(npd_products) &
  = [CHARACTER(LEN=56) ::            &
  "CH3COCH3 -> CH3CO + CH3       ",  &
  "CH3COCH3 -> 2 CH3 + CO        ",  &
  (blank, i=1, npd_products-2)]

REAL (RealK), PARAMETER :: threshold_wavelength_ch3coch3(npd_products) &
  = [REAL(RealK) ::                 &
   338.0E-09_RealK,                 & ! CH3COCH3 -> CH3CO + CH3
   299.0E-09_RealK,                 & ! CH3COCH3 -> 2 CH3 + CO
  (0.0_RealK, i=1, npd_products-2)]   ! JPL 19-5

! 59: CH3COCHO
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_ch3cocho(npd_products) &
  = [CHARACTER(LEN=56) ::            &
  "CH3COCHO -> CH3CO + HCO       ",  &
  "CH3COCHO -> CH4 + 2 CO        ",  &
  "CH3COCHO -> CH3CHO + CO       ",  &
  (blank, i=1, npd_products-3)]

REAL (RealK), PARAMETER :: threshold_wavelength_ch3cocho(npd_products) &
  = [REAL(RealK) ::                 &
   422.0E-09_RealK,                 & ! CH3COCHO -> CH3CO + HCO
   1.0_RealK,                       & ! CH3COCHO -> CH4 + 2 CO
   1.0_RealK,                       & ! CH3COCHO -> CH3CHO + CO
  (0.0_RealK, i=1, npd_products-3)]   ! JPL 19-5

! 60: CHOCHO
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_chocho(npd_products) &
  = [CHARACTER(LEN=56) ::            &
  "CHOCHO -> HCO + HCO           ",  &
  "CHOCHO -> H2 + 2 CO           ",  &
  "CHOCHO -> HCHO + CO           ",  &
  "CHOCHO -> H + CO + HCO        ",  &
  (blank, i=1, npd_products-4)]

REAL (RealK), PARAMETER :: threshold_wavelength_chocho(npd_products) &
  = [REAL(RealK) ::                 &
   399.0E-09_RealK,                 & ! CHOCHO -> HCO + HCO
   1.0_RealK,                       & ! CHOCHO -> H2 + 2 CO
   1.0_RealK,                       & ! CHOCHO -> HCHO + CO
   329.0E-09_RealK,                 & ! CHOCHO -> H + CO + HCO
  (0.0_RealK, i=1, npd_products-4)]   ! JPL 19-5

! 61: C2H5CHO
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_c2h5cho(npd_products) &
  = [CHARACTER(LEN=56) ::            &
  "C2H5CHO -> C2H5 + HCO         ",  &
  "C2H5CHO -> C2H6 + CO          ",  &
  "C2H5CHO -> C2H4 + HCHO        ",  &
  "C2H5CHO -> CH3 + CH2CHO       ",  &
  (blank, i=1, npd_products-4)]

REAL (RealK), PARAMETER :: threshold_wavelength_c2h5cho(npd_products) &
  = [REAL(RealK) ::                 &
   341.0E-09_RealK,                 & ! C2H5CHO -> C2H5 + HCO
   1.0_RealK,                       & ! C2H5CHO -> C2H6 + CO
   926.0E-09_RealK,                 & ! C2H5CHO -> C2H4 + HCHO
   349.0E-09_RealK,                 & ! C2H5CHO -> CH3 + CH2CHO
  (0.0_RealK, i=1, npd_products-4)]   ! JPL 19-5

! 62: HOCH2CHO
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_hoch2cho(npd_products) &
  = [CHARACTER(LEN=56) ::            &
  "HOCH2CHO -> CH2OH + HCO       ",  &
  "HOCH2CHO -> CH3OH + CO        ",  &
  "HOCH2CHO -> OH + CH2CHO       ",  &
  "HOCH2CHO -> HOCH2CO + H       ",  &
  (blank, i=1, npd_products-4)]

REAL (RealK), PARAMETER :: threshold_wavelength_hoch2cho(npd_products) &
  = [REAL(RealK) ::                 &
   347.0E-09_RealK,                 & ! HOCH2CHO -> CH2OH + HCO
  1910.0E-09_RealK,                 & ! HOCH2CHO -> CH3OH + CO
   328.0E-09_RealK,                 & ! HOCH2CHO -> OH + CH2CHO
   314.0E-09_RealK,                 & ! HOCH2CHO -> HOCH2CO + H
  (0.0_RealK, i=1, npd_products-4)]

! 63: C2H5COCH3
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_c2h5coch3(npd_products) &
  = [CHARACTER(LEN=56) ::            &
  "C2H5COCH3 -> CH3CO + C2H5     ",  &
  "C2H5COCH3 -> C2H5CO + CH3     ",  &
  "C2H5COCH3 -> C2H5 + CO + CH3  ",  &
  (blank, i=1, npd_products-3)]

REAL (RealK), PARAMETER :: threshold_wavelength_c2h5coch3(npd_products) &
  = [REAL(RealK) ::                 &
   342.0E-09_RealK,                 & ! C2H5COCH3 -> CH3CO + C2H5
   339.0E-09_RealK,                 & ! C2H5COCH3 -> C2H5CO + CH3
   303.0E-09_RealK,                 & ! C2H5COCH3 -> C2H5 + CO + CH3
  (0.0_RealK, i=1, npd_products-3)]   ! Zborowska 21

! 64: CH3C(O)CH=CH2 (MVK)
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_mvk(npd_products) &
  = [CHARACTER(LEN=56) ::               &
  "CH3C(O)CH=CH2 -> CH3-CH=CH2 + CO  ", &
  "CH3C(O)CH=CH2 -> CH=CH2 + CH3C(O) ", &
  "CH3C(O)CH=CH2 -> CH=CH2C(O) + CH3 ", &
  (blank, i=1, npd_products-3)]

REAL (RealK), PARAMETER :: threshold_wavelength_mvk(npd_products) &
  = [REAL(RealK) ::                 &
   1.0_RealK,                       & ! CH3C(O)CH=CH2 -> CH3-CH=CH2 + CO
   1.0_RealK,                       & ! CH3C(O)CH=CH2 -> CH=CH2 + CH3C(O)
   1.0_RealK,                       & ! CH3C(O)CH=CH2 -> CH=CH2C(O) + CH3
  (0.0_RealK, i=1, npd_products-3)]   ! JPL 19-5

! 65: CH2=C(CH3)CHO (MACR)
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_macr(npd_products) &
  = [CHARACTER(LEN=56) ::               &
  "CH2=C(CH3)CHO -> CH2=CCH3 + CHO   ", &
  "CH2=C(CH3)CHO -> C3H6 + CO        ", &
  "CH2=C(CH3)CHO -> H + CH2=C(CH3)CO ", &
  (blank, i=1, npd_products-3)]

REAL (RealK), PARAMETER :: threshold_wavelength_macr(npd_products) &
  = [REAL(RealK) ::                 &
   1.0_RealK,                       & ! CH2=C(CH3)CHO -> CH2=CCH3 + CHO
   1.0_RealK,                       & ! CH2=C(CH3)CHO -> C3H6 + CO
   1.0_RealK,                       & ! CH2=C(CH3)CHO -> H + CH2=C(CH3)CO
  (0.0_RealK, i=1, npd_products-3)]   ! JPL 19-5

! 66: CH3C(O)OONO2 (PAN)
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_pan(npd_products) &
  = [CHARACTER(LEN=56) ::               &
  "CH3C(O)OONO2 -> CH3C(O)OO + NO2   ", &
  "CH3C(O)OONO2 -> CH3C(O)O + NO3    ", &
  (blank, i=1, npd_products-2)]

REAL (RealK), PARAMETER :: threshold_wavelength_pan(npd_products) &
  = [REAL(RealK) ::                 &
  1004.0E-09_RealK,                 & ! CH3C(O)OONO2 -> CH3C(O)OO + NO2
   963.0E-09_RealK,                 & ! CH3C(O)OONO2 -> CH3C(O)O + NO3
  (0.0_RealK, i=1, npd_products-2)]   ! IUPAC P21

! 67: CH3ONO2
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_ch3ono2(npd_products) &
  = [CHARACTER(LEN=56) ::               &
  "CH3ONO2 -> CH3O + NO2             ", &
  "CH3ONO2 -> HCHO + HONO            ", &
  "CH3ONO2 -> HCHO + NO + OH         ", &
  "CH3ONO2 -> CH3ONO + O(3P)         ", &
  "CH3ONO2 -> CH3 + NO3              ", &
  "CH3ONO2 -> CH2ONO2 + H            ", &
  "CH3ONO2 -> CH3O + NO + O(3P)      ", &
  "CH3ONO2 -> CH3ONO + O(1D)         ", &
  (blank, i=1, npd_products-8)]

REAL (RealK), PARAMETER :: threshold_wavelength_ch3ono2(npd_products) &
  = [REAL(RealK) ::                 &
   697.0E-09_RealK,                 & ! CH3ONO2 -> CH3O + NO2
   1.0_RealK,                       & ! CH3ONO2 -> HCHO + HONO
   497.0E-09_RealK,                 & ! CH3ONO2 -> HCHO + NO + OH
   391.0E-09_RealK,                 & ! CH3ONO2 -> CH3ONO + O(3P)
   344.0E-09_RealK,                 & ! CH3ONO2 -> CH3 + NO3
   294.0E-09_RealK,                 & ! CH3ONO2 -> CH2ONO2 + H
   250.0E-09_RealK,                 & ! CH3ONO2 -> CH3O + NO + O(3P)
   241.0E-09_RealK,                 & ! CH3ONO2 -> CH3ONO + O(1D)
  (0.0_RealK, i=1, npd_products-8)]   ! JPL 19-5

! 68: C2H3
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_c2h3(npd_products) &
  = [CHARACTER(LEN=56) ::               &
  "C2H3 -> C2H2 + H                  ", &
  (blank, i=1, npd_products-1)]

REAL (RealK), PARAMETER :: threshold_wavelength_c2h3(npd_products) &
  = [REAL(RealK) ::                 &
   237.0E-09_RealK,                 & ! C2H3 -> C2H2 + H
  (0.0_RealK, i=1, npd_products-1)]   ! Venot 2012-highest non zero qy

! 69: C2H4
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_c2h4(npd_products) &
  = [CHARACTER(LEN=56) ::               &
  "C2H4 -> C2H2 + H2                 ", &
  "C2H4 -> C2H2 + H + H              ", &
  (blank, i=1, npd_products-2)]

REAL (RealK), PARAMETER :: threshold_wavelength_c2h4(npd_products) &
  = [REAL(RealK) ::                 &
   720.0E-09_RealK,                 & ! C2H4 -> C2H2 + H2
   196.0E-09_RealK,                 & ! C2H4 -> C2H2 + H + H
  (0.0_RealK, i=1, npd_products-2)]   ! Huebner 92

! 70: OH
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_oh(npd_products) &
  = [CHARACTER(LEN=56) ::               &
  "OH -> O(1D) + H                   ", &
  (blank, i=1, npd_products-1)]

REAL (RealK), PARAMETER :: threshold_wavelength_oh(npd_products) &
  = [REAL(RealK) ::                 &
   511.4E-09_RealK,                 & ! OH -> O(1D) + H
  (0.0_RealK, i=1, npd_products-1)]   ! Huebner 1992

! 71: HCO
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_hco(npd_products) &
  = [CHARACTER(LEN=56) ::               &
  "HCO -> H + CO                     ", &
  (blank, i=1, npd_products-1)]

REAL (RealK), PARAMETER :: threshold_wavelength_hco(npd_products) &
  = [REAL(RealK) ::                 &
   229.0E-09_RealK,                 & ! HCO -> H + CO
  (0.0_RealK, i=1, npd_products-1)]   ! Venot 2012-highest non zero xsc

! 72: N2O4
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_n2o4(npd_products) &
  = [CHARACTER(LEN=56) ::               &
  "N2O4 -> NO2 + NO2                 ", &
  (blank, i=1, npd_products-1)]

REAL (RealK), PARAMETER :: threshold_wavelength_n2o4(npd_products) &
  = [REAL(RealK) ::                 &
  453.0E-09_RealK,                  & ! N2O4 -> NO2 + NO2
  (0.0_RealK, i=1, npd_products-1)]   ! Venot 2012-highest non zero xsc

! 73: C2N2
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_c2n2(npd_products) &
  = [CHARACTER(LEN=56) ::               &
  "C2N2 -> C2 + N2                   ", &
  (blank, i=1, npd_products-1)]

REAL (RealK), PARAMETER :: threshold_wavelength_c2n2(npd_products) &
  = [REAL(RealK) ::                 &
  224.0E-09_RealK,                  & ! C2N2 -> C2 + N2
  (0.0_RealK, i=1, npd_products-1)]   ! Venot 2012-highest non zero xsc

! 74: N2H4
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_n2h4(npd_products) &
  = [CHARACTER(LEN=56) ::               &
  "N2H4 -> N2H3 + H                  ", &
  (blank, i=1, npd_products-1)]

REAL (RealK), PARAMETER :: threshold_wavelength_n2h4(npd_products) &
  = [REAL(RealK) ::                 &
  290.0E-09_RealK,                  & ! N2H4 -> N2H3 + H
  (0.0_RealK, i=1, npd_products-1)]   ! Venot 2012-highest non zero xsc

! 75: N2O3
! --------------------------
CHARACTER(LEN=56), PARAMETER :: photol_products_n2o3(npd_products) &
  = [CHARACTER(LEN=56) ::               &
  "N2O3 -> NO2 + NO                  ", &
  (blank, i=1, npd_products-1)]

REAL (RealK), PARAMETER :: threshold_wavelength_n2o3(npd_products) &
  = [REAL(RealK) ::                 &
  398.0E-09_RealK,                  & ! N2O3 -> NO2 + NO
  (0.0_RealK, i=1, npd_products-1)]   ! Venot 2012-highest non zero xsc

CHARACTER(LEN=56), PARAMETER :: photol_products(npd_products, npd_gases) &
  = RESHAPE([CHARACTER(LEN=56) ::    &
  "H2O -> O(3P) + H2             ",  &
  "H2O -> OH(X2Pi) + H           ",  &
  "H2O -> O(1D) + H2             ",  &
  "H2O -> OH(A2Sigma+) + H       ",  &
  "H2O -> O(3P) + H + H          ",  &
  (blank, i=1, npd_products-5),      & ! H2O
  "CO2 -> CO + O(3P)             ",  &
  "CO2 -> CO + O(1D)             ",  &
  "CO2 -> CO + O(1S)             ",  &
  "CO2 -> CO(a3Pi) + O(3P)       ",  &
  "CO2 -> CO2+                   ",  &
  "CO2 -> CO + O+                ",  &
  "CO2 -> CO+ + O(3P)            ",  &
  "CO2 -> O2 + C+                ",  &
  (blank, i=1, npd_products-8),      & ! CO2
  "O3 -> O(3P) + O2(X3Sigmag-)   ",  &
  "O3 -> O(3P) + O2(a1Deltag)    ",  &
  "O3 -> O(3P) + O2(b1Sigmag+)   ",  &
  "O3 -> O(1D) + O2(X3Sigmag-)   ",  &
  "O3 -> O(1D) + O2(a1Deltag)    ",  &
  "O3 -> O(1D) + O2(b1Sigmag+)   ",  &
  "O3 -> 3 O(3P)                 ",  &
  "O3 -> O(1S) + O2(a1Deltag)    ",  &
  (blank, i=1, npd_products-8),      & ! O3
  "N2O -> N2 + O(1D)             ",  &
  "N2O -> N2 + O(3P)             ",  &
  "N2O -> N(4S) + NO(2Pi)        ",  &
  "N2O -> N2 + O(1S)             ",  &
  (blank, i=1, npd_products-4),      & ! N2O
  "CO -> C + O(3P)               ",  &
  (blank, i=1, npd_products-1),      & ! CO
  "CH4 -> CH3 + H                ",  &
  "CH4 -> CH2(1) + H2            ",  &
  "CH4 -> CH2(3) + H + H         ",  &
  "CH4 -> CH + H2 + H            ",  &
  (blank, i=1, npd_products-4),      & ! CH4
  "O2 -> O(3P) + O(3P)           ",  &
  "O2 -> O(3P) + O(1D)           ",  &
  "O2 -> O(1D) + O(1D)           ",  &
  "O2 -> O(3P) + O(1S)           ",  &
  "O2 -> O(1D) + O(1S)           ",  &
  "O2 -> O2+                     ",  &
  "O2 -> O+ + O                  ",  &
  (blank, i=1, npd_products-7),      & ! O2
  "NO -> O(3P) + N(4S)           ",  &
  (blank, i=1, npd_products-1),      & ! NO
  "SO2 -> SO + O(3P)             ",  &
  (blank, i=1, npd_products-1),      & ! SO2
  "NO2 -> NO + O(3P)             ",  &
  "NO2 -> NO + O(1D)             ",  &
  (blank, i=1, npd_products-2),      & ! NO2
  "NH3 -> NH2 + H                ",  &
  (blank, i=1, npd_products-1),      & ! NH3
  "HNO3 -> OH + NO2              ",  &
  "HNO3 -> HONO + O(3P)          ",  &
  "HNO3 -> H + NO3               ",  &
  "HNO3 -> OH + NO2*(12B2)       ",  &
  "HNO3 -> HONO + O(1D)          ",  &
  "HNO3 -> HONO (a3A)+ O(3P)     ",  &
  (blank, i=1, npd_products-6),      & ! HNO3
  "N2 -> N + N                   ",  &
  "N2 -> N2+                     ",  &
  "N2 -> N+ + N                  ",  &
  (blank, i=1, npd_products-3),      & ! N2
  (blank, i=1, npd_products),        & ! CFC11
  (blank, i=1, npd_products),        & ! CFC12
  (blank, i=1, npd_products),        & ! CFC113
  (blank, i=1, npd_products),        & ! HCFC22
  (blank, i=1, npd_products),        & ! HFC125
  (blank, i=1, npd_products),        & ! HFC134a
  (blank, i=1, npd_products),        & ! CFC114
  (blank, i=1, npd_products),        & ! TiO
  (blank, i=1, npd_products),        & ! VO
  "H2 -> H + H                   ",  &
  (blank, i=1, npd_products-1),      & ! H2
  (blank, i=1, npd_products),        & ! He
  "OCS -> CO + S(3P)             ",  &
  "OCS -> CO + S(1D)             ",  &
  "OCS -> CO + S(1S)             ",  &
  (blank, i=1, npd_products-3),      & ! OCS
  (blank, i=1, npd_products),        & ! Na
  (blank, i=1, npd_products),        & ! K
  (blank, i=1, npd_products),        & ! FeH
  (blank, i=1, npd_products),        & ! CrH
  (blank, i=1, npd_products),        & ! Li
  (blank, i=1, npd_products),        & ! Rb
  (blank, i=1, npd_products),        & ! Cs
  (blank, i=1, npd_products),        & ! PH3
  "C2H2 -> C2H + H               ",  &
  (blank, i=1, npd_products-1),      & ! C2H2
  "HCN -> CN + H                 ",  &
  (blank, i=1, npd_products-1),      & ! HCN
  (blank, i=1, npd_products),        & ! H2S
  (blank, i=1, npd_products),        & ! Ar
  (blank, i=1, npd_products),        & ! Dry air
  "O -> O+(4S)                   ",  &
  "O -> O+(2D)                   ",  &
  "O -> O+(2P)                   ",  &
  "O -> O+(4Pe)                  ",  &
  "O -> O+(2Pe)                  ",  &
  "O -> O++                      ",  &
  "O -> O+++                     ",  &
  (blank, i=1, npd_products-7),      & ! O
  "N -> N+                       ",  &
  "N -> N++                      ",  &
  (blank, i=1, npd_products-2),      & ! N
  "NO3 -> NO + O2                ",  &
  "NO3 -> NO2 + O(3P)            ",  &
  (blank, i=1, npd_products-2)    ,  & ! NO3
  "N2O5 -> NO3 + NO2             ",  &
  "N2O5 -> NO3 + NO + O(3P)      ",  &
  (blank, i=1, npd_products-2)    ,  & ! N2O5
  "HONO -> OH + NO               ",  &
  "HONO -> H + NO2               ",  &
  (blank, i=1, npd_products-2)    ,  & ! HONO
  "HO2NO2 -> HO2 + NO2           ",  &
  "HO2NO2 -> OH + NO3            ",  &
  "HO2NO2 -> O(3P) + HNO3        ",  &
  "HO2NO2 -> H + NO2 + O2        ",  &
  "HO2NO2 -> HO2 + NO + O(3P)    ",  &
  "HO2NO2 -> OH + NO2 + O(3P)    ",  &
  "HO2NO2 -> H + O(3P) + NO3     ",  &
  "HO2NO2 -> HONO + O2(1Sigma)   ",  &
  "HO2NO2 -> HONO + O2(1Lambda)  ",  &
  (blank, i=1, npd_products-9)    ,  & ! HO2NO2
  "H2O2 -> OH + OH               ",  &
  "H2O2 -> H2O + O(1D)           ",  &
  "H2O2 -> H + HO2               ",  &
  (blank, i=1, npd_products-3)    ,  & ! H2O2
  "C2H6 -> C2H4 + H2             ",  &
  "C2H6 -> C2H4 + H + H          ",  &
  "C2H6 -> C2H2 + H2 + H2        ",  &
  "C2H6 -> CH4 + CH2(1)          ",  &
  "C2H6 -> CH3 + CH3             ",  &
  (blank, i=1, npd_products-5)    ,  & ! C2H6
  "CH3 -> CH2(1) + H             ",  & !
  (blank, i=1, npd_products-1)    ,  & ! CH3
  "H2CO -> H + HCO               ",  &
  "H2CO -> H2 + CO               ",  &
  "H2CO -> H + H + CO            ",  &
  (blank, i=1, npd_products-3)    ,  & ! H2CO
  "HO2 -> OH + O(3P)             ",  &
  "HO2 -> OH + O(1D)             ",  &
  (blank, i=1, npd_products-2)    ,  & ! HO2
  (blank, i=1, npd_products)      ,  & ! HDO
  (blank, i=1, npd_products)      ,  & ! HCl
  (blank, i=1, npd_products)      ,  & ! HF
  (blank, i=1, npd_products)      ,  & ! cis-OSSO
  (blank, i=1, npd_products)      ,  & ! trans-OSSO
  (blank, i=1, npd_products)      ,  & ! OSO-S
  "CH3CHO -> CH3 + HCO           ",  &
  "CH3CHO -> CH4 + CO            ",  &
  "CH3CHO -> CH3CO + H           ",  &
  (blank, i=1, npd_products-3)    ,  & ! CH3CHO
  "CH3OOH -> CH3O + OH           ",  &
  "CH3OOH -> CH3 + HO2           ",  &
  "CH3OOH -> CH3O2 + H           ",  &
  "CH3OOH -> CH3OH + O(3P)       ",  &
  (blank, i=1, npd_products-4)    ,  & ! CH3OOH
  "CH3COCH3 -> CH3CO + CH3       ",  &
  "CH3COCH3 -> 2 CH3 + CO        ",  &
  (blank, i=1, npd_products-2)    ,  & ! CH3COCH3
  "CH3COCHO -> CH3CO + HCO       ",  &
  "CH3COCHO -> CH4 + 2 CO        ",  &
  "CH3COCHO -> CH3CHO + CO       ",  &
  (blank, i=1, npd_products-3)    ,  & ! CH3COCHO
  "CHOCHO -> HCO + HCO           ",  &
  "CHOCHO -> H2 + 2 CO           ",  &
  "CHOCHO -> HCHO + CO           ",  &
  "CHOCHO -> H + CO + HCO        ",  &
  (blank, i=1, npd_products-4)    ,  & ! CHOCHO
  "C2H5CHO -> C2H5 + HCO         ",  &
  "C2H5CHO -> C2H6 + CO          ",  &
  "C2H5CHO -> C2H4 + HCHO        ",  &
  "C2H5CHO -> CH3 + CH2CHO       ",  &
  (blank, i=1, npd_products-4)    ,  & ! C2H5CHO
  "HOCH2CHO -> CH2OH + HCO       ",  &
  "HOCH2CHO -> CH3OH + CO        ",  &
  "HOCH2CHO -> OH + CH2CHO       ",  &
  "HOCH2CHO -> HOCH2CO + H       ",  &
  (blank, i=1, npd_products-4)    ,  & ! HOCH2CHO
  "C2H5COCH3 -> CH3CO + C2H5     ",  &
  "C2H5COCH3 -> C2H5CO + CH3     ",  &
  "C2H5COCH3 -> C2H5 + CO + CH3  ",  &
  (blank, i=1, npd_products-3)    ,  & ! C2H5COCH3
  "CH3C(O)CH=CH2 -> CH3-CH=CH2 + CO  ", &
  "CH3C(O)CH=CH2 -> CH=CH2 + CH3C(O) ", &
  "CH3C(O)CH=CH2 -> CH=CH2C(O) + CH3 ", &
  (blank, i=1, npd_products-3)        , & ! CH3C(O)CH=CH2 (MVK)
  "CH2=C(CH3)CHO -> CH2=CCH3 + CHO   ", &
  "CH2=C(CH3)CHO -> C3H6 + CO        ", &
  "CH2=C(CH3)CHO -> H + CH2=C(CH3)CO ", &
  (blank, i=1, npd_products-3)        , & ! CH2=C(CH3)CHO (MACR)
  "CH3C(O)OONO2 -> CH3C(O)OO + NO2   ", &
  "CH3C(O)OONO2 -> CH3C(O)O + NO3    ", &
  (blank, i=1, npd_products-2)        , & ! CH3C(O)OONO2 (PAN)
  "CH3ONO2 -> CH3O + NO2             ", &
  "CH3ONO2 -> HCHO + HONO            ", &
  "CH3ONO2 -> HCHO + NO + OH         ", &
  "CH3ONO2 -> CH3ONO + O(3P)         ", &
  "CH3ONO2 -> CH3 + NO3              ", &
  "CH3ONO2 -> CH2ONO2 + H            ", &
  "CH3ONO2 -> CH3O + NO + O(3P)      ", &
  "CH3ONO2 -> CH3ONO + O(1D)         ", &
  (blank, i=1, npd_products-8)        , & ! CH3ONO2
  "C2H3 -> C2H2 + H                  ", &
  (blank, i=1, npd_products-1)        , & ! C2H3
  "C2H4 -> C2H2 + H2                 ", &
  "C2H4 -> C2H2 + H + H              ", &
  (blank, i=1, npd_products-2)        , & ! C2H4
  "OH -> O(1D) + H                   ", &
  (blank, i=1, npd_products-1)        , & ! OH
  "HCO -> H + CO                     ", &
  (blank, i=1, npd_products-1)        , & ! HCO
  "N2O4 -> NO2 + NO2                 ", &
  (blank, i=1, npd_products-1)        , & ! N2O4
  "C2N2 -> C2 + N2                   ", &
  (blank, i=1, npd_products-1)        , & ! C2N2
  "N2H4 -> N2H3 + H                  ", &
  (blank, i=1, npd_products-1)        , & ! N2H4
  "N2O3 -> NO2 + NO                  ", &
  (blank, i=1, npd_products-1),      & ! N2O3
  (blank, i=1, npd_products),        & ! Si
  (blank, i=1, npd_products),        & ! SiO
  (blank, i=1, npd_products),        & ! SiO2
  (blank, i=1, npd_products),        & ! Mg
  (blank, i=1, npd_products),        & ! Mg2
  (blank, i=1, npd_products),        & ! MgO
  (blank, i=1, npd_products),        & ! TiO2
  (blank, i=1, npd_products),        & ! Fe
  (blank, i=1, npd_products),        & ! FeO
  (blank, i=1, npd_products),        & ! Ca
  (blank, i=1, npd_products),        & ! CaO
  (blank, i=1, npd_products),        & ! AlO
  (blank, i=1, npd_products),        & ! Na2
  (blank, i=1, npd_products),        & ! NaO
  (blank, i=1, npd_products),        & ! NaOH
  (blank, i=1, npd_products),        & ! KOH
  (blank, i=1, npd_products),        & ! H-
  (blank, i=1, npd_products),        & ! PS
  (blank, i=1, npd_products),        & ! PO
  (blank, i=1, npd_products),        & ! PN
  (blank, i=1, npd_products),        & ! CH3SH
  (blank, i=1, npd_products),        & ! CH3S
  (blank, i=1, npd_products),        & ! C2H6S
  (blank, i=1, npd_products),        & ! C2H6S2
  (blank, i=1, npd_products),        & ! C3H4
  (blank, i=1, npd_products),        & ! C4H3
  (blank, i=1, npd_products),        & ! SiH4
  (blank, i=1, npd_products),        & ! S2
  (blank, i=1, npd_products),        & ! SF6
  (blank, i=1, npd_products),        & ! CS2
  (blank, i=1, npd_products),        & ! S8
  (blank, i=1, npd_products),        & ! CN
  (blank, i=1, npd_products),        & ! CH3Cl
  (blank, i=1, npd_products),        & ! CH3F
  (blank, i=1, npd_products)         & ! CH3Br
  ], shape=[npd_products, npd_gases] )

! Name used by UKCA for photolysis pathway
CHARACTER(LEN=56), PARAMETER :: photol_fldname(0:npd_products, npd_gases) &
  = RESHAPE([CHARACTER(LEN=56) ::    &
  (blank, i=0, 1),                   & ! H2O
  "jh2o                          ",  & ! H2O -> OH(X2Pi) + H
  (blank, i=3, npd_products),        & ! H2O
  (blank, i=0, 0),                   & ! CO2
  "jco2                          ",  & ! CO2 -> CO + O(3P)
  (blank, i=2, npd_products),        & ! CO2
  (blank, i=0, 0),                   & ! O3
  "jo3b                          ",  & ! O3 -> O(3P) + O2(X3Sigmag-)
  (blank, i=2, 3),                   & ! O3
  "jo3a                          ",  & ! O3 -> O(1D) + O2(X3Sigmag-)
  (blank, i=5, npd_products),        & ! O3
  (blank, i=0, npd_products),        & ! N2O
  (blank, i=0, npd_products),        & ! CO
  (blank, i=0, npd_products),        & ! CH4
  "jo2                           ",  & ! O2 -> Unspecified
  "jo2                           ",  & ! O2 -> O(3P) + O(3P)
  "jo2b                          ",  & ! O2 -> O(3P) + O(1D)
  (blank, i=3, npd_products),        & ! O2
  (blank, i=0, npd_products),        & ! NO
  (blank, i=0, npd_products),        & ! SO2
  (blank, i=0, 0),                   & ! NO2
  "jno2                          ",  & ! NO2 -> NO + O(3P)
  (blank, i=2, npd_products),        & ! NO2
  (blank, i=0, npd_products),        & ! NH3
  (blank, i=0, 0),                   & ! HNO3
  "jhono2                        ",  & ! HNO3 -> OH + NO2
  (blank, i=2, npd_products),        & ! HNO3
  (blank, i=0, npd_products),        & ! N2
  (blank, i=0, npd_products),        & ! CFC11
  (blank, i=0, npd_products),        & ! CFC12
  (blank, i=0, npd_products),        & ! CFC113
  (blank, i=0, npd_products),        & ! HCFC22
  (blank, i=0, npd_products),        & ! HFC125
  (blank, i=0, npd_products),        & ! HFC134a
  (blank, i=0, npd_products),        & ! CFC114
  (blank, i=0, npd_products),        & ! TiO
  (blank, i=0, npd_products),        & ! VO
  (blank, i=0, npd_products),        & ! H2
  (blank, i=0, npd_products),        & ! He
  (blank, i=0, npd_products),        & ! OCS
  (blank, i=0, npd_products),        & ! Na
  (blank, i=0, npd_products),        & ! K
  (blank, i=0, npd_products),        & ! FeH
  (blank, i=0, npd_products),        & ! CrH
  (blank, i=0, npd_products),        & ! Li
  (blank, i=0, npd_products),        & ! Rb
  (blank, i=0, npd_products),        & ! Cs
  (blank, i=0, npd_products),        & ! PH3
  (blank, i=0, npd_products),        & ! C2H2
  (blank, i=0, npd_products),        & ! HCN
  (blank, i=0, npd_products),        & ! H2S
  (blank, i=0, npd_products),        & ! Ar
  (blank, i=0, npd_products),        & ! Dry air
  (blank, i=0, npd_products),        & ! O
  (blank, i=0, npd_products),        & ! N
  (blank, i=0, 0),                   & ! NO3
  "jno3a                         ",  & ! NO3 -> NO + O2
  "jno3b                         ",  & ! NO3 -> NO2 + O(3P)
  (blank, i=3, npd_products),        & ! NO3
  (blank, i=0, 0),                   & ! N2O5
  "jn2o5                         ",  & ! N2O5 -> NO3 + NO2
  (blank, i=2, npd_products),        & ! N2O5
  (blank, i=0, 0),                   & ! HONO
  "jhono                         ",  & ! HONO -> OH + NO
  (blank, i=2, npd_products),        & ! HONO
  "jpna                          ",  & ! HO2NO2 -> Unspecified
  "jpna67                        ",  & ! HO2NO2 -> HO2 + NO2
  "jpna33                        ",  & ! HO2NO2 -> OH + NO3
  (blank, i=3, npd_products),        & ! HO2NO2
  (blank, i=0, 0),                   & ! H2O2
  "jh2o2                         ",  & ! H2O2 -> OH + OH
  (blank, i=2, npd_products),        & ! H2O2
  (blank, i=0, npd_products),        & ! C2H6
  (blank, i=0, npd_products),        & ! CH3
  "jhchoa                        ",  & ! H2CO -> Unspecified
  (blank, i=1, 1),                   & ! H2CO
  "jhchob                        ",  & ! H2CO -> H2 + CO
  (blank, i=3, npd_products),        & ! H2CO
  (blank, i=0, npd_products),        & ! HO2
  (blank, i=0, npd_products),        & ! HDO
  (blank, i=0, npd_products),        & ! HCl
  (blank, i=0, npd_products),        & ! HF
  (blank, i=0, npd_products),        & ! cis-OSSO
  (blank, i=0, npd_products),        & ! trans-OSSO
  (blank, i=0, npd_products),        & ! OSO-S
  "jaceta                        ",  & ! CH3CHO -> Unspecified
  (blank, i=1, 1),                   & ! CH3CHO
  "jacetb                        ",  & ! CH3CHO -> CH4 + CO
  (blank, i=3, npd_products),        & ! CH3CHO
  "jmhp                          ",  & ! CH3OOH -> Unspecified
  (blank, i=1, npd_products),        & ! CH3OOH
  "jaceto                        ",  & ! CH3COCH3 -> Unspecified
  (blank, i=1, npd_products),        & ! CH3COCH3
  "jmkal                         ",  & ! CH3COCHO -> Unspecified
  (blank, i=1, npd_products),        & ! CH3COCHO
  (blank, i=0, npd_products),        & ! CHOCHO
  "jetcho                        ",  & ! C2H5CHO -> Unspecified
  (blank, i=1, npd_products),        & ! C2H5CHO
  (blank, i=0, npd_products),        & ! HOCH2CHO
  (blank, i=0, npd_products),        & ! C2H5COCH3
  "jmvk                          ",  & ! MVK -> Unspecified
  (blank, i=1, npd_products),        & ! MVK
  "jmacr                         ",  & ! MACR -> Unspecified
  (blank, i=1, npd_products),        & ! MACR
  (blank, i=0, 0),                   & ! PAN
  "jpan                          ",  & ! CH3C(O)OONO2 -> CH3C(O)OO + NO2
  (blank, i=2, npd_products),        & ! PAN
  "jmena                         ",  & ! CH3ONO2 -> Unspecified
  (blank, i=1, npd_products),        & ! CH3ONO2
  (blank, i=0, npd_products),        & ! C2H3
  (blank, i=0, npd_products),        & ! C2H4
  (blank, i=0, npd_products),        & ! OH
  (blank, i=0, npd_products),        & ! HCO
  (blank, i=0, npd_products),        & ! N2O4
  (blank, i=0, npd_products),        & ! C2N2
  (blank, i=0, npd_products),        & ! N2H4
  (blank, i=0, npd_products),        & ! N2O3
  (blank, i=0, npd_products),        & ! Si
  (blank, i=0, npd_products),        & ! SiO
  (blank, i=0, npd_products),        & ! SiO2
  (blank, i=0, npd_products),        & ! Mg
  (blank, i=0, npd_products),        & ! Mg2
  (blank, i=0, npd_products),        & ! MgO
  (blank, i=0, npd_products),        & ! TiO2
  (blank, i=0, npd_products),        & ! Fe
  (blank, i=0, npd_products),        & ! FeO
  (blank, i=0, npd_products),        & ! Ca
  (blank, i=0, npd_products),        & ! CaO
  (blank, i=0, npd_products),        & ! AlO
  (blank, i=0, npd_products),        & ! Na2
  (blank, i=0, npd_products),        & ! NaO
  (blank, i=0, npd_products),        & ! NaOH
  (blank, i=0, npd_products),        & ! KOH
  (blank, i=0, npd_products),        & ! H-
  (blank, i=0, npd_products),        & ! PS
  (blank, i=0, npd_products),        & ! PO
  (blank, i=0, npd_products),        & ! PN
  (blank, i=0, npd_products),        & ! CH3SH
  (blank, i=0, npd_products),        & ! CH3S
  (blank, i=0, npd_products),        & ! C2H6S
  (blank, i=0, npd_products),        & ! C2H6S2
  (blank, i=0, npd_products),        & ! C3H4
  (blank, i=0, npd_products),        & ! C4H3
  (blank, i=0, npd_products),        & ! SiH4
  (blank, i=0, npd_products),        & ! S2
  (blank, i=0, npd_products),        & ! SF6
  (blank, i=0, npd_products),        & ! CS2
  (blank, i=0, npd_products),        & ! S8
  (blank, i=0, npd_products),        & ! CN
  (blank, i=0, npd_products),        & ! CH3Cl
  (blank, i=0, npd_products),        & ! CH3F
  (blank, i=0, npd_products)         & ! CH3Br
  ], shape=[npd_products+1, npd_gases] )

! Threshold wavelength defining energy required for photolysis
REAL (RealK), PARAMETER :: threshold_wavelength(npd_products, npd_gases) &
  = RESHAPE ( [REAL(RealK) ::       &
  246.0E-09_RealK,                  & ! H2O -> O(3P) + H2
  242.0E-09_RealK,                  & ! H2O -> OH(X2Pi) + H
  175.0E-09_RealK,                  & ! H2O -> O(1D) + H2
  134.0E-09_RealK,                  & ! H2O -> OH(A2Sigma+) + H
  129.0E-09_RealK,                  & ! H2O -> O(3P) + H + H
  (0.0_RealK, i=1, npd_products-5), & ! H2O
  227.5E-09_RealK,                  & ! CO2 -> CO + O(3P) : Heubner 92
  167.1E-09_RealK,                  & ! CO2 -> CO + O(1D) : Heubner 92
  128.6E-09_RealK,                  & ! CO2 -> CO + O(1S) : Heubner 92
  108.2E-09_RealK,                  & ! CO2 -> CO(a3Pi) + O(3P) : Heubner 92
  89.922E-09_RealK,                 & ! CO2 -> CO2+ : Heubner 92
  65.026E-09_RealK,                 & ! CO2 -> CO + O+ : Heubner 92
  63.693E-09_RealK,                 & ! CO2 -> CO+ + O : Heubner 92
  54.655E-09_RealK,                 & ! CO2 -> O2 + C+ : Heubner 92
  (0.0_RealK, i=1, npd_products-8), & ! CO2
  1180.0E-09_RealK,                 & ! O3 -> O(3P) + O2(X3Sigmag-)
   612.0E-09_RealK,                 & ! O3 -> O(3P) + O2(a1Deltag)
   463.0E-09_RealK,                 & ! O3 -> O(3P) + O2(b1Sigmag+)
   411.0E-09_RealK,                 & ! O3 -> O(1D) + O2(X3Sigmag-)
   310.0E-09_RealK,                 & ! O3 -> O(1D) + O2(a1Deltag)
   267.0E-09_RealK,                 & ! O3 -> O(1D) + O2(b1Sigmag+)
   201.0E-09_RealK,                 & ! O3 -> 3 O(3P)
   196.0E-09_RealK,                 & ! O3 -> O(1S) + O2(a1Deltag)
  (0.0_RealK, i=1, npd_products-8), & ! O3
  336.0E-09_RealK,                  & ! N2O -> N2 + O(1D)
  713.0E-09_RealK,                  & ! N2O -> N2 + O(3P)
  248.0E-09_RealK,                  & ! N2O -> N(4S) + NO(2Pi)
  210.0E-09_RealK,                  & ! N2O -> N2 + O(1S)
  (0.0_RealK, i=1, npd_products-4), & ! N2O
  111.78E-09_RealK,                 & ! CO -> CO + O(3P) :Huebner 92
  (0.0_RealK, i=1, npd_products-1), & ! CO
  277.0E-09_RealK,                  & ! CH4 -> CH3 + H
  237.3E-09_RealK,                  & ! CH4 -> CH2(1) + H2
  132.2E-09_RealK,                  & ! CH4 -> CH2(3) + H + H
  137.0E-09_RealK,                  & ! CH4 -> CH + H2 + H
  (0.0_RealK, i=1, npd_products-4), & ! CH4 :Huebner 92
  242.3E-09_RealK,                  & ! O2 -> O(3P) + O(3P)
  175.0E-09_RealK,                  & ! O2 -> O(3P) + O(1D)
  137.0E-09_RealK,                  & ! O2 -> O(1D) + O(1D)
  132.0E-09_RealK,                  & ! O2 -> O(3P) + O(1S)
  110.0E-09_RealK,                  & ! O2 -> O(1D) + O(1S)
  102.78E-09_RealK,                 & ! O2 -> O2+
   66.2E-09_RealK,                  & ! O2 -> O+ + O
  (0.0_RealK, i=1, npd_products-7), & ! O2
  191.0E-09_RealK,                  & ! NO -> O(3P) + N(4S) :Huebner 92
  (0.0_RealK, i=1, npd_products-1), & ! NO
  218.7E-09_RealK,                  & ! SO2 -> SO + O(3P) : Becker 95
  (0.0_RealK, i=1, npd_products-1), & ! SO2
  398.0E-09_RealK,                  & ! NO2 -> NO + O(3P)
  244.0E-09_RealK,                  & ! NO2 -> NO + O(1D)
  (0.0_RealK, i=1, npd_products-2), & ! NO2
  279.8E-09_RealK,                  & ! NH3 -> NH2 + H :Huebner
  (0.0_RealK, i=1, npd_products-1), & ! NH3
  604.0E-09_RealK,                  & ! HNO3 -> OH + NO2
  393.0E-09_RealK,                  & ! HNO3 -> HONO + O(3P)
  278.0E-09_RealK,                  & ! HNO3 -> H + NO3
  381.0E-09_RealK,                  & ! HNO3 -> OH + NO2*(12B2)
  242.0E-09_RealK,                  & ! HNO3 -> HONO + O(1D)
  216.0E-09_RealK,                  & ! HNO3 -> HONO (a3A)+ O(3P)
  (0.0_RealK, i=1, npd_products-6), & ! HNO3 : IUPAC PNOx2
   98.6E-09_RealK,                  & ! N2 -> N + N
   79.8E-09_RealK,                  & ! N2 -> N2+
   51.0E-09_RealK,                  & ! N2 -> N+ + N
  (0.0_RealK, i=1, npd_products-3), & ! N2
  (0.0_RealK, i=1, npd_products),   & ! CFC11
  (0.0_RealK, i=1, npd_products),   & ! CFC12
  (0.0_RealK, i=1, npd_products),   & ! CFC113
  (0.0_RealK, i=1, npd_products),   & ! HCFC22
  (0.0_RealK, i=1, npd_products),   & ! HFC125
  (0.0_RealK, i=1, npd_products),   & ! HFC134a
  (0.0_RealK, i=1, npd_products),   & ! CFC114
  (0.0_RealK, i=1, npd_products),   & ! TiO
  (0.0_RealK, i=1, npd_products),   & ! VO
  276.885E-09_RealK,                & ! H2 -> H + H :Huebner 92
  (0.0_RealK, i=1, npd_products-1), & ! H2
  (0.0_RealK, i=1, npd_products),   & ! He
  388.0E-09_RealK,                  & ! OCS -> CO + S(3P)
  285.0E-09_RealK,                  & ! OCS -> CO + S(1D)
  209.0E-09_RealK,                  & ! OCS -> CO + S(1S)
  (0.0_RealK, i=1, npd_products-3), & ! OCS
  (0.0_RealK, i=1, npd_products),   & ! Na
  (0.0_RealK, i=1, npd_products),   & ! K
  (0.0_RealK, i=1, npd_products),   & ! FeH
  (0.0_RealK, i=1, npd_products),   & ! CrH
  (0.0_RealK, i=1, npd_products),   & ! Li
  (0.0_RealK, i=1, npd_products),   & ! Rb
  (0.0_RealK, i=1, npd_products),   & ! Cs
  (0.0_RealK, i=1, npd_products),   & ! PH3
  230.6E-09_RealK,                  & ! C2H2 -> C2H + H :Huebner 92
  (0.0_RealK, i=1, npd_products-1), & ! C2H2
  195.0E-09_RealK,                  & ! HCN -> CN + H :Huebner 92
  (0.0_RealK, i=1, npd_products-1), & ! HCN
  (0.0_RealK, i=1, npd_products),   & ! H2S
  (0.0_RealK, i=1, npd_products),   & ! Ar
  (0.0_RealK, i=1, npd_products),   & ! Dry air
   91.25E-09_RealK,                 & ! O -> O+(4S)
   73.18E-09_RealK,                 & ! O -> O+(2D)
   66.58E-09_RealK,                 & ! O -> O+(2P)
   43.50E-09_RealK,                 & ! O -> O+(4Pe)
   31.00E-09_RealK,                 & ! O -> O+(2Pe)
   24.80E-09_RealK,                 & ! O -> O++
   12.179E-09_RealK,                & ! O -> O+++
  (0.0_RealK, i=1, npd_products-7), & ! O
   85.92E-09_RealK,                 & ! N -> N+
   28.00E-09_RealK,                 & ! N -> N++
  (0.0_RealK, i=1, npd_products-2), & ! N
   7320.0E-09_RealK,                & ! NO3 -> NO + O2 : JPL 19-5
   574.0E-09_RealK,                 & ! NO3 -> NO2 + O(3P) : JPL 19-5
  (0.0_RealK, i=1, npd_products-2), & ! NO3
   1255.0E-09_RealK,                & ! N2O5 -> NO3 + NO2 : JPL 19-5
   298.0E-09_RealK,                 & ! N2O5 -> NO3 + NO + O(3P) : JPL 19-5
  (0.0_RealK, i=1, npd_products-2), & ! N2O5
   579.0E-09_RealK,                 & ! HONO -> OH + NO : JPL 19-5
   362.0E-09_RealK,                 & ! HONO -> H + NO2 : JPL 19-5
  (0.0_RealK, i=1, npd_products-2), & ! HONO
  1207.0E-09_RealK,                 & ! HO2NO2 -> HO2 + NO2 : JPL 19-5
   726.0E-09_RealK,                 & ! HO2NO2 -> OH + NO3 : JPL 19-5
   713.0E-09_RealK,                 & ! HO2NO2 -> O(3P) + HNO3 : JPL 19-5
   393.0E-09_RealK,                 & ! HO2NO2 -> H + NO2 + O2 : JPL 19-5
   339.0E-09_RealK,                 & ! HO2NO2 -> HO2 + NO + O(3P) : JPL 19-5
   321.0E-09_RealK,                 & ! HO2NO2 -> OH + NO2 + O(3P) : JPL 19-5
   201.0E-09_RealK,                 & ! HO2NO2 -> H + O(3P) + NO3 : JPL 19-5
   911.0E-09_RealK,                 & ! HO2NO2 -> HONO + O2(1Sigma) : JPL 19-5
   1744.0E-09_RealK,                & ! HO2NO2 -> HONO + O2(1Lambda) : JPL 19-5
  (0.0_RealK, i=1, npd_products-9), & ! HO2NO2
   557.0E-09_RealK,                 & ! H2O2 -> OH + OH : JPL 19-5
   359.0E-09_RealK,                 & ! H2O2 -> H2O + O(1D) : JPL 19-5
   324.0E-09_RealK,                 & ! H2O2 -> H + HO2 : JPL 19-5
  (0.0_RealK, i=1, npd_products-3), & ! H2O2
   874.3E-09_RealK,                 & ! C2H6 -> C2H4 + H2 : Huebner 92
   290.0E-09_RealK,                 & ! C2H6 -> C2H4 + H + H :Huebner 92/Lias 70
   428.7E-09_RealK,                 & ! C2H6 -> C2H2 + H2 + H2 :Chang 2020
   272.6E-09_RealK,                 & ! C2H6 -> CH4 + CH2(1) :Huebner 92
   322.0E-09_RealK,                 & ! C2H6 -> CH3 + CH3 :Huebner 92
  (0.0_RealK, i=1, npd_products-5), & ! C2H6
   216.0E-09_RealK,                 & ! CH3 -> CH2(1) + H
  (0.0_RealK, i=1, npd_products-1), & ! CH3- Venot 2012-highest non zero qy
   330.0E-09_RealK,                 & ! H2CO -> H + HCO : JPL 19-5
   361.0E-09_RealK,                 & ! H2CO -> H2 + CO : JPL 19-5
   283.0E-09_RealK,                 & ! H2CO -> H + H + CO : JPL 19-5
  (0.0_RealK, i=1, npd_products-3), & ! H2CO
   438.0E-09_RealK,                 & ! HO2 -> OH + O(3P) : JPL 19-5
   259.0E-09_RealK,                 & ! HO2 -> OH + O(1D) : JPL 19-5
  (0.0_RealK, i=1, npd_products-2), & ! HO2
  (0.0_RealK, i=1, npd_products),   & ! HDO
  (0.0_RealK, i=1, npd_products),   & ! HCl
  (0.0_RealK, i=1, npd_products),   & ! HF
  (0.0_RealK, i=1, npd_products),   & ! cis-OSSO
  (0.0_RealK, i=1, npd_products),   & ! trans-OSSO
  (0.0_RealK, i=1, npd_products),   & ! OSO-S
   335.0E-09_RealK,                 & ! CH3CHO -> CH3 + HCO : JPL 19-5
   1.0_RealK,                       & ! CH3CHO -> CH4 + CO : JPL 19-5
   320.0E-09_RealK,                 & ! CH3CHO -> CH3CO + H : JPL 19-5
  (0.0_RealK, i=1, npd_products-3), & ! CH3CHO
   645.0E-09_RealK,                 & ! CH3OOH -> CH3O + OH : JPL 19-5
   410.0E-09_RealK,                 & ! CH3OOH -> CH3 + HO2 : JPL 19-5
   334.0E-09_RealK,                 & ! CH3OOH -> CH3O2 + H : JPL 19-5
   670.0E-09_RealK,                 & ! CH3OOH -> CH3OH + O(3P) : JPL 19-5
  (0.0_RealK, i=1, npd_products-4), & ! CH3OOH
   338.0E-09_RealK,                 & ! CH3COCH3 -> CH3CO + CH3 : JPL 19-5
   299.0E-09_RealK,                 & ! CH3COCH3 -> 2 CH3 + CO : JPL 19-5
  (0.0_RealK, i=1, npd_products-2), & ! CH3COCH3
   422.0E-09_RealK,                 & ! CH3COCHO -> CH3CO + HCO : JPL 19-5
   1.0_RealK,                       & ! CH3COCHO -> CH4 + 2 CO : JPL 19-5
   1.0_RealK,                       & ! CH3COCHO -> CH3CHO + CO : JPL 19-5
  (0.0_RealK, i=1, npd_products-3), & ! CH3COCHO
   399.0E-09_RealK,                 & ! CHOCHO -> HCO + HCO : JPL 19-5
   1.0_RealK,                       & ! CHOCHO -> H2 + 2 CO : JPL 19-5
   1.0_RealK,                       & ! CHOCHO -> HCHO + CO : JPL 19-5
   329.0E-09_RealK,                 & ! CHOCHO -> H + CO + HCO : JPL 19-5
  (0.0_RealK, i=1, npd_products-4), & ! CHOCHO
   341.0E-09_RealK,                 & ! C2H5CHO -> C2H5 + HCO : JPL 19-5
   1.0_RealK,                       & ! C2H5CHO -> C2H6 + CO : JPL 19-5
   926.0E-09_RealK,                 & ! C2H5CHO -> C2H4 + HCHO : JPL 19-5
   349.0E-09_RealK,                 & ! C2H5CHO -> CH3 + CH2CHO : JPL 19-5
  (0.0_RealK, i=1, npd_products-4), & ! C2H5CHO
   347.0E-09_RealK,                 & ! HOCH2CHO -> CH2OH + HCO
  1910.0E-09_RealK,                 & ! HOCH2CHO -> CH3OH + CO
   328.0E-09_RealK,                 & ! HOCH2CHO -> OH + CH2CHO
   314.0E-09_RealK,                 & ! HOCH2CHO -> HOCH2CO + H
  (0.0_RealK, i=1, npd_products-4), & ! HOCH2CHO
   342.0E-09_RealK,                 & ! C2H5COCH3 -> CH3CO + C2H5
   339.0E-09_RealK,                 & ! C2H5COCH3 -> C2H5CO + CH3
   303.0E-09_RealK,                 & ! C2H5COCH3 -> C2H5 + CO + CH3
  (0.0_RealK, i=1, npd_products-3), & ! C2H5COCH3 : Zborowska 21
   1.0_RealK,                       & ! CH3C(O)CH=CH2 -> CH3-CH=CH2 + CO
   1.0_RealK,                       & ! CH3C(O)CH=CH2 -> CH=CH2 + CH3C(O)
   1.0_RealK,                       & ! CH3C(O)CH=CH2 -> CH=CH2C(O) + CH3
  (0.0_RealK, i=1, npd_products-3), & ! CH3C(O)CH=CH2 (MVK) : JPL 19-5
   1.0_RealK,                       & ! CH2=C(CH3)CHO -> CH2=CCH3 + CHO
   1.0_RealK,                       & ! CH2=C(CH3)CHO -> C3H6 + CO
   1.0_RealK,                       & ! CH2=C(CH3)CHO -> H + CH2=C(CH3)CO
  (0.0_RealK, i=1, npd_products-3), & ! CH2=C(CH3)CHO (MACR) : JPL 19-5
  1004.0E-09_RealK,                 & ! CH3C(O)OONO2 -> CH3C(O)OO + NO2
   963.0E-09_RealK,                 & ! CH3C(O)OONO2 -> CH3C(O)O + NO3
  (0.0_RealK, i=1, npd_products-2), & ! CH3C(O)OONO2 (PAN) : IUPAC P21
   697.0E-09_RealK,                 & ! CH3ONO2 -> CH3O + NO2
   1.0_RealK,                       & ! CH3ONO2 -> HCHO + HONO
   497.0E-09_RealK,                 & ! CH3ONO2 -> HCHO + NO + OH
   391.0E-09_RealK,                 & ! CH3ONO2 -> CH3ONO + O(3P)
   344.0E-09_RealK,                 & ! CH3ONO2 -> CH3 + NO3
   294.0E-09_RealK,                 & ! CH3ONO2 -> CH2ONO2 + H
   250.0E-09_RealK,                 & ! CH3ONO2 -> CH3O + NO + O(3P)
   241.0E-09_RealK,                 & ! CH3ONO2 -> CH3ONO + O(1D)
  (0.0_RealK, i=1, npd_products-8), & ! CH3ONO2 : JPL 19-5
   237.0E-09_RealK,                 & ! C2H3 -> C2H2 + H
  (0.0_RealK, i=1, npd_products-1), & ! C2H3 : Venot 2012-highest non zero qy
   720.0E-09_RealK,                 & ! C2H4 -> C2H2 + H2
   196.0E-09_RealK,                 & ! C2H4 -> C2H2 + H + H
  (0.0_RealK, i=1, npd_products-2), & ! C2H4 : Huebner 92
   511.4E-09_RealK,                 & ! OH -> O(1D) + H
  (0.0_RealK, i=1, npd_products-1), & ! OH : Huebner 1992
   229.0E-09_RealK,                 & ! HCO -> H + CO
  (0.0_RealK, i=1, npd_products-1), & ! HCO : Venot 2012-highest non zero xsc
  453.0E-09_RealK,                  & ! N2O4 -> NO2 + NO2
  (0.0_RealK, i=1, npd_products-1), & ! N2O4 : Venot 2012-highest non zero xsc
  224.0E-09_RealK,                  & ! C2N2 -> C2 + N2
  (0.0_RealK, i=1, npd_products-1), & ! C2N2 : Venot 2012-highest non zero xsc
  290.0E-09_RealK,                  & ! N2H4 -> N2H3 + H
  (0.0_RealK, i=1, npd_products-1), & ! N2H4 : Venot 2012-highest non zero xsc
  398.0E-09_RealK,                  & ! N2O3 -> NO2 + NO
  (0.0_RealK, i=1, npd_products-1), & ! N2O3 : Venot 2012-highest non zero xsc
  (0.0_RealK, i=1, npd_products),   & ! Si
  (0.0_RealK, i=1, npd_products),   & ! SiO
  (0.0_RealK, i=1, npd_products),   & ! SiO2
  (0.0_RealK, i=1, npd_products),   & ! Mg
  (0.0_RealK, i=1, npd_products),   & ! Mg2
  (0.0_RealK, i=1, npd_products),   & ! MgO
  (0.0_RealK, i=1, npd_products),   & ! TiO2
  (0.0_RealK, i=1, npd_products),   & ! Fe
  (0.0_RealK, i=1, npd_products),   & ! FeO
  (0.0_RealK, i=1, npd_products),   & ! Ca
  (0.0_RealK, i=1, npd_products),   & ! CaO
  (0.0_RealK, i=1, npd_products),   & ! AlO
  (0.0_RealK, i=1, npd_products),   & ! Na2
  (0.0_RealK, i=1, npd_products),   & ! NaO
  (0.0_RealK, i=1, npd_products),   & ! NaOH
  (0.0_RealK, i=1, npd_products),   & ! KOH
  (0.0_RealK, i=1, npd_products),   & ! H-
  (0.0_RealK, i=1, npd_products),   & ! PS
  (0.0_RealK, i=1, npd_products),   & ! PO
  (0.0_RealK, i=1, npd_products),   & ! PN
  (0.0_RealK, i=1, npd_products),   & ! CH3SH
  (0.0_RealK, i=1, npd_products),   & ! CH3S
  (0.0_RealK, i=1, npd_products),   & ! C2H6S
  (0.0_RealK, i=1, npd_products),   & ! C2H6S2
  (0.0_RealK, i=1, npd_products),   & ! C3H4
  (0.0_RealK, i=1, npd_products),   & ! C4H3
  (0.0_RealK, i=1, npd_products),   & ! SiH4
  (0.0_RealK, i=1, npd_products),   & ! S2
  (0.0_RealK, i=1, npd_products),   & ! SF6
  (0.0_RealK, i=1, npd_products),   & ! CS2
  (0.0_RealK, i=1, npd_products),   & ! S8
  (0.0_RealK, i=1, npd_products),   & ! CN
  (0.0_RealK, i=1, npd_products),   & ! CH3Cl
  (0.0_RealK, i=1, npd_products),   & ! CH3F
  (0.0_RealK, i=1, npd_products)    & ! CH3Br
  ], shape=[npd_products, npd_gases] )

! Unless otherwise stated, data comes from JPL publication No. 15-10:
! Chemical Kinetics and Photochemical Data for Use in Atmospheric Studies
! Other references:
!  * JPL 19-5   : JPL publication No. 19-5
!  * Heubner 92 : Heubner et al (1992, p120) DOI: 10.1007/978-94-017-3023-5_1
!  * Becker 95 : Becker et al (1995) DOI: 10.1016/0301-0104(95)00114-4
!  * Zborowska 21 : Zborowska et al (2021) DOI: 10.1016/j.atmosenv.2021.118383
!  * IUPAC Task Group on Atmospheric Chemical Kinetic Data Evaluation:
!    * http://iupac.pole-ether.fr
!    * HNO3: PNOx2: https://iupac-aeris.ipsl.fr/datasheets/pdf/PNOx2.pdf
!    * PAN: P21: https://iupac-aeris.ipsl.fr/datasheets/pdf/P21.pdf
!  * Venot 2012 : Venot et al (2012) DOI: 10.1051/0004-6361/201219310
!  * Lias 70   : Lias et al (1970) DOI: 10.1063/1.1673226
!  * Chang 2020   : Chang et al (2020) DOI: 10.1039/D0SC01746A

END MODULE gas_list_pcf
