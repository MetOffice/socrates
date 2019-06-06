########################################################################
#                                                                      #
#             Makefile for Suite of Radiation Codes                    #
#                                                                      #
########################################################################


# Default target:
all: TARGET_GEN TARGET_CK TARGET_SCAT TARGET_AUX
cdl: TARGET_SCAT AUX_CDL
cdf: TARGET_GEN AUX_CDF TARGET_CK

# Compilers and suffixes:
include Mk_cmd

# Pattern Matching Rules
include Mk_pattern

#-----------------------------------------------------------------------
# Source code for each sub-package of the suite.
include Mk_mod_gen
include Mk_mod_gencore
include Mk_src_gencore
include Mk_src_gen
include Mk_src_aux
include Mk_src_ck
include Mk_src_radcore
include Mk_src_radctrl
include Mk_src_scat

#-----------------------------------------------------------------------
# Special Targets
include Mk_special

# ----------------------------------------------------------------------
# Dependencies (Mk_depend can be generated with the perl script mkdep)
include Mk_depend_cdf
include Mk_depend

#-----------------------------------------------------------------------
# Construction of Libraries.

radlib.$(LIBSUFFIX): \
	   $(MODULES_GEN:.f90=.o) $(MODULES_GENCORE:.f90=.o) \
	   $(SOURCE90_GENCORE:.f90=.o) \
	   $(SOURCE90_GEN:.f90=.o) $(SOURCE77_GEN:.f=.o) \
	   $(SOURCE90_AUX:.f90=.o) $(SOURCE77_AUX:.f=.o) \
	   $(SOURCE_RADCTRL:.f90=.o) $(SOURCE_RADCORE:.f90=.o) 
	$(LIBLINK) radlib.$(LIBSUFFIX) \
	   $(MODULES_GEN:.f90=.o) $(MODULES_GENCORE:.f90=.o) \
	   $(SOURCE90_GENCORE:.f90=.o) \
	   $(SOURCE90_GEN:.f90=.o) $(SOURCE77_GEN:.f=.o) \
	   $(SOURCE90_AUX:.f90=.o) $(SOURCE77_AUX:.f=.o) \
	   $(SOURCE_RADCTRL:.f90=.o) $(SOURCE_RADCORE:.f90=.o)

cklib.$(LIBSUFFIX): \
	   $(MODULES_GEN:.f90=.o) $(MODULES_GENCORE:.f90=.o) \
	   $(SOURCE90_GENCORE:.f90=.o) \
	   $(SOURCE90_GEN:.f90=.o) $(SOURCE77_GEN:.f=.o) \
	   $(SOURCE90_AUX:.f90=.o) $(SOURCE77_AUX:.f=.o) \
	   $(SOURCE_CK:.f90=.o)
	$(LIBLINK) cklib.$(LIBSUFFIX) \
	   $(MODULES_GEN:.f90=.o) $(MODULES_GENCORE:.f90=.o) \
	   $(SOURCE90_GENCORE:.f90=.o) \
	   $(SOURCE90_GEN:.f90=.o) $(SOURCE77_GEN:.f=.o) \
	   $(SOURCE90_AUX:.f90=.o) $(SOURCE77_AUX:.f=.o) \
	   $(SOURCE_CK:.f90=.o)

scatlib.$(LIBSUFFIX): \
	   $(MODULES_GEN:.f90=.o) $(MODULES_GENCORE:.f90=.o) \
	   $(SOURCE90_GENCORE:.f90=.o) \
	   $(SOURCE90_GEN:.f90=.o) $(SOURCE77_GEN:.f=.o) \
	   $(SOURCE90_SCAT:.f90=.o) $(SOURCE77_SCAT:.f=.o)
	$(LIBLINK) scatlib.$(LIBSUFFIX) \
	   $(MODULES_GEN:.f90=.o) $(MODULES_GENCORE:.f90=.o) \
	   $(SOURCE90_GENCORE:.f90=.o) \
	   $(SOURCE90_GEN:.f90=.o) $(SOURCE77_GEN:.f=.o) \
	   $(SOURCE90_SCAT:.f90=.o) $(SOURCE77_SCAT:.f=.o)

#-----------------------------------------------------------------------
# Construction of main programs.
include Mk_target_gen
include Mk_target_ck
include Mk_target_scat
include Mk_target_aux
