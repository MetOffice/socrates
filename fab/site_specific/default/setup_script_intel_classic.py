#!/usr/bin/env python3

'''This file contains a function that sets the default flags for all
Intel classic based compilers in the ToolRepository (ifort, icc).

This function gets called from the default site-specific config file
'''

import argparse
from typing import cast

from fab.api import (BuildConfig, Category, Compiler, Linker,
                     ToolRepository)


def setup_script_intel_classic(build_config: BuildConfig,
                               args: argparse.Namespace) -> None:
    # pylint: disable=unused-argument, too-many-locals
    '''Defines the default flags for all Intel classic compilers.

    :para build_config: the build config from which required parameters
        can be taken.
    :param args: all command line options
    '''

    tr = ToolRepository()
    ifort = tr.get_tool(Category.FORTRAN_COMPILER, "ifort")
    ifort = cast(Compiler, ifort)

    if not ifort.is_available:
        # This can happen if ifort is not in path (in spack environments).
        # To support this common use case, see if mpif90-ifort is available,
        # and initialise this otherwise.
        ifort = tr.get_tool(Category.FORTRAN_COMPILER, "mpif90-ifort")
        ifort = cast(Compiler, ifort)
        if not ifort.is_available:
            # Since some flags depends on version, the code below requires
            # that the intel compiler actually works.
            return

    icc = tr.get_tool(Category.C_COMPILER, "icc")
    icc = cast(Compiler, icc)
    if not icc.is_available:
        icc = tr.get_tool(Category.C_COMPILER, "mpicc-icc")
        icc = cast(Compiler, icc)

    # The base flags
    # ==============
    # The following flags will be applied to all modes:
    ifort.add_flags(["-stand", "f08"],               "base")
    ifort.add_flags(["-mcmodel=medium"],             "base")
    ifort.add_flags(["-assume", "nosource_include"], "base")
    ifort.add_flags(["-g", "-traceback"],            "base")

    icc.add_flags(["-g", "-traceback"],   "base")

    # Rigorous
    # ========
    ifort.add_flags(["-O1", "-no-vec"],      "rigorous")
    ifort.add_flags(["-fp-model", "strict"], "rigorous")
    ifort.add_flags(["-init=snan", "-init=array",
                     "-check", "all",
                     "-check", "noarg_temp_created",
                     "-init=huge"],          "rigorous")

    # Debug
    # =====
    # These overrides are based on Intel 16 and Intel 17 build configs.
    # Be aware that they have not yet been tuned for Intel 19.

    ifort.add_flags(["-O0", "-fp-model=precise"], "debug")

    # Safe
    # ====
    # These overrides are based on Intel 16 and Intel 17 build configs.
    # Be aware that they have not yet been tuned for Intel 19.
    ifort.add_flags(["-O2", "-no-vec", "-fp-model", "precise"], "safe")

    # High
    # ====
    ifort.add_flags(["-O3", "-no-vec", "-fp-model", "precise"], "high")

    # Set up the linker
    # =================
    # This will implicitly affect all ifort based linkers, e.g.
    # linker-mpif90-ifort will use these flags as well.
    linker = tr.get_tool(Category.LINKER, f"linker-{ifort.name}")
    linker = cast(Linker, linker)

    # This likely needs to be update for each site (e.g. adding paths)
