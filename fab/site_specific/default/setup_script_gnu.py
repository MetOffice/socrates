#!/usr/bin/env python3

'''This file contains a function that sets the default flags for all
GNU based compilers in the ToolRepository.

This function gets called from the default site-specific config file
'''

import argparse
from typing import cast

from fab.api import (BuildConfig, Category, Compiler, Linker,
                     ToolRepository)


def setup_script_gnu(build_config: BuildConfig,
                     args: argparse.Namespace) -> None:
    # pylint: disable=unused-argument
    '''Defines the default flags for all GNU compilers.

    :para build_config: the build config from which required parameters
        can be taken.
    :param args: all command line options
    '''

    tr = ToolRepository()
    gfortran = tr.get_tool(Category.FORTRAN_COMPILER, "gfortran")

    if not gfortran.is_available:
        gfortran = tr.get_tool(Category.FORTRAN_COMPILER, "mpif90-gfortran")
        if not gfortran.is_available:
            return
    gfortran = cast(Compiler, gfortran)

    gcc = tr.get_tool(Category.C_COMPILER, "gcc")
    if not gcc.is_available:
        gcc = tr.get_tool(Category.C_COMPILER, "mpif90-gcc")
        if not gcc.is_available:
            return
    gcc = cast(Compiler, gcc)

    # The base flags
    # ==============
    gfortran.add_flags(["-w", "-std=legacy", "-fallow-argument-mismatch"],
                       'base')

    # Rigorous
    # ========
    gfortran.add_flags(['-O0', '-Wall',
                        '-ffpe-trap=invalid,zero,overflow',
                        '-fcheck=all', '-finit-real=nan',
                        '-fimplicit-none',
                        '-Werror=character-truncation',
                        ], "rigorous")

    lfric_checks = ['-Werror=conversion', '-Werror=unused-variable',
                    '-Werror=unused-value']

    # Debug
    # =====
    # -std=f2018 does compile, even though it's listed in the original
    # makefile???
    gfortran.add_flags(["-O0", "-fcheck=all,no-recursion", "-fbacktrace",
                        "-g", "-Wtabs", "-Werror"], "debug")

    # Safe
    # ====
    gfortran.add_flags(["-O1"], "safe")

    # High
    # ====
    gfortran.add_flags(["-O2"], "high")

    # Set up the linker
    # =================z
    # This will implicitly affect all gfortran based linkers, e.g.
    # linker-mpif90-gfortran will use these flags as well.
    linker = tr.get_tool(Category.LINKER, f"linker-{gfortran.name}")
    linker = cast(Linker, linker)

    # This likely needs to be update for each site (e.g. adding paths)
    tr = ToolRepository()
    shell = tr.get_default(Category.SHELL)
    # We must remove the trailing new line, and create a list:
    nc_flibs = shell.run(additional_parameters=["-c", "nf-config --flibs"],
                         capture_output=True).strip().split()
    linker.add_lib_flags("netcdf", nc_flibs, silent_replace=True)
