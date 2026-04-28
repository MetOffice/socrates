#!/usr/bin/env python3

'''This file contains a function that sets the default flags for the
Intel llvm-based compiler.

This function gets called from the default site-specific config file
'''

import argparse
from typing import cast

from fab.api import (BuildConfig, Category, Compiler, Linker,
                     ToolRepository)


def setup_script_intel_llvm(build_config: BuildConfig,
                            args: argparse.Namespace) -> None:
    # pylint: disable=unused-argument, too-many-locals
    '''Defines the default flags for the Intel llvm compilers.

    :para build_config: the build config from which required parameters
        can be taken.
    :param args: all command line options
    '''

    tr = ToolRepository()
    ifx = tr.get_tool(Category.FORTRAN_COMPILER, "ifx")
    ifx = cast(Compiler, ifx)

    if not ifx.is_available:
        # This can happen if ifx is not in path (in spack environments).
        # To support this common use case, see if mpif90-ifx is available,
        # and initialise this otherwise.
        ifx = tr.get_tool(Category.FORTRAN_COMPILER, "mpif90-ifx")
        ifx = cast(Compiler, ifx)
        if not ifx.is_available:
            # Since some flags depends on version, the code below requires
            # that the intel compiler actually works.
            return

    icx = tr.get_tool(Category.C_COMPILER, "icx")
    icx = cast(Compiler, icx)
    if not icx.is_available:
        icx = tr.get_tool(Category.C_COMPILER, "mpicc-icx")
        icx = cast(Compiler, icx)

    # The base flags
    # ==============
    # The following flags will be applied to all modes:
    ifx.add_flags(["-stand", "f08"],               "base")
    ifx.add_flags(["-mcmodel=medium"],             "base")
    ifx.add_flags(["-assume", "nosource_include"], "base")
    ifx.add_flags(["-g", "-traceback"],            "base")

    icx.add_flags(["-g", "-traceback"], "base")

    # Rigorous
    # ========
    ifx.add_flags(["-O0", "-no-vec"],      "rigorous")
    ifx.add_flags(["-fp-model", "strict"], "rigorous")
    ifx.add_flags(["-init=snan", "-init=array",
                   "-check", "all",
                   "-check", "noarg_temp_created"], "rigorous")

    # Debug
    # =====
    ifx.add_flags(["-O0", "-fp-model", "precise"], "debug")

    # Safe
    # ====
    # These overrides are based on Intel 16 and Intel 17 build configs.
    # Be aware that they have not yet been tuned for Intel 19.
    ifx.add_flags(["-O2", "-no-vec", "-fp-model", "precise"], "safe")

    # High
    # ====
    ifx.add_flags(["-O3", "-fp-model", "precise"], "high")

    # Set up the linker
    # =================
    # This will implicitly affect all ifx based linkers, e.g.
    # linker-mpif90-ifx will use these flags as well.
    linker = tr.get_tool(Category.LINKER, f"linker-{ifx.name}")
    linker = cast(Linker, linker)
