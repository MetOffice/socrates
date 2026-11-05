#!/usr/bin/env python3

'''
This file contains a function that sets the default flags for the Cray
compilers and linkers in the ToolRepository.

This function gets called from the default site-specific config file
'''

import argparse
from typing import cast

from fab.api import BuildConfig, Category, Compiler, Linker, ToolRepository


def setup_script_cray(build_config: BuildConfig,
                      args: argparse.Namespace) -> None:
    # pylint: disable=unused-argument
    '''
    Defines the default flags for ftn.

    :param build_config: the Fab build config instance from which
    required parameters can be taken.
    :type build_config: :py:class:`fab.BuildConfig`
    :param argparse.Namespace args: all command line options
    '''

    tr = ToolRepository()
    ftn = tr.get_tool(Category.FORTRAN_COMPILER, "crayftn-ftn")
    ftn = cast(Compiler, ftn)

    if not ftn.is_available:
        return

    # The base flags
    # ==============
    flags = ["-g", "-G0",               # debug, full dwarf information
             "-m", "0",                 # Also issue caution, note and comment
                                        # messages
             "-M", "E664,E7208,E7212",  # Turn into error:
                                        # 664: inconsistent dummy argument
                                        # 7208: explicit interface required
                                        # 7212: different dummy argument
                                        #       characteristics
             "-en",                     # generate Fortran standard messages
             "-ef",                     # use lowercase module names!Important!
             "-e", "m",
             "-hflex_mp=conservative",
             ]

    # Handle accelerator options:
    if args.openacc or args.openmp:
        host = args.host.lower()
    else:
        # Neither openacc nor openmp specified
        host = ""

    if args.openacc:
        if host == "gpu":
            flags.extend(["-h acc"])
        else:
            # CPU
            flags.extend(["-h acc"])
    elif args.openmp:
        if host == "gpu":
            flags.extend([])

    ftn.add_flags(flags, "base")

    # Full debug
    # ==========
    ftn.add_flags(["-Ktrap=fp",    # floating point checking
                   "-R", "bcdps",  # bounds, array shape, collapse,
                                   # pointer, string checking
                   "-O0"],         # No optimisation
                  "full-debug")
    if ftn.get_version() >= (15, 0):
        ftn.add_flags(["-G0"], "full-debug")
    else:
        ftn.add_flags(["-Gfast"], "full-debug")

    # Fast debug
    # ==========
    ftn.add_flags(["-O2"], "fast-debug")
    if ftn.get_version() >= (15, 0):
        ftn.add_flags(["-G2"], "fast-debug")
    else:
        ftn.add_flags(["-Gfast"], "fast-debug")

    # Production
    # ==========
    ftn.add_flags(["-O3"], "production")

    # Set up the linker
    # =================
    linker = tr.get_tool(Category.LINKER, f"linker-{ftn.name}")
    linker = cast(Linker, linker)
    linker.add_lib_flags("gcom", ["-lgcom"])
