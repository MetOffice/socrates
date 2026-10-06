#! /usr/bin/env python3

"""
This module contains a template for setting up a site-specific configuration
file. It is modelled after the Met Office's setup on spice.
"""

import os
from pathlib import Path
from typing import cast

from fab.api import (
    BuildConfig,
    Category,
    Linker,
    ToolRepository,
)

from site_specific.default.config import Config as DefaultConfig

# This might not be required at all, or might need adjustment.
# ------------------------------------------------------------
# Set the path to the clang library on RHEL 9
import clang
import clang.cindex
clang.cindex.Config().set_library_file("/usr/lib64/libclang.so.20.1.8")


class Config(DefaultConfig):
    """
    Template for a site-specific setup file.
    """

    def __init__(self):
        super().__init__()
        tr = ToolRepository()
        # Or one of: intel-classic, intel-llvm, cray, nvidia
        tr.set_default_compiler_suite("gnu")

    def setup_gnu(self, build_config: BuildConfig) -> None:
        super().setup_gnu(build_config)

        tool_repo = ToolRepository()
        gcc = tool_repo.get_tool(Category.FORTRAN_COMPILER, "gfortran")

        # Example for setting up gcom (which is not needed
        # for socrates)
        # -------------------------------------------------
        # umdir = Path(os.environ.get("UMDIR"))
        # gcom = umdir / "gcom" / "gcom8.5"
        # if build_config.mpi:
        #     gcom /= "meto_azspice_gfortran12_mpi"
        # else:
        #     gcom /= "meto_azspice_gfortran12_serial"
        # gcom /= "build"
        # gcc.add_flags([f"-I{gcom}/include"])

        linker = tool_repo.get_tool(Category.LINKER, f"linker-{gcc.name}")
        linker = cast(Linker, linker)

        # Setting up linking option for a dependency:
        # linker.add_lib_flags("gcom", [f"-L{gcom}/lib", "-lgcom"],
        #                      silent_replace=True)

        # This likely needs to be update for each site (e.g. adding paths)
        tr = ToolRepository()
        shell = tr.get_default(Category.SHELL)
        # We must remove the trailing new line, and create a list:
        nc_flibs = shell.run(additional_parameters=["-c", "nf-config --flibs"],
                             capture_output=True).strip().split()
        linker.add_lib_flags("netcdf", nc_flibs, silent_replace=True)
