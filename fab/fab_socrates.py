#!/usr/bin/env python3
##############################################################################
# (c) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file COPYRIGHT
# which you should have received as part of this distribution
##############################################################################

'''
This module contains a Fab-based build script for Socrates.
'''

import argparse
from dataclasses import dataclass
import logging
from pathlib import Path
from typing import cast, Iterable, List, Optional, Union

from fab.fab_base.fab_base import FabBase
from fab.api import (AddFlags, Category, Compiler, Exclude, find_source_files,
                     git_checkout, Include, root_inc_files)


# Since we don't have a proper python package, we cannot use __name__, so set
# up an appropriate dotted name for logging:
logger = logging.getLogger("socrates.fab.fab_socrates")


class FabSocrates(FabBase):
    '''
    A class to build all Socrates binaries using Fab as base class.

    :param str name: name of the build.
    '''

    def __init__(self, name: str):
        super().__init__(name)

        self._root = Path(__file__).parents[1]
        this_dir = Path(".").resolve()

        # See if we can use relative paths, which makes for less
        # directory levels in fab-workspace
        if self._root == this_dir:
            # We are in the Socrates root directory
            self._root = Path(".")
        elif self._root == this_dir.parent:
            # We are one up from the Socrates root directory
            self._root = Path("..")


    def define_command_line_options(
            self,
            parser: Optional[argparse.ArgumentParser] = None
            ) -> argparse.ArgumentParser:
        '''
        Adds the additionally required command line options for the UM.

        :param Optional[argparse.ArgumentParser] parser: a pre-defined
        argument parser. If not, a new instance will be created.

        :returns: the argument parser with the UM specific options added.
        '''

        parser = super().define_command_line_options(parser)
        parser = cast(argparse.ArgumentParser, parser)
        self._root = Path(__file__).resolve().parents[1]

        um_config = parser.add_argument_group("Socrates options")
        um_config.add_argument(
            "--sbin", action="store_true", default=False,
            help="Build all binaries in sbin directory.")

        return parser

    def find_source_files_step(
            self,
            path_filters: Optional[Iterable[Union[Exclude, Include]]] = None
            ):
        '''
        Find the source file. Only scan the directories depending on
        the selected command line options, since parsing source files
        is rather slow.
        '''

        if path_filters is None:
            path_filters = []

        path_filters.extend([Exclude("seaalbedo.f"),
                             Exclude("adt_mitchell.f90"),
                             Exclude("run_disort.f"),
                             Exclude("correlated_k/read_nc.f90"),
                             ])

        if self.args.sbin:
            logger.info("Scanning 'sbin'.")
            find_source_files(self.config,
                              source_root=self._root / "sbin",
                              path_filters=path_filters)
        find_source_files(self.config,
                          source_root=self._root / "src",
                          path_filters=path_filters)
        root_inc_files(self.config, [".h", ".finc"])

    def define_preprocessor_flags_step(self) -> None:
        '''
        Defines the preprocessor flags.
        '''
        super().define_preprocessor_flags_step()

        flags = ['-I$output',
                 ]

        self.add_preprocessor_flags(flags)

    def compile_fortran_step(
            self,
            common_flags: Optional[List[str]] = None,
            path_flags: Optional[List[AddFlags]] = None
            ) -> None:

        fc = self.config.tool_box.get_tool(Category.FORTRAN_COMPILER)
        fc = cast(Compiler, fc)
        new_flags = []

        if common_flags:
            new_flags.extend(common_flags)

        super().compile_fortran_step(common_flags=new_flags,
                                     path_flags=path_flags)

    def analyse_step(self,
                     ignore_dependencies: Optional[Iterable[str]] = None,
                     find_programs: bool = False) -> None:
        super().analyse_step(ignore_dependencies=ignore_dependencies,
                             find_programs=True)

    def get_linker_flags(self) -> List[str]:
        return ["netcdf"]


# ==========================================================================
if __name__ == "__main__":

    # Initialise a top-level logger
    logger = logging.getLogger('um')
    logger.setLevel(logging.DEBUG)
    handler = logging.StreamHandler()
    formatter = logging.Formatter('%(levelname)s: %(name)s: %(message)s')
    handler.setFormatter(formatter)
    logger.addHandler(handler)

    fab_socrates = FabSocrates("socrates")
    fab_socrates.build()
