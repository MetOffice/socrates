#! /usr/bin/env python3


'''
This module contains the default Baf configuration class.
'''

import argparse
from typing import List

from fab.api import BuildConfig, ProfileFlags

from site_specific.default.setup_script_cray import setup_script_cray
from site_specific.default.setup_script_gnu import setup_script_gnu
from site_specific.default.setup_script_intel_classic import (
    setup_script_intel_classic)
from site_specific.default.setup_script_intel_llvm import (
    setup_script_intel_llvm)
from site_specific.default.setup_script_nvidia import setup_script_nvidia


class Config:
    '''
    This class is the default Configuration object for Baf builds.
    It provides several callbacks which will be called from the build
    scripts to allow site-specific customisations.
    '''

    def __init__(self) -> None:
        self._args: argparse.Namespace

    @property
    def args(self) -> argparse.Namespace:
        '''
        :returns: the command line options specified by the user.
        '''
        return self._args

    def get_valid_profiles(self) -> List[str]:
        '''
        Determines the list of all allowed compiler profiles. The first
        entry in this list is the default profile to be used. This method
        can be overwritten by site configs to add or modify the supported
        profiles.

        :returns: List of all supported compiler profiles.
        '''
        return ["rigorous", "debug", "safe", "high"]

    def define_command_line_options(self,
                                    parser: argparse.ArgumentParser) -> None:
        '''
        Callback in which additional, site-specific options can be added,
        and/or the the defaults for the parser can be changed.
        '''
        parser.set_defaults(mpi=False)
        parser.set_defaults(openmp=False)

    def handle_command_line_options(self, args: argparse.Namespace) -> None:
        '''
        Additional callback function executed once all command line
        options have been added. This is for example used to add
        Vernier profiling flags, which are site-specific.

        :param argparse.Namespace args: the command line options added in
        the site configs
        '''
        # Keep a copy of the args, so they can be used when
        # initialising compilers
        self._args = args

    def update_repos(self, dep_info):
        """
        This method is called by the main script to allow each site to
        replace the URLs of repos with e.g. local mirrors.
        """

        # A simplified example to use mirrors could be (which would
        # typically be implemented in a derived, site-specific class)
        # root = Path("/root/of/mirrors")
        # mirrors = {"git@github.com:MetOffice/casim.git": root / "casim",
        #            "git@github.com:MetOffice/jules.git": root / "jules",
        #             }
        # for dependency in dep_info.get_repo_names():
        #     repo_infos = dep_info.get_repo_info(dependency)
        #     for source_ref in repo_infos:
        #         if source_ref.source in mirrors:
        #             logger.info(f"Using mirror "
        #                         f"'{mirrors[source_ref.source]}' for "
        #                         f"'{source_ref.source}")
        #             source_ref.source = mirrors[source_ref.source]

    def update_toolbox(self, build_config: BuildConfig) -> None:
        '''
        Set the default compiler flags for the various compiler
        that are supported.

        :param build_config: the Fab build configuration instance
        '''
        # Define a base profile, which contains the common
        # compilation flags. This 'base' is not accessible to
        # the user, so it's not part of the profile list.
        ProfileFlags.define_profile("base")
        for profile in self.get_valid_profiles():
            ProfileFlags.define_profile(profile, inherit_from="base")

        self.setup_intel_classic(build_config)
        self.setup_intel_llvm(build_config)
        self.setup_gnu(build_config)
        self.setup_nvidia(build_config)
        self.setup_cray(build_config)

    def setup_cray(self, build_config: BuildConfig) -> None:
        '''
        This method sets up the Cray compiler and linker flags.
        For now call an external function, since it is expected that
        this configuration can be very lengthy (once we support
        compiler modes).

        :param build_config: the Fab build configuration instance
        '''
        setup_script_cray(build_config, self.args)

    def setup_gnu(self, build_config: BuildConfig) -> None:
        '''
        This method sets up the Gnu compiler and linker flags.
        For now call an external function, since it is expected that
        this configuration can be very lengthy (once we support
        compiler modes).

        :param build_config: the Fab build configuration instance
        '''
        setup_script_gnu(build_config, self.args)

    def setup_intel_classic(self, build_config: BuildConfig) -> None:
        '''
        This method sets up the Intel classic compiler and linker flags.
        For now call an external function, since it is expected that
        this configuration can be very lengthy (once we support
        compiler modes).

        :param build_config: the Fab build configuration instance
        '''
        setup_script_intel_classic(build_config, self.args)

    def setup_intel_llvm(self, build_config: BuildConfig) -> None:
        '''
        This method sets up the Intel LLVM compiler and linker flags.
        For now call an external function, since it is expected that
        this configuration can be very lengthy (once we support
        compiler modes).

        :param build_config: the Fab build configuration instance
        '''
        setup_script_intel_llvm(build_config, self.args)

    def setup_nvidia(self, build_config: BuildConfig) -> None:
        '''
        This method sets up the Nvidia compiler and linker flags.
        For now call an external function, since it is expected that
        this configuration can be very lengthy (once we support
        compiler modes).

        :param build_config: the Fab build configuration instance
        '''
        setup_script_nvidia(build_config, self.args)
