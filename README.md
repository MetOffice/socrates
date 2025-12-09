
# SOCRATES - Suite Of Community RAdiative Transfer codes based on Edwards and Slingo

## Contributing Guidelines

Welcome!

The following links are here to help set clear expectations for everyone
contributing to this project. By working together under a shared understanding,
we can continuously improve the project while creating a friendly, inclusive
space for all contributors.

### Contributors Licence Agreement

Please see the
[Momentum Contributors Licence Agreement](https://github.com/MetOffice/Momentum/blob/main/CLA.md)

Agreement of the CLA can be shown by adding yourself to the CONTRIBUTORS file
alongside this one, and is a requirement for contributing to this project.

### Code of Conduct

Please be aware of and follow the
[Momentum Code of Coduct](https://github.com/MetOffice/Momentum/blob/main/docs/CODE_OF_CONDUCT.md)

### Working Practices

This project is managed as part of the Simulation Systems group of repositories.

Please follow the Simulation Systems
[Working Practices.](https://metoffice.github.io/simulation-systems/index.html)

Questions are encouraged in the Simulation Systems
[Discussions.](https://github.com/MetOffice/simulation-systems/discussions)

Please be aware of and follow the Simulation Systems
[AI Policy.](https://metoffice.github.io/simulation-systems/FurtherDetails/ai.html)

## What's included?

You should have received this package as a tar file containing the
directories: src/ make/ data/ examples/ idl/ python/ man/ sbin/ docs/

`src/` contains the source code in Fortran 95 (.f90) and a few remaining
in Fortran 77 (.f).

`make/` contains the Makefile which then accesses the various Mk_*
files.

`sbin/` contains scripts that can be used to run the fortran routines.

`man/` contains man pages for scripts in sbin/. For example, running
`man Cl_run_cdf` will give options for that script.

`examples/` and `data/` provide test input for the radiation code.
See the CONTENTS in each directory under examples/ for instructions.

`idl/` and `python/` contain scripts to generate atmospheric profiles etc
in netCDF format to be used as input for the radiation code (l_run_cdf).

`docs/` contain the user guide and technical guide for the ES code.

## Compiling the source code within the Met Office

For users within the Met Office simply run the command:

`./build_code`

to compile the entire suite. To setup your path to the executables
and man pages you should then source the following file:

`. ./set_rad_env`

Individual programs can also be compiled using the build_code script
(build_code will take as an argument the target to pass to the makefile).

For example, to build the routines that don't require netCDF:

`./build_code cdl`

To build just the two-stream/radiance code (netCDF version):

`./build_code l_run_cdf`

## Compiling the source code externally

For external users it should only be necessary to edit the file
make/Mk_cmd to allow compilation of the code on your system. FORTCOMP
and LINK can be changed to your local Fortran compiler. To use the netCDF
routines you must also change INCCDF_PATH and LIBCDF_PATH to point to
your local netCDF installation.

The following commands can then be run to build the suite and setup
your path to the executables and man pages:

`./build_code`
`. ./set_rad_env`

See previous section for building individual routines.

## Compilation of scripts in sbin

There are a small number of utilities in sbin/ which are written
in C and require compilation. A Makefile has been provided:

`cd $RAD_SCRIPT`
`make`

## Running the code

Once you have set your path to the man pages (see section 2/3) you can
find up-to-date instructions for running the following routines:

Two-stream and spherical harmonics radiance codes using netCDF or
text CDL input files:

`man Cl_run_cdf`
`man Cl_run_cdl`

A Mie scattering code for determining optical properties of aerosol
and cloud particles:

`man Cscatter`

A correlated-k code for the calculation of gaseous absorption
coefficients for the spectral files either directly from HITRAN
.par or .xsc databases or line-by-line absorption coefficients in
a netCDF input file:

`man Ccorr_k`

Auxillary routines for format conversion, interpolation etc:

`man Ccdf2cdl`
`man Ccdl2cdf`
`man Cinterp`

These scripts are a command line interface to interactive routines in
the bin/ directory. These routines may be run directly if desired (eg.
l_run_cdf).

It is very useful to study the examples/ directory for common usage
of the code.

## Tested compilers

The full suite has been tested with the following compilers:

Intel ifort 19
GCC gfortran 12.2

To use these compilers within the Met Office run, respectively:
`./build_code azure_ifort19`
`./build_code azure_gfortran12`

On the Monsoon3 collaboration machine:
`./build_code monsoon3_gfortran12`
