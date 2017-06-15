# runOPM
[OPM](http://opm-project.org/) (The Open Porous Media initiative) is a group developing a set of open source tools that allow the scale-up and modeling of fluid flow in subsurface reservoirs.  The tools being developed use the industry standard Eclipse data format for input and output.  OPM is sponsered by a number of Norwegian organizations, including [IRIS](http://www.iris.no/research/energy), [Sintef](http://www.sintef.no/en/information-and-communication-technology-ict/applied-mathematics/computational-geoscience/#/) and [Statoil](https://www.statoil.com/).

This module, along with other associated modules, is an attempt to develop a history match workflow that combines the use of the OPM tools with tools already existing in R. Particular effort is made to incorporate the excellent set of tools developed by the [DICE](http://dice.emse.fr/) and [ReDICE](http://www.redice-project.org/) consortia to improve understanding of complex computer models.

While the current work is at a very early stage, any comments or suggestions are certainly welcome.

For the module to be useful, the OPM tools must first be installed.  Detailed instructons for downloading and installing are available at the [project website](http://opm-project.org/?page_id=36).

The library "libecl", developed by Statoil, is used for parsing the simulation output files.  This version of runOPM uses python wrappers to access libecl, so it is necessary to install python 2.7 as well as the python modules numpy and matplotlib.  Instructions for doing this are [here](http://opm-project.org/?page_id=197&page=6).

This module may be installed from within R using Hadley Wickham's devtools package:

	install.packages("devtools")
	devtools::install_github("gerwathome/runOPM", build_vignettes = TRUE)

The vignette "Using OPM" documents basic workflows, as well as things that I am thinking about as I try to develop this package.  At this point it tends to be more aspirational than documentation of actual functionality.  This vignette is also available to examine [here](https://gerwathome.github.io/runOPM/).

Intended coding syle: [Google R Style Guide](http://google.github.io/styleguide/Rguide.xml) 