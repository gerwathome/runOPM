# runOPM
[OPM](http://opm-project.org/) (The Open Porous Media initiative) is a group developing a set of open source tools that allow the scale-up and modeling of fluid flow in subsurface reservoirs.  The tools being developed use the industry standard Eclipse data format for input and output.  OPM is sponsered by a number of Norwegian organizations, including IRIS, Sintef and Statoil.

This module, along with other associated modules, is an attempt to develop a history match workflow that combines the use of the OPM tools with tools already existing in R. Particular effort is made to incorporate the excellent set of tools developed by the [DICE](http://dice.emse.fr/) and [ReDICE](http://www.redice-project.org/) consortia to improve complex computer models.

While the current work is at a very early stage, any comments or suggestions are certainly welcome.

For the module to be useful, the OPM tools must first be installed.  Detailed instructons for downloading and installing are available at the [project website](http://opm-project.org/?page_id=36).

This module may be installed from within R using Hadley Wickham's devtools package:

	install.packages("devtools")
	devtools::install_github("gerwathome/runOPM", build_vignettes = TRUE)

The vignette "Using OPM" documents basic workflows, as well as things that I am thinking about as I try to develop this package.