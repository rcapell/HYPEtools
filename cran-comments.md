## Resubmission 2
This is a resubmission. In this version we have made the following requested changes:
* Removed redundant "Functions to" from the beginning of DESCRIPTION
* Updated code to use TRUE and FALSE instead of T and F
* Removed default write path from PlotBasinOutput(), PlotBasinSummary(), and functions in functioncollection_export.R
* Updated MapRegionalSources(), PlotSubbasinRouting(), PlotMapPoints(), PlotMapOutput() functions to ensure that they do not write by default to the user's home filespace
* Updated ReadPar(), PlotBasinOutput(), and PlotBasinSummary() functions so that they do not change user options
* Updated BarplotUpstreamClases(), BoxplotSLCClasses(), PlotBasinOutput(), PlotBasinSummary(), PlotDurationCurve(), PlotMapOutput(), PlotMapPoints(), PlotAnnualRegime(), and PlotSimObsRegime() functions to restore par on exit of function
* Updated SumUpstreamArea() function to only use 2 cores by default

## Resubmission
This is a resubmission. In this version we have made the following requested changes:
* Removed the LICENSE file; The DESCRIPTION file now reads "License: LGPL-3"
* Removed cran-comments.md from the tarball by adding it to .Rbuildignore

## Test environments
* local OS (Windows & Linux)
* ubuntu-latest (on GitHub Actions): R release, devel, oldrel-1
* macOS-latest (on GitHub Actions): R release
* windows-latest (on GitHub Actions): R release

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs.

## Downstream dependencies
There are currently no downstream dependencies for HYPEtools.
