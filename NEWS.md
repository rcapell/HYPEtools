# HYPEtools 1.0.0 (2022-08-16)

* Added a `NEWS.md` file to track changes to the package.
* **CRAN** Initial Submission

# HYPEtools 0.5.1 (2021-08-27)

## Highlights

* Further leaflet plotting functionality, with PlotMapPoints
* Various improvements and bug fixes in existing functions

## Notable Shortcomings

* SortGeoData sorting with branches not yet fixed.

## New Functions

* ReadObs: Replaces ReadPTQObs and now supports more HYPE input data types
* CompareFiles: Compare imported HYPE files for differences, typically used in quality control
* PlotSubbasinRouting: Plot routing of subbasins for a HYPE model on an interactive map
* VariableInfo and VariableSearch: Lookup information (e.g. Name, Units) for a specific HYPE variable ID, or find HYPE variable information for a search term.
