## HYPEtools 1.0.0 (2022-08-22)
*New functions and bug fixes*

#### Highlights
* Initial CRAN Submission
* Added vignettes to package
* Updated functions to use sf package instead of sp package for spatial processing
* Fixed bug in SortGeoData()
* Various bug fixes and small improvements

#### Notable Shortcomings
* See issue tracker on HYPEtools' [GitHub development page](https://github.com/rcapell/HYPEtools/issues)

#### New Functions
* IsHeadwater: Quickly query vectors of HYPE SUBIDs to identify if they are headwater subbasins
* IsOutlet: Quickly query vectors of HYPE SUBIDs to identify if they are outlet subbasins
* ReadInfo: Import a HYPE model settings information file as list into R
* ReadUpdate: Import HYPE "update.txt" files as data frames into R

## HYPEtools 0.5.1 (2021-08-27)
*New functions and bug fixes*

#### Highlights
* Further leaflet plotting functionality, with PlotMapPoints
* Various improvements and bug fixes in existing functions

#### Notable Shortcomings
* SortGeoData sorting with branches not yet fixed.

#### New Functions
* ReadObs: Replaces ReadPTQObs and now supports more HYPE input data types
* CompareFiles: Compare imported HYPE files for differences, typically used in quality control
* PlotSubbasinRouting: Plot routing of subbasins for a HYPE model on an interactive map
* VariableInfo and VariableSearch: Lookup information (e.g. Name, Units) for a specific HYPE variable ID, or find HYPE variable information for a search term.

## HYPEtools 0.5.0 (2021-05-13)
*New functions and bug fixes*

#### Highlights
* PlotMapOutput now offers plotting to Leaflet maps
* Rudimentary netCDF file support in ReadTimeOutput (to be extended in coming releases)
* Targeted summary method for GeoData.txt files
* New S3 class for HypeGeoData with summary and merge methods

#### Notable Shortcomings
* SortGeoData sorting with branches not yet fixed.

#### New Functions
* HypeAttrAccess: HYPE data-specific attribute assign/access helpers
* OutletNearObs: Find outlet-near observations in HYPE observation data files
* ReadOutregions/WriteOutRegions: Read/Write 'Outregions.txt' files
* ReadSimass: Read 'simass.txt' files
* RescaleSLCClasses: Rescale SLC classes in a GeoData data frame

## HYPEtools 0.4.6 (2019-09-02)
*Bug fixes and improvements*

#### Highlights
* PlotBasinOutput and PlotBasinSummary now with default printing to pdf file, and new argument 'driver'
* PlotMapOutput now with argument 'col' instead of 'col.ramp.fun', accepts vectors of colors in addition to color ramp functions
* Numerous bug fixes and small improvements

## HYPEtools 0.4.5 (2018-02-20)
*Bug fixes and improvements*

#### Highlights
* Bug fixes in ReadMapOutput, WriteMapOutput, ReadTimeOutput, PlotMapOutput, PlotMapPoints, AnnualRegime
* Export functions updated to data.table's fwrite
* Better performance of AnnualRegime

#### New Functions
* ExtractStats: Calculate aggregated statistics from long-term time series, typically imported HYPE time output files

## HYPEtools 0.4.4 (2017-08-31)
*New functions and bug fixes*

#### Highlights
* Bug fixes

#### New Functions
* WriteMapOutput: Export a HYPE map output file

## HYPEtools 0.4.3 (2016-11-10)
*New functions and bug fixes*

#### Highlights
* Bug fixes, among them a critical one in PlotBasinSummary(), where upstream loads were partly wrong
* Optimised classes are no longer included as elements in optpar lists, but can now be inspected using OptimisedClasses()

#### New Functions
* CreateOptpar: Create a list representing a HYPE optpar.txt file from an imported par.txt file and a selection of parameters
* GwRetention: Calculate nutrient load retention fractions in groundwater parts of HYPE, i.e. after rootzone retention
* OptimisedClasses: Check which classes (land use or soil) of parameters in an imported optpar list are actually optimized, i.e. have a min/max range larger than zero
* ReadForcKey: Import HYPE ForcKey files
* WriteForcKey: Write HYPE ForcKey files
* WriteOptpar: Write a HYPE parameter optimization list to a file

## HYPEtools 0.4.2 (2016-10-05)
*Bug fixes*

#### Highlights
* Div. improvements to PlotBasinSummary()
* Div. bug fixes
* ReadDescription() updated, not backwards-compatible

## HYPEtools 0.4.1 (2016-09-02)
*New functions and bug fixes*

#### Highlights
* Bug fixes and improvements in various functions, changes to ReadDescription() are not backwards-compatible

#### New Functions
* BarplotUpstreamClasses: Plot upstream-averaged landscape property classes of one or several sub-basins as bar plots, e.g. land use or soils
* PlotSimObsRegime: Combined plot for annual regimes with box plot elements for observed variables and ribbon elements for simulated variables
* PlotBasinSummary: Plot a standard suite of plots summarising properties of a sub-basin including upstream area and model performance for discharge and concentrations of nutrients, sediment, and tracers
* CustomColors: Custom color ramp functions

## HYPEtools 0.4.0 (2016-06-12)
*New functions and bug fixes*

#### Highlights
* Much-improved import of large files, thanks to fread() from package data.table
  - ReadPTQobs(), ReadXobs(), ReadGeoData() updated
  - Option to import as data.table object to make use of the speedier data handling for that class
* New S3 classes HypeMultiVar and HypeXobs, improvements to HypeSingleVar
* Attribute preservation when sub-setting
* PBIAS(), Pearson's R (r()), and summary() methods for HypeSingleVar
* New map plot function PlotMapPoints(), mostly for quickly plotting performance measures at measurement points on a map
* New import function for text files with HYPE land use, soil, and crop class descriptions, for easier label plotting: ReadDescription() (the imported file is not a HYPE file, file structure described in ReadDescription help entry)
* New import function for optpar.txt files, imports optpar contents into a list structure which makes it easier to create and update optpar files
* New trivial import/export functions for HYPE files: ReadAllsim(), ReadDamData(), ReadGlacierData(), ReadSubass(), WriteCropData(), WriteDamData(), WriteMgmtData(), WritePointSourceData()
* Numerous bug fixes since last release

## HYPEtools 0.3.9 (2016-02-10)
*Bug fixes*

## HYPEtools 0.3.8 (2015-11-06)
*New functions and bug fixes*

#### Highlights
* Bug fixes in PlotBasinOutput() after testing the initial 0.3-7 release version
* New function ReadWsOutput() to import MC results
* First HYPE-specific S3 class HypeSingleVar, for (multiple) time and map output files, relates to the new import function
* New sub-setting and NSE calculation (for hydroGOF::NSE generic) methods for the new class
* Several other bug fixes and internal updates

## HYPEtools 0.3.7 (2015-10-20)
*New functions and bug fixes*

#### New Functions
* PlotBasinOutput: Plot a standard suite of time series plots from a basin output file, typically used for model performance inspection and/or during manual calibration
* PlotAnnualRegime: Convenience wrapper function for a combined line plot with polygon variation ranges

## HYPEtools 0.3.6 (2015-08-14)
*Bug fixes*

## HYPEtools 0.3.5 (2015-06-30)
*New functions and bug fixes*

#### New Functions
* ReadPointSourceData: Read HYPE PointSourceData file
* UpstreamPointSources: Calculate point source emissions over all upstream areas of a vector of SUBIDs or all SUBIDs in a GeoData table

## HYPEtools 0.3.4 (2015-06-10)
*Bug fixes*

## HYPEtools 0.3.3 (2015-06-09)
*New functions and bug fixes*

#### Highlights
* Update for DirectUpstreamSubids()
* Bug fixes

#### New Functions
* SortGeoData: Sort an imported GeoData.txt file in downstream order, so that all upstream sub-basins are listed in rows above downstream sub-basins

## HYPEtools 0.3.2 (2015-06-04)
*New functions and bug fixes*

#### New Functions
* UpstreamGeoData: Calculate upstream sums and averages for selected variables of imported GeoData.txt files
* UpstreamGroupSLCClasses: Calculate averages of grouped SLC class fractions calculated from imported GeoData.txt and GeoClass.txt or any other user-defined grouping
* EquallySpacedObs: Create equally spaced time series with missing observations from a data frame with irregular observations

## HYPEtools 0.3.1 (2015-01-28)
*New functions and bug fixes*

#### Highlights
* Added new functions, mostly small helper functions: AnnualRegime, HeadwaterSubids, MapRegionalSources, OutletSubids, ReadAquiferdata, WriteAquiferdata, ReadMgmtData, WriteMgmtData
* Various small bug fixes

## HYPEtools 0.3.0 (2014-11-21)
*New functions and bug fixes*

#### Highlights
* Bug fixes
* Read and write functions for pmsf.txt files
* Write function for lake data
* New function UpstreamObs() for extracting upstream average forcing, includes fortran code

## HYPEtools 0.2.5 (2014-10-22)
*New functions and bug fixes*

#### Highlights
* New attribute "timestep" in ReadBasinOutput()
* Extended functionality of ConvertDischarge()
* Bug fixes

#### New Functions
* AllDownstreamSubids: Find all SUBIDs of downstream sub-catchments along the main stem for a single sub-catchment
* OutletIds: Find the identifier of model domain outlets, i.e. the "downstream" ID of outlet catchments, in a GeoData file
* MergeXobs: Merge two Xobs data frames, with handling of overlapping time periods and time periods gaps as well as merging of common columns

## HYPEtools 0.2.4 (2014-08-18)
*New functionalities and bug fixes*

## HYPEtools 0.2.3 (2014-07-14)
*New functionalities and bug fixes*

## HYPEtools 0.2.2 (2014-07-08)
*New functionalities and bug fixes*

## HYPEtools 0.2.1 (2014-05-18)
*Improved functionality*

#### Highlights
* Several functions were added to the package, most noteworthy the first plot function, PlotMapOutput()

## HYPEtools 0.1.2 (2014-02-10)
*Import/Export for common HYPE files complete*
