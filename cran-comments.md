## Test environments

-   local OS (Windows & Linux)
-   ubuntu-latest (on GitHub Actions): R release, devel, oldrel-1
-   macOS-latest (on GitHub Actions): R release
-   windows-latest (on GitHub Actions): R release

## R CMD check results

There were no ERRORs or WARNINGs.

There was one NOTE:
```
Found the following (possibly) invalid URLs:
  URL: https://anaconda.org/conda-forge/r-hypetools
    From: README.md
    Status: 400
    Message: Bad Request
```
This is a valid URL, so the server may blocking some requests. 

R-hub builder also gave the following NOTEs which do not seem to be related to the package:
```
* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found
Skipping checking math rendering: package 'V8' unavailable
* checking for non-standard things in the check directory ... NOTE
Found the following files/directories:
  ''NULL''
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
  ‘pulse-PKdhtXMmr18n’
```

## Downstream dependencies

There are currently no downstream dependencies for HYPEtools.
