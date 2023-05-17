## Test environments

-   local OS (Windows & Linux)
-   ubuntu-latest (on GitHub Actions): R release, devel, oldrel-1
-   macOS-latest (on GitHub Actions): R release
-   windows-latest (on GitHub Actions): R release

## R CMD check results

There were no ERRORs or WARNINGs.

There is one NOTE:

    * checking for detritus in the temp directory ... NOTE
    'lastMiKTeXException'

As noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this could be due to a bug/crash in MiKTeX and can likely be ignored.

## Downstream dependencies

There are currently no downstream dependencies for HYPEtools.
