## Test environments

-   local OS (Windows & Linux)
-   ubuntu-latest (on GitHub Actions): R release, devel, oldrel-1
-   macOS-latest (on GitHub Actions): R release
-   windows-latest (on GitHub Actions): R release

## R CMD check results

There were no ERRORs or WARNINGs.

There were two NOTEs:
```
Possibly misspelled words in DESCRIPTION:
    Brendel (9:43)
    HYPEtools (8:42)
    al (9:54)
    et (9:51)
```
These are spelled correctly.

```
Found the following (possibly) invalid URLs:
  URL: https://anaconda.org/conda-forge/r-hypetools
    From: README.md
    Status: 400
    Message: Bad Request
```
This is a valid URL, so the server may blocking some requests. 

## Downstream dependencies

There are currently no downstream dependencies for HYPEtools.
