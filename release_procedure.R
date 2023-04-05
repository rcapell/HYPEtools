# _____________________________________________________________________________________________________________________________________
# Release Procedure #####
# _____________________________________________________________________________________________________________________________________

# - Process to follow for creating a new CRAN release

# _____________________________________________________________________________________________________________________________________
# Import Packages #####
# _____________________________________________________________________________________________________________________________________

library(dplyr)

# _____________________________________________________________________________________________________________________________________
# 1) Manual File Updates #####
# _____________________________________________________________________________________________________________________________________

# Update DESCRIPTION
# - Package version
# - Package dependencies (cross-check with NAMESPACE - note that ggrepel is called in R/functioncollection_internal.R but doesn't show up in NAMESPACE)
# - Read through all sections and check validity

# Update NEWS.md

# Add release comments to cran-comments.md

# Run update code at bottom of VariableLookup.R

# _____________________________________________________________________________________________________________________________________
# 2) Check Example Excution Time #####
# _____________________________________________________________________________________________________________________________________

# Run devtools check and save results to a temporary directory
check_dir <- tempdir()
devtools::check(incoming = T, args = "--timings", check_dir = check_dir)
timing_df <- read.delim(file.path(check_dir,"HYPEtools.Rcheck", "HYPEtools-Ex.timings")) %>%
  mutate(cpu = user + system) %>% # Calculate CPU time (user + system)
  arrange(desc(cpu))

# Check results
any(timing_df$cpu > 5) # Check if any CPU times > 5s
any(timing_df$elapsed > 5) # Check if any elapsed times >5s

# Cleanup
rm(timing_df)
unlink(check_dir)

# _____________________________________________________________________________________________________________________________________
# 3) Check Formatting #####
# _____________________________________________________________________________________________________________________________________

# check for T/F instead of TRUE/FALSE
lintr::lint_package(linters=list(lintr::T_and_F_symbol_linter()))

# spelling errors - Note that this function does not support having multiple languages in the DESCRIPTION file
devtools::spell_check()

# _____________________________________________________________________________________________________________________________________
# 4) Test Package #####
# _____________________________________________________________________________________________________________________________________

# Specify email to receive the results from the various checks
email <- "hypetools.rene@smhi.se"

# check in different environments
# - You can stop the task in R or use interactive = F if you don't want to see the live output (the check will still be performed)
# - This check takes several hours
devtools::check_rhub(email = email)

# check CRAN environment (may give notes which do not show up otherwise)
# - You can stop the task in R or use show_status = F if you don't want to see the live output (the check will still be performed)
# - This check takes several hours
rhub::check_for_cran(email = email)

# check and build windows binary
devtools::check_win_release(email = email)
devtools::check_win_devel(email = email)

# _____________________________________________________________________________________________________________________________________
# 5) Create Release & Update Repo #####
# _____________________________________________________________________________________________________________________________________

# Commit any changes to GitHub

# Submit to CRAN, includes checks
devtools::release()

# Create version tag in github repo
usethis::use_github_release()

# After successful release
# - Update version number in DESCRIPTION to *.9000
# - Add date to release heading in NEWS.md:
#     ## HYPEtools 1.2.0 (2023-02-10)
# - Add development version heading to NEWS.md:
#     ## HYPEtools 1.4.0.9000
#     *Development Version*
#
#     #### Highlights
