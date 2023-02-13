library(dplyr)

# Update DESCRIPTION
# - Package version
# - Package dependencies (cross-check with NAMESPACE)
# - Read through all sections and check validity

# Update NEWS.md
# - 

# Add release comments to cran-comments.md

# Run update code at bottom of VariableLookup.R

# check example execution time
check_dir <- tempdir()
devtools::check(incoming = T, args = "--timings", check_dir = check_dir)
timing_df <- read.delim(file.path(check_dir,"HYPEtools.Rcheck", "HYPEtools-Ex.timings")) %>%
  mutate(cpu = user + system) %>% # Calculate CPU time (user + system)
  arrange(desc(cpu))

any(timing_df$cpu > 5) # Check if any CPU times > 5s
any(timing_df$elapsed > 5) # Check if any elapsed times >5s
rm(timing_df)
unlink(check_dir)

# check for T/F instead of TRUE/FALSE
lintr::lint_package(linters=list(lintr::T_and_F_symbol_linter()))

# spelling errors - Note that this function does not support having multiple languages in the DESCRIPTION file
devtools::spell_check()

# Specify email to receive the results from the various checks
email <- "hypetools@smhi.se"

# check in different environments
# - You can stop the task in R or use interactive = F if you don't want to see the live output (the check will still be performed)
# - This check takes several hours
devtools::check_rhub(email = email)

# check CRAN environment (may give notes which show not up otherwise)
# - You can stop the task in R or use show_status = F if you don't want to see the live output (the check will still be performed)
# - This check takes several hours
rhub::check_for_cran(email = email)

# check and build windows binary
devtools::check_win_release(email = email)
devtools::check_win_devel(email = email)

# Commit any changes to GitHub

# submit to CRAN, includes checks
devtools::release()

# Create version tag in github repo
usethis::use_github_release()

# After successful release
# - Update version number in DESCRIPTION to *.9000
# - Add version heading to NEWS.md:
#     ## HYPEtools 1.0.0.9000
#     *Development Version*
#     #### Highlights
  
