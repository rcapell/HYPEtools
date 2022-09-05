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
devtools::check(incoming = T, args = "--timings")
timing_df <- read.delim('<Path to Check Outputs>/HYPEtools.Rcheck/HYPEtools-Ex.timings') %>%
  mutate(cpu = user + system) %>% # Calculate CPU time (user + system)
  arrange(desc(cpu))

any(timing_df$cpu > 5) # Check if any CPU times > 5s
any(timing_df$elapsed > 5) # Check if any elapsed times >5s

# check for T/F instead of TRUE/FALSE
lintr::lint_package(linters=list(lintr::T_and_F_symbol_linter()))

# spelling errors
devtools::spell_check()

# check in different environments
devtools::check_rhub()

# check CRAN environment (may give notes which show not up otherwise)
rhub::check_for_cran()

# check and build windows binary
devtools::check_win_release()
devtools::check_win_devel()

# submit to CRAN, includes checks
# GIVES ERROR
# devtools::release()

# Create version tag in github repo
usethis::use_github_release()

# After successful release
# - Update version number in DESCRIPTION to *.9000
# - Add version heading to NEWS.md
