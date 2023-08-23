#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom utils globalVariables
## usethis namespace: end

NULL

# Removes R Check Note on '.' not having a visible binding
# See https://stackoverflow.com/questions/66816638/no-visible-binding-for-global-variable
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

utils::globalVariables("where")