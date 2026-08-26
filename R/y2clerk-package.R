#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom stats p.adjust.methods
## usethis namespace: end

# `label` is used as an unquoted default argument (columns_var = label) in
# pivot_freqs(); it resolves to a data-frame column at runtime, but R CMD
# check's static analysis flags it as an undefined global.
utils::globalVariables("label")

## mockable bindings: start
## mockable bindings: end
NULL
