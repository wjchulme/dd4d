
#' Specify a variable node in the network
#'
#' @param variable_formula A RHS-only formula specified how to simulate that variable. Use `..n` for the number of observations, which is later replaced by `pop_size` in the `bn_simulate` function.
#' @param missing_rate A RHS-only formula. This specifies how missing values should be distributed.
#'    Can use a simple proportion such as `~0.5` or missingness can depend on other values for example using `~plogis(-2 + age*0.05)`, which says missingness increases with age.
#' @param keep logical. Should this variable be kept in the final simulated output or not
#' @param needs A character vector of variables. If any variables given in `needs` are missing / `NA`, then this variable is missing too.
#'
#' @return Object of class `node` and `list`.
#' @export
#'
#' @examples
#' bn_node(variable_formula = ~floor(rnorm(n=..n, mean=60, sd=15)))
bn_node <- function(
  variable_formula,
  missing_rate=~0,
  keep=TRUE,
  needs=character()
){

  stopifnot(rlang::is_formula(variable_formula))
  stopifnot(rlang::is_formula(missing_rate))
  stopifnot(is.logical(keep))

  l <- list(
    variable_formula = variable_formula,
    missing_rate = missing_rate,
    keep = keep,
    needs = c(character(), needs)
  )

  class(l) <- append(class(l), "node")
  l

}
