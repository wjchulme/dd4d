
#' Specify a variable node in the network
#'
#' @param variable_formula formula.
#' @param missing_rate formula. This specifies how missing values should be distributed.
#'    Can use a simple proportion such as `~0.5` or missingness can depend on other values for example using `~plogis(-2 + age*0.05)`, which says missingness increases with age.
#' @param keep logical. should this variable be kept in the final simulated output?
#' @param needs character vector of variables. If any variables given in `needs` are missing, then this variable is missing too.
#'
#' @return Object of class `node` and `list`.
#' @export
#'
#' @examples
#' bn_node(variable_formula = ~floor(rnorm(n=1, mean=60, sd=15)))
bn_node <- function(
  variable_formula,
  missing_rate=~0,
  keep=TRUE,
  #known=FALSE,
  needs=character()
){

  stopifnot(rlang::is_formula(variable_formula))
  stopifnot(rlang::is_formula(missing_rate))
  stopifnot(is.logical(keep))

  l <- list(
    variable_formula = variable_formula,
    missing_rate = missing_rate,
    keep = keep,
    #known = known,
    needs = c(character(), needs)
  )

  class(l) <- append(class(l), "node")
  l

}
