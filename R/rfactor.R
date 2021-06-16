#
#' Random factor variables
#'
#' @param n number of samples
#' @param levels vector of categories to sample from
#' @param p vector of probabilities
#'
#' @return a `factor` vector
#' @export
#'
#' @examples
#' #' rfactor(n=10, levels=c("a","b"), p=c(0.2,0.8))
rfactor <- function(n, levels, p){
  x <- sample(x=levels, size=n, replace=TRUE, prob=p)
  factor(x = x, levels = levels)
}





