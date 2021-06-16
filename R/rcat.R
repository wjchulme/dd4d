#
#' Random categorical or factor variables
#'
#' @param n number of samples
#' @param levels vector of categories to sample from
#' @param p vector of probabilities
#'
#' @return a `character` or `factor` vector
#' @export
#'
#' @examples
#' #' rcat(n=10, levels=c("a","b"), p=c(0.2,0.8))
rcat <- function(n, levels, p){
  sample(x=levels, size=n, replace=TRUE, prob=p)
}

#' @rdname rcat
rfactor <- function(n, levels, p){
  x <- sample(x=levels, size=n, replace=TRUE, prob=p)

  factor(x = x, levels = levels)
}
