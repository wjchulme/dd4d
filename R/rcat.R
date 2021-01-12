#
#' Random categorical variable
#'
#' @param n number of samples
#' @param levels vector of categories to sample from
#' @param p vector of probabilities
#'
#' @return
#' @export
#'
#' @examples
#' #' rcat(n=10, levels=c("a","b"), p=c(0.2,0.8))
rcat <- function(n, levels, p){
  sample(x=levels, size=n, replace=TRUE, prob=p)
}
