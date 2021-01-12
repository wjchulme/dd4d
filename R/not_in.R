#' Inverse Value Matching
#'
#' Complement of \code{%in%}. Returns the elements of \code{x} that are
#' not in \code{y}.
#' @usage x \%ni\% y
#' @param x a vector
#' @param y a vector
#' @export
#' @rdname ni
"%ni%" <- function(x, y) {
  return( !(x %in% y) )
}

#"not in" infix function
#`%ni%` <- Negate(`%in%`)
