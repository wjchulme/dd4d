
#' Plot bn_df object
#'
#' @param bn_df
#'
#' @return
#' @export
#'
#' @examples
bn_plot <- function(bn_df){
  dagitty <- bn2dagitty(bn_df)
  plot(daggity::graphLayout(dagitty))
}

