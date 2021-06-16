
#' Plot bn_df object
#'
#' @param bn_df initialised bn_df object, with simulation instructions. Created with `bn_create`
#'
#' @return plot
#' @export
#'
#' @examples
bn_plot <- function(bn_df){
  dagitty <- bn2dagitty(bn_df)
  plot(dagitty::graphLayout(dagitty))
}

