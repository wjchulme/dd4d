
#' Plot bn_df object
#'
#' @param bn_df initialised bn_df object, with simulation instructions. Created with `bn_create`
#' @param connected_only logical. Only plot nodes that are connected to other nodes
#'
#' @return plot
#' @export
#'
#' @examples
bn_plot <- function(bn_df, connected_only = FALSE){

  if(connected_only){
    dagitty <- bn2dagitty(bn_df[!(purrr::map_lgl(bn_df$parents, ~length(.)==0) & purrr::map_lgl(bn_df$children, ~length(.)==0)), ])
  } else{
    dagitty <- bn2dagitty(bn_df)
  }
  plot(dagitty::graphLayout(dagitty))
}

