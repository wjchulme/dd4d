
#' Converts a bn_df object to a dagitty object
#'
#' @param bn_df initialised bn_df object, with simulation instructions. Created with `bn_create`
#'
#' @return dagitty object
#' @export
#'
#' @examples
#'
bn2dagitty <- function(bn_df){
  dagitty_str = purrr::map2_chr(bn_df$variable, bn_df$dependencies, ~paste0(.x, " <- ", "{", paste0(.y, collapse=" ") , "}"))
  dagitty::dagitty(paste0("dag {", paste0(dagitty_str, collapse=" "), "}"))
}
