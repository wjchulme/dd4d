#' Creates a bayesian network object from a list of nodes
#'
#' Converts list to data frame which is a bit easier to work with, and embellishes with some useful columns.
#' The function performs a few checks on the the list, for instance to make sure the p-DAG is indeed acyclic
#' and that variables using in the expressions are defined elsewhere.

#' The known_variables argument is for passing a character vector of variables names
#' for variables that are already defined externally in a
#' given dataset, which can be passed to bn_simulate

#' whilst variable_formula is the variable name itself, this is to help with the bn_simulate function
#' it doesn't actually lead to self-dependence (eg var depends on var)

#'
#' @param list of node objects, created by `node`.
#' @param known_variables character vector of variables that will be provided by an external dataset
#'
#' @return data.frame
#' @export
#'
#' @examples
bn_create <- function(list, known_variables=NULL){


  stopifnot("'list' must be a list where each element is an object of class 'node'" = all(sapply(list, function(x){"node" %in% class(x)})))

  df <- tibble::enframe(list, name="variable", value="list")
  df <- tidyr::unnest_wider(df, "list")

  # this bit is needed because if there are no nodes with "needs" specified, this variable does not
  # get unnested, so needs to be created explicitly
  # otherwise if at least one "need" is specified, then for all nodes without needs, `need` is converted from
  # character() to NULL, so need to undo.
  if(!("needs" %in% names(df))){
    df$needs = list(character())
  } else{
    df$needs = purrr::map(df$needs, ~{
      if(is.null(.)){
        character()
      } else
      if(length(.)==1 & all(is.na(.))){
        character()
      } else
        .
      })
  }

  df <- dplyr::mutate(df,
    dependencies = purrr::map(variable_formula, ~all.vars(.)),
    missing_formula = purrr::map(missing_rate, ~{
      rhs <- deparse1(rlang::f_rhs(.))
      fun <- stats::as.formula(paste0("~rbernoulli(n=1, p=", rhs, ")"))
      fun
    }),
    known=FALSE
  )

  if(!is.null(known_variables)){

    df_available <-
      tibble::tibble(
        variable = known_variables,
        variable_formula = list(~stats::as.formula(paste0("~", variable))),
        missing_rate = list(~0),
        keep = TRUE,
        dependencies = list(character()),
        missing_formula = list(stats::as.formula("~rbernoulli(n=1, p=0)")),
        known = TRUE,
        needs = list(character())
      )

    df <- dplyr::bind_rows(df_available, df)
  }

  df$in_order <- seq_len(nrow(df))

  dagitty <- bn2dagitty(df)

  stopifnot("graph is not acyclic" = dagitty::isAcyclic(dagitty))
  stopifnot("not all dependencies are defined" = all(purrr::simplify(unique(rlang::flatten(df$dependencies))) %in% df$variable))
  stopifnot("variable names are not unique" = length(df$variable) == length(unique(df$variable)))

  df
}
