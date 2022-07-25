#' Creates a bayesian network object from a list of nodes
#'
#' Converts list to data frame which is a bit easier to work with, and embellishes with some useful columns.
#' The function performs a few checks on the list, for instance to make sure the graph is acyclic
#' and that variables used in the expressions are defined elsewhere or already known.

#' The known_variables argument is for passing a character vector of variables names
#' for variables that are already defined externally in a
#' given dataset, which can be passed to bn_simulate

#' whilst variable_formula is the variable name itself, this is to help with the bn_simulate function
#' it doesn't actually lead to self-dependence (eg var depends on var)

#'
#' @param list of node objects, created by `bn_node`.
#' @param known_variables character vector of variables that will be provided by an external dataset
#'
#' @return data.frame
#' @export
#'
#' @examples
bn_create <- function(list, known_variables=NULL){


  stopifnot("'list' must be a list where each element is an object of class 'node'" = all(sapply(list, function(x){"node" %in% class(x)})))

  df <- tibble::enframe(list, name="variable", value="list")
  # df <- tidyr::unnest_wider(df, "list") # this no longer works after update to tidyr, even with ptype specification, so use tidyr::hoist instead
  df <- tidyr::hoist(df, .col="list", variable_formula=1, missing_rate=2, keep=3, needs=4)

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
    parents = purrr::map(variable_formula, function(x) {parents <- all.vars(x); parents[parents!="..n"] }),
    missing_formula = purrr::map(missing_rate, ~{
      rhs <- deparse1(rlang::f_rhs(.))
      fun <- stats::as.formula(paste0("~rbernoulli(n=..n, p=", rhs, ")"))
      fun
    }),
    missing_parents = purrr::map(missing_formula, function(x) {parents <- all.vars(x); parents[parents!="..n"] }),
    known=FALSE
  )

  if(!is.null(known_variables)){

    df_available <-
      tibble::tibble(
        variable = known_variables,
        variable_formula = list(~stats::as.formula(paste0("~", variable))),
        missing_rate = list(~0),
        keep = TRUE,
        parents = list(character()),
        missing_formula = list(stats::as.formula("~rbernoulli(n=..n, p=0)")),
        missing_parents = list(character()),
        known = TRUE,
        needs = list(character())
      )

    df <- dplyr::bind_rows(df_available, df)
  }

  df$in_order <- seq_len(nrow(df))

  dagitty <- bn2dagitty(df)


  parents_check <- purrr::map(df$variable, ~ dagitty::parents(dagitty, .)) # should be same as 'parents' above
  stopifnot("mismatch between dependencies and parents" = all(purrr::map2_lgl(df$parents, parents_check, ~all(.x %in% .y) & all(.x %in% .y))))

  df$children <- purrr::map(df$variable, ~ dagitty::children(dagitty, .))

  stopifnot("graph is not acyclic" = dagitty::isAcyclic(dagitty))

  if(!all(purrr::simplify(unique(rlang::flatten(df$parents))) %in% df$variable)){
    print(
      purrr::simplify(unique(rlang::flatten(df$parents)))[!(purrr::simplify(unique(rlang::flatten(df$parents))) %in% df$variable)]
    )
    stop("not all dependencies are defined")
  }

  stopifnot("variable names are not unique" = length(df$variable) == length(unique(df$variable)))

  df
}
