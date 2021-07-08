
#' Simulate data from bn_df object
#'
#' @param bn_df initialised bn_df object, with simulation instructions. Created with `bn_create`
#' @param known_df data.frame. Optional data.frame containing upstream variables used for simulation.
#' @param pop_size integer. The size of the dataset to be created.
#' @param keep_all logical. Keep all simulated variables or only keep those specified by `keep`
#' @param .id character. Name of id column placed at the start of the dataset. If NULL (default) then no id column is created.
#'
#' @return tbl
#' @export
#'
#' @examples
bn_simulate <- function(bn_df, known_df=NULL, pop_size, keep_all=FALSE,.id=NULL){

  stopifnot(".id must be NULL or a length 1 character" = ( length(.id)==1 & is.character(.id) | is.null(.id)))

  dagitty <- bn2dagitty(bn_df)

  bn_df1 <- bn_df %>% dplyr::mutate(
    bn_fun = purrr::pmap(tibble::lst(variable, variable_formula), function(variable, variable_formula){

      function(tib){
        row_num <- seq_len(nrow(tib))
        x <- purrr::simplify(purrr::map(row_num,  ~eval(rlang::f_rhs(variable_formula), tib[.,])))
        tib1 <- tib
        tib1[variable] <- x
        tib1
      }
    }),

  )

  #reorder based on dependencies so that simulation will create variables in the right order
  bn_ordered <- dagitty %>%
    dagitty::topologicalOrdering() %>%
    tibble::enframe(name="variable", value='topological_order') %>%
    tidyr::unnest(topological_order) %>%
    dplyr::left_join(bn_df1, ., by='variable') %>%
    dplyr::arrange(topological_order)


  # simulate complete dataset (with a patient ID variable in the initiated dataset)
  if (is.null(known_df)) {
    tbl0 <- tibble::tibble(.id = seq_len(pop_size))
  } else {
    tbl0 <- known_df
  }


  bn_ordered_unknown <- bn_ordered %>% dplyr::filter(!known)

  tblsim_complete <- purrr::compose(!!!bn_ordered_unknown$bn_fun, .dir='forward')(tbl0)

  # make some values missing, according to `missing`

  missing_formula <- stats::setNames(bn_ordered_unknown$missing_formula, bn_ordered_unknown$variable)

  tblsim_missing1 <- purrr::pmap_df(
    tibble::lst(variable = tblsim_complete[bn_ordered_unknown$variable], missing_formula, simdat=list(tblsim_complete)),
    function(variable, missing_formula, simdat){

      row_num <- seq_len(nrow(simdat))
      mask <- purrr::map_lgl(row_num, ~eval(rlang::f_rhs(missing_formula), simdat[.,]))
      if(class(variable)!="factor"){
        NA_type_ <- NA
        mode(NA_type_) <- typeof(variable)
        dplyr::if_else(!mask, variable, NA_type_)
      } else {
        if_else(!mask, variable, factor(NA))
      }
    }
  )

  # make values missing according to `needs`

  needs <- stats::setNames(bn_ordered_unknown$needs, bn_ordered_unknown$variable)

  tblsim_missing2 <- purrr::pmap_df(
    tibble::lst(variable = tblsim_missing1[bn_ordered_unknown$variable], needs, simdat=list(tblsim_missing1)),
    function(variable, needs, simdat){


      if(length(needs)!=0){
        mask <- simdat %>%
          dplyr::select(tidyselect::all_of(needs)) %>%
          dplyr::mutate(dplyr::across(tidyselect::all_of(needs), ~!is.na(.))) %>%
          dplyr::rowwise() %>%
          dplyr::mutate(
            need_satisfied=!all(dplyr::c_across(tidyselect::all_of(needs)))
          ) %>% purrr::pluck("need_satisfied")
      }
      else{
        mask <- rep(FALSE, nrow(simdat))
      }

      if(class(variable)!="factor"){
        NA_type_ <- NA
        mode(NA_type_) <- typeof(variable)
        dplyr::if_else(!mask, variable, NA_type_)
      } else {
        if_else(!mask, variable, factor(NA))
      }
    }
  )


  tblsim <- dplyr::bind_cols(tbl0, tblsim_missing2)

  # choose which variables to return
  returnvars <- bn_df1 %>% dplyr::filter(keep | keep_all, known==FALSE) %>% purrr::pluck("variable")

  tblout <- tblsim %>% dplyr::select(names(tbl0), tidyselect::all_of(returnvars))
  if(is.null(.id)){
    tblout$.id <- NULL
  } else {
    names(tblout)[names(tblout) == '.id'] <- .id
  }

  tblout
}

