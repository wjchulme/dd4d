
#' Simulate data from bn_df object
#'
#' @param bn_df
#' @param data
#' @param pop_size
#' @param keep_all
#'
#' @return
#' @export
#'
#' @examples
bn_simulate <- function(bn_df, data=NULL, pop_size, keep_all=FALSE){

  dagitty <- bn2dagitty(bn_df)

  bn_df1 <- bn_df %>% mutate(
    bn_fun = pmap(lst(variable, variable_formula), function(variable, variable_formula){

      function(tib){
        row_num <- seq_len(nrow(tib))
        x <- simplify(map(row_num,  ~eval(rlang::f_rhs(variable_formula), tib[.,])))
        tib1 <- tib
        tib1[variable] <- x
        tib1
      }
    }),

  )

  #reorder based on dependencies so that simulation will create variables in the right order
  bn_ordered <- dagitty %>%
    topologicalOrdering() %>%
    enframe(name="variable", value='topological_order') %>%
    unnest(topological_order) %>%
    left_join(bn_df1, ., by='variable') %>%
    arrange(topological_order)


  # simulate complete dataset (with a patient ID variable in the initiated dataset)
  if (is.null(data))
    tbl0 <- tibble(ptid = seq_len(pop_size))
  else
    tbl0 <- data


  bn_ordered_unknown <- bn_ordered %>% filter(!known)

  tblsim_complete <- compose(!!!bn_ordered_unknown$bn_fun, .dir='forward')(tbl0)

  # make some values missing, according to missing

  missing_formula <- setNames(bn_ordered_unknown$missing_formula, bn_ordered_unknown$variable)

  tblsim_missing1 <- pmap_df(
    lst(variable = tblsim_complete[bn_ordered_unknown$variable], missing_formula, simdat=list(tblsim_complete)),
    function(variable, missing_formula, simdat){
      NA_type_ <- NA
      mode(NA_type_) <- typeof(variable)
      row_num <- seq_len(nrow(simdat))
      mask <- map_lgl(row_num, ~eval(rlang::f_rhs(missing_formula), simdat[.,]))

      if_else(mask, NA_type_, variable)
    }
  )

  # make values missing according to `needs`

  needs <- setNames(bn_ordered_unknown$needs, bn_ordered_unknown$variable)

  tblsim_missing2 <- pmap_df(
    lst(variable = tblsim_missing1[bn_ordered_unknown$variable], needs, simdat=list(tblsim_missing1)),
    function(variable, needs, simdat){
      NA_type_ <- NA
      mode(NA_type_) <- typeof(variable)

      if(length(needs)!=0){
        mask <- simdat %>%
          select(all_of(needs)) %>%
          mutate(across(all_of(needs), ~!is.na(.))) %>%
          rowwise() %>%
          mutate(
            need_satisfied=!all(c_across(all_of(needs)))
          ) %>% pluck("need_satisfied")


      }
      else{
        mask <- rep(FALSE, nrow(simdat))
      }

      if_else(mask, NA_type_, variable)
    }
  )


  tblsim <- bind_cols(tbl0, tblsim_missing2)

  # choose which variables to return
  returnvars <- bn_df1 %>% filter(keep | keep_all) %>% pluck("variable")

  tblsim %>% select(names(tbl0), all_of(returnvars))
}

