select_spc_xvalues <- function(spc_tbl, xvalues, column_in = "spc_pre",
                               xvalues_in = "xvalues_pre") {
  column_in <- rlang::enquo(column_in)
  column_in_chr <- rlang::quo_name(column_in)
  xvalues_in <- rlang::enquo(xvalues_in)
  xvalues_in_chr <- rlang::quo_name(xvalues_in)
  stopifnot(
    is.numeric(xvalues)
  )
  
  spc <- data.table::rbindlist(dplyr::pull(spc_tbl, !!column_in))
  xvalues_spc <- pull(spc_tbl, !!xvalues_in)
  xvalues_spc_1 <- xvalues_spc[[1]]
  xvars_sel <- xvalues_spc_1 %in% xvalues
  
  spc_vars_sel <-  spc[, ..xvars_sel]
  xvalues_sel <- map(xvalues_spc, ~ .x[xvars_sel])
  
  spc_vars_sel_list <- map(
    purrr::transpose(spc_vars_sel),
    data.table::as.data.table)
  
  dplyr::mutate(spc_tbl %>% dplyr::ungroup(),
    !!column_in_chr := spc_vars_sel_list,
    !!xvalues_in_chr := xvalues_sel
  )
}