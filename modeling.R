add_cv_repeat <- function(data,
                          repeat_fold_col = id,
                          repeat_col = id_repeat) {
  repeat_fold_col <- rlang::ensym(repeat_fold_col)
  repeat_col <- rlang::ensym(repeat_col)
  dplyr::mutate(data,
    !!repeat_col := stringr::str_extract(
      string = !!repeat_fold_col, pattern = "Rep[:digit:]"))
}