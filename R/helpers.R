# Inspired from multidplyr::partition();
# see https://github.com/hadley/multidplyr/blob/master/R/shard.R
partition_spc <- function(spc_tbl, 
                          groups = future::availableCores(),
                          id_nopart = sample_id) {
  id_nopart <- enquo(id_nopart)
  spc_tbl_nested <- spc_tbl %>%
    dplyr::group_by(!!id_nopart) %>%
    tidyr::nest()
  n <- nrow(spc_tbl_nested)
  m <- groups
  part_id <- sample(floor(m * (seq_len(n) - 1L) / n + 1L))
  
  spc_tbl_nested %>%
    dplyr::mutate(part_id = as.integer(part_id)) %>%
    tidyr::unnest()
}

# ggplot plotting helper; Returns min and max values for the x and y axis limits
xy_range <- function(data, x, y, range_scalar = 0.01) {
  x_col <- rlang::enquo(x)
  y_col <- rlang::enquo(y)
  x_vec <- dplyr::pull(data, !!x_col)
  y_vec <- dplyr::pull(data, !!y_col)
  
  stopifnot(is.numeric(x_vec) & is.numeric(y_vec))
  
  min_vec <- if (min(x_vec) < min(y_vec)) x_vec else y_vec
  
  max_vec <- if (max(x_vec) > max(y_vec)) x_vec else y_vec
  
  range_vec <- ifelse(diff(range(min_vec)) > diff(range(max_vec)),
    diff(range(min_vec)), diff(range(max_vec)))
  
  min_xy <- min(min_vec) - range_scalar * range_vec
  max_xy <- max(max_vec) + range_scalar * range_vec
  
  c(
    "min_xy" = min_xy,
    "max_xy" = max_xy
  )
}
