#' Title
#'
#' @param data_to
#' @param data_from
#' @param group_var
#'
#' @return
#' @export
#'
#' @examples
add_scaling_column <- function(data_to, data_from, group_var){
  data_to %>% left_join(data_from, by = group_var) %>%
    mutate(scaling_factor = ifelse(!is.na(scaling_factor), scaling_factor, 1))
}
