#' Descritptive statistics function
#'
#' @param data
#'
#' @return "A data.frame/tibble"

descriptive_stats <- function(data) {
  data %>%
    dplyr::group_by(metabolite) %>%
    dplyr::summarise(dplyr::across(value, list(mean = mean, sd = sd))) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, digits = 1)))
}

#' Plot distributions
#'
#' @param data
#'
#' @return A plot object.

plot_distributions <- function(data) {
  data %>%
    ggplot2::ggplot(ggplot2::aes(x = value)) +
    ggplot2::geom_histogram() +
    ggplot2::facet_wrap(ggplot2::vars(metabolite), scales = "free")
}


#' Convert column value strings into snakecase
#'
#' @param data Data with the strinc columns
#' @param cols The column to convert into snakecase
#'
#' @return a data frame

column_values_to_snake_case <- function(data, cols) {
  data %>%
    dplyr::mutate(dplyr::across({{ cols }}, snakecase::to_snake_case))
}

#' Format table into wide and some data wrangling
#'
#' @param data
#'
#' @return a data frame
metabolites_to_wider <- function(data) {
  data %>%
    tidyr::pivot_wider(
        names_from = metabolite,
        values_from = value,
        values_fn = mean,
        names_prefix = "metabolite_")
}
