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
      names_prefix = "metabolite_"
    )
}

#' Transformation recipe to pre-process the data
#'
#' @param data The lipidomics dataset.
#' @param metabolite_variable The column of the metabolite variable
#'
#' @return Recipe with specifications
create_recipe_spec <- function(data, metabolite_variable) {
  recipes::recipe(data) %>%
    recipes::update_role({{ metabolite_variable }}, age, gender, new_role = "predictor") %>%
    recipes::update_role(class, new_role = "outcome") %>%
    recipes::step_normalize(tidyselect::starts_with("metabolite_"))
}

#' Create a workflow object of the model and the transformations
#'
#' @param model_specs The model specifications
#' @param recipe_specs The recipe specifications
#'
#' @return workflow object
create_model_workflow <- function(model_specs, recipe_specs) {
  workflows::workflow() %>%
    workflows::add_model(model_specs) %>%
    workflows::add_recipe(recipe_specs)
}

#' Create a tidy output of the model results.
#'
#' @param workflow_fitted_model The model workflow object that has been fitted
#'
#' @return A data frame.
tidy_model_output <- function(workflow_fitted_model) {
  workflow_fitted_model %>%
    workflows::extract_fit_parsnip() %>%
    broom::tidy(exponentiate = TRUE)
}

#' Convert the long form dataset into a list of wide form data frames.
#'
#' @param data lipidomics
#'
#' @return A l ist of data frames
split_by_metabolite <- function(data) {
  data %>%
    column_values_to_snake_case(metabolite) %>%
    dplyr::group_split(metabolite) %>%
    purrr::map(metabolites_to_wider)
}

#' Generate the results of the model
#'
#' @param data lipidomics
#'
#' @return
generate_model_results <- function(data) {
  create_model_workflow(
    parsnip::logistic_reg() %>%
      parsnip::set_engine("glm"),
    data %>%
      create_recipe_spec(tidyselect::starts_with("metabolite_"))
  ) %>%
    parsnip::fit(data) %>%
    tidy_model_output()
}

#' Calculates model estimates
#'
#' @param model_results
#' @param data lipidomics
#'
#' @return table of results
add_original_metabolite_names <- function(model_results, data) {
  data %>%
    dplyr::select(metabolite) %>%
    dplyr::mutate(term = metabolite) %>%
    column_values_to_snake_case(term) %>%
    dplyr::mutate(term = stringr::str_c("metabolite_", term)) %>%
    dplyr::distinct(term, metabolite) %>%
        dplyr::right_join(model_results, by = "term")
}

#' Calculate model estimates
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
calculate_estimates <- function(data) {
    data %>%
        split_by_metabolite() %>%
        purrr::map(generate_model_results) %>%
        purrr::list_rbind() %>%
        dplyr::filter(str_detect(term, "metabolite_"))%>%
        add_original_metabolite_names(data)
}

#' Plot the model estimates
#'
#' @param results model estimates
#'
#' @return a plot
plot_estimates <- function(results) {
    results %>% ggplot(aes(
        x = estimate,
        y = metabolite,
        xmin = estimate - std.error,
        xmax = estimate + std.error
    )) +
        geom_pointrange() +
        coord_fixed(xlim = c(0,5))

}
