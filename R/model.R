#' Fit a logarithmic growth curve to real-time fluorescence data
#'
#'
#' \code{fit_model} fits a logarithmic growth curve to the fluorescence curves and extracts numeric values for growth rate (rise time) and endpoint fluorescence.
#'
#'
#' @param data a dataframe containing the imported real-time fluorescence data, variables *cycle*, *well*, and *intensity*
#' @param control a character string indicating the well which will serve as a baseline control (e.g. *"A1"* or *"H12"*)
#' @param clean boolean parameter option to remove raw data which is otherwise nested in a list column *data*;
#' defaults to TRUE leaving only variables *well*, *rate*, and *endpoint*
#' @return a [tibble][tibble::tibble-package] containing the calculated variables *rate* and *endpoint*
#' @export
fit_model <- function(data, control = NULL, clean = TRUE) {
  if (!(clean %in% c(TRUE, FALSE))) {
    stop("argument clean must be TRUE or FALSE")
  }
  if (missing(control) | is.null(control)) {
    stop("a control must be specified")
  }

  my_model <- function(df) {
    stats::nls(
      formula = intensity ~ a + (b * log10(cycle)),
      start = list(a = min(data[["intensity"]]), b = 1),
      data = df
    )
  }

  nestedData <- data %>%
    tidyr::nest(spectra = -well) %>%
    dplyr::mutate(model = purrr::map(spectra, my_model)) %>%
    dplyr::mutate(
      rate = purrr::map_dbl(
        model,
        ~ .x %>%
          broom::tidy() %>%
          dplyr::filter(term == "b") %>%
          dplyr::pull(estimate) %>%
          round(digits = 0)
      )
    ) %>%
    dplyr::mutate(endpoint = purrr::map_dbl(
      spectra,
      ~ round((max(.x$intensity) - max(data[data$well == control, "intensity"][[1]])) /
                (max(data$intensity) - max(data[data$well == control, "intensity"][[1]])), digits = 3)
    ))

  if (clean) {
    return(nestedData %>% dplyr::select(well, rate, endpoint))
  } else {return(nestedData)}
}




#' Generate a plot from real-time fluorescence data model parameters
#'
#'
#' \code{view_model} generates a plot of the variables derived from the model fit to real-time fluorescence data. Endpoint fluorescence is plotted against rate (rise time).
#'
#'
#' @param data a dataframe containing the imported real-time fluorescence data, variables *cycle*, *well*, and *intensity*
#' @param control a character string indicating the well to mark as the baseline control (e.g. *"A1"* or *"H12"*);
#' defaults to NULL (i.e. no control point marked on the plot)
#' @param color grouping variable to map to `geom_point` aesthetic *color*;
#' defaults to the *well* variable
#' @param size grouping variable to map to `geom_point` aesthetic *size*
#' @param shape grouping variable to map to `geom_point` aesthetic *shape*
#' @param palette character string indicating which ColorBrewer palette to use;
#' defaults to "Set3"
#' @param ncolors numeric value indicating how many colors from the specified palette to use to generate a spectrum of colors for the *color* grouping variable;
#' defaults to 12 for palette "Set3"
#' @param interactive boolean parameter to display a static or interactive plot using `plotly`;
#' defaults to TRUE
#' @return a [tibble][tibble::tibble-package] containing the calculated variables *rate* and *endpoint*
#' @export
view_model <- function(data, control = NULL, color = well, size = NULL, shape = NULL, palette = "Set3", ncolors = 12, interactive = TRUE) {
  if (!(interactive %in% c(TRUE, FALSE))) {
    stop("argument interactive must be TRUE or FALSE")
  }

  color <- rlang::enquo(color)
  color_string <- rlang::as_name(color)
  size <- rlang::enquo(size)
  shape <- rlang::enquo(shape)

  if (rlang::as_label(color) == "map_color" | rlang::as_label(color) == "map") {
    paletteValues <- data$map_color %>%
      purrr::set_names() %>%
      purrr::map(xkcdcolors::name2color)
    color <- rlang::quo(map_color)
  } else {
    paletteValues <- colorRampPalette(RColorBrewer::brewer.pal(ncolors, palette))(length(unique(data[[color_string]])))
  }

  p <- data %>%
    ggplot2::ggplot(ggplot2::aes(x = rate, y = endpoint, text = well)) +
    ggplot2::geom_point(
      data = data %>%
        dplyr::filter(!(well %in% c(control))),
      ggplot2::aes(
        color = !!color,
        size = !!size,
        shape = !!shape
      )
    ) +
    ggplot2::geom_point(data = data %>% dplyr::filter(well %in% c(control)), color = "red", shape = 4) +
    ggplot2::scale_color_manual(values = paletteValues) +
    ggplot2::theme(legend.position = "right", legend.justification = "top") +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "right",
      panel.grid = ggplot2::element_blank()
    )

  if (interactive) {
    plotly::ggplotly(p + ggplot2::theme(legend.position = "none"), tooltip = "text")
  } else {p}

}
