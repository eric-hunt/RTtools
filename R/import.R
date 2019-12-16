#' Import instrument data files
#'
#'
#' \code{import_traces} imports Excel data files exported from Bio-Rad real-time fluorescnce thermal cyclers.
#'
#'
#' @param path a character string path to the instrument export Excel file containing the raw data
#' @param cutoff a numeric value specifiying how many cycles to ignore
#' (if the assay requries the instrument lid to be opened for reagent addition after temperature equilibration);
#' defaults to *5*
#' @param sheet a numeric value representing the Excel workbook sheet which contains the raw data;
#' defaults to the first sheet, i.e. *1*
#' @return a [tibble][tibble::tibble-package] in "long" form containing the variables *cycle*, *well*, and *intensity*
#' @export
import_traces <- function(path, cutoff = 5, sheet = 1) {
  if (!(is.numeric(cutoff))) {
    stop("cycle must be a numeric value")
  }
  if (!(is.numeric(sheet))) {
    stop("select a numeric value for workbook sheet which contains raw data")
  }

  wellOrder <- c("A1", "B1", "C1", "D1", "E1", "F1", "G1", "H1", "A2", "B2",
                 "C2", "D2", "E2", "F2", "G2", "H2", "A3", "B3", "C3", "D3", "E3",
                 "F3", "G3", "H3", "A4", "B4", "C4", "D4", "E4", "F4", "G4", "H4",
                 "A5", "B5", "C5", "D5", "E5", "F5", "G5", "H5", "A6", "B6", "C6",
                 "D6", "E6", "F6", "G6", "H6", "A7", "B7", "C7", "D7", "E7", "F7",
                 "G7", "H7", "A8", "B8", "C8", "D8", "E8", "F8", "G8", "H8", "A9",
                 "B9", "C9", "D9", "E9", "F9", "G9", "H9", "A10", "B10", "C10",
                 "D10", "E10", "F10", "G10", "H10", "A11", "B11", "C11", "D11",
                 "E11", "F11", "G11", "H11", "A12", "B12", "C12", "D12", "E12",
                 "F12", "G12", "H12")

  readxl::read_excel(path = path, sheet = sheet) %>%
    dplyr::rename("cycle" = "Cycle") %>%
    dplyr::filter(cycle > cutoff) %>%
    dplyr::mutate(cycle = cycle - cutoff) %>%
    tidyr::pivot_longer(-cycle, names_to = "well", values_to = "intensity") %>%
    purrr::modify_at(.at = c("well"), readr::parse_factor, levels = wellOrder) %>%
    dplyr::arrange(well, cycle)
}




#' View real-time fluorescence traces
#'
#'
#' \code{view_traces} plots either raw fluorescence traces, or modeled curves fit to the raw data.
#'
#'
#' @param data a dataframe containing the imported real-time fluorescence data, variables *cycle*, *well*, and *intensity*
#' @param color grouping variable used to color traces or curves;
#' defaults to the *well* variable
#' (if the assay requries the instrument lid to be opened for reagent addition after temperature equilibration);
#' defaults to *5*
#' @param model boolean parameter to choose plotting modeled curves instead of raw data traces;
#' defaults to FALSE, i.e. plotting raw data
#' @param interactive boolean parameter to display a static or interactive plot using `plotly`;
#' defaults to TRUE
#' @return returns a plot of fluorescence traces or modeled curves, *intensity* vs *cycle*
#' @export
view_traces <- function(data, color = well, model = FALSE, interactive = TRUE) {
  if (!(model %in% c(TRUE, FALSE))) {
    stop("argument model must be TRUE or FALSE")
  }
  if (!(interactive %in% c(TRUE, FALSE))) {
    stop("argument interactive must be TRUE or FALSE")
  }

  color <- rlang::enquo(color)

  p <- data %>%
    ggplot2::ggplot(ggplot2::aes(x = cycle, y = intensity, text = well, color = !!color)) +
    # ggplot2::geom_path() +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "none",
      panel.grid = ggplot2::element_blank()
    )

  if (model) {
    p <- p +
      ggplot2::geom_smooth(
        data = data,
        alpha = 0.5,
        se = FALSE,
        method = "nls",
        method.args = list(
          formula = y ~ a + (b * log10(x)),
          start = list(a = min(data[["intensity"]]), b = 1)
        )
      )
  } else {
    p <- p + ggplot2::geom_path()
  }

  if (interactive) {
    plotly::ggplotly(p, tooltip = "text")
  } else {
    p + ggplot2::theme(legend.position = "right")
  }
}
