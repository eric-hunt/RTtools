% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model.R
\name{fit_model}
\alias{fit_model}
\title{Fit a logarithmic growth curve to real-time fluorescence data}
\usage{
fit_model(data, control = NULL, clean = TRUE)
}
\arguments{
\item{data}{a dataframe containing the imported real-time fluorescence data, variables *cycle*, *well*, and *intensity*}

\item{control}{a character string indicating the well which will serve as a baseline control (e.g. *"A1"* or *"H12"*)}

\item{clean}{boolean parameter option to remove raw data which is otherwise nested in a list column *data*;
defaults to TRUE leaving only variables *well*, *rate*, and *endpoint*}
}
\value{
a [tibble][tibble::tibble-package] containing the calculated variables *rate* and *endpoint*
}
\description{
\code{fit_model} fits a logarithmic growth curve to the fluorescence curves and extracts numeric values for growth rate (rise time) and endpoint fluorescence.
}
