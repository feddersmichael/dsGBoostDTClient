% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds.select_split.R
\name{ds.select_split}
\alias{ds.select_split}
\title{Choose the best split}
\usage{
ds.select_split(leaves, spp_cand, data_classes, reg_par)
}
\arguments{
\item{leaves}{Histogram-sums for all features and split-bins for each leaf.}

\item{spp_cand}{The candidates for a possible further split of the current
tree.}

\item{data_classes}{Data class for all features.}

\item{reg_par}{Regularisation parameters which specify the loss function.}
}
\value{
The best possible split under the possible ones.
}
\description{
Choose the best split
}
