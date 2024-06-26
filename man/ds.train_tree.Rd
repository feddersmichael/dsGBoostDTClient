% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds.train_tree.R
\name{ds.train_tree}
\alias{ds.train_tree}
\title{Train a single tree}
\usage{
ds.train_tree(
  data_name,
  bounds_and_levels,
  data_classes,
  output_var,
  amt_trees,
  max_splits = 5L,
  split_method,
  loss_function,
  amt_spp,
  cand_select,
  weight_update,
  reg_par = c(lambda = 5, gamma = 5),
  dropout_rate = 0.05,
  ithess_stop,
  add_par = NULL,
  datasources = NULL
)
}
\arguments{
\item{data_name}{The name under which the data is saved on the server.}

\item{bounds_and_levels}{The maximum and minimum values for numeric features
and levels for factor features.}

\item{data_classes}{Data class for all features.}

\item{output_var}{Name of the output variable.}

\item{amt_trees}{How many trees have been built already.}

\item{max_splits}{The maximum amount of splits in the trained tree.}

\item{split_method}{Through which method we choose the tree-splits.}

\item{loss_function}{The type of loss function under which we optimise the
tree.}

\item{amt_spp}{The amount of splitting point candidates per feature.}

\item{cand_select}{Splitting-point selection for numeric and factor features.}

\item{weight_update}{Through which method we choose the weights for our tree.}

\item{reg_par}{Regularisation parameter which prevent overfitting.}

\item{dropout_rate}{Chance that a tree is not used for building the next
tree.}

\item{ithess_stop}{Maximum amount of times we update the split-point
candidates if the split-method is "totally_random"}

\item{add_par}{Additional parameters for the iterative hessian mode.}

\item{datasources}{DATASHIELD server connection.}
}
\value{
The trained tree.
}
\description{
Train a single tree
}
