#' Gurobi R package
#'
#' This section documents the Gurobi R interface. For those of you who
#' are not familiar with R, it is an environment for doing numerical
#' computing, a free language for statistical computing. Please visit
#' the [R website](https://www.r-project.org) for more
#' information. This manual begins with a quick overview of the
#' methods provided by our R API.  It then continues with a
#' comprehensive presentation of all of the available methods,their
#' arguments, and their return values.  If you are new to the Gurobi
#' Optimizer, we suggest that you start with the [Quick Start
#' Guide](https://www.gurobi.com/documentation/quickstart.html),
#' [Example Tour](https://www.gurobi.com/documentation/9.0/examples/index.html).
#' These documents provide concrete examples of how to use the methods
#' described here.
#'
#' @section Notes:
#' A quick note for new users: the convention in math programming is
#' that variables are non-negative unless specified otherwise.  You'll
#' need to explicitly set lower bounds if you want variables to be
#' able to take negative values.
#'
#' @name gurobi-package
#' @useDynLib gurobi
#' @aliases gurobi-package gurobi_package
#' @docType package
#' @author Gurobi Optimization
#' @keywords optimize
NULL


