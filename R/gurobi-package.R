#' Gurobi Optimizer Interface Stub
#'
#' This is a stub package. The full \code{gurobi} R package is distributed
#' by Gurobi Optimization and requires a Gurobi license.
#'
#' Install the real package following the instructions at
#' \url{https://www.gurobi.com/documentation/current/refman/r_ins_the_r_package.html}.
#'
#' @name gurobi-package
#' @aliases gurobi-package
#' @docType package
NULL

#' Gurobi Optimizer (Stub)
#'
#' This stub signals that the real \code{gurobi} package from Gurobi
#' Optimization is required.
#'
#' @param ... Ignored.
#' @return Nothing; always raises an error.
#' @export
gurobi <- function(...) {
  stop(
    "This is a stub package. Install the real 'gurobi' package from:\n",
    "https://www.gurobi.com/documentation/current/refman/r_ins_the_r_package.html",
    call. = FALSE
  )
}
