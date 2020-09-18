#' Optimize a model
#'
#' @name gurobi
#'
#' @param model The model list must contain a valid Gurobi model. See
#' \code{\link{gurobi_model}} for more information.
#' @param params The params \code{list}, when provided, contains a list of
#' modified Gurobi parameters. See the \code{\link{gurobi_params}}
#' documentation for more details.
#' @param env The env list, when provided, allows you to use Gurobi Compute
#' Server or Gurobi Instant Cloud. See \code{\link{gurobi_env}} for more
#' information.
#' @return a list with named components: see [The optimization result].
#'
#' @description
#'
#' This function optimizes the given model.  The algorithm used for the
#' optimization depends on the model type (simplex or barrier for a continuous
#' model; branch-and-cut for a MIP model).  Upon successful completion it will
#' return a \code{list} variable containing solution information.
#'
#' Please consult c("(FIXME-MRhrefH: #1; #2; #3)", "this section",
#' "sec:VarsConstraintsObjectives", "variables_and_constraints")(FIXME-MRhrefH:
#' this section; sec:VarsConstraintsObjectives; variables_and_constraints) for
#' a discussion of some of the practical issues associated with solving a
#' precisely defined mathematical model using finite-precision floating-point
#' arithmetic.
#'
#' @section The optimization result:
#'
#' The c("\\code{\\link{#3}}", "gurobi", "r:gurobi",
#' "gurobi")\code{\link{gurobi}} function returns a \code{list}, with the
#' various results of the optimization stored in its named components. The
#' specific results that are available depend on the type of model that was
#' solved, the parameters used, and the status of the optimization. The
#' following is a list of named components that might be available in the
#' returned result.  We will discuss the circumstances under which each will be
#' available after presenting the list.
#'
#' @section Model named components:
#'
#' \itemize{ \item[status] The status of the optimization, returned as a
#' string. The desired result is \code{'OPTIMAL'}, which indicates that an
#' optimal solution to the model was found. Other status are possible, for
#' example if the model has no feasible solution or if you set a Gurobi
#' parameter that leads to early solver termination. See the c("(FIXME-MRhrefH:
#' #1; #2; #3)", "Status Code", "sec:StatusCodes",
#' "optimization_status_codes")(FIXME-MRhrefH: Status Code; sec:StatusCodes;
#' optimization_status_codes) section for further information on the Gurobi
#' status codes.
#'
#' \item[objval] The objective value of the computed solution.  Note that for
#' multi-objective models \code{result$objval} will be a vector, where
#' \code{result$objvalc("[[#1]]", "i")[[i]]} stores the value for c("#1", " ")
#' \code{model$multiobjc("[[#1]]", "i")[[i]]}.
#'
#' \item[objbound] Best available bound on solution (lower bound for
#' minimization, upper bound for maximization).
#'
#' \item[objboundc] The best unrounded bound on the optimal objective. In
#' contrast to \code{objbound}, this attribute does not take advantage of
#' objective integrality information to round to a tighter bound.  For example,
#' if the objective is known to take an integral value and the current best
#' bound is 1.5, \code{ObjBound} will return 2.0 while \code{ObjBoundC} will
#' return 1.5.
#'
#' \item[mipgap] Current relative MIP optimality gap; computed as \eqn{\vert
#' ObjBound-ObjVal\vert/\vert ObjVal\vert}{|ObjBound-ObjVal|/|ObjVal|} (where
#' \eqn{ObjBound}{ObjBound} and \eqn{ObjVal}{ObjVal} are the MIP objective
#' bound and incumbent solution objective, respectively). Returns
#' \code{GRB_INFINITY} when an incumbent solution has not yet been found, when
#' no objective bound is available, or when the current incumbent objective is
#' 0.  This is only available for mixed-integer problems.
#'
#' \item[runtime] The elapsed wall-clock time (in seconds) for the
#' optimization.
#'
#' \item[itercount] Number of simplex iterations performed.
#'
#' \item[baritercount] Number of barrier iterations performed.
#'
#' \item[nodecount] Number of branch-and-cut nodes explored.
#'
#' \item[farkasproof] Magnitude of infeasibility violation in Farkas
#' infeasibility proof. Only available if the model was found to be infeasible.
#' Please refer to c("(FIXME-MRhrefH: #1; #2; #3)", "FarkasProof",
#' "attr:FarkasProof", "farkasproof")(FIXME-MRhrefH: FarkasProof;
#' attr:FarkasProof; farkasproof) for details. }
#'
#' @section Variable named components:
#'
#' \itemize{ \item[x] The computed solution. This vector contains one entry for
#' each column of \code{A}.
#'
#' \item[rc] Variable reduced costs for the computed solution. This vector
#' contains one entry for each column of \code{A}.
#'
#' \item[vbasis] Variable basis status values for the computed optimal basis.
#' You generally should not concern yourself with the contents of this vector.
#' If you wish to use an advanced start later, you would simply copy the
#' \code{vbasis} and \code{cbasis} named components into the corresponding
#' named components for the next model. This vector contains one entry for each
#' column of \code{A}.
#'
#' \item[unbdray] Unbounded ray. Provides a vector that, when added to any
#' feasible solution, yields a new solution that is also feasible but improves
#' the objective. Only available if the model is found to be unbounded.  This
#' vector contains one entry for each column of \code{A}. }
#'
#' @section Linear constraint named components:
#'
#' \itemize{ \item[slack] The constraint slack for the computed solution. This
#' vector contains one entry for each row of \code{A}.
#'
#' \item[pi] Dual values for the computed solution (also known as \emph{shadow
#' prices}). This vector contains one entry for each row of \code{A}.
#'
#' \item[cbasis] Constraint basis status values for the computed optimal basis.
#' This vector contains one entry for each row of \code{A}.
#'
#' \item[farkasdual] Farkas infeasibility proof. Only available if the model
#' was found to be infeasible.  Please refer to c("(FIXME-MRhrefH: #1; #2;
#' #3)", "FarkasDual", "attr:FarkasDual", "farkasdual")(FIXME-MRhrefH:
#' FarkasDual; attr:FarkasDual; farkasdual) for details. }
#'
#' @section Quadratic constraint named components:
#'
#' \itemize{ \item[qcslack] The quadratic constraint slack in the current
#' solution.  This vector contains one entry for each quadratic constraint.
#'
#' \item[qcpi] The dual values associated with the quadratic constraints.  This
#' vector contains one entry for each quadratic constraint. }
#'
#' @section Solution Pool named components:
#'
#' \itemize{
#' \item[pool] When multiple solutions are found during the
#' optimization call, these solutions are returned in this named component. A A
#' list of lists.  When present, each list has the following named components:
#' \itemize{ \item[objval] Stores the objective value of the \eqn{i}{i}-th
#' solution in \code{result$poolc("[[#1]]", "i")[[i]]$objval}. Note that when
#' the model is a multi-objective model, instead of a single value,
#' \code{result$pool{c("[[#1]]", "i")[[i]]}$objval{c("[#1]", "j")[j]}} stores
#' the value of the \eqn{j}{j}-th objective function for the \eqn{i}{i}-th
#' solution.
#'
#' \item[xn] Stores the \eqn{i}{i}-th solution in \code{result$poolc("[[#1]]",
#' "i")[[i]]$xn}. This vector contains one entry for each column of \code{A}. }
#' Note that to query the number of solutions stored, you can query the length
#' of \code{result$pool}.
#'
#' \item[poolobjbound] For single-objective MIP optimization problems, this
#' value gives a bound on the best possible objective of an undiscovered
#' solution.  The difference between this value and \code{objbound} is that the
#' former gives an objective bound for undiscovered solutions, while the latter
#' gives a bound for any solution.
#'
#' }
#'
#' @section What is Available When:
#'
#' The \code{status} named component will be present in all cases. It indicates
#' whether Gurobi was able to find a proven optimal solution to the model. In
#' cases where a solution to the model was found, optimal or otherwise, the
#' \code{objval} and \code{x} named components will be present.
#'
#' For linear and quadratic programs, if a solution is available, then the
#' \code{pi} and \code{rc} named components will also be present. For models
#' with quadratic constraints, if the parameter \code{qcpdual} is set to 1, the
#' named component \code{qcpi} will be present. If the final solution is a
#' \emph{basic} solution (computed by simplex), then \code{vbasis} and
#' \code{cbasis} will be present. If the model is an unbounded linear program
#' and the c("(FIXME-MRhrefH: #1; #2; #3)", "InfUnbdInfo",
#' "parameter:InfUnbdInfo", "infunbdinfo")(FIXME-MRhrefH: InfUnbdInfo;
#' parameter:InfUnbdInfo; infunbdinfo) parameter is set to 1, the named
#' component \code{unbdray} will be present. Finally, if the model is an
#' infeasible linear program and the c("(FIXME-MRhrefH: #1; #2; #3)",
#' "InfUnbdInfo", "parameter:InfUnbdInfo", "infunbdinfo")(FIXME-MRhrefH:
#' InfUnbdInfo; parameter:InfUnbdInfo; infunbdinfo) parameter is set to 1, the
#' named components \code{farkasdual} and \code{farkasproof} will be set.
#'
#' For mixed integer problems, no dual information (i.e. \code{pi},
#' \code{slack}, \code{rc}, \code{vbasis}, \code{cbasis}, \code{qcslack},
#' \code{qcpi}, \code{ubdray} or \code{farkasdual}) is ever available. When
#' multiple solutions are found, the \code{pool} and \code{poolobjbound} named
#' components will be present. Depending on the \code{status} named component
#' value, the named components \code{nodecount}, \code{objbound},
#' \code{objbundc} and \code{mipgap} will be available.
#'
#' For continuous and mixed-integer models, under normal execution, the named
#' components \code{runtime}, \code{itercount} and \code{baritercount} will be
#' available.
#'
#'
#' @author Gurobi Optimization
#' @seealso The \code{\link{gurobi_package}} documentation contains general
#' information on the Gurobi R interface. The \code{\link{gurobi_problem}}
#' documentation has a detailed description of the types of models that gurobi
#' is able to optimize. The \code{\link{gurobi_usage}} documentation has an
#' overview on the usage of the Gurobi package.
#'
#' For a detailed description of variables used as input and output of gurobi
#' functions see \code{\link{gurobi_model}}, \code{\link{gurobi_params}} and
#' \code{\link{gurobi_env}}.
#'
#' The functions provided by the Gurobi package are: \code{\link{gurobi}},
#' \code{\link{gurobi_iis}}, \code{\link{gurobi_feasrelax}},
#' \code{\link{gurobi_relax}}, \code{\link{gurobi_read}} and
#' \code{\link{gurobi_write}}.
#' @references Gurobi Optimization (\url{http://www.gurobi.com}).
#'
#' Gurobi Optimizer Reference Manual
#' (\url{http://www.gurobi.com/documentation/9.0/refman/index.html}).
#' @keywords optimize
#' @examples
#'
#' result <- gurobi(model, params)
#' if (result$status == 'OPTIMAL') {
#'   print(result$objval)
#'   print(result$x)
#' } else {
#'   cat('Optimization returned status:', formatC(result$status), '\n')
#' }
#'
#'
#' @export gurobi
gurobi <- function(model, params = NULL, env = NULL)
{
  gurobi_common_wrapper(env_args(env, params),
                        model_args(model),
                        list(optimize = list()))
}



#' Read a model from a file.
#'
#' @param filename Name of the file to read. Note that the type of the file is
#' encoded in the file name suffix. The filename suffix should be one of .mps,
#' .rew, .lp, .rlp, .ilp, or .opb (see the c("(FIXME-MRhrefH: #1; #2; #3)",
#' "file formats", "sec:FileFormats", "model_file_formats")(FIXME-MRhrefH: file
#' formats; sec:FileFormats; model_file_formats) section for details on Gurobi
#' file formats). The files can be compressed, so additional suffixes of .gz,
#' .bz2, .zip, or .7z are accepted.
#' @param env The env list, when provided, allows you to use Gurobi Compute
#' Server or Gurobi Instant Cloud. See \code{\link{gurobi_env}} for more
#' information.
#' @return A \code{model} list variable, as described in the
#' c("\\code{\\link{#3}}", "model", "r:model",
#' "gurobi_model")\code{\link{gurobi_model}} section.
#' @author Gurobi Optimization
#' @seealso The \code{\link{gurobi_package}} documentation contains general
#' information on the Gurobi R interface. The \code{\link{gurobi_problem}}
#' documentation has a detailed description of the types of models that gurobi
#' is able to optimize. The \code{\link{gurobi_usage}} documentation has an
#' overview on the usage of the Gurobi package.
#'
#' For a detailed description of variables used as input and output of gurobi
#' functions see \code{\link{gurobi_model}}, \code{\link{gurobi_params}} and
#' \code{\link{gurobi_env}}.
#'
#' The functions provided by the Gurobi package are: \code{\link{gurobi}},
#' \code{\link{gurobi_iis}}, \code{\link{gurobi_feasrelax}},
#' \code{\link{gurobi_relax}}, \code{\link{gurobi_read}} and
#' \code{\link{gurobi_write}}.
#' @references Gurobi Optimization (\url{http://www.gurobi.com}).
#'
#' Gurobi Optimizer Reference Manual
#' (\url{http://www.gurobi.com/documentation/9.0/refman/index.html}).
#' @keywords optimize
#' @examples
#' %
#' model  <-  gurobi_read('stein9.mps')
#' result <-  gurobi(model)
#'
#' @export gurobi_read
gurobi_read <- function(filename, env = NULL)
{
  to_model(gurobi_common_wrapper(env_args(env, NULL),
                                 list(),
                                 list(readmodel =
                                           list(filename = to_chr(filename)))))
}



#' Write a model to a file.
#'
#' @param model The model list must contain a valid Gurobi model. See
#' \code{\link{gurobi_model}} for more information.
#' @param filename Name of the file to write. Note that the type of the file is
#' encoded in the file name suffix. The filename suffix should be one of .mps,
#' .rew, .lp, .rlp, or .ilp, to indicate the desired file format (see the
#' c("(FIXME-MRhrefH: #1; #2; #3)", "file formats", "sec:FileFormats",
#' "model_file_formats")(FIXME-MRhrefH: file formats; sec:FileFormats;
#' model_file_formats) section for details on Gurobi file formats). The files
#' can be compressed, so additional suffixes of .gz, .bz2, .zip, or .7z are
#' accepted.
#' @param env The env list, when provided, allows you to use Gurobi Compute
#' Server or Gurobi Instant Cloud. See \code{\link{gurobi_env}} for more
#' information.
#' @author Gurobi Optimization
#' @seealso The \code{\link{gurobi_package}} documentation contains general
#' information on the Gurobi R interface. The \code{\link{gurobi_problem}}
#' documentation has a detailed description of the types of models that gurobi
#' is able to optimize. The \code{\link{gurobi_usage}} documentation has an
#' overview on the usage of the Gurobi package.
#'
#' For a detailed description of variables used as input and output of gurobi
#' functions see \code{\link{gurobi_model}}, \code{\link{gurobi_params}} and
#' \code{\link{gurobi_env}}.
#'
#' The functions provided by the Gurobi package are: \code{\link{gurobi}},
#' \code{\link{gurobi_iis}}, \code{\link{gurobi_feasrelax}},
#' \code{\link{gurobi_relax}}, \code{\link{gurobi_read}} and
#' \code{\link{gurobi_write}}.
#' @references Gurobi Optimization (\url{http://www.gurobi.com}).
#'
#' Gurobi Optimizer Reference Manual
#' (\url{http://www.gurobi.com/documentation/9.0/refman/index.html}).
#' @keywords optimize
#' @examples
#'
#' model            <- list()
#' model$A          <- matrix(c(1,2,3,1,1,0), nrow=2, byrow=T)
#' model$obj        <- c(1,1,2)
#' model$modelsense <- 'max'
#' model$rhs        <- c(4,1)
#' model$sense      <- c('<', '>')
#' gurobi_write(model, 'mymodel.mps');
#' gurobi_write(model, 'mymodel.lp');
#' gurobi_write(model, 'mymodel.mps.bz2');
#'
#'
#' @export gurobi_write
gurobi_write <- function(model, filename, env = NULL)
{
  gurobi_common_wrapper(env_args(env, NULL),
                        model_args(model),
                        list(write = list(filename = to_chr(filename))))
}



#' Compute an Irreducible Inconsistent Subsystem (IIS).
#'
#' An IIS is a subset of the constraints and variable bounds with the following
#' properties: \itemize{ \item the subsystem represented by the IIS is
#' infeasible, and \item if any of the constraints or bounds of the IIS is
#' removed, the subsystem becomes feasible. } Note that an infeasible model may
#' have multiple IISs. The one returned by Gurobi is not necessarily the one
#' with minimum cardinality; there may exist others with fewer constraints or
#' bounds.  If an IIS computation is interrupted before completion, Gurobi will
#' return the smallest IIS found to that point.
#'
#' You can obtain information about the outcome of the IIS computation from the
#' returned IIS result (described below). Note that this method can be used to
#' compute IISs for both continuous and MIP models.
#'
#'
#' @param model The model list must contain a valid Gurobi model. See
#' \code{\link{gurobi_model}} for more information.
#' @param params The params \code{list}, when provided, contains a list of
#' modified Gurobi parameters. See the \code{\link{gurobi_params}}
#' documentation for more details.
#' @param env The env list, when provided, allows you to use Gurobi Compute
#' Server or Gurobi Instant Cloud. See \code{\link{gurobi_env}} for more
#' information.
#' @return
#'
#' The \code{gurobi_iis()} function returns a \code{list}, with various results
#' stored in its named components. The specific results that are available
#' depend on the type of model.
#'
#' The returned \code{list} will always contain the following \code{named
#' components}: \itemize{ \item[minimal] A logical scalar that indicates
#' whether the computed IIS is minimal. It will normally be true, but it may be
#' false if the IIS computation was stopped early (due to a time limit or a
#' user interrupt). \item[Arows] c("A logical vector that indicates wheather a
#' #1 appears in the computed IIS.", "linear constraint")A logical vector that
#' indicates wheather a linear constraint appears in the computed IIS.
#' \item[lb] c("A logical vector that indicates wheather a #1 appears in the
#' computed IIS.", "lower bound")A logical vector that indicates wheather a
#' lower bound appears in the computed IIS. \item[ub] c("A logical vector that
#' indicates wheather a #1 appears in the computed IIS.", "upper bound")A
#' logical vector that indicates wheather a upper bound appears in the computed
#' IIS. }
#'
#' If your model contains general constraints, the returned \code{list} will
#' also contain the following \code{named components}: \itemize{ \item
#' [genconmax] c("A logical vector that indicates wheather a #1 appears in the
#' computed IIS.", "general MAX constraint")A logical vector that indicates
#' wheather a general MAX constraint appears in the computed IIS. \item
#' [genconmin] c("A logical vector that indicates wheather a #1 appears in the
#' computed IIS.", "general MIN constraint")A logical vector that indicates
#' wheather a general MIN constraint appears in the computed IIS. \item
#' [genconand] c("A logical vector that indicates wheather a #1 appears in the
#' computed IIS.", "general AND constraint")A logical vector that indicates
#' wheather a general AND constraint appears in the computed IIS. \item
#' [genconor] c("A logical vector that indicates wheather a #1 appears in the
#' computed IIS.", "general OR constraint")A logical vector that indicates
#' wheather a general OR constraint appears in the computed IIS. \item
#' [genconabs] c("A logical vector that indicates wheather a #1 appears in the
#' computed IIS.", "general ABS constraint")A logical vector that indicates
#' wheather a general ABS constraint appears in the computed IIS. \item
#' [genconind] c("A logical vector that indicates wheather a #1 appears in the
#' computed IIS.", "general INDICATOR constraint")A logical vector that
#' indicates wheather a general INDICATOR constraint appears in the computed
#' IIS. \item [genconpwl] c("A logical vector that indicates wheather a #1
#' appears in the computed IIS.", "general piecewise-linear function
#' constraint")A logical vector that indicates wheather a general
#' piecewise-linear function constraint appears in the computed IIS. \item
#' [genconpoly] c("A logical vector that indicates wheather a #1 appears in the
#' computed IIS.", "polynomial function constraint")A logical vector that
#' indicates wheather a polynomial function constraint appears in the computed
#' IIS. \item [genconexp] c("A logical vector that indicates wheather a #1
#' appears in the computed IIS.", "natural exponential function constraint")A
#' logical vector that indicates wheather a natural exponential function
#' constraint appears in the computed IIS. \item [genconexpa] c("A logical
#' vector that indicates wheather a #1 appears in the computed IIS.",
#' "exponential function constraint")A logical vector that indicates wheather a
#' exponential function constraint appears in the computed IIS. \item
#' [genconlog] c("A logical vector that indicates wheather a #1 appears in the
#' computed IIS.", "natural logarithmic function constraint")A logical vector
#' that indicates wheather a natural logarithmic function constraint appears in
#' the computed IIS. \item [genconloga] c("A logical vector that indicates
#' wheather a #1 appears in the computed IIS.", "logarithmic function
#' constraint")A logical vector that indicates wheather a logarithmic function
#' constraint appears in the computed IIS. \item [genconpow] c("A logical
#' vector that indicates wheather a #1 appears in the computed IIS.", "power
#' function constraint")A logical vector that indicates wheather a power
#' function constraint appears in the computed IIS. \item [genconsin] c("A
#' logical vector that indicates wheather a #1 appears in the computed IIS.",
#' "SIN function constraint")A logical vector that indicates wheather a SIN
#' function constraint appears in the computed IIS. \item [genconcos] c("A
#' logical vector that indicates wheather a #1 appears in the computed IIS.",
#' "COS function constraint")A logical vector that indicates wheather a COS
#' function constraint appears in the computed IIS. \item [gencontan] c("A
#' logical vector that indicates wheather a #1 appears in the computed IIS.",
#' "TAN function constraint")A logical vector that indicates wheather a TAN
#' function constraint appears in the computed IIS. }
#'
#' If your model contains SOS constraints, the returned \code{list} will also
#' contain the following \code{named component}: \itemize{ \item[sos] A logical
#' vector that indicates whether an SOS constraint appears in the computed IIS
#' }
#'
#' If your model contains quadratic constraints, the returned \code{list} will
#' also contain the following \code{named component}: \itemize{ \item[quadcon]
#' c("A logical vector that indicates wheather a #1 appears in the computed
#' IIS.", "quadratic constraint")A logical vector that indicates wheather a
#' quadratic constraint appears in the computed IIS. }
#' @author Gurobi Optimization
#' @seealso The \code{\link{gurobi_package}} documentation contains general
#' information on the Gurobi R interface. The \code{\link{gurobi_problem}}
#' documentation has a detailed description of the types of models that gurobi
#' is able to optimize. The \code{\link{gurobi_usage}} documentation has an
#' overview on the usage of the Gurobi package.
#'
#' For a detailed description of variables used as input and output of gurobi
#' functions see \code{\link{gurobi_model}}, \code{\link{gurobi_params}} and
#' \code{\link{gurobi_env}}.
#'
#' The functions provided by the Gurobi package are: \code{\link{gurobi}},
#' \code{\link{gurobi_iis}}, \code{\link{gurobi_feasrelax}},
#' \code{\link{gurobi_relax}}, \code{\link{gurobi_read}} and
#' \code{\link{gurobi_write}}.
#' @references Gurobi Optimization (\url{http://www.gurobi.com}).
#'
#' Gurobi Optimizer Reference Manual
#' (\url{http://www.gurobi.com/documentation/9.0/refman/index.html}).
#' @keywords optimize
#' @examples
#' %
#' model <-  gurobi_read('examples/data/klein1.mps')
#' iis <-  gurobi_iis(model)
#'
#' @export gurobi_iis
gurobi_iis <- function(model, params = NULL, env = NULL)
{
  to_iis(gurobi_common_wrapper(env_args(env, params),
                               model_args(model),
                               list(computeIIS = list())))
}



#' Compute a feasibility relaxation
#'
#' This function computes a feasibility relaxation for the input \code{model}
#' argument.  The feasibility relaxation is a model that, when solved,
#' minimizes the amount by which the solution violates the bounds and linear
#' constraints of the original model.  You must provide a penalty to associate
#' with relaxing each individual bound or constraint (through the
#' \code{penalties} argument).  These penalties are interpreted in different
#' ways, depending on the value of the \code{relaxobjtype} argument.
#'
#'
#' @param model The model list must contain a valid Gurobi model. See
#' \code{\link{gurobi_model}} for more information.
#' @param relaxobjtype The approach used to impose penalties on violations. If
#' you specify \code{relaxobjtype=0}, the objective for the feasibility
#' relaxation is to minimize the sum of the weighted magnitudes of the bound
#' and constraint violations. If you specify \code{relaxobjtype=1}, the
#' objective for the feasibility relaxation is to minimize the weighted sum of
#' the squares of the bound and constraint violations. If you specify
#' \code{relaxobjtype=2}, the objective for the feasibility relaxation is to
#' minimize the weighted count of bound and constraint violations. In all
#' cases, the weights are taken from \code{penalties$lb}, \code{penalties$ub}
#' and \code{penalties$rhs}. You can provide the special penalty value
#' \code{Inf} to indicate that the corresponding bound or constraint cannot be
#' relaxed.
#' @param minrelax The \code{minrelax} argument is a boolean that controls the
#' type of feasibility relaxation that is created.  If \code{minrelax=False},
#' optimizing the returned model gives a solution that minimizes the cost of
#' the violation.  If \code{minrelax=True}, optimizing the returned model finds
#' a solution that minimizes the original objective, but only from among those
#' solutions that minimize the cost of the violation.  Note that
#' \code{gurobi_feasrelax} must solve an optimization problem to find the
#' minimum possible relaxation when \code{minrelax=True}, which can be quite
#' expensive.
#' @param penalties The \code{penalties} argument is a A list of lists, having
#' the following optional named components (default: all \code{Inf}): \code{lb}
#' Penalty for violating each lower bound.  \code{ub} Penalty for violating
#' each upper bound.  \code{rhs} Penalty for violating each constraint.
#'
#' To give an example, if a constraint with \code{penalties.rhs} value \code{p}
#' is violated by 2.0, it would contribute \code{2*p} to the feasibility
#' relaxation objective for \code{relaxobjtype=0}, \code{2*2*p} for
#' \code{relaxobjtype=1}, and \code{p} for \code{relaxobjtype=2}.
#' @param env The env list, when provided, allows you to use Gurobi Compute
#' Server or Gurobi Instant Cloud. See \code{\link{gurobi_env}} for more
#' information.
#' @return A \code{list} containing two named components: \code{result$model},
#' a list variable, as described in the c("\\code{\\link{#3}}", "model",
#' "r:model", "gurobi_model")\code{\link{gurobi_model}} argument section.
#' \code{result$feasobj}, a scalar. If \code{minrelax==true} this is the
#' relaxation problem objective value, 0.0 otherwise.
#' @author Gurobi Optimization
#' @seealso The \code{\link{gurobi_package}} documentation contains general
#' information on the Gurobi R interface. The \code{\link{gurobi_problem}}
#' documentation has a detailed description of the types of models that gurobi
#' is able to optimize. The \code{\link{gurobi_usage}} documentation has an
#' overview on the usage of the Gurobi package.
#'
#' For a detailed description of variables used as input and output of gurobi
#' functions see \code{\link{gurobi_model}}, \code{\link{gurobi_params}} and
#' \code{\link{gurobi_env}}.
#'
#' The functions provided by the Gurobi package are: \code{\link{gurobi}},
#' \code{\link{gurobi_iis}}, \code{\link{gurobi_feasrelax}},
#' \code{\link{gurobi_relax}}, \code{\link{gurobi_read}} and
#' \code{\link{gurobi_write}}.
#' @references Gurobi Optimization (\url{http://www.gurobi.com}).
#'
#' Gurobi Optimizer Reference Manual
#' (\url{http://www.gurobi.com/documentation/9.0/refman/index.html}).
#' @keywords optimize
#' @examples
#' %
#' penalties  <- list()
#' model  <-  gurobi_read('stein9.mps')
#' penalties$lb <-  c("rep(1,#1)", "length(model$lb)")rep(1,length(model$lb))
#' penalties$ub <-  c("rep(1,#1)", "length(model$ub)")rep(1,length(model$ub))
#' penalties$rhs <-  c("rep(1,#1)", "length(model$rhs)")rep(1,length(model$rhs))
#' feasrelaxresult  <-  gurobi_feasrelax(model, 0, False, penalties)
#'
#' @export gurobi_feasrelax
gurobi_feasrelax <- function(model, relaxobjtype, minrelax, penalties,
                             params = NULL, env = NULL)
{
  to_feasrelax(gurobi_common_wrapper(env_args(env, params),
                                     model_args(model),
                                     list(feasrelax =
                                                   feasrelax_args(relaxobjtype,
                                                                  minrelax,
                                                                  penalties,
                                                                  model$A))))
}



#' Create the relaxation of a MIP model.
#'
#' Transforms integer variables into continuous variables, and removes
#' SOS and general constraints.
#'
#' @param model The model list must contain a valid Gurobi model. See
#' \code{\link{gurobi_model}} for more information.
#' @param env The env list, when provided, allows you to use Gurobi Compute
#' Server or Gurobi Instant Cloud. See \code{\link{gurobi_env}} for more
#' information.
#' @return A \code{model} list variable, as described in the
#' c("\\code{\\link{#3}}", "model", "r:model",
#' "gurobi_model")\code{\link{gurobi_model}} parameter section.
#' @author Gurobi Optimization
#' @seealso The \code{\link{gurobi_package}} documentation contains general
#' information on the Gurobi R interface. The \code{\link{gurobi_problem}}
#' documentation has a detailed description of the types of models that gurobi
#' is able to optimize. The \code{\link{gurobi_usage}} documentation has an
#' overview on the usage of the Gurobi package.
#'
#' For a detailed description of variables used as input and output of gurobi
#' functions see \code{\link{gurobi_model}}, \code{\link{gurobi_params}} and
#' \code{\link{gurobi_env}}.
#'
#' The functions provided by the Gurobi package are: \code{\link{gurobi}},
#' \code{\link{gurobi_iis}}, \code{\link{gurobi_feasrelax}},
#' \code{\link{gurobi_relax}}, \code{\link{gurobi_read}} and
#' \code{\link{gurobi_write}}.
#' @references Gurobi Optimization (\url{http://www.gurobi.com}).
#'
#' Gurobi Optimizer Reference Manual
#' (\url{http://www.gurobi.com/documentation/9.0/refman/index.html}).
#' @keywords optimize
#' @examples
#'
#' model  <-  gurobi_read('stein9.mps')
#' relaxed <-  gurobi_relax(model)
#'
#' @export gurobi_relax
gurobi_relax <- function(model, env = NULL)
{
  to_model(gurobi_common_wrapper(env_args(env, NULL),
                                 model_args(model),
                                 list(relaxmodel = list())))
}

gurobi_presolve <- function(model, params = NULL, env = NULL)
{
  to_model(gurobi_common_wrapper(env_args(env, params),
                                 model_args(model),
                                 list(presolvemodel = list())))
}
