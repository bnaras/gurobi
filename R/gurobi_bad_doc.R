###' @name gurobi_env
###' The gurobi environment
###'
###' @description
###' The optional \code{env} argument is also a \code{list}. It allows you to
###' solve your problem on a Gurobi Compute Server or the Gurobi Instant Cloud.
###'
###' @section Using a Compute Server License:
###'
###' Gurobi Compute Server allows you to offload optimization jobs to a remote
###' server.  Servers are organized into clusters.  By providing the name of any
###' node within the cluster, your job will automatically be sent to the least
###' heavily loaded node in the cluster.  If all nodes are at capacity, your job
###' will be placed in a queue, and will proceed once capacity becomes available.
###' You can find additional information about Gurobi Compute Server in the
###' \href{http://www.gurobi.com/documentation/9.0/remoteservices/remoteservices.htmlfile
###' formats}.
###'
###' The following is an enumeration of all of the named components of the
###' \code{env} argument that Gurobi will take into account.
###'
###' \itemize{
###' \item[computeserver] A Compute Server.  You can refer to the
###' server using its name or its IP address.  If you are using a non-default
###' port, the server name should be followed by the port number (e.g.,
###' \code{server1:61000}).
###'
###' \item[password (optional)] User password on the Compute Server cluster.
###' Obtain this from your Compute Server administrator.
###'
###' \item[priority (optional)] The priority of the job.  Priorities must be
###' between -100 and 100, with a default value of 0 (by convention). Higher
###' priority jobs are chosen from the server job queue before lower priority
###' jobs.  A job with priority 100 runs immediately, bypassing the job queue and
###' ignoring the job limit on the server. You should exercise caution with
###' priority 100 jobs, since they can severely overload a server, which can
###' cause jobs to fail, and in extreme cases can cause the server to crash.
###'
###' \item[router (optional)] The router for the Compute Server cluster.  A
###' router can be used to improve the robustness of a Compute Server deployment.
###' You can refer to the router using either its name or its IP address.  A
###' typical Remote Services deployment will not use a router, so you typically
###' will not need to set this.
###'
###' \item[CStlsinsecure (optional)] Indicates whether to use insecure mode in
###' the TLS (Transport Layer Security).  Set this to 0 unless your server
###' administrator tells you otherwise.
###' }
###'
###' Here is an example of how to use an \code{env} argument to connect to a
###' Compute Server:\cr
###' \preformatted{
###' env <- list()
###' env$computeserver <- 'server1.mycompany.com:61000'
###' env$priority <- 5
###' }
###'
###' @section Using a Gurobi Instant Cloud License:
###'
###' Gurobi Instant Cloud allows you to offload optimization jobs to a Gurobi
###' Compute Server on the cloud.  If an appropriate machine is already running,
###' the job will run on that machine. It will automatically launch a new machine
###' otherwise. Note that launching a new machine can take a few minutes. You can
###' find additional information about the Gurobi Instant Cloud service
###' c("(FIXME-MRhrefH: #1; #2; #3)", "here", "sec:Cloud",
###' "gurobi_instant_cloud")(FIXME-MRhrefH: here; sec:Cloud;
###' gurobi_instant_cloud).
###'
###' The following is an enumeration of all of the named components of the
###' \code{env} argument that Gurobi will take into account.
###'
###' \itemize{
###' \item[accessid] The access ID for your Gurobi Instant Cloud
###' license. This can be retrieved from the Gurobi Instant Cloud website.  When
###' used in combination with your \code{secretkey}, this allows you to launch
###' Instant Cloud instances and submit jobs to them.
###'
###' \item[secretkey] The secret key for your Gurobi Instant Cloud license. This
###' can be retrieved from the Gurobi Instant Cloud website.  When used in
###' combination with your \code{accessid}, this allows you to launch Instant
###' Cloud instances and submit jobs to them.  Note that you should keep your
###' secret key private.
###'
###' \item[pool (optional)] The machine pool.  Machine pools allow you to create
###' fixed configurations on the Instant Cloud website (capturing things like
###' type of machine, geographic region, etc.), and then launch and share
###' machines from client programs without having to restate configuration
###' information each time you launch a machine. If not provided, your job will
###' be launched in the default pool associated with your cloud license.
###'
###' \item[priority (optional)] The priority of the job.  Priorities must be
###' between -100 and 100, with a default value of 0 (by convention). Higher
###' priority jobs are chosen from the server job queue before lower priority
###' jobs.  A job with priority 100 runs immediately, bypassing the job queue and
###' ignoring the job limit on the server. You should exercise caution with
###' priority 100 jobs, since they can severely overload a server, which can
###' cause jobs to fail, and in extreme cases can cause the server to crash.
###' }
###'
###' Here is an example of how to use an \code{env} argument to launch a Gurobi
###' Instant Cloud instance:\cr
###' \preformatted{
###' env <- list()
###' env$accessid <- '3d1ecef9-dfad-eff4-b3fa'
###' env$secretkey <- 'ae6L23alJe3+fas'
###' }
###'
###' Note that when creating an environment variable, you need to choose to use
###' either Compute Server or Instant Cloud.  Populating named components for
###' both will result in an error.
###'
###' @author Gurobi Optimization
###' @seealso The \code{\link{gurobi_package}} documentation contains general
###' information on the Gurobi R interface. The \code{\link{gurobi_problem}}
###' documentation has a detailed description of the types of models that gurobi
###' is able to optimize. The \code{\link{gurobi_usage}} documentation has an
###' overview on the usage of the Gurobi package.
###'
###' For a detailed description of variables used as input and output of gurobi
###' functions see \code{\link{gurobi_model}}, \code{\link{gurobi_params}} and
###' \code{\link{gurobi_env}}.
###'
###' The functions provided by the Gurobi package are: \code{\link{gurobi}},
###' \code{\link{gurobi_iis}}, \code{\link{gurobi_feasrelax}},
###' \code{\link{gurobi_relax}}, \code{\link{gurobi_read}} and
###' \code{\link{gurobi_write}}.
###' @references Gurobi Optimization (\url{http://www.gurobi.com}).
###'
###' Gurobi Optimizer Reference Manual
###' (\url{http://www.gurobi.com/documentation/9.0/refman/index.html}).
###' @keywords optimize
##NULL
##
##
##
###' @name gurobi_model
###'
###' Model variables store optimization problems (as described in the
###' c("\\code{\\link{#3}}", "problem", "r:problem",
###' "gurobi_problem")\code{\link{gurobi_problem}} statement).
###'
###' Models can be built in a number of ways.  You can populate the appropriate
###' named components of the \code{model} list using standard R routines. You can
###' also read a model from a file, using c("\\code{\\link{#3}}", "gurobi_read",
###' "r:gurobi_read", "gurobi_read")\code{\link{gurobi_read}}. A few API
###' functions (c("\\code{\\link{#3}}", "gurobi_feasrelax", "r:gurobi_feasrelax",
###' "gurobi_feasrelax")\code{\link{gurobi_feasrelax}} and
###' c("\\code{\\link{#3}}", "gurobi_relax", "r:gurobi_relax",
###' "gurobi_relax")\code{\link{gurobi_relax}}) also return models.
###'
###' Note that all c("#2", "vector fields within the \\code{model} variable must
###' be dense vectors\nexcept for the linear part of the quadratic constraints
###' and INDICATOR general constraints,\nall matrix fields must be\nsparse
###' matrices, and all strings, names, etc. must be\n\\code{char} arrays.",
###' "matrix named components within the\n\\code{model} variable can be dense or
###' sparse. Sparse matrices\nshould be built using either \\code{sparseMatrix}
###' from the\n\\code{Matrix} package, or \\code{simple_triplet_matrix}\nfrom the
###' \\code{slam} package." )matrix named components within the \code{model}
###' variable can be dense or sparse. Sparse matrices should be built using
###' either \code{sparseMatrix} from the \code{Matrix} package, or
###' \code{simple_triplet_matrix} from the \code{slam} package.
###'
###' The following is an enumeration of all of the named components of the
###' \code{model} argument that Gurobi will take into account when optimizing the
###' model:
###'
###' @section Commonly used named components:
###'
###' \itemize{ \item[A] The linear constraint matrix.
###'
###' \item[obj (optional)] The linear objective vector (the \code{c} vector in
###' the c("\\code{\\link{#3}}", "problem", "r:problem",
###' "gurobi_problem")\code{\link{gurobi_problem}} statement).  When present, you
###' must specify one value for each column of \code{A}. When absent, each
###' variable has a default objective coefficient of 0.
###'
###' \item[sense (optional)] The senses of the linear constraints. Allowed values
###' are \code{'='}, \code{'<'}, or \code{'>'}. You must specify one value for
###' each row of \code{A}, or a single value to specify that all constraints have
###' the same sense. When absent, all senses default to \code{'<'}.
###'
###' \item[rhs (optional)] The right-hand side vector for the linear constraints
###' (\eqn{b}{b} in the c("\\code{\\link{#3}}", "problem", "r:problem",
###' "gurobi_problem")\code{\link{gurobi_problem}} statement). You must specify
###' one value for each row of \code{A}.  When absent, the right-hand side vector
###' defaults to the zero vector.
###'
###' \item[lb (optional)] The lower bound vector. When present, you must specify
###' one value for each column of \code{A}. When absent, each variable has a
###' default lower bound of 0.
###'
###' \item[ub (optional)] The upper bound vector. When present, you must specify
###' one value for each column of \code{A}. When absent, the variables have
###' infinite upper bounds.
###'
###' \item[vtype (optional)] The variable types. This vector is used to capture
###' variable integrality constraints. Allowed values are \code{'C'}
###' (continuous), \code{'B'} (binary), \code{'I'} (integer), \code{'S'}
###' (semi-continuous), or \code{'N'} (semi-integer). Binary variables must be
###' either 0 or 1. Integer variables can take any integer value between the
###' specified lower and upper bounds. Semi-continuous variables can take any
###' value between the specified lower and upper bounds, or a value of zero.
###' Semi-integer variables can take any integer value between the specified
###' lower and upper bounds, or a value of zero. When present, you must specify
###' one value for each column of \code{A}, or a single value to specify that all
###' variables have the same type. When absent, each variable is treated as being
###' continuous. Refer to c("(FIXME-MRhrefH: #1; #2; #3)", "this section",
###' "subsection:Variables", "variables")(FIXME-MRhrefH: this section;
###' subsection:Variables; variables) for more information on variable types.
###'
###' \item[modelsense (optional)] The optimization sense. Allowed values are
###' \code{'min'} (minimize) or \code{'max'} (maximize). When absent, the default
###' optimization sense is minimization.
###'
###' \item[modelname (optional)] The name of the model. The name appears in the
###' Gurobi log, and when writing a model to a file.
###'
###' \item[objcon (optional)] The constant offset in the objective function
###' (\eqn{\mathrm{alpha}}{alpha} in the c("\\code{\\link{#3}}", "problem",
###' "r:problem", "gurobi_problem")\code{\link{gurobi_problem}} statement).
###'
###' \item[varnames (optional)] The variable names vector. A character vector.
###' When present, each element of this vector defines the name of a variable.
###' You must specify a name for each column of \code{A}.
###'
###' \item[constrnames (optional)] The constraint names vector. A character
###' vector. When present, each element of the vector defines the name of a
###' constraint. You must specify a name for each row of \code{A}.
###'
###' }
###'
###' @section Quadratic objective and constraint named components:
###'
###' \itemize{ \item[Q (optional)] The quadratic objective matrix. When present,
###' \code{Q} must be a square matrix whose row and column counts are equal to
###' the number of columns in \code{A}.
###'
###' \item[quadcon (optional)] The quadratic constraints. A A list of lists. When
###' present, each element in \code{quadcon} defines a single quadratic
###' constraint: \eqn{x^TQc\, x + q^Tx \le \mathrm{beta}}{x'Qc x + q'x <= beta}.
###'
###' The \code{Qc} matrix must be a square matrix whose row and column counts are
###' equal to the number of columns of \code{A}. c("#2", "There are\ntwo options
###' to store the matrix: (i) in\n\\code{model$quadcon\\MRpos{i}$Qc} as a
###' sparse\nmatrix; (ii) through three dense
###' vectors\n\\code{model$quadcon\\MRpos{i}$Qrow},\n\\code{model$quadcon\\MRpos{i}$Qcol},
###' and\n\\code{model$quadcon\\MRpos{i}$Qval} specifying\nthe matrix in triple
###' format, with row indices, column indices, and\nvalues, respectively.", "It
###' is stored in\n\\code{model$quadcon\\MRpos{i}$Qc}.")It is stored in
###' \code{model$quadconc("[[#1]]", "i")[[i]]$Qc}.
###'
###' The optional \code{q} vector defines the linear terms in the constraint. It
###' can be a dense vector specifying a value for each column of \code{A} or a
###' sparse vector (c("#2", "sparse n-by-1 matrix", "should be built
###' using\n\\code{sparseVector} from the \\code{Matrix} package")should be built
###' using \code{sparseVector} from the \code{Matrix} package). It is stored in
###' \code{model$quadconc("[[#1]]", "i")[[i]]$q}.
###'
###' The scalar \code{beta} is stored in \code{model$quadconc("[[#1]]",
###' "i")[[i]]$rhs}. It defines the right-hand side value for the constraint.
###'
###' The optional \code{sense} string defines the sense of the quadratic
###' constraint. Allowed values are \code{'<'}, \code{'='} or \code{'>'}.  If not
###' present, the default sense is \code{'<'}. It is stored in
###' \code{model$quadconc("[[#1]]", "i")[[i]]$sense}.
###'
###' The optional \code{name} string defines the name of the quadratic
###' constraint. It is stored in \code{model$quadconc("[[#1]]", "i")[[i]]$name}.
###'
###' }
###'
###' @section SOS constraint named components:
###'
###' \itemize{ \item[sos (optional)] The Special Ordered Set (SOS) constraints. A
###' A list of lists. When present, each entry in \code{sos} defines a single SOS
###' constraint. A SOS constraint can be of type 1 or 2. The type of SOS
###' constraint \eqn{i}{i} is specified via \code{model$sosc("[[#1]]",
###' "i")[[i]]$type}. A type 1 SOS constraint is a set of variables where at most
###' one variable in the set may take a value other than zero. A type 2 SOS
###' constraint is an ordered set of variables where at most two variables in the
###' set may take non-zero values. If two take non-zeros values, they must be
###' contiguous in the ordered set. The members of an SOS constraint are
###' specified by placing their indices in vector \code{model$sosc("[[#1]]",
###' "i")[[i]]$index}. Weights associated with SOS members are provided in vector
###' \code{model$sosc("[[#1]]", "i")[[i]]$weight}. Please refer to
###' c("(FIXME-MRhrefH: #1; #2; #3)", "this section",
###' "subsubsection:SOSConstraints", "constraints")(FIXME-MRhrefH: this section;
###' subsubsection:SOSConstraints; constraints) for details on SOS constraints.
###'
###' }
###'
###' @section Multi-objective named components:
###'
###' \itemize{ \item[multiobj (optional)] Multi-objective specification for the
###' model. A A list of lists. When present, each entry in \code{multiobj}
###' defines a single objective of a multi-objective problem.  Please refer to
###' the c("(FIXME-MRhrefH: #1; #2; #3)", "Multiple Objectives",
###' "sec:MultipleObjectives", "multiple_objectives")(FIXME-MRhrefH: Multiple
###' Objectives; sec:MultipleObjectives; multiple_objectives) section for more
###' details on multi-objective optimization. Each objective \eqn{i}{i} may have
###' the following named components: \itemize{ \item[objn] Specified via
###' \code{model$multiobjc("[[#1]]", "i")[[i]]$objn}. This is the \emph{i}-th
###' objective vector.
###'
###' \item[objcon (optional)] Specified via \code{model$multiobjc("[[#1]]",
###' "i")[[i]]$objcon}. If provided, this is the \emph{i}-th objective constant.
###' The default value is 0.
###'
###' \item[priority (optional)] Specified via \code{model$multiobjc("[[#1]]",
###' "i")[[i]]$priority}. If provided, this value is the \emph{hierarchical}
###' priority for this objective. The default value is 0.
###'
###' \item[weight (optional)] Specified via \code{model$multiobjc("[[#1]]",
###' "i")[[i]]$weight}. If provided, this value is the multiplier used when
###' aggregating objectives. The default value is 1.0.
###'
###' \item[reltol (optional)] Specified via \code{model$multiobjc("[[#1]]",
###' "i")[[i]]$reltol}. If provided, this value specifies the relative objective
###' degradation when doing hierarchical multi-objective optimization. The
###' default value is 0.
###'
###' \item[abstol (optional)] Specified via \code{model$multiobjc("[[#1]]",
###' "i")[[i]]$abstol}. If provided, this value specifies the absolute objective
###' degradation when doing hierarchical multi-objective optimization. The
###' default value is 0.
###'
###' \item[name (optional)] Specified via \code{model$multiobjc("[[#1]]",
###' "i")[[i]]$name}. If provided, this string specifies the name of the
###' \emph{i}-th objective function. }
###'
###' Note that when multiple objectives are present, the \code{result$objval}
###' named component that is returned in the result of an optimization call will
###' be a vector of the same length as \code{model$multiobj}.
###'
###' A multi-objective model cannot have other objectives. Thus, combining
###' \code{model$multiobj} with any of \code{model$obj}, \code{model$objcon},
###' \code{model$pwlobj}, or \code{model$Q} is an error.
###'
###' }
###'
###' @section General constraint named components:
###'
###' The list of lists described below are used to add \emph{general constraints}
###' to a model.
###'
###' Mathematical programming has traditionally defined a set of fundamental
###' constraint types: variable bound constraints, linear constraints, quadratic
###' constraints, integrality constraints, and SOS constraints.  These are
###' typically treated directly by the underlying solver (although not always),
###' and are fundamental to the overall algorithm.  Gurobi accepts a number of
###' additional constraint types, which we collectively refer to as \emph{general
###' (function) constraints}.  These are typically \emph{not} treated directly by
###' the solver.  Rather, they are transformed by presolve into constraints (and
###' variables) chosen from among the fundamental types listed above.  In some
###' cases, the resulting constraint or constraints are mathematically equivalent
###' to the original; in others, they are approximations.  If such constraints
###' appear in your model, but if you prefer to reformulate them yourself using
###' fundamental constraint types instead, you can certainly do so.  However,
###' note that Gurobi can sometimes exploit information contained in the other
###' constraints in the model to build a more efficient formulation than what you
###' might create.
###'
###' The additional constraint types that fall under this \emph{general
###' constraint} umbrella are:
###'
###' \itemize{ \item MAX (genconmax): set a decision variable equal to the
###' maximum value from among a set of decision variables \item MIN (genconmin):
###' set a decision variable equal to the minimum value from among a set of
###' decision variables \item ABS (genconabs): set a decision variable equal to
###' the absolute value of some other decision variable \item AND (genconand):
###' set a binary variable equal to one if and only if all of a set of binary
###' decision variables are equal to one \item OR (genconor): set a binary
###' variable equal to one if and only if at least one variable out of a set of
###' binary decision variables is equal to one \item INDICATOR (genconind):
###' whenever a given binary variable takes a certain value, then the given
###' linear constraint must be satisfied \item Piecewise-linear constraints
###' (genconpwl): set a variable equal to the piecewise-linear function defined
###' by a set of points using some other variable \item Polynomial (genconpoly):
###' set a variable equal to the polynomial function defined by some other
###' variable \item Natural exponential (genconexp): set a variable equal to the
###' natural exponential function by some other variable \item Exponential
###' (genconexpa): set a variable equal to the exponential function by some other
###' variable \item Natural logarithm (genconlog): set a variable equal to the
###' natural logarithmic function by some other variable \item Logarithm
###' (genconloga): set a variable equal to the logarithmic function by some other
###' variable \item Power (genconpow): set a variable equal to the power function
###' by some other variable \item SIN (genconsin): set a variable equal to the
###' sine function by some other variable \item COS (genconcos): set a variable
###' equal to the cosine function by some other variable \item TAN (gencontan):
###' set a variable equal to the tangent function by some other variable }
###'
###' Please refer to c("(FIXME-MRhrefH: #1; #2; #3)", "this section",
###' "subsubsection:GeneralConstraints", "constraints")(FIXME-MRhrefH: this
###' section; subsubsection:GeneralConstraints; constraints) for additional
###' details on general constraints.
###'
###' \itemize{ \item [genconmax (optional)] A A list of lists. When present, each
###' entry in \code{genconmax} defines a MAX general constraint of the form
###' \deqn{x[\mathrm{resvar}] =
###' \max\left\{\mathrm{con},x[j]:j\in\mathrm{vars}\right\}}{x[resvar] = max(
###' con, x[j]:j in vars)} Each entry may have the following named components:
###' \itemize{ c("\\MRGCitem{#1}{resvar}. Index of the variable in the left-hand
###' side of the constraint.", "genconmax")c("\\item[#2] Specified via
###' \\code{model$#1[[i]]$#2}", "genconmax", "resvar")\item[resvar] Specified via
###' \code{model$genconmax[[i]]$resvar}. Index of the variable in the left-hand
###' side of the constraint. c("\\MRGCitem{#1}{vars}, it is a vector of indices
###' of variables in the right-hand side of the constraint.",
###' "genconmax")c("\\item[#2] Specified via \\code{model$#1[[i]]$#2}",
###' "genconmax", "vars")\item[vars] Specified via
###' \code{model$genconmax[[i]]$vars}, it is a vector of indices of variables in
###' the right-hand side of the constraint. \item[con (optional)] Specified via
###' \code{model$genconmaxc("[[#1]]", "i")[[i]]$con}. When present, specifies the
###' constant on the left-hand side. Default value is \eqn{-\infty}{-Inf}.
###' c("\\MRGCOitem{#1}{name}. When present, specifies the name of the
###' \\emph{i}-th #2 general constraint.", "genconmax", "MAX")c("\\item[#2
###' (optional)] Specified via \\code{model$#1{\\MRpos{i}}$#2}", "genconmax",
###' "name")\item[name (optional)] Specified via
###' \code{model$genconmax{c("[[#1]]", "i")[[i]]}$name}. When present, specifies
###' the name of the \emph{i}-th MAX general constraint. }
###'
###' \item [genconmin (optional)] A A list of lists. When present, each entry in
###' \code{genconmax} defines a MIN general constraint of the form
###' \deqn{x[\mathrm{resvar}] =
###' \min\left\{\mathrm{con},x[j]:j\in\mathrm{vars}\right\}}{x[resvar] = min(
###' con, x[j]:j in vars)} Each entry may have the following named components:
###' \itemize{ c("\\MRGCitem{#1}{resvar}. Index of the variable in the left-hand
###' side of the constraint.", "genconmin")c("\\item[#2] Specified via
###' \\code{model$#1[[i]]$#2}", "genconmin", "resvar")\item[resvar] Specified via
###' \code{model$genconmin[[i]]$resvar}. Index of the variable in the left-hand
###' side of the constraint. c("\\MRGCitem{#1}{vars}, it is a vector of indices
###' of variables in the right-hand side of the constraint.",
###' "genconmin")c("\\item[#2] Specified via \\code{model$#1[[i]]$#2}",
###' "genconmin", "vars")\item[vars] Specified via
###' \code{model$genconmin[[i]]$vars}, it is a vector of indices of variables in
###' the right-hand side of the constraint. \item[con (optional)] Specified via
###' \code{model$genconminc("[[#1]]", "i")[[i]]$con}. When present, specifies the
###' constant on the left-hand side. Default value is \eqn{\infty}{Inf}.
###' c("\\MRGCOitem{#1}{name}. When present, specifies the name of the
###' \\emph{i}-th #2 general constraint.", "genconmin", "MIN")c("\\item[#2
###' (optional)] Specified via \\code{model$#1{\\MRpos{i}}$#2}", "genconmin",
###' "name")\item[name (optional)] Specified via
###' \code{model$genconmin{c("[[#1]]", "i")[[i]]}$name}. When present, specifies
###' the name of the \emph{i}-th MIN general constraint. }
###'
###' \item [genconabs (optional)] A A list of lists. When present, each entry in
###' \code{genconmax} defines an ABS general constraint of the form
###' \deqn{x[\mathrm{resvar}] = \vert x[\mathrm{argvar}]\vert}{x[resvar] =
###' |x[argvar]|} Each entry may have the following named components: \itemize{
###' c("\\MRGCitem{#1}{resvar}. Index of the variable in the left-hand side of
###' the constraint.", "genconabs")c("\\item[#2] Specified via
###' \\code{model$#1[[i]]$#2}", "genconabs", "resvar")\item[resvar] Specified via
###' \code{model$genconabs[[i]]$resvar}. Index of the variable in the left-hand
###' side of the constraint. \item[argvar] Specified via
###' \code{model$genconabsc("[[#1]]", "i")[[i]]$argvar}. Index of the variable in
###' the right-hand side of the constraint. c("\\MRGCOitem{#1}{name}. When
###' present, specifies the name of the \\emph{i}-th #2 general constraint.",
###' "genconabs", "ABS")c("\\item[#2 (optional)] Specified via
###' \\code{model$#1{\\MRpos{i}}$#2}", "genconabs", "name")\item[name (optional)]
###' Specified via \code{model$genconabs{c("[[#1]]", "i")[[i]]}$name}. When
###' present, specifies the name of the \emph{i}-th ABS general constraint. }
###'
###' \item [genconand (optional)] A A list of lists. When present, each entry in
###' \code{genconand} defines an AND general constraint of the form
###' \deqn{x[\mathrm{resvar}] = \mathrm{and}\{x[i]:i\in\mathrm{vars}\}}{x[resvar]
###' = AND(x[i]: i in vars)} Each entry may have the following named components:
###' \itemize{ c("\\MRGCitem{#1}{resvar}. Index of the variable in the left-hand
###' side of the constraint.", "genconand")c("\\item[#2] Specified via
###' \\code{model$#1[[i]]$#2}", "genconand", "resvar")\item[resvar] Specified via
###' \code{model$genconand[[i]]$resvar}. Index of the variable in the left-hand
###' side of the constraint. c("\\MRGCitem{#1}{vars}, it is a vector of indices
###' of variables in the right-hand side of the constraint.",
###' "genconand")c("\\item[#2] Specified via \\code{model$#1[[i]]$#2}",
###' "genconand", "vars")\item[vars] Specified via
###' \code{model$genconand[[i]]$vars}, it is a vector of indices of variables in
###' the right-hand side of the constraint. c("\\MRGCOitem{#1}{name}. When
###' present, specifies the name of the \\emph{i}-th #2 general constraint.",
###' "genconand", "AND")c("\\item[#2 (optional)] Specified via
###' \\code{model$#1{\\MRpos{i}}$#2}", "genconand", "name")\item[name (optional)]
###' Specified via \code{model$genconand{c("[[#1]]", "i")[[i]]}$name}. When
###' present, specifies the name of the \emph{i}-th AND general constraint. }
###'
###' \item [genconor (optional)] A A list of lists. When present, each entry in
###' \code{genconor} defines an OR general constraint of the form
###' \deqn{x[\mathrm{resvar}] = \mathrm{or}\{x[i]:i\in\mathrm{vars}\}}{x[resvar]
###' = OR(x[i]: i in vars)} Each entry may have the following named components:
###' \itemize{ c("\\MRGCitem{#1}{resvar}. Index of the variable in the left-hand
###' side of the constraint.", "genconor")c("\\item[#2] Specified via
###' \\code{model$#1[[i]]$#2}", "genconor", "resvar")\item[resvar] Specified via
###' \code{model$genconor[[i]]$resvar}. Index of the variable in the left-hand
###' side of the constraint. c("\\MRGCitem{#1}{vars}, it is a vector of indices
###' of variables in the right-hand side of the constraint.",
###' "genconor")c("\\item[#2] Specified via \\code{model$#1[[i]]$#2}",
###' "genconor", "vars")\item[vars] Specified via
###' \code{model$genconor[[i]]$vars}, it is a vector of indices of variables in
###' the right-hand side of the constraint. c("\\MRGCOitem{#1}{name}. When
###' present, specifies the name of the \\emph{i}-th #2 general constraint.",
###' "genconor", "OR")c("\\item[#2 (optional)] Specified via
###' \\code{model$#1{\\MRpos{i}}$#2}", "genconor", "name")\item[name (optional)]
###' Specified via \code{model$genconor{c("[[#1]]", "i")[[i]]}$name}. When
###' present, specifies the name of the \emph{i}-th OR general constraint. }
###'
###' \item [genconind (optional)] A A list of lists. When present, each entry in
###' \code{genconind} defines an INDICATOR general constraint of the form
###' \deqn{x[\mathrm{binvar}] = \mathrm{binval}\Rightarrow\sum\left(
###' x\MRpos{j}\cdot\mathrm{a}\MRpos{j}\right) \mathrm{sense} \mathrm{rhs}}{if
###' (x[binvar] = binval) then (a'x sense rhs)} This constraint states that when
###' the binary variable \eqn{x[\mathrm{binvar}]}{x[binvar]} takes the value
###' \code{binval} then the linear constraint
###' \eqn{\sum\left(x[\mathrm{vars}\MRpos{j}]\cdot\mathrm{val}\MRpos{j}\right)
###' \mathrm{sense} \mathrm{rhs}}{sum(x[vars[i]] * a[i]: i in vars) sense rhs}
###' must hold. Note that \code{sense} is one of \code{'='}, \code{'<'}, or
###' \code{'>'} for equality (\eqn{=}{=}), less than or equal (\eqn{\leq}{<=}) or
###' greater than or equal (\eqn{\geq}{>=}) constraints. Each entry may have the
###' following named components: \itemize{ c("\\item[#2] Specified via
###' \\code{model$#1[[i]]$#2}", "genconind", "binvar")\item[binvar] Specified via
###' \code{model$genconind[[i]]$binvar}. Index of the implicating binary
###' variable. c("\\item[#2] Specified via \\code{model$#1[[i]]$#2}",
###' "genconind", "binval")\item[binval] Specified via
###' \code{model$genconind[[i]]$binval}. Value for the binary variable that
###' forces the following linear constraint to be satisfied. It can be either 0
###' or 1. c("\\item[#2] Specified via \\code{model$#1[[i]]$#2}", "genconind",
###' "a")\item[a] Specified via \code{model$genconind[[i]]$a}.  Vector of
###' coefficients of variables participating in the implied linear constraint.
###' You can specify a value for \code{a} for each column of \code{A} (dense
###' vector) or pass a sparse vector (c("#2", "sparse n-by-1 matrix", "should be
###' built using\n\\code{sparseVector} from the \\code{Matrix} package")should be
###' built using \code{sparseVector} from the \code{Matrix} package).
###' c("\\item[#2] Specified via \\code{model$#1[[i]]$#2}", "genconind",
###' "sense")\item[sense] Specified via \code{model$genconind[[i]]$sense}. Sense
###' of the implied linear constraint. Must be one of \code{'='}, \code{'<'}, or
###' \code{'>'}. c("\\item[#2] Specified via \\code{model$#1[[i]]$#2}",
###' "genconind", "rhs")\item[rhs] Specified via \code{model$genconind[[i]]$rhs}.
###' Right-hand side value of the implied linear constraint.
###' c("\\MRGCOitem{#1}{name}. When present, specifies the name of the
###' \\emph{i}-th #2 general constraint.", "genconind", "INDICATOR")c("\\item[#2
###' (optional)] Specified via \\code{model$#1{\\MRpos{i}}$#2}", "genconind",
###' "name")\item[name (optional)] Specified via
###' \code{model$genconind{c("[[#1]]", "i")[[i]]}$name}. When present, specifies
###' the name of the \emph{i}-th INDICATOR general constraint. }
###'
###' \item [genconpwl (optional)] A A list of lists. When present, each entry in
###' \code{genconpwl} defines a piecewise-linear constraint of the form
###' \deqn{x[\mathrm{yvar}] = f(x[\mathrm{xvar}])}{x[yvar] = f([xvar])} The
###' breakpoints for $f$ are provided as arguments. Refer to the description of
###' c("(FIXME-hyperref: #1; #2; #3; #4)", "piecewise-linear objectives", "", "",
###' "subsubsection:PiecewiseObj")(FIXME-hyperref: piecewise-linear objectives; ;
###' ; subsubsection:PiecewiseObj) for details of how piecewise-linear functions
###' are defined
###'
###' Each entry may have the following named components: \itemize{
###' c("(FIXME-MRGCfuncxvar: #1)", "genconpwl")(FIXME-MRGCfuncxvar: genconpwl)
###' c("(FIXME-GCfunction: #1)", "genconpwl")(FIXME-GCfunction: genconpwl)
###' c("\\item[#2] Specified via \\code{model$#1[[i]]$#2}", "genconpwl",
###' "xpts")\item[xpts] Specified via \code{model$genconpwl[[i]]$xpts}. Specifies
###' the $x$ values for the points that define the piecewise-linear function.
###' Must be in non-decreasing order. c("\\item[#2] Specified via
###' \\code{model$#1[[i]]$#2}", "genconpwl", "ypts")\item[ypts] Specified via
###' \code{model$genconpwl[[i]]$ypts}. Specifies the $y$ values for the points
###' that define the piecewise-linear function. c("\\MRGCOitem{#1}{name}. When
###' present, specifies the name of the \\emph{i}-th #2 general constraint.",
###' "genconpwl", "piecewise-linear")c("\\item[#2 (optional)] Specified via
###' \\code{model$#1{\\MRpos{i}}$#2}", "genconpwl", "name")\item[name (optional)]
###' Specified via \code{model$genconpwl{c("[[#1]]", "i")[[i]]}$name}. When
###' present, specifies the name of the \emph{i}-th piecewise-linear general
###' constraint. }
###'
###' \item [genconpoly (optional)] A A list of lists. When present, each entry in
###' \code{genconpoly} defines a polynomial function constraint of the form
###' \deqn{x[\mathrm{yvar}] = p_0 x[\mathrm{xvar}]^d + p_1 x[\mathrm{xvar}]^{d-1}
###' + ... + p_{d-1} x[\mathrm{xvar}] + p_{d}}{x[yvar] = p_0 x[xvar]^d + p_1
###' x[xvar]^{d-1} + ... + p_{d-1} x[xvar] + p_{d}} (FIXME-GCfunction:
###' )(FIXME-GCfunction: )
###'
###' Each entry may have the following named components: \itemize{
###' c("(FIXME-MRGCfuncxvar: #1)", "genconpoly")(FIXME-MRGCfuncxvar: genconpoly)
###' c("(FIXME-GCfunction: #1)", "genconpoly")(FIXME-GCfunction: genconpoly)
###' c("\\item[#2] Specified via \\code{model$#1[[i]]$#2}", "genconpoly",
###' "p")\item[p] Specified via \code{model$genconpoly[[i]]$p}. Specifies the
###' coefficients for the polynomial function (starting with the coefficient for
###' the highest power). If $x^d$ is the highest power term, a dense vector of
###' length $d+1$ is returned. c("(FIXME-GCfuncname: #1; #2)", "genconpoly",
###' "polynomial")(FIXME-GCfuncname: genconpoly; polynomial)
###' c("(FIXME-GCfuncoptions: #1; #2)", "genconpoly",
###' "polynomial")(FIXME-GCfuncoptions: genconpoly; polynomial) }
###'
###' \item [genconexp (optional)] A A list of lists. When present, each entry in
###' \code{genconexp} defines the natural exponential function constraint of the
###' form \deqn{x[\mathrm{yvar}] = \mathrm{exp}(x[\mathrm{xvar}])}{x[yvar] =
###' exp(x[xvar])} (FIXME-GCfunction: )(FIXME-GCfunction: )
###'
###' Each entry may have the following named components: \itemize{
###' c("(FIXME-MRGCfuncxvar: #1)", "genconexp")(FIXME-MRGCfuncxvar: genconexp)
###' c("(FIXME-GCfunction: #1)", "genconexp")(FIXME-GCfunction: genconexp)
###' c("(FIXME-GCfuncname: #1; #2)", "genconexp", "natural
###' exponential")(FIXME-GCfuncname: genconexp; natural exponential)
###' c("(FIXME-GCfuncoptions: #1; #2)", "genconexp", "natural
###' exponential")(FIXME-GCfuncoptions: genconexp; natural exponential) }
###'
###' \item [genconexpa (optional)] A A list of lists. When present, each entry in
###' \code{genconexpa} defines an exponential function constraint of the form
###' \deqn{x[\mathrm{yvar}] = \mathrm{a}^{x[\mathrm{xvar}]}}{x[yvar] = a^{
###' x[xvar]}} (FIXME-GCfunction: )(FIXME-GCfunction: )
###'
###' Each entry may have the following named components: \itemize{
###' c("(FIXME-MRGCfuncxvar: #1)", "genconexpa")(FIXME-MRGCfuncxvar: genconexpa)
###' c("(FIXME-GCfunction: #1)", "genconexpa")(FIXME-GCfunction: genconexpa)
###' c("\\item[#2] Specified via \\code{model$#1[[i]]$#2}", "genconexpa",
###' "a")\item[a] Specified via \code{model$genconexpa[[i]]$a}. Specifies the
###' base of the exponential function $a > 0$. c("(FIXME-GCfuncname: #1; #2)",
###' "genconexpa", "exponential")(FIXME-GCfuncname: genconexpa; exponential)
###' c("(FIXME-GCfuncoptions: #1; #2)", "genconexpa",
###' "exponential")(FIXME-GCfuncoptions: genconexpa; exponential) }
###'
###' \item [genconlog (optional)] A A list of lists. When present, each entry in
###' \code{genconlog} defines the natural logarithmic function constraint of the
###' form \deqn{x[\mathrm{yvar}] = \mathrm{log}(x[\mathrm{xvar}])}{x[yvar] =
###' log(x[xvar])} (FIXME-GCfunction: )(FIXME-GCfunction: )
###'
###' Each entry may have the following named components: \itemize{
###' c("(FIXME-MRGCfuncxvar: #1)", "genconlog")(FIXME-MRGCfuncxvar: genconlog)
###' c("(FIXME-GCfunction: #1)", "genconlog")(FIXME-GCfunction: genconlog)
###' c("(FIXME-GCfuncname: #1; #2)", "genconlog", "natural
###' logarithmic")(FIXME-GCfuncname: genconlog; natural logarithmic)
###' c("(FIXME-GCfuncoptions: #1; #2)", "genconlog", "natural
###' logarithmic")(FIXME-GCfuncoptions: genconlog; natural logarithmic) }
###'
###' \item [genconloga (optional)] A A list of lists. When present, each entry in
###' \code{genconloga} defines a logarithmic function constraint of the form
###' \deqn{x[\mathrm{yvar}] =
###' \mathrm{log}(x[\mathrm{xvar}])\setminus\mathrm{log}(a)}{x[yvar] =
###' logb(x[xvar], base=a)} (FIXME-GCfunction: )(FIXME-GCfunction: )
###'
###' Each entry may have the following named components: \itemize{
###' c("(FIXME-MRGCfuncxvar: #1)", "genconloga")(FIXME-MRGCfuncxvar: genconloga)
###' c("(FIXME-GCfunction: #1)", "genconloga")(FIXME-GCfunction: genconloga)
###' c("\\item[#2] Specified via \\code{model$#1[[i]]$#2}", "genconloga",
###' "a")\item[a] Specified via \code{model$genconloga[[i]]$a}. Specifies the
###' base of the logarithmic function $a > 0$. c("(FIXME-GCfuncname: #1; #2)",
###' "genconloga", "logarithmic")(FIXME-GCfuncname: genconloga; logarithmic)
###' c("(FIXME-GCfuncoptions: #1; #2)", "genconloga",
###' "logarithmic")(FIXME-GCfuncoptions: genconloga; logarithmic) }
###'
###' \item [genconpow (optional)] A A list of lists. When present, each entry in
###' \code{genconpow} defines a power function constraint of the form
###' \deqn{x[\mathrm{yvar}] = x[\mathrm{xvar}]^\mathrm{a}}{x[yvar] = x[xvar]^a}
###' (FIXME-GCfunction: )(FIXME-GCfunction: )
###'
###' Each entry may have the following named components: \itemize{
###' c("(FIXME-MRGCfuncxvar: #1)", "genconpow")(FIXME-MRGCfuncxvar: genconpow)
###' c("(FIXME-GCfunction: #1)", "genconpow")(FIXME-GCfunction: genconpow)
###' c("\\item[#2] Specified via \\code{model$#1[[i]]$#2}", "genconpow",
###' "a")\item[a] Specified via \code{model$genconpow[[i]]$a}. Specifies the
###' exponent of the power function. c("(FIXME-GCfuncname: #1; #2)", "genconpow",
###' "power")(FIXME-GCfuncname: genconpow; power) c("(FIXME-GCfuncoptions: #1;
###' #2)", "genconpow", "power")(FIXME-GCfuncoptions: genconpow; power) }
###'
###' \item [genconsin (optional)] A A list of lists. When present, each entry in
###' \code{genconsin} defines the sine function constraint of the form
###' \deqn{x[\mathrm{yvar}] = \mathrm{sin}(x[\mathrm{xvar}])}{x[yvar] =
###' sin(x[xvar])} (FIXME-GCfunction: )(FIXME-GCfunction: )
###'
###' Each entry may have the following named components: \itemize{
###' c("(FIXME-MRGCfuncxvar: #1)", "genconsin")(FIXME-MRGCfuncxvar: genconsin)
###' c("(FIXME-GCfunction: #1)", "genconsin")(FIXME-GCfunction: genconsin)
###' c("(FIXME-GCfuncname: #1; #2)", "genconsin", "sine")(FIXME-GCfuncname:
###' genconsin; sine) c("(FIXME-GCfuncoptions: #1; #2)", "genconsin",
###' "sine")(FIXME-GCfuncoptions: genconsin; sine) }
###'
###' \item [genconcos (optional)] A A list of lists. When present, each entry in
###' \code{genconcos} defines the cosine function constraint of the form
###' \deqn{x[\mathrm{yvar}] = \mathrm{cos}(x[\mathrm{xvar}])}{x[yvar] =
###' cos(x[xvar])} (FIXME-GCfunction: )(FIXME-GCfunction: )
###'
###' Each entry may have the following named components: \itemize{
###' c("(FIXME-MRGCfuncxvar: #1)", "genconcos")(FIXME-MRGCfuncxvar: genconcos)
###' c("(FIXME-GCfunction: #1)", "genconcos")(FIXME-GCfunction: genconcos)
###' c("(FIXME-GCfuncname: #1; #2)", "genconcos", "cosine")(FIXME-GCfuncname:
###' genconcos; cosine) c("(FIXME-GCfuncoptions: #1; #2)", "genconcos",
###' "cosine")(FIXME-GCfuncoptions: genconcos; cosine) }
###'
###' \item [gencontan (optional)] A A list of lists. When present, each entry in
###' \code{gencontan} defines the tangent function constraint of the form
###' \deqn{x[\mathrm{yvar}] = \mathrm{tan}(x[\mathrm{xvar}])}{x[yvar] =
###' tan(x[xvar])} (FIXME-GCfunction: )(FIXME-GCfunction: )
###'
###' Each entry may have the following named components: \itemize{
###' c("(FIXME-MRGCfuncxvar: #1)", "gencontan")(FIXME-MRGCfuncxvar: gencontan)
###' c("(FIXME-GCfunction: #1)", "gencontan")(FIXME-GCfunction: gencontan)
###' c("(FIXME-GCfuncname: #1; #2)", "gencontancos", "tangent")(FIXME-GCfuncname:
###' gencontancos; tangent) c("(FIXME-GCfuncoptions: #1; #2)", "gencontan",
###' "tangent")(FIXME-GCfuncoptions: gencontan; tangent) }
###'
###' }
###'
###' @section Advanced named components:
###'
###' \itemize{
###' \item[pwlobj (optional)] The piecewise-linear objective functions.
###' A A list of lists. When present, each entry in \code{pwlobj} defines a
###' piecewise-linear objective function for a single variable. The index of the
###' variable whose objective function is being defined is stored in
###' \code{model$pwlobjc("[[#1]]", "i")[[i]]$var}. The \eqn{x}{x} values for the
###' points that define the piecewise-linear function are stored in
###' \code{model$pwlobjc("[[#1]]", "i")[[i]]$x}. The values in the \eqn{x}{x}
###' vector must be in non-decreasing order. The \eqn{y}{y} values for the points
###' that define the piecewise-linear function are stored in
###' \code{model$pwlobjc("[[#1]]", "i")[[i]]$y}.
###'
###' \item[vbasis (optional)] The variable basis status vector. Used to provide
###' an advanced starting point for the simplex algorithm. You would generally
###' never concern yourself with the contents of this vector, but would instead
###' simply pass it from the result of a previous optimization run to the input
###' of a subsequent run. When present, you must specify one value for each
###' column of \code{A}.
###'
###' \item[cbasis (optional)] The constraint basis status vector. Used to provide
###' an advanced starting point for the simplex algorithm. Consult the
###' \code{vbasis} description for details. When present, you must specify one
###' value for each row of \code{A}.
###'
###' \item[varhintval (optional)] A set of user hints.  If you know that a
###' variable is likely to take a particular value in high quality solutions of a
###' MIP model, you can provide that value as a hint.  You can also (optionally)
###' provide information about your level of confidence in a hint with the
###' \code{varhintpri} named component. If present, you must specify one value
###' for each column of \code{A}. Use a value of \code{NA} for variables where no
###' such hint is known. For more details, please refer to the c("(FIXME-MRhrefH:
###' #1; #2; #3)", "VarHitVal", "attr:VarHintVal", "varhintval")(FIXME-MRhrefH:
###' VarHitVal; attr:VarHintVal; varhintval) attribute documentation.
###'
###' \item[varhintpri (optional)] Priorities on user hints.  After providing
###' variable hints through the \code{varhintval} list, you can optionally also
###' provide hint priorities to give an indication of your level of confidence in
###' your hints. If present, you must specify a value for each column of
###' \code{A}. For more details, please refer to the c("(FIXME-MRhrefH: #1; #2;
###' #3)", "VarHintPri", "attr:VarHintPri", "varhitpri")(FIXME-MRhrefH:
###' VarHintPri; attr:VarHintPri; varhitpri) attribute documentation.
###'
###' \item[branchpriority (optional)] Variable branching priority. If present,
###' the value of this attribute is used as the primary criteria for selecting a
###' fractional variable for branching during the MIP search.  Variables with
###' larger values always take priority over those with smaller values.  Ties are
###' broken using the standard branch variable selection criteria. If present,
###' you must specify one value for each column of \code{A}.
###'
###' \item[pstart (optional)] The current simplex start vector.  If you set
###' \code{pstart} values for every variable in the model and \code{dstart}
###' values for every constraint, then simplex will use those values to compute a
###' warm start basis.  For more details, please refer to the c("(FIXME-MRhrefH:
###' #1; #2; #3)", "PStart", "attr:PStart", "pstart")(FIXME-MRhrefH: PStart;
###' attr:PStart; pstart) attribute documentation.
###'
###' \item[dstart (optional)] The current simplex start vector.  If you set
###' \code{dstart} values for every linear constraint in the model and
###' \code{pstart} values for every variable, then simplex will use those values
###' to compute a warm start basis.  For more details, please refer to the
###' c("(FIXME-MRhrefH: #1; #2; #3)", "DStart", "attr:DStart",
###' "dstart")(FIXME-MRhrefH: DStart; attr:DStart; dstart) attribute
###' documentation.
###'
###' \item[lazy (optional)] Determines whether a linear constraint is treated as
###' a \emph{lazy constraint}. If present, you must specify one value for each
###' row of \code{A}. For more details, please refer to the c("(FIXME-MRhrefH:
###' #1; #2; #3)", "Lazy", "attr:Lazy", "lazy")(FIXME-MRhrefH: Lazy; attr:Lazy;
###' lazy) attribute documentation.
###'
###' \item[start (optional)] The MIP start vector. The MIP solver will attempt to
###' build an initial solution from this vector. When present, you must specify a
###' start value for each variable. Note that you can set the start value for a
###' variable to \code{NA}, which instructs the MIP solver to try to fill in a
###' value for that variable.
###'
###' \item[partition (optional)] The MIP variable partition number, which is used
###' by the MIP solution improvement heuristic.  If present, you must specify one
###' value for each variable of \code{A}. For more details, please refer to the
###' c("(FIXME-MRhrefH: #1; #2; #3)", "Partition", "attr:Partition",
###' "partition")(FIXME-MRhrefH: Partition; attr:Partition; partition) attribute
###' documentation.
###'
###' }
###'
###' If any of the mandatory components listed above are missing, the
###' \code{gurobi()} function will return an error.
###'
###' Below is an example that demonstrates the construction of a simple
###' optimization model:\cr
###'
###' %\MRmodelex c("#2", "\n\\preformatted{\nmodel.A = sparse([1 2 3; 1 1
###' 0]);\nmodel.obj = [1 1 1];\nmodel.modelsense = 'max';\nmodel.rhs = [4;
###' 1];\nmodel.sense = '<>'\n}", "\n\\preformatted{\nmodel <- list()\nmodel\\$A
###' <- matrix(c(1,2,3,1,1,0), nrow=2, byrow=T)\nmodel\\$obj <-
###' c(1,1,1)\nmodel\\$modelsense <- 'max'\nmodel\\$rhs <- c(4,1)\nmodel\\$sense
###' <- c('<', '>')\n}\n\nYou can also build \\code{A} as a sparse matrix,
###' using\neither \\code{sparseMatrix} or
###' \\code{simple_triplet_matrix}:\\cr\n\n\\preformatted{\nmodel\\$A <-
###' spMatrix(2, 3, c(1, 1, 1, 2, 2), c(1, 2, 3, 1, 2), c(1, 2, 3, 1,
###' 1))\nmodel\\$A <- simple_triplet_matrix(c(1, 1, 1, 2, 2), c(1, 2, 3, 1, 2),
###' c(1, 2, 3, 1, 1))\\\n}\n\nNote that the Gurobi R interface allows you to
###' specify a scalar value\nfor most of the array-valued components.  The
###' specified value will be\nexpanded to an array of the appropriate size, with
###' each component of\nthe array equal to the scalar (e.g., \\code{model\\$obj
###' <- 1} would\nbe equivalent to \\code{model\\$obj <- c(1,1,1)} in the
###' example).\n" ) \preformatted{ model <- list() model\$A <-
###' matrix(c(1,2,3,1,1,0), nrow=2, byrow=T) model\$obj <- c(1,1,1)
###' model\$modelsense <- 'max' model\$rhs <- c(4,1) model\$sense <- c('<', '>')
###' }
###'
###' You can also build \code{A} as a sparse matrix, using either
###' \code{sparseMatrix} or \code{simple_triplet_matrix}:\cr
###'
###' \preformatted{ model\$A <- spMatrix(2, 3, c(1, 1, 1, 2, 2), c(1, 2, 3, 1,
###' 2), c(1, 2, 3, 1, 1)) model\$A <- simple_triplet_matrix(c(1, 1, 1, 2, 2),
###' c(1, 2, 3, 1, 2), c(1, 2, 3, 1, 1))\ }
###'
###' Note that the Gurobi R interface allows you to specify a scalar value for
###' most of the array-valued components.  The specified value will be expanded
###' to an array of the appropriate size, with each component of the array equal
###' to the scalar (e.g., \code{model\$obj <- 1} would be equivalent to
###' \code{model\$obj <- c(1,1,1)} in the example).
###'
###' @author Gurobi Optimization
###' @seealso The \code{\link{gurobi_package}} documentation contains general
###' information on the Gurobi R interface. The \code{\link{gurobi_problem}}
###' documentation has a detailed description of the types of models that gurobi
###' is able to optimize. The \code{\link{gurobi_usage}} documentation has an
###' overview on the usage of the Gurobi package.
###'
###' For a detailed description of variables used as input and output of gurobi
###' functions see \code{\link{gurobi_model}}, \code{\link{gurobi_params}} and
###' \code{\link{gurobi_env}}.
###'
###' The functions provided by the Gurobi package are: \code{\link{gurobi}},
###' \code{\link{gurobi_iis}}, \code{\link{gurobi_feasrelax}},
###' \code{\link{gurobi_relax}}, \code{\link{gurobi_read}} and
###' \code{\link{gurobi_write}}.
###' @references Gurobi Optimization (\url{http://www.gurobi.com}).
###'
###' Gurobi Optimizer Reference Manual
###' (\url{http://www.gurobi.com/documentation/9.0/refman/index.html}).
###' @keywords optimize
##NULL
##
##
###' @name gurobi_problem
###'
###' The gurobi problem statement
###'
###' @section Models:
###'
###' Our Gurobi R interface enables you to express problems of the following
###' form:
###'
###' \tabular{lll}{ minimize \tab \eqn{x^TQx + c^Tx + \mathrm{alpha}}{x'Q x + c'x
###' + alpha} \tab \cr subject to \tab \eqn{Ax = b}{Ax = b} \tab (linear
###' constraints)\cr \tab \eqn{\ell \le x \le u}{l <= x <= u} \tab (bound
###' constraints) \cr \tab some \eqn{x_j}{x[j]} integral \tab (integrality
###' constraints)\cr \tab \eqn{x^TQc\, x + q^Tx \le \mathrm{beta}}{x'Qc x + q'x
###' <= beta} \tab (quadratic constraints) \cr \tab some \eqn{x_i}{x[i]} in SOS
###' \tab (special ordered set constraints) \cr \tab min, max, abs, or, ...  \tab
###' (general constraints) \cr }
###'
###' Models are stored as \code{list} variables, each consisting of multiple
###' \emph{named components}.  The named components capture the different model
###' components listed above.  Many of these model components are optional. For
###' example, integrality constraints may be omitted.
###'
###' An optimization model may be loaded from a file (using the
###' c("\\code{\\link{#3}}", "gurobi_read", "r:gurobi_read",
###' "gurobi_read")\code{\link{gurobi_read}} function), or it can be built by
###' populating the appropriate named components of a model variable (using
###' standard R constructs).  We will discuss the details of how models are
###' represented in the c("\\code{\\link{#3}}", "model", "r:model",
###' "gurobi_model")\code{\link{gurobi_model}} argument section.
###'
###' We often refer to the \emph{class} of an optimization model.  A model with a
###' linear objective function, linear constraints, and continuous variables is a
###' \emph{Linear Program (LP)}.  If the objective is quadratic, the model is a
###' \emph{Quadratic Program (QP)}.  If any of the constraints are quadratic, the
###' model is a \emph{Quadratically-Constrained Program (QCP)}.  We will
###' sometimes refer to a few special cases of QCP: QCPs with convex constraints,
###' QCPs with non-convex constraints, \emph{bilinear programs}, and
###' \emph{Second-Order Cone Programs (SOCP)}. If the model contains any integer
###' variables, semi-continuous variables, semi-integer variables, Special
###' Ordered Set (SOS) constraints, or general constraints, the model is a
###' \emph{Mixed Integer Program (MIP)}.  We'll also sometimes discuss special
###' cases of MIP, including \emph{Mixed Integer Linear Programs (MILP)},
###' \emph{Mixed Integer Quadratic Programs (MIQP)}, \emph{Mixed Integer
###' Quadratically-Constrained Programs (MIQCP)}, and \emph{Mixed Integer
###' Second-Order Cone Programs (MISOCP)}.  The Gurobi Optimizer handles all of
###' these model classes.
###'
###'
###'
###' @author Gurobi Optimization
###' @seealso The \code{\link{gurobi_package}} documentation contains general
###' information on the Gurobi R interface. The \code{\link{gurobi_problem}}
###' documentation has a detailed description of the types of models that gurobi
###' is able to optimize. The \code{\link{gurobi_usage}} documentation has an
###' overview on the usage of the Gurobi package.
###'
###' For a detailed description of variables used as input and output of gurobi
###' functions see \code{\link{gurobi_model}}, \code{\link{gurobi_params}} and
###' \code{\link{gurobi_env}}.
###'
###' The functions provided by the Gurobi package are: \code{\link{gurobi}},
###' \code{\link{gurobi_iis}}, \code{\link{gurobi_feasrelax}},
###' \code{\link{gurobi_relax}}, \code{\link{gurobi_read}} and
###' \code{\link{gurobi_write}}.
###' @references Gurobi Optimization (\url{http://www.gurobi.com}).
###'
###' Gurobi Optimizer Reference Manual
###' (\url{http://www.gurobi.com/documentation/9.0/refman/index.html}).
###' @keywords optimize
##NULL
##
##
##
##
##
###' @name gurobi_usage
###'
###' Common usage of the Gurobi R interface
###'
###' @section Solving a Model:
###'
###' Once you have built a model, you can call c("\\code{\\link{#3}}", "gurobi",
###' "r:gurobi", "gurobi")\code{\link{gurobi}} to compute a solution. By default,
###' c("\\code{\\link{#3}}", "gurobi", "r:gurobi", "gurobi")\code{\link{gurobi}}
###' will use the c("(FIXME-MRhrefH: #1; #2; #3)", "concurrent optimizer",
###' "sec:Concurrent", "concurrent_optimizer")(FIXME-MRhrefH: concurrent
###' optimizer; sec:Concurrent; concurrent_optimizer) to solve LP models, the
###' barrier algorithm to solve QP models and QCP models with convex constraints,
###' and the branch-and-cut algorithm to solve mixed integer models. The solution
###' is returned as a \code{list} variable. We will discuss the details of how
###' optimization results are represented when we discuss the
###' c("\\code{\\link{#3}}", "gurobi", "r:gurobi", "gurobi")\code{\link{gurobi}}
###' function.
###'
###' Here is a simple example of a likely sequence of commands in the R API:
###' \preformatted{ model <- gurobi_read('examples/data/stein9.mps') result <-
###' gurobi(model) }
###'
###' @section Multiple Solutions and Multiple Objectives:
###'
###' By default, the Gurobi Optimizer assumes that your goal is to find one
###' proven optimal solution to a model with a single objective function. Gurobi
###' provides features that allow you to relax either of these assumptions.  You
###' should refer to the section on c("(FIXME-MRhrefH: #1; #2; #3)", "Solution
###' Pools", "sec:SolutionPool", "solution_pool")(FIXME-MRhrefH: Solution Pools;
###' sec:SolutionPool; solution_pool) for information on how to request more than
###' one solution, or the section on c("(FIXME-MRhrefH: #1; #2; #3)", "Multiple
###' Objectives", "sec:MultipleObjectives", "multiple_objectives")(FIXME-MRhrefH:
###' Multiple Objectives; sec:MultipleObjectives; multiple_objectives) for
###' information on how to specify multiple objective functions and control the
###' trade-off between them.
###'
###' @section Infeasible Models:
###'
###' You have a few options if a model is found to be infeasible.  You can try to
###' diagnose the cause of the infeasibility, attempt to repair the
###' infeasibility, or both.  To obtain information that can be useful for
###' diagnosing the cause of an infeasibility, call c("\\code{\\link{#3}}",
###' "gurobi_iis", "r:gurobi_iis", "gurobi_iis")\code{\link{gurobi_iis}} to
###' compute an Irreducible Inconsistent Subsystem (IIS).  This method can be
###' used for both continuous and MIP models, but you should be aware that the
###' MIP version can be quite expensive. We will discuss the details of how IIS
###' results are represented in the c("\\code{\\link{#3}}", "gurobi_iis",
###' "r:gurobi_iis", "gurobi_iis")\code{\link{gurobi_iis}} function
###' documentation.
###'
###' To attempt to repair an infeasibility, call c("\\code{\\link{#3}}",
###' "gurobi_feasrelax", "r:gurobi_feasrelax",
###' "gurobi_feasrelax")\code{\link{gurobi_feasrelax}} to compute a feasibility
###' relaxation for the model. This relaxation allows you to find a solution that
###' minimizes the magnitude of the constraint violation.
###'
###' @section Managing Parameters:
###'
###' The Gurobi optimizer provides a set of parameters that allow you to control
###' many of the details of the optimization process.  Factors like feasibility
###' and optimality tolerances, choices of algorithms, strategies for exploring
###' the MIP search tree, etc., can be controlled by modifying Gurobi parameters
###' before beginning the optimization.
###'
###' Each Gurobi parameter has a default value.  Desired parameter changes are
###' passed in a \code{list} variable.  The name of each named component within
###' this list must be the name of a Gurobi parameter, and the associated value
###' should be the desired value of that parameter.  You can find a complete list
###' of the available Gurobi parameters c("(FIXME-MRhrefH: #1; #2; #3)", "here",
###' "sec:Parameters", "parameters")(FIXME-MRhrefH: here; sec:Parameters;
###' parameters). We will provide additional details on changing parameter
###' settings in the c("\\code{\\link{#3}}", "params", "r:params",
###' "gurobi_params")\code{\link{gurobi_params}} argument section.
###'
###' @section Monitoring Progress:
###'
###' Progress of the optimization can be monitored through Gurobi logging. By
###' default, Gurobi will send output to the screen.  A few simple controls are
###' available for modifying the default logging behavior.  If you would like to
###' direct output to a file as well as to the screen, set the c("(FIXME-MRhrefH:
###' #1; #2; #3)", "LogFile", "parameter:LogFile", "logfile")(FIXME-MRhrefH:
###' LogFile; parameter:LogFile; logfile) parameter to the name of your desired
###' log file. The frequency of logging output can be controlled with the
###' c("(FIXME-MRhrefH: #1; #2; #3)", "DisplayInterval",
###' "parameter:DisplayInterval", "displayinterval")(FIXME-MRhrefH:
###' DisplayInterval; parameter:DisplayInterval; displayinterval) parameter, and
###' logging can be turned off entirely with the c("(FIXME-MRhrefH: #1; #2; #3)",
###' "OutputFlag", "parameter:OutputFlag", "outputflag")(FIXME-MRhrefH:
###' OutputFlag; parameter:OutputFlag; outputflag) parameter.  A detailed
###' description of the Gurobi log file can be found in the c("(FIXME-MRhrefH:
###' #1; #2; #3)", "Logging", "sec:Logging", "logging")(FIXME-MRhrefH: Logging;
###' sec:Logging; logging) section.
###'
###' @section Error Handling:
###'
###' If unsuccessful, the methods of the Gurobi R interface will return an error
###' code and an error message.  A list of possible error codes can be found in
###' the c("(FIXME-MRhrefH: #1; #2; #3)", "Error Code", "sec:ErrorCodes",
###' "error_codes")(FIXME-MRhrefH: Error Code; sec:ErrorCodes; error_codes)
###' section.
###'
###' @section Environments:
###'
###' By default, the various Gurobi functions will look for a valid license file
###' and create a local Gurobi environment. This environment exists for as long
###' as the corresponding R API function is running, and is released upon
###' completion.
###'
###' Another option is to provide an optional \code{env} argument (also through a
###' \code{list}).  This argument allows you to solve the given problem on a
###' Gurobi Compute Server or using the Gurobi Instant Cloud. We will discuss
###' this topic further in the c("\\code{\\link{#3}}", "env", "r:env",
###' "gurobi_env")\code{\link{gurobi_env}} argument section.
###'
###' Gurobi will check the current working directory for a file named
###' \code{gurobi.env}, and it will attempt to read parameter settings from this
###' file if it exists.  The file should be in c("(FIXME-MRhrefH: #1; #2; #3)",
###' "PRM", "format:PRM", "prm_format")(FIXME-MRhrefH: PRM; format:PRM;
###' prm_format) format (briefly, each line should contain a parameter name,
###' followed by the desired value for that parameter).
###'
###' @author Gurobi Optimization
###' @seealso The \code{\link{gurobi_package}} documentation contains general
###' information on the Gurobi R interface. The \code{\link{gurobi_problem}}
###' documentation has a detailed description of the types of models that gurobi
###' is able to optimize. The \code{\link{gurobi_usage}} documentation has an
###' overview on the usage of the Gurobi package.
###'
###' For a detailed description of variables used as input and output of gurobi
###' functions see \code{\link{gurobi_model}}, \code{\link{gurobi_params}} and
###' \code{\link{gurobi_env}}.
###'
###' The functions provided by the Gurobi package are: \code{\link{gurobi}},
###' \code{\link{gurobi_iis}}, \code{\link{gurobi_feasrelax}},
###' \code{\link{gurobi_relax}}, \code{\link{gurobi_read}} and
###' \code{\link{gurobi_write}}.
###' @import methods
###' @references Gurobi Optimization (\url{http://www.gurobi.com}).
###'
###' Gurobi Optimizer Reference Manual
###' (\url{http://www.gurobi.com/documentation/9.0/refman/index.html}).
###' @keywords optimize
##NULL
##
###' @name gurobi_params
###'
###' The gurobi optimizer parameters
###'
###' @description
###' As mentioned previously, the Gurobi optimizer provides a set of parameters
###' that allow you to control many of the details of the optimization process.
###' Factors like feasibility and optimality tolerances, choices of algorithms,
###' strategies for exploring the MIP search tree, etc., can be controlled by
###' modifying Gurobi parameters before beginning the optimization.
###'
###'
###' Parameter changes are specified using a \code{list} variable having multiple
###' \code{named components}, which is passed as an argument to the appropriate
###' Gurobi function (e.g., c("\\code{\\link{#3}}", "gurobi", "r:gurobi",
###' "gurobi")\code{\link{gurobi}}).  The name of each named component must be
###' the name of a Gurobi parameter, and the associated value should be the
###' desired value of that parameter.  You can find a complete list of the
###' available Gurobi parameters c("(FIXME-MRhrefH: #1; #2; #3)", "here",
###' "sec:Parameters", "parameters")(FIXME-MRhrefH: here; sec:Parameters;
###' parameters).
###'
###' To create a list that would set the Gurobi c("(FIXME-MRhrefH: #1; #2; #3)",
###' "Method", "parameter:Method", "method")(FIXME-MRhrefH: Method;
###' parameter:Method; method) parameter to 2 and the c("(FIXME-MRhrefH: #1; #2;
###' #3)", "ResultFile", "parameter:ResultFile", "resultfile")(FIXME-MRhrefH:
###' ResultFile; parameter:ResultFile; resultfile) parameter to \code{model.mps},
###' you would do the following:\cr \preformatted{ params <- list() params$Method
###' <- 2 params$ResultFile <- 'model.mps' }
###'
###' We should say a bit more about the \code{ResultFile} parameter. If this
###' parameter is set, the optimization model that is eventually passed to Gurobi
###' will also be output to the specified file. The filename suffix should be one
###' of \code{.mps}, \code{.lp}, \code{.rew}, or \code{.rlp}, to indicate the
###' desired file format (see the c("(FIXME-MRhrefH: #1; #2; #3)", "file format",
###' "sec:FileFormats", "model_file_formats")(FIXME-MRhrefH: file format;
###' sec:FileFormats; model_file_formats) section for details on Gurobi file
###' formats).
###'
###' @author Gurobi Optimization
###' @seealso The \code{\link{gurobi_package}} documentation contains general
###' information on the Gurobi R interface. The \code{\link{gurobi_problem}}
###' documentation has a detailed description of the types of models that gurobi
###' is able to optimize. The \code{\link{gurobi_usage}} documentation has an
###' overview on the usage of the Gurobi package.
###'
###' For a detailed description of variables used as input and output of gurobi
###' functions see \code{\link{gurobi_model}}, \code{\link{gurobi_params}} and
###' \code{\link{gurobi_env}}.
###'
###' The functions provided by the Gurobi package are: \code{\link{gurobi}},
###' \code{\link{gurobi_iis}}, \code{\link{gurobi_feasrelax}},
###' \code{\link{gurobi_relax}}, \code{\link{gurobi_read}} and
###' \code{\link{gurobi_write}}.
###' @references Gurobi Optimization (\url{http://www.gurobi.com}).
###'
###' Gurobi Optimizer Reference Manual
###' (\url{http://www.gurobi.com/documentation/9.0/refman/index.html}).
###' @keywords optimize
##NULL
##
