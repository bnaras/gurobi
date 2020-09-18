# R wrapper for Gurobi Optimizer

# Warnings --------------------------------------------------------------------

depr_msg <- function(msg) .Deprecated(msg = msg)

warn_msg <- function(msg) warning(msg, call. = FALSE)

# Coercion error handlers -----------------------------------------------------

stopname <- function(expr, src = NULL, sep = "$")
{
  x <- list(src = src, sep = sep, expr = expr)
  class(x) <- "stopname"
  return(x)
}

is.stopname <- function(x) inherits(x, "stopname")

toString.stopname <- function(x)
{
  if (is.null(x$src))
    expr_to_str(x$expr)
  else
    paste0(expr_to_str(x$src),expr_to_str(x$sep),expr_to_str(x$expr))
}

expr_to_str <- function(x) if (is.language(x)) deparse(x) else toString(x)

set_to_str <- function(x) paste0("{",expr_to_str(x),"}")

seq_to_str <- function(lb, ub) paste0(expr_to_str(lb),",...,",expr_to_str(ub))

stop_msg <- function(msg) stop(msg, call. = FALSE)

stop_op <- function(x, op, y)
{
  stop_msg(paste0(expr_to_str(x)," ",op," ",expr_to_str(y)))
}

stop_func_op <- function(f, x, op, y)
{
  stop_op(paste0(expr_to_str(f),"(",expr_to_str(x),")"), op, y)
}

stop_na <- function(x) stop_op(x, "contains", NA)

stop_nin <- function(x, s) stop_op(x, "value NOT in", s)

stop_not <- function(x, y) stop_op(x, "is NOT", y)

stop_length_0 <- function(x) stop_func_op("length", x, "==", 0L)

stop_length_ne <- function(x, y) stop_func_op("length", x, "!=", y)

stop_length_nin <- function(x, lb, ub)
{
  stop_func_op("length", x, "NOT in", seq_to_str(lb, ub))
}

stop_length_ne_length <- function(x, y)
{
  stop_length_ne(x, paste0("length(",expr_to_str(y),")"))
}

stop_ncol_0 <- function(x) stop_func_op("ncol", x, "==", 0L)

stop_ncol_ne <- function(x, y) stop_func_op("ncol", x, "!=", y)

stop_nrow_ne <- function(x, y) stop_func_op("nrow", x, "!=", y)

stop_typeof_eq <- function(x, y) stop_func_op("typeof", x, "==", y)

stop_islist_F <- function(x) stop_func_op("is.list", x, "==", FALSE)

# Coercion --------------------------------------------------------------------

is_empty <- function(xname, x, OPT = FALSE)
{
  if (length(x) > 0L)
    return(FALSE)

  if (OPT)
    return(TRUE)

  stop_length_0(xname)
}

resize <- function(xname, x, lenname, len, REP = FALSE)
{
  if (len < 0L)   # -1L will return first element
    return(x[1L]) # no error when length(x) > 1L (extra elements ignored)

  if (length(x) == len)
    return(x)

  if (REP && length(x) == 1L && len > 1L)
    return(rep(x, len))

  stop_length_ne(xname, lenname)
}

to_bin <- function(x, src = NULL, OPT = FALSE)
{
  xname <- stopname(substitute(x), src)

  if (is_empty(xname, x, OPT = OPT))
    return(NULL)

  if (!is.logical(x))
    stop_typeof_eq(xname, typeof(x))

  if (anyNA(x))
    stop_na(xname)

  if (x[1L]) 1L else 0L
}

to_chr <- function(x, src = NULL, OPT = FALSE, NUMOK = FALSE,
                   valid = NULL, len = -1L, REP = FALSE)
{
  xname <- stopname(substitute(x), src)

  if (is_empty(xname, x, OPT = OPT))
    return(NULL)

  if (!is.character(x)) {
    if (NUMOK && is.numeric(x))
      x <- as.character(x)
    else
      stop_typeof_eq(xname, typeof(x))
  }

  if (anyNA(x))
    stop_na(xname)

  if (!is.null(valid) && !all(x %in% valid))
    stop_nin(xname, set_to_str(valid))

  resize(xname, x, substitute(len), len, REP = REP)
}

to_dbl <- function(x, src = NULL, OPT = FALSE,
                   na = NULL, len = -1L, REP = FALSE)
{
  xname <- stopname(substitute(x), src)

  if (is_empty(xname, x, OPT = OPT))
    return(NULL)

  if (!is.double(x)) {
    if (is.integer(x))
      x <- as.double(x)
    else
      stop_typeof_eq(xname, typeof(x))
  }

  if (anyNA(x)) {
    if (!is.null(na))
      x[is.na(x)] <- na
    else
      stop_na(xname)
  }

  resize(xname, x, substitute(len), len, REP = REP)
}

to_int <- function(x, src = NULL, OPT = FALSE, lb = -.Machine$integer.max,
                   ub = .Machine$integer.max, len = -1L)
{
  xname <- stopname(substitute(x), src)

  if (is_empty(xname, x, OPT = OPT))
    return(NULL)

  if (!is.numeric(x))
    stop_typeof_eq(xname, typeof(x))

  if (anyNA(x))
    stop_na(xname)

  if (min(x) < lb || max(x) > ub) {
    if (lb <= -.Machine$integer.max && ub >= .Machine$integer.max)
      stop_nin(xname, "integer range")
    else if (lb + 1L < ub)
      stop_nin(xname, seq_to_str(lb, ub))
    else
      stop_nin(xname, set_to_str(c(lb, ub)))
  }

  if (is.double(x))
    x <- as.integer(x)

  resize(xname, x, substitute(len), len)
}

to_idx <- function(x, src = NULL, N = .Machine$integer.max, minlen = 1L)
{
  xname      <- stopname(substitute(x), src)
  Nname      <- substitute(N)
  minlenname <- substitute(minlen)

  if (is_empty(xname, x))
    return(NULL)

  if (!is.numeric(x))
    stop_typeof_eq(xname, typeof(x))

  if (anyNA(x))
    stop_na(xname)

  if (min(x) < 1L || max(x) > N)
    stop_nin(xname, seq_to_str(1L, Nname))

  if (is.double(x))
    x <- as.integer(x)

  if (length(x) < minlen || length(x) > N)
    stop_length_nin(xname, minlenname, Nname)

  return(x - 1L)
}

# convert a dense vector or sparse vector to a sparse list
#
# We can handle:
# - dense vector: For a dense vector there are two options: (i) standard
#                 dense vector; (ii) vector with a single element which
#                 will should be repeated number of variable times
# - sparseVector
to_vectorList <- function(x, src = NULL, OPT = FALSE, len = -1L)
{
  xname <- stopname(substitute(x), src)

  if (is.null(x) || is_empty(xname, x, OPT = OPT)) {
    if (OPT)
      return(list(i = integer(), v = double()))
    else
      stop_length_0(xname)
  }
  if (is.vector(x)) {
    # a dense vector objects cannot contain any duplicate entries
    if (!is.numeric(x))
      stop_typeof_eq(xname, typeof(x))
    if (anyNA(x))
      stop_na(xname)

    # if the vector has a length of 1 and we have more variable we resize
    # the vector by repeating the first element number of variable times
    x <- resize(xname, x, substitute(len), len, REP = TRUE)

    w <- which(x != 0)    # collect the indices of all non-zero elements
    v <- as.double(x[w])  # collect the corresponds non-zero values
    i <- w - 1L           # adjust the variable indices (R uses 1-base)
  } else if (is(x, "sparseVector")) {
    # sparse vector objects cannot contain any duplicate entries
    if (anyNA(x))
      stop_na(xname)

    v <- as.double(x@x)        # collect the non-zero values
    i <- as.integer(x@i) - 1L  # collect the indices and adjust them (R uses 1-base)
  } else {
    stop_not(xname, "vector or sparseVector")
  }

  list(i = i, v = v)
}

to_triplets <- function(x, src = NULL, OPT = FALSE, N = -1L, AGG = FALSE)
{
  xname <- stopname(substitute(x), src)
  Nname <- substitute(N)

  if (is.null(x)) {
    if (OPT)
      return(list(i = integer(), j = integer(), v = double()))
    else
      stop_length_0(xname)
  }

  if (is.matrix(x)) {
    AGG <- FALSE # matrix objects cannot result in duplicate triplets
    if (!is.numeric(x))
      stop_typeof_eq(xname, typeof(x))
    if (anyNA(x))
      stop_na(xname)
    w <- which(x != 0)
    v <- as.double(x[w])
    w <- w - 1L
    i <- w %% nrow(x)
    j <- w %/% nrow(x)
  } else if (is(x, "sparseMatrix")) {
    x <- as(x, "dsparseMatrix")
    if (is(x, "TsparseMatrix")) {
      x <- as(x, "dgTMatrix")
      i <- x@i
      j <- x@j
    } else {
      AGG <- FALSE # d*RMatrix and d*CMatrix cannot contain duplicate triplets
      if (is(x, "RsparseMatrix")) {
        x <- as(x, "dgRMatrix")
        i <- .Call("decompress_sparse_pointers", x@p, PACKAGE = "gurobi")
        j <- x@j
      } else {
        x <- as(x, "dgCMatrix") # Coerce all other dsparseMatrix subclasses
        i <- x@i
        j <- .Call("decompress_sparse_pointers", x@p, PACKAGE = "gurobi")
      }
    }
    if (anyNA(x@x))
      stop_na(xname)
    v <- as.double(x@x)
  } else if (is(x, "simple_triplet_matrix")) {
    if (!is.numeric(x$v))
      stop_typeof_eq(stopname("v", src = xname), typeof(x$v))
    i <- x$i - 1L
    j <- x$j - 1L
    if (anyNA(x$v))
      stop_na(xname)
    v <- as.double(x$v)
  } else {
    stop_not(xname, "matrix, sparseMatrix or simple_triplet_matrix")
  }

  if (N >= 0L) {
    if (ncol(x) != N)
      stop_ncol_ne(xname, Nname)
    if (nrow(x) != N)
      stop_nrow_ne(xname, Nname)
  }

  if (AGG) {
    sorted <- .Call("are_triplets_sorted", i, j, PACKAGE = "gurobi")
    if (sorted != "byboth") # "byboth" cannot contain duplicate triplets
      v <- .Call("aggregate_triplets", sorted, i, j, v,
                                       c(nrow(x), ncol(x)), PACKAGE = "gurobi")
  }

  list(i = i, j = j, v = v)
}

# Extracting arguments to build GRBenv ----------------------------------------

loadenv_args <- function(env)
{
  args             <- list()
  args$logfilename <- to_chr(env$logfile, OPT = TRUE)
  return(args)
}

loadclientenv_args <- function(env)
{
  args               <- list()
  args$logfilename   <- to_chr(env$logfile, OPT = TRUE)
  args$computeserver <- to_chr(env$computeserver)
  args$router        <- to_chr(env$router, OPT = TRUE)
  args$password      <- to_chr(env$password, OPT = TRUE)
  args$tlsinsecure   <- to_bin(env$tlsinsecure, OPT = TRUE)
  args$priority      <- to_int(env$priority, OPT = TRUE, lb = -100L, ub = 100L)
  return(args)
}

loadcloudenv_args <- function(env)
{
  args             <- list()
  args$logfilename <- to_chr(env$logfile, OPT = TRUE)
  args$accessid    <- to_chr(env$accessid)
  args$secretkey   <- to_chr(env$secretkey)
  args$pool        <- to_chr(env$pool, OPT = TRUE)
  args$priority    <- to_int(env$priority, OPT = TRUE, lb = -100L, ub = 100L)
  return(args)
}

setparam_args <- function(i, names, params)
{
  args           <- list()
  args$paramname <- names[i]
  args$value     <- to_chr(params[[i]], NUMOK = TRUE)
  return(args)
}

env_args <- function(env, params)
{
  args <- list()

  if (!is.null(env)) {
    stopifnot(is.list(env))
    stopifnot(is.null(env$computeserver) || is.null(env$accessid))

    if (!is.null(env$computeserver))
      args$loadclientenv <- loadclientenv_args(env)
    else if (!is.null(env$accessid))
      args$loadcloudenv <- loadcloudenv_args(env)
    else
      args$loadenv <- loadenv_args(env)
  }

  if (length(params) > 0L) {
    stopifnot(is.list(params), length(names(params)) > 0L)
    args$setparam <- lapply(seq_along(params), setparam_args,
                            names(params), params)
  }

  return(args)
}

# Extracting arguments to build GRBmodel --------------------------------------

VTYPES      <- c("C", "I", "B", "S", "N")
SENSES      <- c("<", "<=", ">", ">=", "=")
MODELSENSES <- c("max", "Max", "MAX", "min", "Min", "MIN")

newmodel_args <- function(model)
{
  args      <- list()
  args$name <- to_chr(model$modelname, OPT = TRUE)
  return(args)
}

addvars_args <- function(model, A)
{
  args          <- list()
  args$numvars  <- ncol(A)
  args$obj      <- to_dbl(model$obj, OPT = TRUE, len = ncol(A), REP = TRUE)
  args$lb       <- to_dbl(model$lb, OPT = TRUE, len = ncol(A), REP = TRUE)
  args$ub       <- to_dbl(model$ub, OPT = TRUE, len = ncol(A), REP = TRUE)
  args$vtype    <- to_chr(model$vtype, OPT = TRUE, len = ncol(A), REP = TRUE,
                          valid = VTYPES)
  args$varnames <- to_chr(model$varnames, OPT = TRUE, len = ncol(A))
  return(args)
}

addconstrs_args <- function(model, A)
{
  args             <- list()
  args$numconstrs  <- nrow(A)
  args$sense       <- to_chr(model$sense, OPT = TRUE,
                             len = nrow(A), REP = TRUE, valid = SENSES)
  args$rhs         <- to_dbl(model$rhs, OPT = TRUE, len = nrow(A), REP = TRUE)
  args$constrnames <- to_chr(model$constrnames, OPT = TRUE, len = nrow(A))
  return(args)
}

addqpterms_args <- function(model, A)
{
  Qmat <- to_triplets(model$Q, OPT = TRUE, N = ncol(A))

  if (length(Qmat$v) == 0L)
    return(NULL)

  args        <- list()
  args$numqnz <- length(Qmat$v)
  args$qrow   <- Qmat$i
  args$qcol   <- Qmat$j
  args$qval   <- Qmat$v
  return(args)
}

addcone_args <- function(i, cones, Qcval, A, lb)
{
  if (is.list(cones[[i]])) {
    Qcind <- to_idx(unlist(cones[[i]]), N = ncol(A), minlen = 2L)

    if (length(Qcind) != length(cones[[i]]))
      stop_length_ne_length("unlist(cones[[i]])", "cones[[i]]")

    first <- as.integer(cones[[i]][[1L]])
  } else {
    Qcind <- to_idx(cones[[i]], N = ncol(A), minlen = 2L)

    first <- as.integer(cones[[i]][1L])
  }

  args            <- list()
  args$rhslb      <- lb[first] # First index corresponds to RHS variable
  args$numqnz     <- length(Qcind)
  args$qrow       <- Qcind
  args$qcol       <- Qcind
  args$qval       <- Qcval
  args$constrname <- paste0("cone",i - 1L)
  return(args)
}

addqconstr_args <- function(quadcon, src, A)
{
  if (!is.list(quadcon))
    stop_islist_F(src)

  Qc    <- quadcon$Qc
  Qcmat <- to_triplets(Qc, src, N = ncol(A))
  q     <- quadcon$q
  qvec  <- to_vectorList(q, src, OPT = TRUE, len = ncol(A))
  sense <- quadcon$sense
  rhs   <- quadcon$rhs
  name  <- quadcon$name

  args            <- list()
  if (length(Qcmat$v) > 0L) {
    args$numqnz   <- length(Qcmat$v)
    args$qrow     <- Qcmat$i
    args$qcol     <- Qcmat$j
    args$qval     <- Qcmat$v
  }

  # collect linear part
  args$numlnz   <- length(qvec$v)
  args$lind     <- qvec$i
  args$lval     <- qvec$v

  args$sense      <- to_chr(sense, src, OPT = TRUE, valid = SENSES)
  args$rhs        <- to_dbl(rhs, src, OPT = TRUE)
  args$constrname <- to_chr(name, src, OPT = TRUE)
  return(args)
}

addsos_args <- function(sos, src, A)
{
  if (!is.list(sos))
    stop_islist_F(src)

  type   <- sos$type
  index  <- sos$index
  weight <- sos$weight

  if (length(index) != length(weight))
    stop_length_ne_length(stopname("index", src), stopname("weight", src))

  args            <- list()
  args$types      <- to_int(type, src, lb = 1L, ub = 2L)
  args$ind        <- to_idx(index, src, N = ncol(A))
  args$weight     <- to_dbl(weight, src, len = length(weight))
  args$nummembers <- length(args$ind)
  return(args)
}

setpwlobj_args <- function(pwlobj, src, A)
{
  if (!is.list(pwlobj))
    stop_islist_F(src)

  var <- pwlobj$var
  x   <- pwlobj$x
  y   <- pwlobj$y

  if (length(x) == 0L)
    stop_length_0(stopname("x", src))

  if (length(x) != length(y))
    stop_length_ne_length(stopname("x", src), stopname("y", src))

  args         <- list()
  args$var     <- to_idx(var, src, N = ncol(A))[1] # Extra elements ignored
  args$x       <- to_dbl(x, src, len = length(x))
  args$y       <- to_dbl(y, src, len = length(y))
  args$npoints <- length(args$x)
  return(args)
}

addgenconstrXxx_args <- function(gencon, src, A)
{
  if (!is.list(gencon))
    stop_islist_F(src)

  resvar <- gencon$resvar
  vars   <- gencon$vars
  con    <- gencon$con
  name   <- gencon$name

  args          <- list()
  args$resvar   <- to_idx(resvar, src, N = ncol(A))[1] # Extra elements ignored
  args$vars     <- to_idx(vars, src, N = ncol(A))
  args$nvars    <- length(args$vars)
  args$constant <- to_dbl(con, src, OPT = TRUE)
  args$name     <- to_chr(name, src, OPT = TRUE)
  return(args)
}

addgenconstrAbs_args <- function(gencon, src, A)
{
  if (!is.list(gencon))
    stop_islist_F(src)

  resvar <- gencon$resvar
  argvar <- gencon$argvar
  name   <- gencon$name

  args        <- list()
  args$resvar <- to_idx(resvar, src, N = ncol(A))[1] # Extra elements ignored
  args$argvar <- to_idx(argvar, src, N = ncol(A))[1] # Extra elements ignored
  args$name   <- to_chr(name, src, OPT = TRUE)
  return(args)
}

addgenconstrInd_args <- function(gencon, src, A)
{
  if (!is.list(gencon))
    stop_islist_F(src)

  binvar <- gencon$binvar
  binval <- gencon$binval
  a      <- gencon$a
  avec   <- to_vectorList(a, src, OPT = TRUE, len = ncol(A))
  sense  <- gencon$sense
  rhs    <- gencon$rhs
  name   <- gencon$name

  args        <- list()
  args$binvar <- to_idx(binvar, src, N = ncol(A))[1] # Extra elements ignored
  args$binval <- to_int(binval, src, lb = 0L, ub = 1L)
  args$nvars  <- length(avec$v)
  args$ind    <- avec$i
  args$val    <- avec$v
  args$sense  <- to_chr(sense, src, OPT = TRUE, valid = SENSES)
  args$rhs    <- to_dbl(rhs, src, OPT = TRUE)
  args$name   <- to_chr(name, src, OPT = TRUE)
  return(args)
}

addgenconstrPWL_args <- function(gencon, src, A)
{
  if (!is.list(gencon))
    stop_islist_F(src)

  xvar <- gencon$xvar
  yvar <- gencon$yvar
  xpts <- gencon$xpts
  ypts <- gencon$ypts
  name <- gencon$name

  if (length(xpts) == 0L)
    stop_length_0(stopname("xpts", src))

  if (length(xpts) != length(ypts))
    stop_length_ne_length(stopname("xpts", src), stopname("ypts", src))

  args      <- list()
  args$xvar <- to_idx(xvar, src, N = ncol(A))[1] # Extra elements ignored
  args$yvar <- to_idx(yvar, src, N = ncol(A))[1] # Extra elements ignored
  args$xpts <- to_dbl(xpts, src, len = length(xpts))
  args$ypts <- to_dbl(ypts, src, len = length(ypts))
  args$npts <- length(args$xpts)

  args$name <- to_chr(name, src, OPT = TRUE)
  return(args)
}

addgenconstrPoly_args <- function(gencon, src, A)
{
  if (!is.list(gencon))
    stop_islist_F(src)

  xvar            <- gencon$xvar
  yvar            <- gencon$yvar
  p               <- gencon$p
  funcpieces      <- gencon$funcpieces
  funcpiecelength <- gencon$funcpiecelength
  funcpieceerror  <- gencon$funcpieceerror
  funcpieceratio  <- gencon$funcpieceratio
  name            <- gencon$name

  args                 <- list()
  args$xvar            <- to_idx(xvar, src, N = ncol(A))[1] # Extra elements ignored
  args$yvar            <- to_idx(yvar, src, N = ncol(A))[1] # Extra elements ignored
  args$p               <- to_dbl(p, src, len = length(p))
  args$plen            <- length(args$p)
  args$funcpieces      <- to_int(funcpieces, src, OPT = TRUE)
  args$funcpiecelength <- to_dbl(funcpiecelength, src, OPT = TRUE)
  args$funcpieceerror  <- to_dbl(funcpieceerror, src, OPT = TRUE)
  args$funcpieceratio  <- to_dbl(funcpieceratio, src, OPT = TRUE)
  args$name            <- to_chr(name, src, OPT = TRUE)
  return(args)
}

# handles y = exp_a(x), y = log_a(x), y = x^a
addgenconstrTwoVarsBase_args <- function(gencon, src, A)
{
  if (!is.list(gencon))
    stop_islist_F(src)

  xvar            <- gencon$xvar
  yvar            <- gencon$yvar
  a               <- gencon$a
  funcpieces      <- gencon$funcpieces
  funcpiecelength <- gencon$funcpiecelength
  funcpieceerror  <- gencon$funcpieceerror
  funcpieceratio  <- gencon$funcpieceratio
  name            <- gencon$name

  args                 <- list()
  args$xvar            <- to_idx(xvar, src, N = ncol(A))[1] # Extra elements ignored
  args$yvar            <- to_idx(yvar, src, N = ncol(A))[1] # Extra elements ignored
  args$a               <- to_dbl(a, src, OPT = FALSE)
  args$funcpieces      <- to_int(funcpieces, src, OPT = TRUE)
  args$funcpiecelength <- to_dbl(funcpiecelength, src, OPT = TRUE)
  args$funcpieceerror  <- to_dbl(funcpieceerror, src, OPT = TRUE)
  args$funcpieceratio  <- to_dbl(funcpieceratio, src, OPT = TRUE)
  args$name            <- to_chr(name, src, OPT = TRUE)
  return(args)
}

# handles y = exp(x), y = log(x), y = sin(x), y = cos(x), y = tan(x)
addgenconstrTwoVars_args <- function(gencon, src, A)
{
  if (!is.list(gencon))
    stop_islist_F(src)

  xvar            <- gencon$xvar
  yvar            <- gencon$yvar
  funcpieces      <- gencon$funcpieces
  funcpiecelength <- gencon$funcpiecelength
  funcpieceerror  <- gencon$funcpieceerror
  funcpieceratio  <- gencon$funcpieceratio
  name            <- gencon$name

  args                 <- list()
  args$xvar            <- to_idx(xvar, src, N = ncol(A))[1] # Extra elements ignored
  args$yvar            <- to_idx(yvar, src, N = ncol(A))[1] # Extra elements ignored
  args$funcpieces      <- to_int(funcpieces, src, OPT = TRUE)
  args$funcpiecelength <- to_dbl(funcpiecelength, src, OPT = TRUE)
  args$funcpieceerror  <- to_dbl(funcpieceerror, src, OPT = TRUE)
  args$funcpieceratio  <- to_dbl(funcpieceratio, src, OPT = TRUE)
  args$name            <- to_chr(name, src, OPT = TRUE)
  return(args)
}

setobjectiven_args <- function(multiobj, src, A)
{
  if (!is.list(multiobj))
    stop_islist_F(src)

  objn     <- multiobj$objn
  objn     <- to_dbl(objn, src, len = ncol(A), REP = TRUE)
  objnnzs  <- which(objn != 0)
  objcon   <- multiobj$objcon
  priority <- multiobj$priority
  weight   <- multiobj$weight
  reltol   <- multiobj$reltol
  abstol   <- multiobj$abstol
  name     <- multiobj$name

  args          <- list()
  args$lnz      <- length(objnnzs)
  args$lind     <- objnnzs - 1L
  args$lval     <- objn[objnnzs]
  args$constant <- to_dbl(objcon, src, OPT = TRUE)
  args$priority <- to_int(priority, src, OPT = TRUE)
  args$weight   <- to_dbl(weight, src, OPT = TRUE)
  args$reltol   <- to_dbl(reltol, src, OPT = TRUE)
  args$abstol   <- to_dbl(abstol, src, OPT = TRUE)
  args$name     <- to_chr(name, src, OPT = TRUE)
  return(args)
}

setattr_args <- function(model, A)
{
  args <- list()

  # Check for invalid combinations

  stopifnot(!is.null(model$varhintval) || is.null(model$varhintpri),
            is.null(model$vbasis) == is.null(model$cbasis),
            is.null(model$pstart) == is.null(model$dstart))

  # Model attributes

  args$modelsense <- to_chr(model$modelsense, OPT = TRUE, valid = MODELSENSES)
  args$objcon     <- to_dbl(model$objcon, OPT = TRUE)

  # Variable attributes

  args$start          <- to_dbl(model$start, OPT = TRUE,
                                na = 1e101, len = ncol(A))
  args$varhintval     <- to_dbl(model$varhintval, OPT = TRUE,
                                na = 1e101, len = ncol(A))
  args$varhintpri     <- to_int(model$varhintpri, OPT = TRUE, len = ncol(A))
  args$branchpriority <- to_int(model$branchpriority, OPT = TRUE,
                                len = ncol(A))
  args$partition      <- to_int(model$partition, OPT = TRUE, len = ncol(A))
  args$vbasis         <- to_int(model$vbasis, OPT = TRUE,
                                lb = -3L, ub = 0L, len = ncol(A))
  args$pstart         <- to_dbl(model$pstart, OPT = TRUE, len = ncol(A))

  # Constraint attributes

  args$cbasis <- to_int(model$cbasis, OPT = TRUE,
                        lb = -1L, ub = 0L, len = nrow(A))
  args$dstart <- to_dbl(model$dstart, OPT = TRUE, len = nrow(A))
  args$lazy   <- to_int(model$lazy, OPT = TRUE,
                        lb = 0L, ub = 3L, len = nrow(A))

  return(args)
}

model_args <- function(model)
{
  stopifnot(is.list(model))

  stopifnot(is.null(model$multiobj) || is.null(model$obj),
            is.null(model$multiobj) || is.null(model$Q),
            is.null(model$multiobj) || is.null(model$pwlobj),
            is.null(model$multiobj) || is.null(model$objcon))

  Amat <- to_triplets(model$A, AGG = TRUE) # A is only mandatory named
  stopifnot(ncol(model$A) >= 1L)           # component of model and must be a
  A <- model$A                             # matrix such that ncol() >= 1L

  args            <- list()
  args$newmodel   <- newmodel_args(model)
  args$addvars    <- addvars_args(model, A)
  args$addconstrs <- addconstrs_args(model, A)

  if (length(Amat$v) > 0L) {
    args$chgcoeffs <- list(numchgs = length(Amat$v),
                           cind = Amat$i, vind = Amat$j, val = Amat$v)
  }

  args$addqpterms <- addqpterms_args(model, A)

  if (length(model$cones) > 0L) {
    stopifnot(is.list(model$cones))
    Qcval <- rep(1, ncol(A))
    Qcval[1L] <- -1 # First index corresponds to RHS variable
    args$addcone <- lapply(seq_along(model$cones), addcone_args,
                           model$cones, Qcval, A, args$addvars$lb)
    depr_msg("model$cones has been deprecated -- use model$quadcon")
  }

  lapply_args <- function(x, fun)
  {
    xname <- substitute(x)

    if (length(x) == 0L)
      return(NULL)

    if (!is.list(x))
      stop_islist_F(xname)

    src <- stopname("[[i]]", src = xname, sep = "")

    lapply(x, fun, src, A)
  }

  args$addqconstr       <- lapply_args(model$quadcon, addqconstr_args)
  args$addsos           <- lapply_args(model$sos, addsos_args)
  args$setpwlobj        <- lapply_args(model$pwlobj, setpwlobj_args)
  args$addgenconstrMax  <- lapply_args(model$genconmax, addgenconstrXxx_args)
  args$addgenconstrMin  <- lapply_args(model$genconmin, addgenconstrXxx_args)
  args$addgenconstrAbs  <- lapply_args(model$genconabs, addgenconstrAbs_args)
  args$addgenconstrAnd  <- lapply_args(model$genconand, addgenconstrXxx_args)
  args$addgenconstrOr   <- lapply_args(model$genconor, addgenconstrXxx_args)
  args$addgenconstrInd  <- lapply_args(model$genconind, addgenconstrInd_args)
  args$addgenconstrPWL  <- lapply_args(model$genconpwl, addgenconstrPWL_args)
  args$addgenconstrPoly <- lapply_args(model$genconpoly, addgenconstrPoly_args)
  args$addgenconstrExpA <- lapply_args(model$genconexpa, addgenconstrTwoVarsBase_args)
  args$addgenconstrLogA <- lapply_args(model$genconloga, addgenconstrTwoVarsBase_args)
  args$addgenconstrPow  <- lapply_args(model$genconpow, addgenconstrTwoVarsBase_args)
  args$addgenconstrExp  <- lapply_args(model$genconexp, addgenconstrTwoVars_args)
  args$addgenconstrLog  <- lapply_args(model$genconlog, addgenconstrTwoVars_args)
  args$addgenconstrSin  <- lapply_args(model$genconsin, addgenconstrTwoVars_args)
  args$addgenconstrCos  <- lapply_args(model$genconcos, addgenconstrTwoVars_args)
  args$addgenconstrTan  <- lapply_args(model$gencontan, addgenconstrTwoVars_args)
  args$setobjectiven    <- lapply_args(model$multiobj, setobjectiven_args)
  args$setattr          <- setattr_args(model, A)
  return(args)
}

# Extracting arguments to build actions ---------------------------------------

feasrelax_args <- function(relaxobjtype, minrelax, penalties, A)
{
  stopifnot(is.list(penalties))

  args              <- list()
  args$relaxobjtype <- to_int(relaxobjtype, lb = 0L, ub = 2L)
  args$minrelax     <- to_bin(minrelax)
  args$lbpen        <- to_dbl(penalties$lb, OPT = TRUE,
                              len = ncol(A), REP = TRUE)
  args$ubpen        <- to_dbl(penalties$ub, OPT = TRUE,
                              len = ncol(A), REP = TRUE)
  args$rhspen       <- to_dbl(penalties$rhs, OPT = TRUE,
                              len = nrow(A), REP = TRUE)
  return(args)
}

# Wrappers for gurobi_common() calls ------------------------------------------

gurobi_common_wrapper <- function(env_args, model_args, action_args)
{
  tryCatch({
    .Call("gurobi_common", env_args, model_args,
                           action_args, PACKAGE = "gurobi")
  }, error = function(err) {
    if (grepl("cannot allocate vector of size ", err$message))
      stop_msg("Out of memory -- restart R to avoid potential memory leaks")
    else
      stop_msg(err$message)
  })
}


#' @importFrom slam simple_triplet_matrix
#' @importFrom Matrix sparseMatrix
to_mat <- function(x, mtype)
{
  if (is.null(x))
    return(NULL)

  if (mtype == "matrix") {
    A <- matrix(0, nrow = x$nrow, ncol = x$ncol)
    A[x$i + x$nrow * (x$j - 1L)] <- x$v
    return (A)
  } else if (mtype == "sparseMatrix") {
    Matrix::sparseMatrix(i = x$i, j = x$j, x = x$v, dims = c(x$nrow, x$ncol))
  } else { # default
    slam::simple_triplet_matrix(i = x$i, j = x$j, v = x$v,
                          nrow = x$nrow, ncol = x$ncol)
  }
}

#' @importFrom Matrix sparseVector
to_sparseVec <- function(x)
{
  if (is.null(x))
    return(NULL)

  Matrix::sparseVector(i = x$i, x = x$x, length = x$length)
}

MATRIXTYPES <- c("", "matrix", "sparseMatrix", "simple_triplet_matrix")

to_model <- function(x)
{
  # Coerce matrix components

  mtype <- Sys.getenv("GRB_R_MATRIXTYPE", unset = "simple_triplet_matrix")
  if (!all(mtype %in% MATRIXTYPES))
    warn_msg("Invalid GRB_R_MATRIXTYPE -- defaulting to simple_triplet_matrix")

  # convert the matrices into the requested format
  x$A <- to_mat(x$A, mtype)
  x$Q <- to_mat(x$Q, mtype)

  # handle the matrices and linear parts of the quadratic constraints
  if (length(x$quadcon) > 0L)
    x$quadcon <- lapply(x$quadcon,
                        function(y) { y$Qc <- to_mat(y$Qc, mtype); y$q <- to_sparseVec(y$q); return(y) })

  # handle the linear sum of the indicator constraints
  if (length(x$genconind) > 0L)
    x$genconind <- lapply(x$genconind,
                          function(y) { y$a <- to_sparseVec(y$a); return(y) })

  return(x)
}

to_iis <- function(x)
{
  # Coerce all integer vector components to logical

  lapply(x, function(y) as.logical(y))
}

to_feasrelax <- function(x)
{
  # Coerce model component

  x$model <- to_model(x$model)

  return(x)
}
