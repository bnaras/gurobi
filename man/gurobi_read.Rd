% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gurobi.R
\name{gurobi_read}
\alias{gurobi_read}
\title{Read a model from a file.}
\usage{
gurobi_read(filename, env = NULL)
}
\arguments{
\item{filename}{Name of the file to read. Note that the type of the file is
encoded in the file name suffix. The filename suffix should be one of .mps,
.rew, .lp, .rlp, .ilp, or .opb (see the c("(FIXME-MRhrefH: #1; #2; #3)",
"file formats", "sec:FileFormats", "model_file_formats")(FIXME-MRhrefH: file
formats; sec:FileFormats; model_file_formats) section for details on Gurobi
file formats). The files can be compressed, so additional suffixes of .gz,
.bz2, .zip, or .7z are accepted.}

\item{env}{The env list, when provided, allows you to use Gurobi Compute
Server or Gurobi Instant Cloud. See \code{\link{gurobi_env}} for more
information.}
}
\value{
A \code{model} list variable, as described in the
c("\\code{\\link{#3}}", "model", "r:model",
"gurobi_model")\code{\link{gurobi_model}} section.
}
\description{
Read a model from a file.
}
\examples{
\%
model  <-  gurobi_read('stein9.mps')
result <-  gurobi(model)

}
\references{
Gurobi Optimization (\url{http://www.gurobi.com}).

Gurobi Optimizer Reference Manual
(\url{http://www.gurobi.com/documentation/9.0/refman/index.html}).
}
\seealso{
The \code{\link{gurobi_package}} documentation contains general
information on the Gurobi R interface. The \code{\link{gurobi_problem}}
documentation has a detailed description of the types of models that gurobi
is able to optimize. The \code{\link{gurobi_usage}} documentation has an
overview on the usage of the Gurobi package.

For a detailed description of variables used as input and output of gurobi
functions see \code{\link{gurobi_model}}, \code{\link{gurobi_params}} and
\code{\link{gurobi_env}}.

The functions provided by the Gurobi package are: \code{\link{gurobi}},
\code{\link{gurobi_iis}}, \code{\link{gurobi_feasrelax}},
\code{\link{gurobi_relax}}, \code{\link{gurobi_read}} and
\code{\link{gurobi_write}}.
}
\author{
Gurobi Optimization
}
\keyword{optimize}
