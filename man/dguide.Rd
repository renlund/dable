% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dguide.R
\name{dguide}
\alias{dguide}
\alias{term_type}
\alias{value_type}
\title{description guide}
\usage{
dguide(data, id = NULL, elim.set = NULL, vtab = NULL, stab = NULL, ...)

term_type(
  term,
  data,
  stab = NULL,
  bnry.list = NULL,
  real.tol = NULL,
  catg.tol = NULL
)

value_type(x, bnry.list = list(), real.tol = 0, catg.tol = Inf)
}
\arguments{
\item{data}{data.frame, the data}

\item{id}{character vector; id variables used.}

\item{elim.set}{character vector, optionally provide names of variables to
ignore}

\item{vtab}{data.frame, optionally provide a variable table (vtab)}

\item{stab}{data.frame, optionally provide a 'survival-table'}

\item{...}{arguments passed to \code{term_type}}

\item{term}{character, name of variables to describe}

\item{bnry.list}{list, optionally provide list of values (length 2 vectors)
that are the only values considered to be associated with binary variables}

\item{real.tol}{numeric, if given, a numeric variable with at most this
number of unique value will be considered a 'catg', instead of 'real',
variable}

\item{catg.tol}{numeric, if given, a categorical variable with more than
this number of unqique values will be considered a 'lcat' variable and
be described differently}

\item{x}{vector of values}
}
\description{
Determine how to describe a data frame.
}
\section{Functions}{
\itemize{
\item \code{term_type()}: term_type determines the type of a variable in a data set
through its name (through its appearance in a 'survival-list' if given)
and after that through its values by value_type

\item \code{value_type()}: value_type determines the type of a variable through its values

}}
