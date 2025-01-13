##' standard baseline functions
##'
##' Description functions that work together
##' @param x input vector
##' @param weight case weight
##' @param ... arguments passed along
##' @name baseline-standard
NULL

##' @rdname baseline-standard
##' @details real.bl: (weighted) median and interquartile range
##' @export
real.bl <- function(x, weight = NULL, ...){
    q <- quartiles(x = x, weight = weight)
    data.frame(Variable = di.Variable(x, ...),
               Summary = sprintf("%s (%s - %s)", dform.num(q$Q2),
                                 dform.num(q$Q1), dform.num(q$Q2)),
               Summary.info = "Numeric variable: Median (Q1-Q3)")
}
attr(real.bl, "meta") <- c("Summary.info", "Variable")

##' @rdname baseline-standard
##' @details real2.bl: (weighted) mean and standard deviation
##' @export
real2.bl <- function(x, weight = NULL, ...){
    r <- mean_sd(x = x, weight = weight, ...)
    data.frame(Variable = di.Variable(x, ...),
               Summary = sprintf("%s (%s)", dform.num(r$Mean), dform.num(r$SD)),
               Summary.info = "Numeric variable: Mean(SD)")
}
attr(real2.bl, "meta") <- c("Summary.info", "Variable")

##' @rdname baseline-standard
##' @details bnry.bl: (weighted) count and percent of
##'     non-reference value as single string
##' @export
bnry.bl <- function(x, weight = NULL, ...){
    r <- bnry.count_prop(x = x, weight = weight, ...)
    data.frame(Variable = di.Variable(x, ...),
               Summary = sprintf("%s (%s%%)",
                                 dform.num(r$Count),
                                 dform.num(100 * r$Proportion)),
               Summary.info = "Categorical variable: Count (Percent)")
}
attr(bnry.bl, "meta") <- c("Variable", "Summary.info")

##' @rdname baseline-standard
##' @details catg.bl: (weighted) count and proportion of
##'     categorical levels as single string
##' @export
catg.bl <- function(x, weight = NULL, ...){
    r <- catg.count_prop(x = x, weight = weight, ...)
    data.frame(Variable = di.Variable(x, ...),
               Summary = sprintf("%s (%s%%)",
                                 dform.num.vec(r$Count),
                                 dform.num.vec(100 * r$Proportion)),
               Summary.info = "Categorical variable: Count (Percent)")
}
attr(catg.bl, "meta") <- c("Variable", "Summary.info")

##' @rdname baseline-standard
##' @details lcat.bl: number of unique non-missing values in x
##' @export
lcat.bl <- function(x, ...){
    r <- n.unique(x)
    data.frame(Variable = di.Variable(x, ...),
               Summary = sprintf("{%s}", r),
               Summary.info = "Categorical variable: {unique values}")
}
attr(lcat.bl, "meta") <- c("Variable", "Summary.info")

##' @rdname baseline-standard
##' @details date.bl: min/max
##' @export
date.bl <- function(x, ...){
    r <- min_max(x)
    data.frame(Variable = di.Variable(x, ...),
               Summary = sprintf("%s/%s", r$Min, r$Max),
               Summary.info = "Date variables: min/max")
}
attr(date.bl, "meta") <- c("Variable", "Summary.info")

##' @rdname baseline-standard
##' @details surv.bl: events; rate
##' @export
surv.bl <- function(time, event, weight = NULL, time.unit = NULL, ...){
    r <- eventrate(time = time, event = event, weight = weight,
                   time.unit = time.unit, ...)
    data.frame(Variable = di.Variable(event, ...),
               Summary = sprintf("%s; %s",
                                 dform.num(r$Events),
                                 dform.num(r$Rate)),
               Summary.info = "Time-to-event variable: events; rate")
}
attr(surv.bl, "meta") <- c("Variable", "Summary.info")
