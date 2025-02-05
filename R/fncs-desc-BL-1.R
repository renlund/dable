##' Set baseline functions
##'
##' Quickly set baseline functions to some predefined set of functions.
##' @param desc the 'theme' for descriptives
##' @param comp the 'theme' for comparisons (not yet implemented)
##' @param test the 'theme' for testers (not yet implemented)
##' @export
bl_theme <- function(desc = "default", comp = NULL, test = NULL){
    param <- dpget_all()
    properties(desc, class = c("character", "numeric"), length = 1, na.ok = FALSE)
    if(!is.null(comp) || !is.null(test)){
        s <- paste0("options for 'comp' and 'test' not yet implemented")
        warning(s)
    }
    types <- c("real", "bnry", "catg", "lcat", "date", "surv")
    d <- if(is.character(desc)){
             switch(desc,
                    "default" = 0)
         } else desc
    x <- sprintf(paste0("%s.bl", d), types)
    p <- sprintf("%s.desc.bl", types)
    for(i in seq_along(x)) dpset(param = p[i], value = x[i])
    invisible(param)
}

##' default baseline functions (0-1)
##'
##' Description functions that work together. Function sets 0 and 1 are
##' identical except for type real; in set 0 description is by median (Q1-Q3)
##' and in set 1 by mean (standard deviation).
##' @param x input vector
##' @param weight case weight
##' @param ... arguments passed along
##' @name baseline-standard
NULL

NA_desc <- function(na) paste0("[", na,"]")
NA_desc_append <- function(s, na = 0){
    n <- length(s)
    x <- rep(c(NA_desc(na), ""), c(1,n-1))
    if(na == 0) s else paste(s, x)
}
NA_info <- function() "[n] is missing count (if applicable)"
NA_info_append <- function(s, na){
    ## if(na == 0) s else paste0(s, ". ", NA_info())
    ## no, need to add this always, else meta variables don't match (causes
    ## problems in merge2parts)
    paste0(s, ". ", NA_info())
}


##' @rdname baseline-standard
##' @details real.bl0: (weighted) median and interquartile range
##' @export
real.bl0 <- function(x, weight = NULL, ...){
    q <- quartile(x = x, weight = weight)
    ## data.frame(Variable = di.Variable(x, ...),
    ##            Summary = sprintf("%s (%s - %s)", dform.num(q$Q2),
    ##                              dform.num(q$Q1), dform.num(q$Q2)),
    ##            Summary.info = "Numeric variable: Median (Q1-Q3)")
    n.na <- sum(is.na(x))
    s <- sprintf("%s (%s - %s)", dform.num(q$Q2),
                 dform.num(q$Q1), dform.num(q$Q2))
    si <- "Numeric variable: Median (Q1-Q3)"
    data.frame(Variable = di.Variable(x, ...),
               Summary = NA_desc_append(s, n.na),
               Summary.info = NA_info_append(si, n.na))

}
attr(real.bl0, "meta") <- c("Variable", "Summary.info")

##' @rdname baseline-standard
##' @details real.bl1: (weighted) mean and standard deviation
##' @export
real.bl1 <- function(x, weight = NULL, ...){
    r <- mean_sd(x = x, weight = weight, ...)
    ## data.frame(Variable = di.Variable(x, ...),
    ##            Summary = sprintf("%s (%s)", dform.num(r$Mean), dform.num(r$SD)),
    ##            Summary.info = "Numeric variable: Mean(SD)")
    n.na <- sum(is.na(x))
    s <- sprintf("%s (%s)", dform.num(r$Mean), dform.num(r$SD))
    si <- "Numeric variable: Mean (SD)"
    data.frame(Variable = di.Variable(x, ...),
               Summary = NA_desc_append(s, n.na),
               Summary.info = NA_info_append(si, n.na))
}
attr(real.bl1, "meta") <- c("Summary.info", "Variable")

##' @rdname baseline-standard
##' @details bnry.bl0/1: (weighted) count and percent of
##'     non-reference value as single string
##' @export
bnry.bl0 <- function(x, weight = NULL, ...){
    r <- bnry.count_prop(x = x, weight = weight, ...)
    ## data.frame(Variable = di.Variable(x, ...),
    ##            Summary = sprintf("%s (%s%%)",
    ##                              dform.num(r$Count),
    ##                              dform.num(100 * r$Proportion)),
    ##            Summary.info = "Categorical variable: Count (Percent)")
    n.na <- sum(is.na(x))
    s <- sprintf("%s (%s%%)",
                 dform.num(r$Count),
                 dform.num(100 * r$Proportion))
    si <- "Categorical variable: Count (Percent)"
    data.frame(Variable = di.Variable(x, ...),
               Summary = NA_desc_append(s, n.na),
               Summary.info = NA_info_append(si, n.na))
}
attr(bnry.bl0, "meta") <- c("Variable", "Summary.info")
bnry.bl1 <- bnry.bl0

##' @rdname baseline-standard
##' @details catg.bl0/1: (weighted) count and proportion of
##'     categorical levels as single string
##' @export
catg.bl0 <- function(x, weight = NULL, ...){
    r <- catg.count_prop(x = x, weight = weight, ...)
    ## data.frame(Variable = di.Variable(x, ...),
    ##            Summary = sprintf("%s (%s%%)",
    ##                              dform.num.vec(r$Count),
    ##                              dform.num.vec(100 * r$Proportion)),
    ##            Summary.info = "Categorical variable: Count (Percent)")
    n.na <- sum(is.na(x))
    s <- sprintf("%s (%s%%)",
                 dform.num.vec(r$Count),
                 dform.num.vec(100 * r$Proportion))
    si <- "Categorical variable: Count (Percent)"
    data.frame(Variable = di.Variable(x, ...),
               Summary = NA_desc_append(s, n.na),
               Summary.info = NA_info_append(si, n.na))
}
attr(catg.bl0, "meta") <- c("Variable", "Summary.info")
catg.bl1 <- catg.bl0

##' @rdname baseline-standard
##' @details lcat.bl0/1: number of unique non-missing values in x
##' @export
lcat.bl0 <- function(x, ...){
    r <- n.unique(x)
    ## data.frame(Variable = di.Variable(x, ...),
    ##            Summary = sprintf("{%s}", r),
    ##            Summary.info = "Categorical variable: {unique values}")
    n.na <- sum(is.na(x))
    s <- sprintf("{%s}", r)
    si <- "Categorical variable: {unique values}"
    data.frame(Variable = di.Variable(x, ...),
               Summary = NA_desc_append(s, n.na),
               Summary.info = NA_info_append(si, n.na))
}
attr(lcat.bl0, "meta") <- c("Variable", "Summary.info")
lcat.bl1 <- lcat.bl0

##' @rdname baseline-standard
##' @details date.bl0/1: min/max
##' @export
date.bl0 <- function(x, ...){
    r <- min_max(x)
    ## data.frame(Variable = di.Variable(x, ...),
    ##            Summary = sprintf("%s/%s", r$Min, r$Max),
    ##            Summary.info = "Date variables: min/max")
    n.na <- sum(is.na(x))
    s <- sprintf("%s/%s", r$Min, r$Max)
    si <- "Date variables: min/max"
    data.frame(Variable = di.Variable(x, ...),
               Summary = NA_desc_append(s, n.na),
               Summary.info = NA_info_append(si, n.na))
}
attr(date.bl0, "meta") <- c("Variable", "Summary.info")
date.bl1 <- date.bl0

##' @rdname baseline-standard
##' @details surv.bl0/1: events; rate
##' @export
surv.bl0 <- function(time, event, weight = NULL, time.unit = NULL, ...){
    r <- eventrate(time = time, event = event, weight = weight,
                   time.unit = time.unit, ...)
    ## data.frame(Variable = di.Variable(event, ...),
    ##            Summary = sprintf("%s; %s",
    ##                              dform.num(r$Events),
    ##                              dform.num(r$Rate)),
    ##            Summary.info = "Time-to-event variable: events; rate")
    n.na <- sum(is.na(time) | is.na(event))
    s <- sprintf("%s; %s",
                 dform.num(r$Events),
                 dform.num(r$Rate))
    si <- "Time-to-event variable: events; rate"
    data.frame(Variable = di.Variable(x, ...),
               Summary = NA_desc_append(s, n.na),
               Summary.info = NA_info_append(si, n.na))
}
attr(surv.bl0, "meta") <- c("Variable", "Summary.info")
surv.bl1 <- surv.bl0
