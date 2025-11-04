##' set baseline functions
##'
##' Quickly set baseline functions to some predefined set of functions.
##' @param desc the 'theme' for descriptives (0,1,2)
##' @param comp the 'theme' for comparisons (std, diff)
##' @param test the 'theme' for testers (not yet implemented)
##' @export
bl_theme <- function(desc = 0, comp = "std", test = NULL){
    param <- dpget_all()
    if(!is.null(desc)){
        properties(desc, class = c("character", "numeric"),
                   length = 1, na.ok = FALSE)
        one_of(as.character(desc), nm = "'desc'",
               set = c("0", "default", "1", "2"))
    }
    if(!is.null(comp)){
        properties(comp, class = c("character"), length = 1, na.ok = FALSE)
        one_of(comp, nm = "'comp'", set = c("std", "diff"))
    }
    if(!is.null(test)){
        s <- paste0("options for 'test' not yet implemented")
        warning(s)
    }
    types <- c("real", "bnry", "catg", "lcat", "date", "surv")
    if(!is.null(desc)){
        d <- if(is.character(desc)){
                 switch(desc,
                        "default" = 0)
             } else desc
        x <- sprintf(paste0("%s.bl", d), types)
        p <- sprintf("%s.desc.bl", types)
        for(i in seq_along(x)) dpset(param = p[i], value = x[i])
    }
    if(!is.null(comp)){
        x <- sprintf(paste0("%s.", comp, ".bl"), types)
        p <- sprintf("%s.comp.bl", types)
        for(i in seq_along(x)) dpset(param = p[i], value = x[i])

    }
    invisible(param)
}

##' default baseline functions (0-1)
##'
##' Description functions that work together. Function sets 0 and 1 are
##' identical except for type real; in set 0 description is by median (Q1-Q3)
##' and in set 1 by mean (standard deviation).
##' @param x input vector
##' @param weight case weight
##' @param time time component of time-to-event variable
##' @param event event component of time-to-event variable
##' @param time.unit unit of time to present in
##' @param date.format the format of dates
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
    paste0(s, ". ", NA_info())
}

NAsign <- "-"
changeNA <- function(x){
    if(is.na(x) || is.null(x)){
       NAsign
    } else x
}

##' @rdname baseline-standard
##' @details real.bl0: (weighted) median and interquartile range
##' @export
real.bl0 <- function(x, weight = NULL, ...){
    q <- quartile(x = x, weight = weight)
    n.na <- sum(is.na(x))
    s <- if(is.na(q$Q2)){
             NAsign
         } else {
             sprintf("%s (%s - %s)", dafonumb1(q$Q2),
                     dafonumb1(q$Q1), dafonumb1(q$Q2))
         }
    si <- "Numeric variable: Median (Q1-Q3)"
    data.frame(Variable = di.Variable(x, ...),
               desc = NA_desc_append(s, n.na),
               desc.info = NA_info_append(si, n.na))

}
attr(real.bl0, "meta") <- c("Variable", "desc.info")

##' @rdname baseline-standard
##' @details real.bl1: (weighted) mean and standard deviation
##' @export
real.bl1 <- function(x, weight = NULL, ...){
    r <- mean_sd(x = x, weight = weight, ...)
    n.na <- sum(is.na(x))
    s <- if(is.na(r$Mean)){
             NAsign
         } else {
             sprintf("%s (%s)", dafonumb1(r$Mean), dafonumb1(r$SD))
         }
    si <- "Numeric variable: Mean (SD)"
    data.frame(Variable = di.Variable(x, ...),
               desc = NA_desc_append(s, n.na),
               desc.info = NA_info_append(si, n.na))
}
attr(real.bl1, "meta") <- c("desc.info", "Variable")

##' @rdname baseline-standard
##' @details bnry.bl0/1: (weighted) count and percent of
##'     non-reference value as single string
##' @export
bnry.bl0 <- function(x, weight = NULL, ...){
    r <- bnry.count_prop(x = x, weight = weight, ...)
    n.na <- sum(is.na(x))
    s <- if(is.na(r$Proportion)){
             NAsign
         } else {
             sprintf("%s (%s%%)",
                     dafonumb1(r$Count, no04int = TRUE),
                     dafonumb1(100 * r$Proportion))
         }
    si <- "Categorical variable: Count (Percent)"
    data.frame(Variable = di.Variable(x, ...),
               desc = NA_desc_append(s, n.na),
               desc.info = NA_info_append(si, n.na))
}
attr(bnry.bl0, "meta") <- c("Variable", "desc.info")
bnry.bl1 <- bnry.bl0

##' @rdname baseline-standard
##' @details catg.bl0/1: (weighted) count and proportion of
##'     categorical levels as single string
##' @export
catg.bl0 <- function(x, weight = NULL, ...){
    r <- catg.count_prop(x = x, weight = weight, ...)
    n.na <- sum(is.na(x))
    s <- if(all(is.na(r$Proportion))){
             rep(NAsign, nrow(r))
         } else {
             sprintf("%s (%s%%)",
                     dafonumb(r$Count, no04int = TRUE),
                     dafonumb(100 * r$Proportion))
         }
    si <- "Categorical variable: Count (Percent)"
    data.frame(Variable = di.Variable(x, ...),
               desc = NA_desc_append(s, n.na),
               desc.info = NA_info_append(si, n.na))
}
attr(catg.bl0, "meta") <- c("Variable", "desc.info")
catg.bl1 <- catg.bl0

##' @rdname baseline-standard
##' @details lcat.bl0/1: number of unique non-missing values in x
##' @export
lcat.bl0 <- function(x, ...){
    r <- n.unique(x)
    n.na <- sum(is.na(x))
    s <- if(is.na(r) || is.null(r) || length(r) == 0) "" else sprintf("=%s=", r)
    si <- "Categorical variable: =unique values="
    data.frame(Variable = di.Variable(x, ...),
               desc = NA_desc_append(s, n.na),
               desc.info = NA_info_append(si, n.na))
}
attr(lcat.bl0, "meta") <- c("Variable", "desc.info")
lcat.bl1 <- lcat.bl0

##' @rdname baseline-standard
##' @details date.bl0/1: min/max
##' @export
date.bl0 <- function(x, date.format = dpget("date.format"), ...){
    properties(date.format, class = c("character"), length = 1,
               na.ok = FALSE)
    r <- min_max(x)
    n.na <- sum(is.na(x))
    s <- if(is.na(r$Min)){
        NAsign
    } else {
        sprintf("%s/%s",
                changeNA(format(r$Min, format = date.format)),
                changeNA(format(r$Max, format = date.format)))
    }
    si <- "Date variables: min/max"
    data.frame(Variable = di.Variable(x, ...),
               desc = NA_desc_append(s, n.na),
               desc.info = NA_info_append(si, n.na))
}
attr(date.bl0, "meta") <- c("Variable", "desc.info")
date.bl1 <- date.bl0

##' @rdname baseline-standard
##' @details surv.bl0/1: events; rate
##' @export
surv.bl0 <- function(time, event, weight = NULL, time.unit = NULL, ...){
    r <- eventrate(time = time, event = event, weight = weight,
                   time.unit = time.unit, ...)
    n.na <- sum(is.na(time) | is.na(event))
    s <- sprintf("%s; %s",
                 changeNA(dafonumb1(r$Events)),
                 changeNA(dafonumb1(r$Rate)))
    si <- "Time-to-event variable: events; rate"
    data.frame(Variable = di.Variable(event, ...),
               desc = NA_desc_append(s, n.na),
               desc.info = NA_info_append(si, n.na))
}
attr(surv.bl0, "meta") <- c("Variable", "desc.info")
surv.bl1 <- surv.bl0
