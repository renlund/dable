##' standard baseline functions 2
##'
##' Description functions that work together
##' @param x input vector
##' @param weight case weight
##' @param time time component of time-to-event variable
##' @param event event component of time-to-event variable
##' @param time.unit unit of time to present in
##' @param date.format the format for dates, default "%Y-%m-%d" (not
##'     automatically equal to package default)
##' @param ... arguments passed along
##' @name baseline-standard-2
NULL

##' @rdname baseline-standard-2
##' @details stats.bl2: (weighted) various stats
##' @export
real.bl2 <- function(x, weight = NULL, ...){
    stat <- c("Mean (SD)",
              "Median (Q1,Q3)",
              "Min, Max")
    Q <- quartile(x, weight)
    value <- c(sprintf("%s (%s)",
                       dafonumb1(d.mean(x, weight), NAtext = "N/A"),
                       dafonumb1(d.sd(x, weight), NAtext = "N/A")),
               sprintf("%s (%s, %s)",
                       dafonumb1(Q$Q2, NAtext = "N/A"),
                       dafonumb1(Q$Q1, NAtext = "N/A"),
                       dafonumb1(Q$Q3, NAtext = "N/A")),
               sprintf("%s, %s",
                       dafonumb1(d.min(x), NAtext = "N/A"),
                       dafonumb1(d.max(x), NAtext = "N/A")))
    has_missing <- di.dots("missing", strict = TRUE, ...)
    if(isTRUE(has_missing)){
        n <- length(x)
        m <- sum(is.na(x))
        stat <- c(stat, dpget("NAalias"))
        value <- c(value, sprintf("%s", m))
    }
    lab <- di.dots("label", ...)
    data.frame(Variable = var_lev_combine(lab, stat, sep = "; "),
               Summary = value)
}
attr(real.bl2, "meta") <- c("Variable")

##' @rdname baseline-standard-2
##' @details catg.bl2 / bnry.bl2: (weighted) count and percent
##' @export
catg.bl2 <- function(x, weight = NULL, ...){
    r <- catg.count_prop(x = x, weight = weight, ...)
    L <- levels(x)
    S <- sprintf("%s (%s%%)",
                 dafonumb(r$Count, NAtext = "N/A", no04int = TRUE),
                 dafonumb(100 * r$Proportion, NAtext = "N/A"))
    has_missing <- di.dots("missing", strict = TRUE, ...)
    if(isTRUE(has_missing)){
        L <- c(L, dpget("NAalias"))
        S <- c(S, sum(is.na(x)))
    }
    lab <- di.dots("label", ...)
    data.frame(Variable = var_lev_combine(paste0(lab, ", n (%)"), L),
               Summary = S)
}
attr(catg.bl2, "meta") <- c("Variable")
bnry.bl2 <- catg.bl2

##' @rdname baseline-standard-2
##' @details lcat.bl2: unique values
##' @export
lcat.bl2 <- function(x, ...){
    N <- n.unique(x)
    L <- "Unique"
    has_missing <- di.dots("missing", strict = TRUE, ...)
    if(isTRUE(has_missing)){
        N <- c(N, sum(is.na(x)))
        L <- c(L, dpget("NAalias"))
    }
    lab <- di.dots("label", ...)
    data.frame(Variable = var_lev_combine(lab, L, sep = "; "),
               Summary = N)
}
attr(lcat.bl2, "meta") <- c("Variable")

##' @rdname baseline-standard-2
##' @details date.bl2: min, max
##' @export
date.bl2 <- function(x, date.format = "%Y-%m-%d", ...){
    stat <- c("Min", "Max")
    ## value <- as.character(c(d.min(x), d.max(x)))
    mm <- date.min_max(x, date.format)
    value <- c(mm$min, mm$max)
    has_missing <- di.dots("missing", strict = TRUE, ...)
    if(isTRUE(has_missing)){
        stat <- c(stat, dpget("NAalias"))
        value <- c(value, sum(is.na(x)))
    }
    lab <- di.dots("label", ...)
    data.frame(Variable = var_lev_combine(lab, stat, sep = "; "),
               Summary = value)
}
attr(date.bl2, "meta") <- c("Variable")

##' @rdname baseline-standard-2
##' @details surv.bl2: Event, rate
##' @export
surv.bl2 <- function(time, event, weight = NULL, time.unit = NULL, ...){
    r <- eventrate(time = time, event = event, weight = weight,
                   time.unit = time.unit, ...)
    stat <- c("Events", "Rate")
    value <- c(dafonumb1(r$Events), dafonumb1(r$Rate))
    lab <- di.dots("label", ...)
    data.frame(Variable = var_lev_combine(lab, stat, sep = "; "),
               Summary = value)
}
attr(surv.bl2, "meta") <- c("Variable")
