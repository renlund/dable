.stddiff <- function() "Std. diff."

## ------------------------------------------------------------------------ real

##' 'real' comparers
##'
##' functions to compare real variables
##' @param x input vector
##' @param weight case weight
##' @param g grouping variable
##' @param ... arguments passed
##' @name comp-real
NULL

##' @rdname comp-real
##' @description real.std: (weighted) standardized difference for 'real'
##' @export
real.std <- function(x, g, weight = NULL, ...){
    if(!is.factor(g)) g <- factor(g)
    x_i <- g == levels(g)[1]
    y_i <- g == levels(g)[2]
    ( d.mean(x = x[x_i], weight = weight[x_i]) -
      d.mean(x = x[y_i], weight = weight[y_i]) ) /
        sqrt( (d.sd(x = x[x_i], weight = weight[x_i])^2 +
               d.sd(x = x[y_i], weight = weight[y_i])^2  ) / 2 )
}
attr(real.std, "label") <- .stddiff()

##' @rdname comp-real
##' @description mean_diff: (weighted) difference in mean value
##' @export
mean_diff <- function(x, g, weight = NULL, ...){
    if(!is.factor(g)) g <- factor(g)
    x_i <- g == levels(g)[1]
    y_i <- g == levels(g)[2]
    d.mean(x = x[x_i], weight = weight[x_i]) -
        d.mean(x = x[y_i], weight = weight[y_i])
}
attr(mean_diff, "label") <- .stddiff()

##' @rdname comp-real
##' @description no.std: no standardized difference calculated
##' @export
no.std <- function(x, g, weight = NULL, ...){
    NA_real_
}
attr(no.std, "label") <- .stddiff()

## ------------------------------------------------------------------------ catg

##' 'catg' comparers
##'
##' functions to compare catg variables
##' @param x input vector
##' @param g grouping variable
##' @param weight case weight
##' @param catg.full.length logical; return dimension equal to the input (is
##'     this EVER wanted?)
##' @param ... arguments passed
##' @importFrom MASS ginv
##' @name comp-catg
NULL

##' @rdname comp-catg
##' @description catg.std: (weighted) standardized difference for 'catg'
##' @export
catg.std <- function(x, g, weight = NULL, catg.full.length = FALSE, ...){
    if(!is.factor(g)) g <- factor(g)
    x_i <- g == levels(g)[1]
    y_i <- g == levels(g)[2]
    p1 <- catg.count_prop(x = x[x_i], weight = weight[x_i])[["Proportion"]]
    p2 <- catg.count_prop(x = x[y_i], weight = weight[y_i])[["Proportion"]]
    k <- length(p1)
    if(k == 1) return(NA_real_)
    S <- matrix(NA, nrow = k, ncol = k)
    for(i in 1:k){
        for(j in 1:k){
            if(i == j){
                S[i, j] <- (p1[i]*(1-p1[i]) + p2[i]*(1-p2[i])) / 2
            } else {
                S[i, j] <- -(p1[i]*p1[j] + p2[i]*p2[j]) / 2
            }
        }
    }
    STDD <- if(all(S == 0)){
             Inf
         } else if(is.null(
                    tryCatch(INV <- MASS::ginv(S), ##solve(S)
                             error = function(e){
                                 NULL
                             })
                )){
             Inf
         } else{
             sqrt(t(p1-p2) %*% INV %*% (p1-p2))
         }
    if(catg.full.length) c(STDD, rep(NA, max(k-1,0))) else as.numeric(STDD)
}
attr(catg.std, "label") <- .stddiff()

##' @rdname comp-catg
##' @description prop_diff: (weighted) difference in proportion
##' @export
prop_diff <- function(x, g, weight = NULL, catg.full.length = FALSE, ...){
    if(!is.factor(g)) g <- factor(g)
    x_i <- g == levels(g)[1]
    y_i <- g == levels(g)[2]
    p1 <- catg.count_prop(x = x[x_i], weight = weight[x_i])[["Proportion"]]
    p2 <- catg.count_prop(x = x[y_i], weight = weight[y_i])[["Proportion"]]
    p1 - p2
}
attr(prop_diff, "label") <- "Difference in proportion"


## ------------------------------------------------------------------------ bnry

##' 'bnry' comparers
##'
##' functions to compare bnry variables
##' @param x input vector
##' @param weight case weight
##' @param g grouping variable
##' @param ... arguments passed
##' @name comp-bnry
NULL

##' @rdname comp-bnry
##' @description bnry.std: (weighted) standardized difference for 'bnry'
##' @export
bnry.std <- function(x, g, weight = NULL, ...){
    if(!is.factor(g)) g <- factor(g)
    x_i <- g == levels(g)[1]
    y_i <- g == levels(g)[2]
    p1 <- bnry.count_prop(x = x[x_i], weight = weight[x_i])[["Proportion"]]
    p2 <- bnry.count_prop(x = x[y_i], weight = weight[y_i])[["Proportion"]]
    (p1 - p2) / sqrt((p1*(1-p1) + p2*(1-p2)) / 2)
}
attr(bnry.std, "label") <- .stddiff()

##' @rdname comp-bnry
##' @description risk_diff: (weighted) risk difference
##' @export
risk_diff <- function(x, g, weight = NULL, ...){
    if(!is.factor(g)) g <- factor(g)
    x_i <- g == levels(g)[1]
    y_i <- g == levels(g)[2]
    p1 <- bnry.count_prop(x = x[x_i], weight = weight[x_i])[["Proportion"]]
    p2 <- bnry.count_prop(x = x[y_i], weight = weight[y_i])[["Proportion"]]
    p1 - p2
}
attr(risk_diff, "label") <- "Risk difference"


##' @rdname comp-bnry
##' @description risk_ratio: (weighted) risk ratio
##' @export
risk_ratio <- function(x, g, weight = NULL, ...){
    if(!is.factor(g)) g <- factor(g)
    x_i <- g == levels(g)[1]
    y_i <- g == levels(g)[2]
    p1 <- bnry.count_prop(x = x[x_i], weight = weight[x_i])[["Proportion"]]
    p2 <- bnry.count_prop(x = x[y_i], weight = weight[y_i])[["Proportion"]]
    p1 / p2
}
attr(risk_ratio, "label") <- "Risk ratio"

##' @rdname comp-bnry
##' @description odds_ratio: (weighted) odds ratio
##' @export
odds_ratio <- function(x, g, weight = NULL, ...){
    if(!is.factor(g)) g <- factor(g)
    x_i <- g == levels(g)[1]
    y_i <- g == levels(g)[2]
    p1 <- bnry.count_prop(x = x[x_i], weight = weight[x_i])[["Proportion"]]
    p2 <- bnry.count_prop(x = x[y_i], weight = weight[y_i])[["Proportion"]]
    (p1 / (1-p1) ) / (p2 / (1-p2))
}
attr(odds_ratio, "label") <- "Odds ratio"


## ------------------------------------------------------------------------ date

##' 'date' comparers
##'
##' functions to compare date variables
##' @param x input vector
##' @param weight case weight
##' @param g grouping variable
##' @param ... arguments passed
##' @name comp-date
NULL

##' @rdname comp-date
##' @description date.std: (weighted) standardized difference for 'date' ( = real.std
##'     applied to x interpreted as an integer)
##' @export
date.std <- function(x, g, weight = NULL, ...){
    real.std(x = as.integer(x), g = g, weight = weight)
}
attr(date.std, "label") <- .stddiff()

## ------------------------------------------------------------------------ surv

##' 'surv' comparers
##'
##' functions to compare surv variables
##' @param time time component of time-to-event variable
##' @param event event component of time-to-event variable
##' @param weight case weight
##' @param g grouping variable
##' @param ... arguments passed
##' @name comp-surv
NULL

##' @rdname comp-surv
##' @description surv.std: (weighted) standardized difference for
##' 'surv'
##' @export
surv.std <- function(time, event, g, weight = NULL, ...){
    if(!is.factor(g)) g <- factor(g)
    x_i <- g == levels(g)[1]
    y_i <- g == levels(g)[2]
    n1 <- d.sum(x = event[x_i], weight = weight[x_i])
    n2 <- d.sum(x = event[y_i], weight = weight[y_i])
    t1 <- d.sum(x = time[x_i], weight = weight[x_i])
    t2 <- d.sum(x = time[y_i], weight = weight[y_i])
    (n1 / t1 - n2 / t2) / sqrt((n1 / t1 + n2 / t2) / 2)
}
attr(surv.std, "label") <- .stddiff()

##' @rdname comp-surv
##' @details rate_ratio: ratio of the (weighted) rates
##' @export
rate_ratio <- function(time, event, g, weight = NULL, ...){
    if(!is.factor(g)) g <- factor(g)
    x_i <- g == levels(g)[1]
    y_i <- g == levels(g)[2]
    e1 <- eventrate(time = time[x_i], event = event[x_i], weight = weight[x_i])
    e2 <- eventrate(time = time[y_i], event = event[y_i], weight = weight[y_i])
    e1$Rate / e2$Rate
}
attr(rate_ratio, "label") <- "Rate ratio"

##' @rdname comp-surv
##' @details hazard_ratio: (weighted) hazard ratio
##' @export
hazard_ratio <- function(time, event, g, weight = NULL, ...){
    if(!is.factor(g)) g <- factor(g)
    mod <- survival::coxph(survival::Surv(time, event) ~ g, weight = weight)
    as.numeric(exp(mod$coefficients)[1])
}
attr(hazard_ratio, "label") <- "Hazard ratio"

##' @rdname comp-surv
##' @details hr_ci:  (weighted) hazard ratio and confidence interval
##' @export
hr_ci <- function(time, event, g, weight = NULL, ...){
    if(!is.factor(g)) g <- factor(g)
    mod <- survival::coxph(survival::Surv(time, event) ~ g, weight = weight)
    v <- exp(as.numeric(c(mod$coefficients[1], stats::confint(mod))))
    sprintf("%s (%s-%s)", dform.num.vec(v))
}
attr(hr_ci, "label") <- "Hazard ratio"
