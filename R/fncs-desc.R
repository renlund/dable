empty <- function(...) NA
placeholder <- function(...) "placeholder"
attr(placeholder, "label") <- "Summary"
placeholder2 <- function(...) data.frame(p=NA, p.info="no test")

## --------------------------------------------------------------------- general
##' general describers
##'
##' functions to describe any variable
##' @param x input vector
##' @param weight case weight
##' @name desc-all
NULL

##' @rdname desc-all
##' @details d.n: length of non-missing x or sum of weights (if given)
##' @export
d.n <- function(x, weight = NULL, ...){
    if(is.null(weight)) length(x[!is.na(x)]) else sum(w[!is.na(x)])
}
attr(d.n, "label") <- "N"

##' @rdname desc-all
##' @details d.length: length of vector
##' @export
d.length <- function(x, ...){
    length(x)
}
attr(d.length, "label") <- "Length"

##' @rdname desc-all
##' @details d.sum: (weighted) sum of vector
##' @export
d.sum <- function(x, weight = NULL, ...){
    y <- if(is.null(weight)) x else x * weight
    sum(y, na.rm = TRUE)
}
attr(d.sum, "label") <- "Sum"

##' @rdname desc-all
##' @details n.unique: number of unique non-missing values in x
##' @export
n.unique <- function(x, ...) length(unique(x[!is.na(x)]))
attr(n.unique, "label") <- "Unique"

##' @rdname desc-all
##' @details n.NA: number of missing values in x
##' @export
n.NA <- function(x, ...) sum(is.na(x))
attr(n.NA, "label") <- "Missing"

##' @rdname desc-all
##' @details d.mode: (weighted) mode (most common value(s))
##' @export
d.mode <- function(x, weight = NULL, ...){
    if(is.null(weight)) weight <- rep(1L, length(x))
    count <- tapply(X = weight, INDEX = x, FUN = sum, na.rm = TRUE)
    paste0(names(which.max(count)), collapse = "; ")
}
attr(d.mode, "label") <- "Mode"


## ------------------------------------------------------------------------ real

##' 'real' describers
##'
##' functions to describe real variables
##' @param x input vector
##' @param weight case weight
##' @name desc-real
NULL

##' @rdname desc-real
##' @details d.min: minimum
##' @export
d.min <- function(x, ...){
    if(all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
}
attr(d.min, "label") <- "Min"

##' @rdname desc-real
##' @details d.max: maximum
##' @export
d.max <- function(x, ...){
    if(all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
}
attr(d.max, "label") <- "Max"

##' @rdname desc-real
##' @details min_max: minimum and maximum
##' @export
min_max <- function(x, ...){
    data.frame(Min = d.min(x),
               Max = d.max(x))
}

##' @rdname desc-real
##' @details d.mean: (weighted) mean value
##' @export
d.mean <- function(x, weight = NULL, ...){
    ## browser()
    if(is.null(weight)){
        mean(x, na.rm = TRUE)
    } else {
        stats::weighted.mean(x, w = weight, na.rm = TRUE)
    }
}
attr(d.mean, "label") <- "Mean"

##' @rdname desc-real
##' @details d.sd: (weighted) standard deviation
##' @export
d.sd <- function(x, weight = NULL, ...){
    if(is.null(weight)){
        sd(x, na.rm = TRUE)
    } else {
        y <- x[!is.na(x)]
        w <- weight[!is.na(x)]
        M <- sum(w)
        if(M == 1){
            NA_real_
        } else {
            sqrt( sum( w * (y - d.mean(y, w))^2 ) / (M - 1) )
        }
    }
}
attr(d.sd, "label") <- "SD"

##' @rdname desc-real
##' @details mean_sd: (weighted) mean and standard deviation
##' @export
mean_sd <- function(x, weight = NULL, ...){
    data.frame(Mean = d.mean(x, weight),
               SD = d.sd(x, weight))
}


if(FALSE){

    ## weighted quantiles
    ## https://aakinshin.net/posts/weighted-quantiles/

}

## ------------------------------------------------------------------------ bnry

##' 'bnry' describers
##'
##' functions to describe bnry variables
##' @param x input vector
##' @param weight case weight
##' @name desc-bnry
NULL

##' @rdname desc-bnry
##' @details bnry.count_prop: (weighted) count and proportion of non-reference
##' @export
bnry.count_prop <- function(x, weight = NULL, ...){
    ref <- levels(as.factor(x))[1]
    if(is.null(weight)) weight <- rep(1L, length(x))
    i <- !is.na(x)
    if(any(!i)){
        x <- x[i]
        weight <- weight[i]
    }
    count <- sum(weight[x != ref])
    count[is.na(count)] <- 0 ## bad soution?
    W <- sum(weight)
    data.frame(
        Level = ref,
        Count = count,
        Proportion = if(W != 0) count / W else NA_real_
    )
}
attr(bnry.count_prop, "meta") <- c("Level")


## ------------------------------------------------------------------------ catg

##' 'catg' describers
##'
##' functions to describe catg variables
##' @param x input vector
##' @param weight case weight
##' @name desc-catg
NULL

##' @rdname desc-catg
##' @details catg.count_prop: (weighted) count and proportion of categorical
##'     levels
##' @export
catg.count_prop <- function(x, weight = NULL, ...){
    if(is.null(weight)) weight <- rep(1L, length(x))
    count <- tapply(X = weight, INDEX = x, FUN = sum, na.rm = TRUE)
    count[is.na(count)] <- 0 ## bad solution?
    W <- sum(weight[!is.na(x)])
    data.frame(Level = attr(count, "dimnames")[[1]],
               Count = as.numeric(count),
               Proportion = if(W != 0) count / W else NA_real_)
}
attr(catg.count_prop, "meta") <- "Level" ## XK ?



## ----------------------------------------------------------------------- lcat

##' 'lcat' describers
##'
##' functions to describe lcat variables
##' @param x input vector
##' @param weight case weight
##' @name desc-lcat
NULL


## ------------------------------------------------------------------------ date

##' 'date' describers
##'
##' functions to describe date variables
##' @param x input vector
##' @param weight case weight
##' @name desc-date
NULL


## ------------------------------------------------------------------------ surv

##' 'surv' describers
##'
##' functions to describe surv variables
##' @param time time component of time-to-event variable
##' @param event event component of time-to-event variable
##' @param weight case weight
##' @name desc-surv
NULL

##' @rdname desc-surv
##' @details eventrate: (weighted) time, events and rate thereof
##' @param time.unit numeric; if provided 'time' will be divided by this number
##' @export
eventrate <- function(time, event, weight = NULL, time.unit = NULL, ...){
    if(!is.null(time.unit)) time <- time / time.unit
    r <- data.frame(Time = d.sum(time, weight = weight),
                    Events = d.sum(event, weight = weight))
    r$Rate <- with(r, expr = Events / Time)
    r
}
