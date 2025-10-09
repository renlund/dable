##' dable formatting functions
##'
##' What can we say about formatting? (XK add details)
##' @param x input
##' @param digits number of digits presented after formatting? Vector of length
##'     three to give number of digits for small (absolute value smaller than
##'     1), medium (absolute value between 1 and 10) and large (absolute value
##'     larger than 10) numbers. Default: c("small" = 2, "mid" = 2, "large" = 1)
##' @param scientific if to use scientific number representation, and what the
##'     cut offs should be, default: list(use = TRUE, low = 1e-8, high = 1e8)
##' @param p are numbers p-value? default: FALSE
##' @param p.bound cut off for p-value presentation
##' @param NAtext how to show NA in tables
##' @param no04int logical; should an integer value be printed without zeros?
##'     (XK add details as to why this is even a thing)
##' @param output what is the output? e.g. "latex"
##' @param substitute the table of substitutions
##' @param ... arguments passed
##' @name dable-formatting
NULL

##' @rdname dable-formatting
##' @details dafonumb: format a numeric/integer vector
##' @export
dafonumb <- function(x, ...){
    n <- length(x)
    if(n == 0){
        character(0)
    } else {
        r <- rep(NA_character_, n)
        for(i in 1:n) r[i] <- dafonumb1(x[i], ...)
        r
    }
}

##' @rdname dable-formatting
##' @details dafonumb1: format a single numeric/integer (helper function for dafonumb)
##' @export
dafonumb1 <- function(x, digits = NULL, scientific = NULL,
                      p = NULL, p.bound = NULL, NAtext = NULL,
                      no04int = NULL, ...){
    properties(x, class = c("numeric", "integer"), length = 1, na.ok = TRUE)
    if(is.null(no04int)) no04int <- FALSE
    properties(no04int, class = "logical", length = 1, na.ok = FALSE)
    if(no04int & as.integer(x) == x) return(sprintf("%.0f", x))
    ## check or get all parameters
    digits  <- dparam("digits", digits)
    scientific <- dparam("scientific", scientific)
    sc <- scientific$use
    sc.low <- scientific$low
    sc.high <- scientific$high
    p       <- dparam("p", p)
    p.bound <- dparam("p.bound", p.bound)
    NAtext  <- dparam("NAtext", NAtext)
    if(is.na(x)) return(NAtext)
    if(isTRUE(x == 0) && !p) return("0")
    dig.s <- digits["small"]
    dig.m <- digits["mid"]
    dig.l <- digits["large"]
    if(p){
        if(x < 0 || x > 1){
            warning("asked to format a value < 0 or > 1 as a p-value")
        } else {
            return(
                if(x >= p.bound){
                    format(x, digits = dig.s, scientific = FALSE)
                } else {
                    paste0("<", format(p.bound, digits = dig.s, scientific = FALSE))
                }
            )
        }
    }
    ax <- abs(x)
    size <- if(ax < 1) "small" else if(ax < 10) "mid" else "large"
    sc.cand <- ax <= sc.low || ax >= sc.high  ##& !identical(x, 0L)
    SC <- sc && sc.cand
    if(SC){
        format(as.numeric(x), digits = dig.s, scientific = TRUE)
    } else if(is.integer(x)){
        as.character(x)
    } else if(size == "small"){
        formatC(x, digits = dig.s, format = "fg", flag = "#")
    } else if(size == "mid"){
        dprintf(x, dig.m, "f")
    } else if(size == "large"){
        dprintf(x, dig.l, "f")
    } else{
        stop("this is an error message no-one should ever see")
    }
}

dprintf <- function(x, d, type) sprintf(paste0("%.", d, type), x)

##' @rdname dable-formatting
##' @details dafotext: format a vector of strings
##' @export
dafotext <- function(x, ...){
    n <- length(x)
    if(n == 0){
        character(0)
    } else {
        r <- rep(NA_character_, n)
        for(i in 1:n) r[i] <- dafotext1(x[i], ...)
    }
    r
}

##' @rdname dable-formatting
##' @details dafotext1: format a single string (helper function for dafotext)
##' @export
dafotext1 <- function(x, output = dpget("output"),
                       substitute = dpget("substitute"), ...){
    if("Date" %in% class(x)) x <- as.character(x)
    properties(x, class = c("character", "factor"), length = 1, na.ok = TRUE)
    ## shorten too long strings with ... ?
    n <- nrow(substitute)
    if(n > 0){
        for(i in 1:n){
            if(substitute$out[i] != output) next
            x <- gsub(pattern = substitute$pat[i],
                      replacement = substitute$rep[i],
                      x = x,
                      fixed = substitute$fix[i])
        }
    }
    x
}
