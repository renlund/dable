##' peek at an object
##'
##' a printing method for interactive use
##' @param x object to take a peek at
##' @param right logical, passed to print
##' @param ... passed to print
##' @export
peek <- function(x, right = FALSE, ...){
    t <- attr(x, "type")
    if(t == "baseline"){
        ii <- unique(grep("info", x = names(x), value = TRUE))
        for(i in ii) x <- dable_prune(x, rm = i)
        print.data.frame(dable_format(x, output = "console"), right = right, ...)
    } else if(t %in% c("real", "catg", "bnry", "lcat", "surv", "date")){
        print.data.frame(x, right = right, ...)
    } else {
        print(x, ...)
    }
}
