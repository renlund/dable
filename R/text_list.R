##' collapse vector to a text list
##'
##' This function will collapse a vector c("A", "B", "C") to a text list,
##' e.g. "A, B, C" or "A, B, and C" depending on the arguments.
##' @param x vector; the text elements you want to collapse to a list
##' @param and logical; should there be and 'and' between the penultimate and
##'     last entry of x when collapsed into a list?
##' @param set logical; wrap result in '{}'?
##' @param collapse character, passed to paste
##' @return character (length 1)
##' @examples
##' x <- LETTERS[1:3]
##' text_list(x, and = TRUE, set = TRUE)
##' text_list(x, and = TRUE, set = FALSE)
##' text_list(x, and = FALSE, set = TRUE)
##' text_list(x, and = FALSE, set = FALSE)
##' @export
text_list <- function(x, and = TRUE, set = FALSE, collapse = ", "){
    properties(and, class = "logical", length = 1, na.ok = FALSE)
    properties(set, class = "logical", length = 1, na.ok = FALSE)
    properties(collapse, class = "character", length = 1, na.ok = FALSE)
    n <- length(x)
    if(n <= 1){
        if(set) paste0("{", x, "}") else x
    } else {
        if(and){
            y <- rep(c(collapse, ""), c(n-1, 1))
            y[n-1] <- paste0(y[n-1], "and ")
            z <- paste0(x, y)
            r <- paste0(z, collapse = "")
        } else {
            r <- paste0(x, collapse = collapse)
        }
        if(set){
            paste0("{", r, "}")
        } else r
    }
}
