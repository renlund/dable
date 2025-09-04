
dform.num <- function(x, digits = NULL, scientific = NULL,
                      p = NULL, p.bound = NULL, NAtext = NULL,
                      no04int = NULL){
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

dform.num.vec <- function(x, ...){
    n <- length(x)
    if(n == 0){
        character(0)
    } else {
        r <- rep(NA_character_, n)
        for(i in 1:n) r[i] <- dform.num(x[i], ...)
        r
    }
}

dprintf <- function(x, d, type) sprintf(paste0("%.", d, type), x)

dform.text <- function(x, output = NULL){
    if("Date" %in% class(x)) x <- as.character(x)
    properties(x, class = c("character", "factor"), length = 1, na.ok = TRUE)
    ## shorten too long strings with ... ?
    ## options for latexifying text ?
    ut <- dparam("output", output)
    if(ut == "latex"){
        x <- gsub(pattern = "(.*)([^\\\\])(%)(.*)",
                  replacement = "\\1\\2\\\\%\\4", x = x)
    }
    x
}

dform.text.vec <- function(x, ...){
    n <- length(x)
    if(n == 0){
        character(0)
    } else {
        r <- rep(NA_character_, n)
        for(i in 1:n) r[i] <- dform.text(x[i], ...)
    }
    r
}
