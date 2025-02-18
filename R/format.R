
dform.num <- function(x, digits = NULL, sc = NULL, sc.low = NULL, sc.high = NULL,
                      p = NULL, p.bound = NULL, NAtext = NULL){
    properties(x, class = c("numeric", "integer"), length = 1, na.ok = TRUE)
    ## if(identical(x, 0L)) return("0")
    if(isTRUE(x == 0)) return("0")
    ## check or get all parameters
    digits  <- dparam("digits", digits)
    sc      <- dparam("sc", sc)
    sc.low  <- dparam("sc.low", sc.low)
    sc.high <- dparam("sc.high", sc.high)
    p       <- dparam("p", p)
    p.bound <- dparam("p.bound", p.bound)
    NAtext  <- dparam("NAtext", NAtext)
    if(is.na(x)) return(NAtext)
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



if(FALSE){

    ## test
    dform.num(10.12345)
    dform.num(10.02345)
    dform.num(10L)

    dform.num(1.12345)
    dform.num(1.02345)
    dform.num(1.00245)
    dform.num(1L)

    dform.num(0.0012345)
    dform.num(0.0010045)
    dform.num(0L)

    dform.num(0.01235, p = TRUE)
    dform.num(0.00001235, p = TRUE)

    dform.num(987654321)
    dform.num(987654321L)

    dform.num(NA_real_)
    dform.num(NaN)

}
