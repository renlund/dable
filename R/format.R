dform.num <- function(x,
                      digits = c("small" = 2, "mid" = 2, "large" = 1),
                      sc = TRUE, sc.low = 1e-8, sc.high = 1e8,
                      p = FALSE, p.bound = 1e-4,
                      miss = ""){
    properties(x, class = c("numeric", "integer"), length = 1, na.ok = TRUE)
    if(identical(x, 0L)) return("0")
    if(is.na(x)) return(miss)
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

dformat.text <- function(x){
    properties(x, class = c("character", "factor"), length = 1, na.ok = TRUE)
    ## shorten too long strings with ... ?
    ## options for latexifying text ?
    x
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

}
