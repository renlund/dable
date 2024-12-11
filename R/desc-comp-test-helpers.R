## this file should be called describe-compare-test-helpers.R

default.gtab.group.name <- function() options("dable.gtab.group.name")[[1]]

## get correct names in describer, contraster, etc
labeller <- function(lab, nm, n){
    l <- if(!is.null(lab)) lab else if(!is.null(nm)) nm else sprintf("V%s", 1:n)
    properties(l, class = "character", length = c(1,n))
    if(length(l) < n){
        warning("label length mismatch with function output")
        rep(l, n)
    } else l
}

## get correct 'part' attribute in describer, contraster, etc.
parter <- function(part, append, nm, meta){
    n <- length(nm)
    r <- rep(part, n)
    if(!is.null(meta)){
        i <- which(nm %in% meta)
        if(length(i) > 0) r[i] <- "meta"
    }
    ifelse(r %in% c("desc", "comp", "test"),
           yes = paste0(r, ":", append),
           no = r)
}

merge_2parts <- function(x = NULL, y = NULL){
    if(is.null(x)) return(y)
    if(is.null(y)) return(x)
    ix <- which(attr(x, "part") == "meta")
    nx <- names(x)[ix]
    iy <- which(attr(y, "part") == "meta")
    ny <- names(y)[iy]
    same <- intersect(nx, ny)

    foo <- function(x,y) paste0(x, "_/_",y)
    x_same <- Reduce(foo, x = x[, same, drop = FALSE], accumulate = FALSE)
    x_ref <- tapply(x, INDEX = x_same, FUN = nrow)
    y_same <- Reduce(foo, x = y[, same, drop = FALSE], accumulate = FALSE)
    y_ref <- tapply(y, INDEX = y_same, FUN = nrow)
    if( any(names(x_ref) != names(y_ref)) ){
        s <- paste0("[merge_2parts:] trying to merge two parts with ",
                    "non-matching meta variables. Review the functions ",
                    "used to make sure they match.")
        stop(s)
    }
    part <- c(attr(x, "part"), attr(y, "part"))
    if(all(x_ref == y_ref)){
        r <- cbind(x, y)
    } else {
        if( !all(x_ref==1) & !all(y_ref==1) ){
            s <- paste0("[merge_2parts:] trying to merge two parts where ",
                        "the rle of the meta variables are unequal. This is ",
                        "only permitted if one of them has rle:s of ones.")
            stop(s)
        } else {
            if(all(x_ref == 1)){
                r <- cbind(NAexpandDF(x, times = y_ref, notNA = same), y)
            } else {
                r <- cbind(x, NAexpandDF(y, times = x_ref, notNA = same))
            }
        }
    }
    attr(r, "part") <- part
    r
}

NAexpandDF <- function(df, times, notNA){
    oth <- setdiff(names(df), notNA)
    A <- as.data.frame(lapply(df[, notNA, drop = FALSE], rep, times = times),
                       check.names = FALSE)
    B <- as.data.frame(lapply(df[, oth, drop = FALSE], NAexpand, times = times),
                       check.names = FALSE)
    cbind(A, B)[, names(df), drop = FALSE]
}

NAexpand <- function(x, times){
    r <- rep(NA, sum(times))
    x_at <- cumsum(c(1, times[-length(times)]))
    r[x_at] <- x
    r
}


if(FALSE){

    d <- test_data()[, c("country", "region", "gender")]
    g <- dguide(d)
    x <- dable(d, "catg", guide=g, gtab = "gender")
    y <- dable(d, "catg", guide=g, gtab = "gender", part = c(F,F,T),
               fnc = list(NULL,NULL,"bnry.chisq"))
    ## y <- dable(d, "catg", guide=g, gtab = "gender", part = c(F,F,T))
    merge_2parts(x, y)

    NAexpandDF(y, c(3,5))
    str(NAexpandDF(y, c(3,5)))

    rle(x$term)
    rle(y$term)


    df <- data.frame(x=1:3,y=LETTERS[3:1])
    times <- c(2,4,1)
}

merge_parts <- function(...){
    dots <- list(...)
    r <- Reduce(f = merge_2parts, x = dots)
    prune_meta(r)
}

prune_meta <- function(x){
    nm <- names(x)
    part <- attr(x, "part")
    indx <- meta_fixer(nm, part)
    r <- x[, indx]
    names(r) <- nm[indx]
    attr(r, "part") <- part[indx]
    r
}

meta_fixer <- function(nm, part){
    x <- paste0(nm, ":", part)
    d <- !duplicated(x)
    w1 <- which(d)
    part.copy <- part
    part.copy[!d] <- ""
    w2 <- which(part.copy == "meta")
    c(w2, setdiff(w1, w2))
}

stab2guide <- function(s, g){
    gr_nm <- g$group[g$term %in% c(s$event)]
    data.frame(term = s$label, type = "surv", class = NA_character_,
               label = s$label, group = gr_nm)
}
