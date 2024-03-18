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
        i <- which(nm == meta)
        if(length(i) > 0) r[i] <- "meta"
    }
    ifelse(r %in% c("desc", "comp", "test"),
           yes = paste0(r, ":", append),
           no = r)
}

## combine parts: this could perhaps be improved by NOT demanding that parts to
## be combined need have the same number of rows. Something like a merge might
## be feasible, but the attributes need to stay intact
merge_2parts <- function(x = NULL, y = NULL){
    if(is.null(x)) return(y)
    if(is.null(y)) return(x)
    ix <- which(attr(x, "part") == "meta")
    nx <- names(x)[ix]
    iy <- which(attr(y, "part") == "meta")
    ny <- names(y)[iy]
    same <- intersect(nx, ny)
    if(length(same) > 0){
        dummy <- TRUE
        for(s in same){
            dummy <- dummy && all( x[[s]] == y[[s]] )
        }
        part <- c(attr(x, "part"), attr(y, "part"))
        if(dummy){
            r <- cbind(x, y)
        } else {
            STRICT <- TRUE
            if(STRICT){
                s <- paste0("[marge_2parts:] trying to merge 2 parts which ",
                            "are not identical on the variables associated ",
                            "with the 'meta' attribute. Are you perhaps using",
                            " functions for different part of the table that ",
                            "produce an unequal number of rows per variable?")
                stop(s)
            } else {
                ## BAD IDEA
                warning("x and y not identical on 'meta'")
                r <- merge(x, y, all.x = TRUE, by = same)
                ## merge destroys the attributes
            }
        }
        attr(r, "part") <- part
        r
    } else {
        stop("x and y have no overlapping 'meta'")
    }
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
