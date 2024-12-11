## Note: these functions are 'imported' (copied) from another package, but will
## probably undergo many changes.

default.vtab.group.name <- function() dparam("vtab.group.name")

##' variable table (vtab) functions
##'
##' @param vtab data.frame; a "variable table" (vtab)
##' @param vl list; a "variable list" (vlist) - older specification of variables
##'     not really used any more
##' @name vtab-fncs
NULL

##' @rdname vtab-fncs
##' @details check_vtab: check a variable table, i.e. that it contains 'term',
##'     'label' and possibly 'group'. It will addd variable group if missing.
##' @export
check_vtab <- function(vtab){
    properties(vtab, class = "data.frame")
    if( !("group" %in% names(vtab)) ){
        vtab$group <- default.vtab.group.name()
    }
    inclusion(names(vtab), nm = "names of variable table",
              include = c("term", "label", "group"))
    properties(vtab$label, class = "character", na.ok = FALSE)
    properties(vtab$group, class = "character", na.ok = FALSE)
    properties(vtab$term, class = "character", na.ok = TRUE)
    if(any(is.na(vtab$term))){
        lab_v <- vtab$label[is.na(vtab$term)]
        stab <- attr(vtab, "stab")
        if(is.null(stab)){
            s <- paste0("Missing values in 'term' is only allowed in ",
                        "additional information exists in 'stab' attribute.")
            stop(s)
        } else {
            stab <- check_stab(stab)
            lab_s <- stab$label
            if(!all(lab_v %in% lab_s)){
                s <- paste0("Some labels in vtab are missing from stab.")
                stop(s)
            }
        }
    }
    vtab
}

rbind.vtab <- function(x, y){
    r <- rbind(x, y)
    attr(r, "stab") <- rbind(attr(x, "stab"), attr(y, "stab"))
    rownames(r) <- NULL
    r
}

delist <- function(x){
    properties(x, class = "list")
    R <- NULL
    for(i in seq_along(x)){
        y <- x[[i]]
        if(is.list(y)) y <- delist(x[[i]])
        R <- c(R, y)
    }
    R
}

shuffle <- function (x, y){
    n <- length(x)
    if (length(y) != n) stop("no shuffle for you!")
    N <- 2 * n
    r <- rep(NA, N)
    r[seq(1, N, 2)] <- x
    r[seq(2, N, 2)] <- y
    r
}

################################################################################
##        older things related to 'variable list'                             ##
################################################################################

##' @rdname vtab-fncs
##' @details vlist2vtab: convert older variable list specification (vlist, not
##'     in use anymore) to a vtab
##' @export
vlist2vtab <- function(vl){
    properties(vl, class = "list")
    n <- unlist(lapply(vl, length))
    x <- delist(vl)
    d <- data.frame(term = names(x),
                    label = x,
                    group = rep(names(vl), n))
    rownames(d) <- NULL
    d
}

invert_vlist <- function(x){
    properties(x, class = "list")
    foo <- function(z) setNames(object = names(z), nm = z)
    lapply(x, foo)
}
