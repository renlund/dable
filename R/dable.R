##' Data description
##'
##' This function generates a data description.
##' @param data data.frame
##' @param type character; either a data type or 'baseline' for all types
##' @param bl.rm character vector; types to remove from baseline table
##' @param guide object created with \code{dguide}
##' @param gtab a grouping table which is a named list of logical vectors equal
##'     in length to the numbers of rows of data. It can also be created by
##'     pointing to a term in data.
##' @param part list that specifies which 'parts' of the table you want. The
##'     parts are for 'description', 'comparison' and 'test'. There is more
##'     funcionality hidden here, but that must be explained in a vignette.
##' @param fnc list of functions for the different parts. Currently only
##'     supported for a non-baseline type.
##' @param weight a vector of weights, or the name of a term in data that
##'     provides these
##' @param ... arguments passed to functions that describes, compares and tests
##' @export
dable <- function(data,
                  type = "baseline",
                  bl.rm = NULL,
                  guide = NULL,
                  gtab = NULL,
                  part = list(desc = TRUE, comp = NA, test = NA),
                  fnc = list(desc = NULL, comp = NULL, test = NULL),
                  weight = NULL,
                  ...) {
    Dots <- list(...) ## Dots <- as.list(NULL)
    properties(data, class = "data.frame")
    if(is.null(guide)){
        guide <- dguide(data)
    } else {
        properties(guide, class = "data.frame")
        inclusion(names(guide), nm = "names of guide",
                  include = c("term", "type", "class", "label", "group"))
        guide <- subset_guide(guide, term = names(data))
        if(nrow(guide) == 0) stop("no guide terms in data")
    }
    Types <- check_type(type = type, bl.rm = bl.rm, guide = guide)
    BL <- type %in% .baseline
    Dots$.table.type <- if(BL) "baseline" else type
    if(BL && !is.null(unlist(fnc))){
        s <- paste0("Baseline tables cannot currently be created by specifying",
                    " functions (there are too many to specify!) - these must ",
                    "instead be set by e.g. 'dpset'. So the 'fnc' argument ",
                    "will be nullified")
        ## XK maybe not! allowing this, but recognising that the functions must
        ## work for all types allows functions similar to dtable_missing
        warning(s)
        fnc <- list(desc = NULL, comp = NULL, test = NULL)
    }
    N <- nrow(data)
    ## if(any(guide$type == .unknown.type)){
    ##     guide <- guide[guide$type != .unknown.type, ]
    ## } ## ??
    properties(gtab, class = c("NULL", "character", "data.frame"))
    gtab_given <- !is.null(gtab)
    if(!gtab_given){
        gtab <- data.frame(All = rep(TRUE, N))
        names(gtab) <- dparam("gtab.group.name")
    } else if( is.character(gtab) ){
        properties(gtab, class = "character", length = 1, na.ok = FALSE)
        Dots$.gtab.term <- gtab
        if(dparam("gtab.defvar.rm")){
            guide$type[guide$term == gtab] <- .hide.type
        }
        gtab <- gtab_maker(term = gtab, data = data)
    } else {
        check_gtab(gtab, n = N)
    }
    M <- length(gtab)
    if(!is.null(weight)){
        properties(weight, class = "character", length = 1, na.ok = FALSE)
        inclusion(names(data), nm = "names of data", include = weight)
        if(dparam("weight.defvar.rm")){
            guide$type[guide$term == weight] <- .hide.type
        }
        Weight <- data[[weight]]
        if(any(is.na(Weight))) warning("there are missing weights") ## XK?
        if(any(!is.na(Weight) & Weight < 0)) stop("negative weights not allowed")
        Dots$.weight.term <- weight
    } else {
        Weight <- NULL
    }
    properties(fnc, class = "list", length = 0:3)
    Data <- guidify(data, guide)
    Spec <- part_spec(part, gtab)
    R <- NULL
    for(t in Types){ ## t = Types[1]
        if(t == "surv"){
            Stab <- guide2stab(guide)
            Term <- Stab$label
        } else {
            Term <- guide[guide$type == t, "term", drop = TRUE]
            Stab <- NULL
        }
        if(length(Term) == 0) next
        r <- table_creator(data = Data, term = Term, type = t, bl = BL,
                           gtab = gtab, stab = Stab, dots = Dots,
                           spec = Spec,
                           fnc = expand_list(fnc, n = 3, fill = NULL),
                           guide = guide)
        nm_is <- names(R)
        nm_new <- names(r)
        if( !is.null(R) ){
            if( !setequal(nm_is, nm_new) ){
                etxt <- paste0("Trying to rbind table with name set {",
                               paste0(nm_new, collapse = ", "),
                               "} onto created table with name set {",
                               paste0(nm_is, collapse = ", "),
                               "} will create problems.\n")
                warning(etxt)
            }
        }
        R <- rbind(R, r)
    }
    if(is.null(R)){
        message("no table produced")
        return(invisible(data.frame()))
    }
    u <- attr(guide, "unit.id")
    ATTR <- attributer(gtab = gtab[, Spec[[1]], drop = FALSE],
                       units = if(!is.null(u)) Data[[u]] else NULL,
                       weight = Weight)
    add_attr <- function(i) attr(R, which = names(ATTR)[i]) <<- ATTR[[i]]
    lapply(seq_along(ATTR), add_attr)
    attr(R, "type") <- if(BL) "baseline" else type
    attr(R, "guide") <- guide
    al <- align(R$term, template = guide$term)
    R[al$order,]
}

.types <- c("real", "bnry", "catg", "lcat", "surv", "date")
.baseline <- c("baseline", "bl")

check_type <- function(type, bl.rm, guide){
    properties(type, class = "character", length = 1, na.ok = FALSE)
    one_of(type, nm = "type", set = c(.types, .baseline))
    if(any(type %in% .baseline)){
        type <- .types
        BL <- TRUE
    } else {
        BL <- FALSE
    }
    if(BL && !is.null(bl.rm)){
        properties(bl.rm, class = "character", na.ok = FALSE)
        for(i in seq_along(bl.rm)){
            one_of(bl.rm[i], nm = "all elements of 'bl.rm'",
                   set = .types)
        }
        type <- setdiff(type, bl.rm)
    }
    type <- type[type %in% unique(guide$type)]
    if(length(type) == 0){
        stop("specification leaves leaves no type")
    } else type
}

attributer <- function(gtab, units = NULL, weight = NULL){
    n <- nrow(gtab)
    L <- list(rows = n)
    n_gtab <- unlist(lapply(gtab, sum, na.rm = TRUE))
    G <- if(!is.null(n_gtab)) length(n_gtab) > 1 || n_gtab[1] != n else FALSE
    if(G) L$gtab_rows <- n_gtab
    if(!is.null(units)){
        u_fnc <- function(x) length(unique(x[!is.na(x)]))
        u_fnc2 <- function(x) u_fnc(units[x])
        L$units <- u_fnc(units)
        if(G) L$gtab_units <- unlist(lapply(gtab, u_fnc2))
    }
    if(!is.null(weight)){
        wsum <- function(x) sum(weight[x], na.rm = TRUE)
        L$weight <- sum(weight, na.rm = TRUE)
        if(G) L$gtab_weight <- unlist(lapply(gtab, wsum))
    }
    L
}

guide2stab <- function(guide){
    attr(guide, "stab")
}
