## functions should perhaps be organized better so that it is clear which
## functions (in particular .bl-fncs) play nice with each other

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
        ## guide <- guide[guide$term %in% names(data), ]
        guide <- subset.guide(guide, term = names(data))
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
        if(dparam("gtab.defvar.rm")){
            guide$type[guide$term == gtab] <- .hide.type
        }
        gtab <- create_gtab(term = gtab, data = data)
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
        Dots$.weight <- weight ## Weight ??????
    } else {
        Weight <- NULL
        Dots$.weight <- NULL
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
        ## ---
        nm_is <- names(R)
        nm_new <- names(r)
        if( !is.null(R) ){
            ## if(length(nm_is) != length(nm_new) ||
            ##    any(nm_is != nm_new)){
            if( !setequal(nm_is, nm_new) ){
                etxt <- paste0("Trying to rbind table with name set {",
                               paste0(nm_new, collapse = ", "),
                               "} onto created table with name set {",
                               paste0(nm_is, collapse = ", "),
                               "} will create problems.\n")
                warning(etxt)
            }
        }
        ## ---
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

if(FALSE){
    d <- test_data()
    g <- dguide(data = d, unit.id = "pid",
                    vtab = test_vtab(), stab = test_stab())
    data <- d[, c("country", "area", "region", "gender")]

    x <- dable(data = data, type = "bl", gtab = "gender", guide=g)
    y <- dable(data = data, type = "bl", gtab = "gender", guide=g, part = c(F,F,T))
    merge_2parts(x, y)[,c(1:2,6:8)]


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

if(FALSE){

    dguide(test_data())

    g <- dguide(test_data(), vtab = test_vtab(), stab = test_stab(),
                unit.id = "id", elim.set = "pid")
    dable(test_data(), guide = g)
    dable(test_data(), guide = g, gtab = "gender")
    dable(test_data(), guide = g, gtab = "gender", weight = "importance")



    d <- dable(test_data(), guide = g, gtab = "gender", weight = "importance")
    ## XK check warning ! XK
    str(d)

    data = test_data()
    type = "baseline"
    bl.rm = NULL
    guide = dguide(test_data(), unit.id = "id")
    gtab = "gender"
    part = list(desc = TRUE, comp = NA, test = NA)
    fnc = list(desc = NULL, comp = NULL, test = NULL)
    weight = "importance"

    ## ---------

    data <- test_data()
    vtab <- test_vtab()
    stab <- test_stab()
    unit.id <- "id"
    ## elim.set <- NULL

    type = "baseline"
    bl.rm = NULL
    guide = dguide(data, unit.id = "id", vtab = vtab, stab = stab)
    guide$type[guide$term %in% c("pid")] <- "lcat"
    gtab = "gender"
    part = list(desc = TRUE, comp = NA, test = NA)
    fnc = list(desc = NULL, comp = NULL, test = NULL)
    weight = "importance"
    Dots = as.list(NULL)


}


guide2stab <- function(guide){
    attr(guide, "stab")
}

## guide2stab <- function(guide){
##     g <- guide[substr(guide$type, 1, 4) == "surv",]
##     if(nrow(g) == 0) return(NULL)
##     g$lab <- sub(pattern = " \\(time\\)$", replacement = "", g$label)
##     if(length(unique(g$lab)) != nrow(g)/2){
##         s <- paste0("Trying to identify the surv components from the ",
##                     "guide labels fails. Either use the same label for ",
##                     "both components associated with the same time-to-event ",
##                     "variable or use 'LAB' for the event- and 'LAB (time)' ",
##                     "for the time component.")
##         stop(s)
##     }
##     g$component <- sub(pattern = "(^.*)(\\.)(.*$)", replacement = "\\3", g$type)
##     g2 <- g[, c("lab","term", "component")]
##     g3 <- stats::reshape(g2, direction = "wide", idvar = "lab", timevar = "component")
##     names(g3)[names(g3) == "lab"] <- "label"
##     names(g3)[names(g3) == "term.t"] <- "time"
##     names(g3)[names(g3) == "term.e"] <- "event"
##     g3[, c("label", "time", "event")]
## }
