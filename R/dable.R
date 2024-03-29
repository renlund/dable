## functions should perhaps be organized better so that it is clear which
## functions (in particular .bl-fncs) play nice with each other

dable <- function(data,
                  type = "baseline",
                  bl.rm = NULL, ## "surv" ?
                  guide = NULL,
                  gtab = NULL,
                  part = list(desc = TRUE, comp = NA, test = NA),
                  fnc = list(desc = NULL, comp = NULL, test = NULL),
                  weight = NULL,
                  ...) {
    Dots <- list(...)
    properties(data, class = "data.frame")
    Types <- check_type(type = type, bl.rm = bl.rm)
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
    if(is.null(guide)) guide <- dguide(data)
    properties(guide, class = "data.frame")
    inclusion(names(guide), nm = "names of guide",
              include = c("term", "type")) ## maybe more?
    if(any(guide$type == "hide")) guide[guide$type != "hide", ]
    Data <- guidify(data, guide)
    properties(gtab, class = c("NULL", "character", "data.frame"))
    gtab_given <- !is.null(gtab)
    if(!gtab_given){
        gtab <- data.frame(All = rep(TRUE, N))
        names(gtab) <- dparam("gtab.group.name")
    } else if( is.character(gtab) ){
        properties(gtab, class = "character", length = 1, na.ok = FALSE)
        if(dparam("gtab.defvar.rm")) guide <- guide[guide$term != gtab,]
        gtab <- create_gtab(term = gtab, data = Data)
    } else {
        check_gtab(gtab, n = N)
    }
    M <- length(gtab)
    if(!is.null(weight)){
        properties(weight, class = "character", length = 1, na.ok = FALSE)
        inclusion(names(data), nm = "names of data", include = weight)
        Weight <- Data[[weight]]
        if(any(is.na(Weight))) warning("there are missing weights") ## XK?
        if(any(!is.na(Weight) & Weight < 0)) stop("negative weights not allowed")
        Dots$weight <- Weight
    } else Weight <- NULL
    properties(fnc, class = "list", length = 0:3)
    Spec <- part_spec(part, gtab)
    R <- NULL
    for(t in Types){
        if(t == "surv"){
            Stab <- guide2stab(guide)
            Term <- Stab$label
        } else {
            Term <- guide[guide$type == t, "term", drop = TRUE]
            Stab <- NULL
        }
        if(length(Term) == 0) next
        ## r <- create_table(data = Data, term = Term, type = t, bl = BL,
        ##                   gtab = gtab, stab = Stab, dots = Dots,
        ##                   spec = Spec,
        ##                   fnc = expand_list(fnc, n = 3, fill = NULL))
        r <- table_creator(data = Data, term = Term, type = t, bl = BL,
                          gtab = gtab, stab = Stab, dots = Dots,
                          spec = Spec,
                          fnc = expand_list(fnc, n = 3, fill = NULL),
                          guide = guide)
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
    al <- align(R$term, template = guide$term)
    R[al$order,]
}

.types <- c("real", "bnry", "catg", "lcat", "surv", "date")
.baseline <- c("baseline", "bl")

check_type <- function(type, bl.rm){
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
    if(length(type) == 0){
        stop("bl.rm specification leaves leaves no type")
    } else type
}

attributer <- function(gtab, units = NULL, weight = NULL){
    n <- nrow(gtab)
    L <- list(size = n)
    n_gtab <- unlist(lapply(gtab, sum, na.rm = TRUE))
    G <- if(!is.null(n_gtab)) length(n_gtab) > 1 || n_gtab[1] != n else FALSE
    if(G) L$gtab_size <- n_gtab
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

    gt <- data.frame(A=c(T,T,T,F,F,F),B=c(F,F,F,T,T,F),C=c(F,F,F,F,F,T))
    check_gtab(gt, 6)
    attributer(gt, units = 1:6)
    attributer(gt, units = c(1,1:5))
    attributer(gt, weight = 1:6)
    attributer(gt, units = 11:16, weight = 6:1)

    gt <- data.frame(Foo = c(T,T,T,T,T,T))
    check_gtab(gt, 6)
    attributer(gt, units = 1:6)
    attributer(gt, units = c(1,1:5))
    attributer(gt, weight = 1:6)
    attributer(gt, units = 11:16, weight = 6:1)

    gt <- data.frame(Foo = c(T,T,F,F,T,T))
    check_gtab(gt, 6)
    attributer(gt, units = 1:6)
    attributer(gt, units = c(1,1:5))
    attributer(gt, weight = 1:6)
    attributer(gt, units = 11:16, weight = 6:1)

    gt <- data.frame(Foo = c(T,T,F,F,T,T))[, integer(), drop = FALSE]
    attributer(gt, units = 1:6)
    attributer(gt, units = c(1,1:5))
    attributer(gt, weight = 1:6)
    attributer(gt, units = 11:16, weight = 6:1)



    dable(test_data()) ## bad labels for surv ! XK

    g <- dguide(test_data(), vtab = test_vtab(), stab = test_stab(),
                unit.id = "id", elim.set = "pid")
    d <- dable(test_data(), guide = g, gtab = "gender", weight = "measA")
    ## XK check warning ! XK
    str(d)

}

guide2stab <- function(guide){
    g <- guide[substr(guide$type, 1, 4) == "surv",]
    g$lab <- sub(pattern = " \\(time\\)$", replacement = "", g$label)
    if(length(unique(g$lab)) != nrow(g)/2){
        s <- paste0("Trying to identify the surv components from the ",
                    "guide labels fails. Either use the same label for ",
                    "both components associated with the same time-to-event ",
                    "variable or use 'LAB' for the event- and 'LAB (time)' ",
                    "for the time component.")
        stop(s)
    }
    g$component <- sub(pattern = "(^.*)(\\.)(.*$)", replacement = "\\3", g$type)
    g2 <- g[, c("lab","term", "component")]
    g3 <- stats::reshape(g2, direction = "wide", idvar = "lab", timevar = "component")
    names(g3)[names(g3) == "lab"] <- "label"
    names(g3)[names(g3) == "term.t"] <- "time"
    names(g3)[names(g3) == "term.e"] <- "event"
    g3[, c("label", "time", "event")]
}
