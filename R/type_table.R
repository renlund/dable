
create_table <- function(data, term, type, bl, gtab, stab = NULL,
                         dots = list(), spec, fnc){
    desc <- length(spec[[1]]) > 0
    comp <- length(spec[[2]]) > 0
    test <- length(spec[[3]]) > 0
    R <- NULL
    if(desc){
        What <- if(bl) ".desc.bl" else ".desc"
        Default <- dable.default(paste0(type, What))
        Fnc <- if(!is.null(fnc[[1]])) fnc[[1]] else Default
        R <- if(type == "surv"){
                 describe.surv(fnc = Fnc, data = data, label = term,
                               stab = stab, gtab = gtab,
                               spec = spec[[1]], args = dots, part = "desc")
             } else {
                 describe(fnc = Fnc, data = data, term = term, gtab = gtab,
                          spec = spec[[1]], args = dots, part = "desc")
             }
    }
    if(comp){
        What <- if(bl) ".comp.bl" else ".comp"
        Default <- dable.default(paste0(type, What))
        Fnc <- if(!is.null(fnc[[2]])) fnc[[2]] else Default
        CO <- if(type == "surv"){
                  contrast.surv(fnc = Fnc, data = data, label = term,
                                stab = stab, gtab = gtab,
                                spec = spec[[2]], args = dots, part = "comp")
              } else {
                  contrast(fnc = Fnc, data = data, term = term, gtab = gtab,
                           spec = spec[[2]], args = dots, part = "comp")
              }
        R <- merge_parts(R, CO)
    }
    if(test){
        What <- if(bl) ".test.bl" else ".test"
        Default <- dable.default(paste0(type, What))
        Fnc <- if(!is.null(fnc[[3]])) fnc[[3]] else Default
        TE <- if(type == "surv"){
                  contrast.surv(fnc = Fnc, data = data, label = term,
                                stab = stab, gtab = gtab,
                                spec = spec[[3]], args = dots, part = "test")
              } else {
                  contrast(fnc = Fnc, data = data, term = term, gtab = gtab,
                           spec = spec[[3]], args = dots, part = "test")
              }
        R <- merge_parts(R, TE)
    }
    R
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
    if(BL && !is.null(unlist(fnc))){
        s <- paste0("Baseline tables cannot currently be created by specifying",
                    " functions (there are too many to specify!) - these must ",
                    "instead be set by e.g. 'dpset'. So the 'fnc' argument ",
                    "will be nullified")
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
    if( weight_check(weight, n = N, nm = names(Data)) == "character" ){
        weight_check(Data[[weight]], n = N, nm = names(Data))
        if(dable.default("weight.defvar.rm")) guide <- guide[guide$term != weight,]
        weight <- Data[[weight]]
    }
    Dots$weight <- weight
    ## XK what else should be available to functions, i.e. passed along in Dots ??
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
        r <- create_table(data = Data, term = Term, type = t, bl = BL,
                          gtab = gtab, stab = Stab, dots = Dots,
                          spec = Spec,
                          fnc = expand_list(fnc, n = 3, fill = NULL))
        R <- rbind(R, r)
    }
    if(is.null(R)){
        message("no table produced")
        return(invisible(data.frame()))
    }
    ## add attributes
    specs <- unique(unlist(Spec)) ## sort ?
    gtab_spec <- gtab[, specs, drop = FALSE]
    n_gtab <- unlist(lapply(gtab_spec, sum, na.rm = TRUE))
    unit.id <- attr(guide, "unit.id")
    unit.id_used <- !is.null(unit.id)
    weight_used <- !is.null(weight)
    gtab_used <- length(specs) > 1 || n_gtab[1] != nrow(Data)
    attr(R, "size") <- nrow(Data)
    u_fnc <- function(x) length(unique(stats::na.omit(x)))
    if(unit.id_used){
        attr(R, "units") <- u_fnc(Data[[unit.id]])
    }
    if(weight_used){
        attr(R, "weight") <- sum(weight, na.rm = TRUE)
    }
    if(gtab_used){
        attr(R, "gtab_size") <- n_gtab
        if(unit.id_used){
            u_fnc2 <- function(x) u_fnc(Data[[unit.id]][x])
            attr(R, "gtab_units") <- unlist(lapply(gtab_spec, u_fnc2))
        }
        if(weight_used){
            attr(R, "gtab_weight") <- unlist(lapply(
                gtab_spec,
                FUN = function(x) sum(w[x], na.rm =TRUE)
            ))
        }
    }
    ## XK also, align with vtab
    R
}

weight_check <- function(weight, n, nm) {
    if( is.null(weight) ) {
        "NULL"
    } else {
        properties(weight, class = c("character", "integer", "numeric"),
                   na.ok = FALSE)
        if( is.character(weight) ) {
            properties(weight, length = 1)
            inclusion(nm, "names of data", include = weight)
            "character"
        } else {
            properties(weight, length = n)
            if( any(weight < 0) ) stop("negative weights not allowed")
            "numeric"
        }
    }
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

## type_table <- function(type,
##                        data,
##                        guide = NULL,
##                        gtab = NULL,
##                        part = list(desc = TRUE, comp = NA, test = NA),
##                        fnc = list(desc = NULL, comp = NULL, test = NULL),
##                        weight = NULL,
##                        ...) {
##     dots <- list(...)
##     properties(type, class = "character", length = 1, na.ok = FALSE)
##     ## one_of(type, set = dable_types()) ## XK ?
##     properties(data, class = "data.frame")
##     N <- nrow(data)
##     if(is.null(guide)) guide <- dguide(data)
##     properties(guide, class = "data.frame")
##     inclusion(names(guide), nm = "names if guide",
##               include = c("term", "type")) ## maybe more?
##     if(any(guide$type == "hide")) guide[guide$type != "hide", ]
##     data <- guidify(data, guide)
##     properties(gtab, class = c("NULL", "character", "data.frame"))
##     gtab_given <- !is.null(gtab)
##     if(!gtab_given){
##         gtab <- data.frame(All = rep(TRUE, N))
##         names(gtab) <- dable.default("gtab.group.name")
##     } else if( is.character(gtab) ){
##         properties(gtab, class = "character", length = 1, na.ok = FALSE)
##         if(dable.default("gtab.defvar.rm")) guide <- guide[guide$term != gtab,]
##         gtab <- create_gtab(term = gtab, data = data)
##     } else {
##         check_gtab(gtab, n = N)
##     }
##     M <- length(gtab)
##     part <- part_spec(part, gtab)
##     properties(fnc, class = "list", length = 0:3)
##     fnc <- expand_list(fnc, n = 3, fill = NULL)
##     desc.spec <- part[[1]]
##     comp.spec <- part[[2]]
##     test.spec <- part[[3]]
##     desc <- length(desc.spec) > 0
##     comp <- length(comp.spec) > 0
##     test <- length(test.spec) > 0
##     if( weight_check(weight, n = N, nm = names(data)) == "character" ){
##         weight_check(data[[weight]], n = N, nm = names(data))
##         if(dable.default("weight.defvar.rm")) guide <- guide[guide$term != weight,]
##         weight <- data[[weight]]
##     }
##     dots$weight <- weight
##     if(type == "surv"){
##         stab <- guide2stab(guide)
##         term <- stab$label
##     } else {
##         term <- guide[guide$type == type, "term"]
##         stab <- NULL
##     }
##     R <- NULL
##     if(desc){
##         desc.def <- dable.default(paste0(type, ".desc"))
##         desc.fnc <- if(!is.null(fnc[[1]])) fnc[[1]] else desc.def
##         R <- if(type == "surv"){
##                  describe.surv(fnc = desc.fnc, data = data, label = term,
##                                stab = stab, gtab = gtab,
##                                spec = desc.spec, args = dots, part = "desc")
##              } else {
##                  describe(fnc = desc.fnc, data = data, term = term, gtab = gtab,
##                           spec = desc.spec, args = dots, part = "desc")
##              }
##     }
##     if(comp){
##         comp.def <- dable.default(paste0(type, ".comp"))
##         comp.fnc <- if(!is.null(fnc[[2]])) fnc[[2]] else comp.def
##         CO <- if(type == "surv"){
##                   contrast.surv(fnc = comp.fnc, data = data, label = term,
##                                 stab = stab, gtab = gtab,
##                                 spec = comp.spec, args = dots, part = "comp")
##               } else {
##                   contrast(fnc = comp.fnc, data = data, term = term, gtab = gtab,
##                        spec = comp.spec, args = dots, part = "comp")
##               }
##         R <- merge_parts(R, CO)
##     }
##     if(test){
##         test.def <- dable.default(paste0(type, ".test"))
##         test.fnc <- if(!is.null(fnc[[3]])) fnc[[3]] else test.def
##         TE <- if(type == "surv"){
##                   contrast.surv(fnc = test.fnc, data = data, label = term,
##                                 stab = stab, gtab = gtab,
##                                 spec = test.spec, args = dots, part = "test")
##               } else {
##                   contrast(fnc = test.fnc, data = data, term = term, gtab = gtab,
##                            spec = test.spec, args = dots, part = "test")
##               }
##         R <- merge_parts(R, TE)
##     }
##     if(is.null(R)){
##         message("no table produced")
##         return(invisible(data.frame()))
##     }
##     ## add attributes
##     ## attr(R, "spec") <- list(desc = desc.spec, comp = comp.spec, test = test.spec)
##     specs <- sort(unique(c(unlist(desc.spec), unlist(comp.spec), unlist(test.spec))))
##     ## attr(R, "remove_this") <- specs
##     gtab_spec <- gtab[, specs, drop = FALSE]
##     n_gtab <- unlist(lapply(gtab_spec, sum, na.rm = TRUE))
##     unit.id <- attr(guide, "unit.id")
##     unit.id_used <- !is.null(unit.id)
##     weight_used <- !is.null(weight)
##     gtab_used <- length(specs) > 1 || n_gtab[1] != nrow(data)
##     attr(R, "size") <- nrow(data)
##     u_fnc <- function(x) length(unique(stats::na.omit(x)))
##     if(unit.id_used){
##         attr(R, "units") <- u_fnc(data[[unit.id]])
##     }
##     if(weight_used){
##         attr(R, "weight") <- sum(weight, na.rm = TRUE)
##     }
##     if(gtab_used){
##         attr(R, "gtab_size") <- n_gtab
##         if(unit.id_used){
##             u_fnc2 <- function(x) u_fnc(data[[unit.id]][x])
##             attr(R, "gtab_units") <- unlist(lapply(gtab_spec, u_fnc2))
##         }
##         if(weight_used){
##             attr(R, "gtab_weight") <- unlist(lapply(
##                 gtab_spec,
##                 FUN = function(x) sum(w[x], na.rm =TRUE)
##             ))
##         }
##     }
##     ## XK also, align with vtab
##     R
## }
