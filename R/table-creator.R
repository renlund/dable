table_creator <- function(data, term, type, bl, gtab, stab = NULL,
                          dots = list(), spec, fnc, guide){
    desc <- length(spec[[1]]) > 0
    comp <- length(spec[[2]]) > 0
    test <- length(spec[[3]]) > 0
    ## browser()
    R <- NULL
    if(desc){
        What <- if(bl) ".desc.bl" else ".desc"
        Default <- dable.default(paste0(type, What))
        Fnc <- if(!is.null(fnc[[1]])) fnc[[1]] else Default
        R <- descomtes(fnc = Fnc, data = data, term = term, gtab = gtab,
                       spec = spec[[1]], surv = type == "surv", stab = stab,
                       guide = guide, args = dots, part = "desc")
    }
    if(comp){
        What <- if(bl) ".comp.bl" else ".comp"
        Default <- dable.default(paste0(type, What))
        Fnc <- if(!is.null(fnc[[2]])) fnc[[2]] else Default
        CO <- descomtes(fnc = Fnc, data = data, term = term, gtab = gtab,
                        spec = spec[[2]], surv = type == "surv", stab = stab,
                        guide = guide, args = dots, part = "comp")
        R <- merge_parts(R, CO)
    }
    if(test){
        What <- if(bl) ".test.bl" else ".test"
        Default <- dable.default(paste0(type, What))
        Fnc <- if(!is.null(fnc[[3]])) fnc[[3]] else Default
        TE <- descomtes(fnc = Fnc, data = data, term = term, gtab = gtab,
                        spec = spec[[3]], surv = type == "surv", stab = stab,
                        guide = guide, args = dots, part = "test")
        R <- merge_parts(R, TE)
    }
    R
}

descomtes <- function(fnc, data, term, gtab, spec, surv, stab, guide, args, part){
    if(is.null(gtab)){
        gtab <- data.frame(x = rep(TRUE, nrow(data)))
        names(gtab) <- default.gtab.group.name()
    }
    if(is.null(fnc)){
        warning("'descomtes' has been given description function NULL")
        fnc <- empty
    }
    Lab <- attr(get(fnc), "label")
    Meta <- attr(get(fnc), "meta")
    weight <- args$.weight
    Weight <- if(is.null(weight)) NULL else data[[weight]]
    RR <- NULL
    if(part == "test") spec <- list(spec) ## special case ...
    for(i in seq_along(spec)){ ## i = 1
        l <- list()
        spec_i <- spec[[i]]
        for(j in seq_along(term)){ ## j = 1
            term_j <- term[j]
            args$.term <- term_j
            args$.label <- guide$label[guide$term == term_j]
            args$.type <- guide$type[guide$term == term_j]
            args$.group <- guide$group[guide$term == term_j]
            args$.missing <- attr(guide, "missing")[term_j]
            l[[j]] <-
                if(surv){
                    time.var <- stab$time[stab$label == term_j]
                    event.var <- stab$event[stab$label == term_j]
                    args$.type <- "surv"
                    args$.label <- args$.term
                    args$.term <- event.var ## XK ?
                    args$.group <-  guide$group[guide$term == event.var]
                    args$.missing <- attr(guide, "missing")[event.var]
                    ## browser()
                    if(part == "desc"){
                        do.call(
                            what = xFnc.surv,
                            args = list(
                                fnc = fnc,
                                time = data[gtab[, spec_i], time.var, drop = TRUE],
                                event = data[gtab[, spec_i], event.var, drop = TRUE],
                                weight = Weight[gtab[, spec_i]],
                                args = args
                            )
                        )
                    } else {
                        do.call(
                            what = xgFnc.surv,
                            args = list(
                                fnc = fnc,
                                time = data[[time.var]],
                                event = data[[event.var]],
                                g = gtab2factor(gtab = gtab[,spec_i]),
                                weight = Weight,
                                args = args
                            )
                        )
                    }
                } else {
                    if(part == "desc"){
                        do.call(
                            what = xFnc,
                            args = list(
                                fnc = fnc,
                                x = data[gtab[, spec_i], term_j, drop = TRUE],
                                weight = Weight[gtab[, spec_i]],
                                args = args
                            )
                        )
                    } else {
                        do.call(
                            what = xgFnc,
                            args = list(
                                fnc = fnc,
                                x = data[, term_j, drop = TRUE],
                                g = gtab2factor(gtab = gtab[,spec_i,drop=FALSE]),
                                weight = Weight,
                                args = args
                            )
                        )
                    }
                }
        }
        r <- do.call(rbind, l)
        rownames(r) <- NULL
        nc <- ncol(r)
        names(r) <- labeller(lab = Lab, nm = names(l[[1]]), n = nc)
        ## browser()
        attr(r, "part") <- parter(part = part, append = names(gtab)[spec_i],
                                  nm = names(r), meta = Meta)
        N <- lapply(l, nrow)
        R <- cbind(data.frame(term = rep(term, N)), r)
        attr(R, "part") <- c("meta", attr(r, "part"))
        RR <- merge_parts(RR, R)
    }
    RR
}

## XK these helpers must explicitly take a 'weight' argument

xFnc <- function(fnc, x, weight, args){
    r <- tryCatch(expr = do.call(what = fnc,
                                 args = c(list(x = x,
                                               weight = weight),
                                          args)),
                  error = function(e){
                      warning(e, "\n")
                      do.call(
                          what = fnc,
                          args = c(list(x = rep(NA, length(x))),
                                   args)
                      )
                  })
    as.data.frame(r)
}

xFnc.surv <- function(fnc, time, event, weight, args = list()){
    r <- tryCatch(expr = do.call(what = fnc,
                                 args = c(list(time = time,
                                               event = event,
                                               weight = weight),
                                          args)),
                  error = function(e){
                      warning(e, "\n")
                      do.call(
                          what = fnc,
                          args = c(list(time = rep(NA, length(time)),
                                        event = rep(NA), length(event)),
                                   args)
                      )
                  })
    as.data.frame(r)
}

xgFnc <- function(fnc, x, g, weight, args){
    r <- tryCatch(expr = do.call(what = fnc,
                                 args = c(list(x = x,
                                               g = g,
                                               weight = weight), args)),
                  error = function(e){
                      warning(e, "\n")
                      do.call(
                          what = fnc,
                          args = c(list(x = rep(NA, length(x)),
                                        g = g),
                                   args)
                      )
                  })
    as.data.frame(r)
}

xgFnc.surv <- function(fnc, time, event, g, weight, args = list()){
    r <- tryCatch(expr = do.call(what = fnc,
                                 args = c(list(time = time,
                                               event = event,
                                               g = g,
                                               weight = weight), args)),
                  error = function(e){
                      warning(e, "\n")
                      do.call(
                          what = fnc,
                          args = c(list(time = rep(NA, length(time)),
                                        event = rep(NA, length(event)),
                                        g = g),
                                   args)
                      )
                  })
    as.data.frame(r)
}
