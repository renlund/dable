table_creator <- function(data, term, type, bl, gtab, stab = NULL,
                          dots = list(), spec, fnc, guide){
    desc <- length(spec[[1]]) > 0
    comp <- length(spec[[2]]) > 0
    test <- length(spec[[3]]) > 0
    R <- NULL
    if(desc){
        What <- if(bl) ".desc.bl" else ".desc"
        Default <- dable.default(paste0(type, What))
        Fnc <- if(!is.null(fnc[[1]])) fnc[[1]] else Default
        R <- descomtes(fnc = Fnc, data = data, term = term, gtab = gtab,
                       spec = spec[[1]], surv = type == "surv", stab,
                       guide = guide, args = dots, part = "desc")
    }
    if(comp){
        What <- if(bl) ".comp.bl" else ".comp"
        Default <- dable.default(paste0(type, What))
        Fnc <- if(!is.null(fnc[[2]])) fnc[[2]] else Default
        CO <- descomtes(fnc = Fnc, data = data, term = term, gtab = gtab,
                        spec = spec[[2]], surv = type == "surv", stab,
                        guide = guide, args = dots, part = "comp")
        R <- merge_parts(R, CO)
    }
    if(test){
        What <- if(bl) ".test.bl" else ".test"
        Default <- dable.default(paste0(type, What))
        Fnc <- if(!is.null(fnc[[3]])) fnc[[3]] else Default
        TE <- descomtes(fnc = Fnc, data = data, term = term, gtab = gtab,
                        spec = spec[[3]], surv = type == "surv", stab,
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
    RR <- NULL
    if(part == "test") spec <- list(spec) ## special case ...
    for(i in seq_along(spec)){
        l <- list()
        spec_i <- spec[[i]]
        for(j in seq_along(term)){
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
                    if(part == "desc"){
                        do.call(
                            what = xFnc.surv,
                            args = list(
                                time = data[gtab[, spec_i], time.var, drop = TRUE],
                                event = data[gtab[, spec_i], event.var, drop = TRUE],
                                fnc = fnc,
                                args = args
                            )
                        )
                    } else {
                        do.call(
                            what = xgFnc.surv,
                            args = list(
                                time = data[[time.var]],
                                event = data[[event.var]],
                                g = gtab2factor(gtab = gtab[,spec_i]),
                                fnc = fnc,
                                args = args
                            )
                        )
                    }
                } else {
                    if(part == "desc"){
                        do.call(
                            what = xFnc,
                            args = list(
                                x = data[gtab[, spec_i], term_j, drop = TRUE],
                                fnc = fnc,
                                args = args
                            )
                        )
                    } else {
                        ## browser()
                        do.call(
                            what = xgFnc,
                            args = list(
                                x = data[, term_j, drop = TRUE],
                                g = gtab2factor(gtab = gtab[,spec_i,drop=FALSE]),
                                fnc = fnc,
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
        attr(r, "part") <- parter(part = part, append = names(gtab)[spec_i],
                                  nm = names(r), meta = Meta)
        N <- lapply(l, nrow)
        R <- cbind(data.frame(term = rep(term, N)), r)
        attr(R, "part") <- c("meta", attr(r, "part"))
        RR <- merge_parts(RR, R)
    }
    RR
}

xFnc <- function(x, fnc, args){
    r <- tryCatch(expr = do.call(what = fnc, args = c(list(x = x), args)),
                  error = function(e){
                      warning(e)
                      tryCatch(expr = do.call(
                                   what = fnc,
                                   args = c(list(x = NA), args)
                               ), error = function(e) NA)
                  })
    as.data.frame(r)
}

xFnc.surv <- function(time, event, fnc, args = list()){
    r <- tryCatch(expr = do.call(what = fnc,
                                 args = c(list(time = time,
                                               event = event), args)),
                  error = function(e){
                      warning(e)
                      tryCatch(expr = do.call(
                                   what = fnc,
                                   args = c(list(time = NA, event = NA), args)
                               ), error = function(e) NA)
                  })
    as.data.frame(r)
}

xgFnc <- function(x, g, fnc, args){
    r <- tryCatch(expr = do.call(what = fnc, args = c(list(x = x, g = g), args)),
                  error = function(e){
                      warning(e)
                      tryCatch(expr = do.call(
                                   what = fnc,
                                   args = c(list(x = NA, g = NA), args)
                               ), error = function(e) NA)
                  })
    as.data.frame(r)
}

xgFnc.surv <- function(time, event, g, fnc, args = list()){
    r <- tryCatch(expr = do.call(what = fnc,
                                 args = c(list(time = time,
                                               event = event,
                                               g = g), args)),
                  error = function(e){
                      warning(e)
                      tryCatch(expr = do.call(
                                   what = fnc,
                                   args = c(list(time = NA,
                                                 event = NA,
                                                 g = NA), args)
                               ), error = function(e) NA)
                  })
    as.data.frame(r)
}
