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

contrast <- function(fnc, data, term = NULL, gtab, spec,
                     args = list(), part = "comp"){
    if(is.null(fnc)){
        warning("'contrast' has been given contrasting function NULL")
        fnc <- empty
    }
    if(!is.list(spec)) spec <- list(spec)
    Lab <- attr(get(fnc), "label")
    Meta <- attr(get(fnc), "meta")
    RR <- NULL
    for(i in seq_along(spec)){
        spec_i <- spec[[i]]
        l <- lapply(X = data[, term, drop = FALSE], FUN = xgFnc,
                    g = gtab2factor(gtab = gtab[,spec_i]), fnc = fnc, args = args)
        r <- do.call(rbind, l)
        rownames(r) <- NULL
        nc <- ncol(r)
        names(r) <- labeller(lab = Lab, nm = names(l[[1]]), n = nc)
        attr(r, "part") <- parter(part = part,
                                  append = paste0(names(gtab)[spec_i], collapse = ":"),
                                  nm = names(r),
                                  meta = Meta)
        N <- lapply(l, nrow)
        R <- cbind(data.frame(term = rep(term, N)), r)
        attr(R, "part") <- c("meta", attr(r, "part"))
        RR <- merge_parts(RR, R)
    }
    RR
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

contrast.surv <- function(fnc, data, label = NULL, stab = NULL, gtab, spec,
                          args = list(), part = ""){
    if(is.null(fnc)){
        warning("'contrast' has been given contrasting function NULL")
        fnc <- empty
    }
    Lab <- attr(get(fnc), "label")
    Meta <- attr(get(fnc), "meta")
    RR <- NULL
    if(!is.list(spec)) spec <- list(spec)
    for(i in seq_along(spec)){
        l <- list()
        spec_i <- spec[[i]]
        for(j in seq_along(label)){
            time.var <- stab$time[stab$label == label[j]]
            event.var <- stab$event[stab$label == label[j]]
            l[[j]] <- do.call(
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
        r <- do.call(rbind, l)
        rownames(r) <- NULL
        nc <- ncol(r)
        names(r) <- labeller(lab = Lab, nm = names(l[[1]]), n = nc)
        attr(r, "part") <- parter(part = part,
                                  append = paste0(names(gtab)[spec_i], collapse = ":"),
                                  nm = names(r),
                                  meta = Meta)
        N <- lapply(l, nrow) ## rowsOrLength
        R <- cbind(data.frame(term = rep(label, N)), r)
        attr(R, "part") <- c("meta", attr(r, "part"))
        RR <- merge_parts(RR, R)
    }
    RR
}
