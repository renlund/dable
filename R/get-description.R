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

describe <- function(fnc, data, term = NULL, gtab = NULL, spec = 1,
                     args = list(), part = "desc"){
    if(is.null(gtab)){
        gtab <- data.frame(x = rep(TRUE, nrow(data)))
        names(gtab) <- default.gtab.group.name()
    }
    if(is.null(fnc)){
        warning("'describe' has been given description function NULL")
        fnc <- empty
    }
    Lab <- attr(get(fnc), "label")
    Meta <- attr(get(fnc), "meta")
    RR <- NULL
    for(i in seq_along(spec)){
        l <- lapply(X = data[gtab[, spec[i]], term, drop = FALSE],
                    FUN = xFnc, fnc = fnc, args = args)
        r <- do.call(rbind, l)
        rownames(r) <- NULL
        nc <- ncol(r)
        names(r) <- labeller(lab = Lab, nm = names(l[[1]]), n = nc)
        attr(r, "part") <- parter(part = part, append = names(gtab)[spec[i]],
                                  nm = names(r), meta = Meta)
        N <- lapply(l, nrow) ## rowsOrLength
        R <- cbind(data.frame(term = rep(term, N)), r)
        attr(R, "part") <- c("meta", attr(r, "part"))
        RR <- merge_parts(RR, R)
    }
    RR
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

describe.surv <- function(fnc, data, label = NULL, stab = NULL, gtab = NULL, spec = 1,
                          args = list(), part = "desc"){
    if(is.null(gtab)){
        gtab <- data.frame(x = rep(TRUE, nrow(data)))
        names(gtab) <- default.gtab.group.name()
    }
    if(is.null(fnc)){
        warning("'describe' has been given description function NULL")
        fnc <- empty
    }
    Lab <- attr(get(fnc), "label")
    Meta <- attr(get(fnc), "meta")
    RR <- NULL
    for(i in seq_along(spec)){
        l <- list()
        for(j in seq_along(label)){
            time.var <- stab$time[stab$label == label[j]]
            event.var <- stab$event[stab$label == label[j]]
            l[[j]] <- do.call(
                what = xFnc.surv,
                args = list(
                    time = data[gtab[, spec[i]], time.var, drop = TRUE],
                    event = data[gtab[, spec[i]], event.var, drop = TRUE],
                    fnc = fnc,
                    args = args
                )
            )
        }
        r <- do.call(rbind, l)
        rownames(r) <- NULL
        nc <- ncol(r)
        names(r) <- labeller(lab = Lab, nm = names(l[[1]]), n = nc)
        attr(r, "part") <- parter(part = part, append = names(gtab)[spec[i]],
                                  nm = names(r), meta = Meta)
        N <- lapply(l, nrow) ## rowsOrLength
        R <- cbind(data.frame(term = rep(label, N)), r)
        attr(R, "part") <- c("meta", attr(r, "part"))
        RR <- merge_parts(RR, R)
    }
    RR
}
