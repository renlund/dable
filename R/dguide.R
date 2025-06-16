.unknown.type <- "unknown"
.hide.type <- "hide"

##' description guide
##'
##' Determine how to describe a data frame.
##' @param data data.frame, the data
##' @param id character vector; id variables used.
##' @param elim.set character vector, optionally provide names of variables to
##'     ignore
##' @param vtab data.frame, optionally provide a variable table (vtab)
##' @param stab data.frame, optionally provide a survival table (stab)
##' @param ... arguments passed to \code{term_type}
##' @export
dguide <- function(data, id = NULL, elim.set = NULL,
                   ## unit.id = NULL, oth.id = NULL,
                   vtab = NULL, stab = NULL, ...){
    properties(data, class = "data.frame")
    ## unit.id <- dparam("unit.id", unit.id)
    ## oth.id <- setdiff(dparam("oth.id", oth.id), unit.id)
    id <- dparam("id", id)
    unit.id <- oth.id <- NULL
    if(!is.null(id)){
        for(j in seq_along(id)){
            i <- id[j]
            if( j == 1 & i %in% names(data) ) unit.id <- i
            if( j > 1 & i %in% names(data) ) oth.id <- c(oth.id, i)
            if( !(i %in% names(data)) ) next
            if(any(is.na(data[[i]]))){
                s <- paste0("id variable '", i, "' contains missing values")
                warning(s)
            }
        }
    }
    properties(elim.set, class = c("NULL", "character"), na.ok = FALSE)
    properties(vtab, class = c("NULL", "data.frame"))
    properties(stab, class = c("NULL", "data.frame"))
    terms <- setdiff(names(data), c(elim.set))
    ## if(!is.null(unit.id)){
    ##     inclusion(names(data), "names of data", include = unit.id)
    ##     if(any(is.na(data[[unit.id]]))){
    ##         s <- paste0("unit id variable '", unit.id,"' contains missing values")
    ##         warning(s)
    ##     }
    ## }
    vtab_given <- !is.null(vtab)
    if(is.null(vtab)) vtab <- extract_labels(data[, terms, drop = FALSE])
    vtab <- check_vtab(vtab)
    if(is.null(stab)){
        stab <- extract_stab_from_names(names(data))
    } else {
        stab <- check_stab(stab)
    }
    if(!is.null(stab)) vtab <- combine_vs_tab(vtab, stab)
    tt0 <- term_type(term = terms, data = data, stab = stab, ...)
    ## tt0 <- term_type(term = terms, data = data, stab = stab)
    lev <- attr(tt0, "levels")
    terms_all <- terms
    terms <- setdiff(terms, c(stab$event, stab$time))
    no_vt <- setdiff(terms, vtab$term)
    if(length(no_vt) > 0){
        vt_xtra <- check_vtab(extract_labels(data[, no_vt, drop = FALSE]))
        vtab <- rbind_vtab(vtab, vt_xtra)
    }
    tt <- merge(x = tt0,
                y = vtab[, c("term", "label", "group")],
                by = "term", all.x = TRUE, sort = FALSE)
    if(!is.null(unit.id)){
        tt$type[tt$term == unit.id] <- "unit.id"
    }
    if(length(oth.id) > 0) tt$type[tt$term %in% oth.id] <- "lcat"
    al <- align(x = tt$term, template = vtab$term, group = vtab$group, all = TRUE)
    TT <- tt[al$order, ]
    attr(TT, "stab") <- attr(vtab, "stab")
    attr(TT, "levels") <- lev
    attr(TT, "missing") <- unlist(lapply(X = data[, terms_all, drop = FALSE],
                                         FUN = function(x) any(is.na(x))))
    if(!is.null(unit.id)) attr(TT, "unit.id") <- unit.id
    class(TT) <- c("dguide", class(TT))
    TT
}

##' @exportS3Method
print.dguide <- function(x, ...){
    cat("data guide:\n")
    print(as.data.frame(x))
    stab <- attr(x, "stab")
    if(!is.null(stab)){
        cat("with stab:\n")
        print(as.data.frame(stab))
    }
    invisible(NULL)
}


extract_labels <- function(x){
    terms <- names(x)
    foo <- function(z){
        l <- attr(z, "label")
        if(!is.null(l) &&
           is.character(l) &&
           length(l) == 1) l else NA_character_
    }
    ls <- unlist(lapply(x, foo))
    data.frame(term = terms,
               label = ifelse(!is.na(ls), ls, terms))
}

subset_guide <- function(guide, term){
    stab <- guide2stab(guide)
    st <- stab[stab$time %in% term & stab$event %in% term,]
    G <- guide[guide$term %in% term | guide$label %in% st$label, ]
    attr(G, "stab") <- if(!is.null(st) && nrow(st)>0) st else NULL
    G
}

guidify <- function(data, guide){
    kill <- guide$term[guide$type == .unknown.type]
    catgify <- guide$term[guide$type %in% c("catg", "bnry", "lcat")]
    stab <- attr(guide, "stab")
    surv_term <- c(stab$event, stab$time)
    LEV <- attr(guide, "levels")
    for(nm in names(data)){
        if(nm %in% kill) data[[nm]] <- NULL
        if( !(nm %in% c(guide$term, surv_term)) ) data[[nm]] <- NULL
        if(nm %in% catgify){
            if(is.factor(data[[nm]])) next
            if(nm %in% names(LEV)){
                data[[nm]] <- factor(data[[nm]], levels = LEV[[nm]])
            } else {
                x <- data[[nm]]
                v <- sort(unique(x[!is.na(x)]))
                data[[nm]] <- factor(x, levels = v)
            }
        }
    }
    data
}

##' @describeIn dguide term_type determines the type of a variable in a data set
##'     through its name (through its appearance in a 'survival-list' if given)
##'     and after that through its values by value_type
##' @param term character, name of variables to describe
##' @param stab data.frame, optionally provide a 'survival-table'
##' @param bnry.list list, optionally provide list of values (length 2 vectors)
##'     that are the only values considered to be associated with binary variables
##' @param real.tol numeric, if given, a numeric variable with at most this
##'     number of unique value will be considered a 'catg', instead of 'real',
##'     variable
##' @param catg.tol numeric, if given, a categorical variable with more than
##'     this number of unqique values will be considered a 'lcat' variable and
##'     be described differently
##' @export
term_type <- function(term, data, stab = NULL, bnry.list = NULL,
                      real.tol = NULL, catg.tol = NULL){
    properties(term, class = "character", na.ok = FALSE)
    properties(data, class = "data.frame")
    inclusion(names(data), nm = "names of data", include = term)
    if(is.null(stab)){
        stab <- extract_stab_from_names(names(data))
    } else stab <- check_stab(stab)
    bnry.list <- dparam("bnry.list", bnry.list)
    real.tol <- dparam("real.tol", real.tol)
    catg.tol <- dparam("catg.tol", catg.tol)
    wsc <- which_surv_component(term, stab)
    type <- ifelse(test = wsc == "time",
                   yes = "surv.t",
                   no = ifelse(test = wsc == "event",
                               yes = "surv.e",
                               no = wsc))
    L <- list()
    vt_index <- which(is.na(type))
    for(i in vt_index){
        vt <- value_type(x = data[[term[i]]],
                         bnry.list = bnry.list,
                         real.tol = real.tol,
                         catg.tol = catg.tol)
        type[i] <- vt$type
        if(!identical(vt$levels, NA_integer_)) L[[term[i]]] <- vt$levels
    }
    r <- data.frame(term = term,
                    type = type,
                    class = unlist(lapply(X = data[, term, drop =FALSE],
                                          FUN = function(z) class(z)[1])))
    if(!is.null(stab)){
        for(i in 1:nrow(stab)){
            ev <- stab$event[i]
            t <- stab$time[i]
            ev_i <- which(r$term == ev)
            t_i <- which(r$term == t)
            r$type[ev_i] <- "surv"
            r$term[ev_i] <- event_time_comb_name(ev, t)
            r$class[ev_i] <- paste(r$class[ev_i], "/", r$class[t_i])
            r <- r[r$term != t,]
        }
    }
    attr(r, "levels") <- rm_na_from_list(L)
    rownames(r) <- NULL
    r
}

rm_na_from_list <- function(l){
    foo <- function(x) !(length(x) == 1 && is.na(x))
    index <- unlist(lapply(l, foo))
    l[index]
}

##' @describeIn dguide value_type determines the type of a variable through its values
##' @param x vector of values
##' @export
value_type <- function(x, bnry.list = list(), real.tol = 0, catg.tol = Inf){
    properties(bnry.list, class = c("list"))
    properties(real.tol, class = c("integer", "numeric"), length = 1, na.ok = FALSE)
    properties(catg.tol, class = c("integer", "numeric"), length = 1, na.ok = FALSE)
    klass <- class(x)
    x_val <- if("factor" %in% klass){
                 levels(x)
             } else {
                 sort(unique(stats::na.omit(x)))
             }
    n_val <- length(x_val)
    type <- if(n_val == 0){
                "missing" ## ?
            } else if( n_val == 2 && is.bnry(x_val, bnry.list) ){
                "bnry"
            } else if( any( c("character", "factor", "logical") %in% klass ) ){
                if(n_val <= catg.tol) "catg" else "lcat"
            } else if( any( c("integer", "numeric") %in% klass ) ){
                if(n_val > real.tol) "real" else "catg"
            } else if("Date" %in% klass){
                "date"
            } else .unknown.type
    list(type = type,
         class = klass[1],
         levels = if(type %in% c("bnry", "catg", "lcat")) x_val else NA)
}

is.bnry <- function(x, bnry.list){
    if(length(bnry.list) == 0){
        FALSE
    } else {
        foo <- function(z) setequal(z, x)
        any(unlist(lapply(bnry.list, foo)))
    }
}
