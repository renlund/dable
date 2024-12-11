##' extract attributes
##'
##' extract tidy version of attributes from a dtable
##' @param dt a dable object
##' @export
dable_attr <- function(dt){
    a <- attributes(dt)
    n <- a$size
    w <- a$weight
    u <- a$units
    R <- data.frame(
        measure = c(
            if(!is.null(n))   "size"   else NULL,
            if(!is.null(w))   "weight" else NULL,
            if(!is.null(u))   "units"  else NULL
        ),
        total = dform.num.vec(c(n, w, u)),
        stringsAsFactors = FALSE
    )
    if(!is.null(a$gtab_size)){
        tmp <- rbind(size = a$gtab_size,
                     weight = dform.num.vec(a$gtab_weight),
                     units = a$gtab_units)
        Q <- as.data.frame(tmp)
        Q$measure <- rownames(tmp)
        rownames(Q) <- NULL
        merge(R, Q, sort = FALSE)
    } else {
        R
    }
}

##' get attributes as text
##'
##' extract attributes of dtable to a string (or character vector)
##' @param dt a dtable
##' @param attr which attributes to extract
##' @param units.name character, name of units
##' @param sep separator between attributes for output
##' @param vector return vector instead of a single string
##' @param rm.if.all exclude info on units if all unique
##' @export
attr2text <- function(dt, attr = c("size", "weight", "units", "info"),
                      units.name = NULL,
                      sep = ". ", vector = FALSE, rm.if.all = FALSE){
    if(is.null(units.name)) units.name <- dpget("units.name")
    if(nrow(dt) == 0) return("")
    da <- dable_attr(dt)
    gr <- setdiff(names(da), c("measure", "total"))
    n <- length(gr)
    foo <- function(m, g, text){
        if(!m %in% attr) return(NULL)
        N <- subset(da, da$measure == "size")$total
        x <- subset(da, da$measure == m)
        if(nrow(x)==0) return(NULL)
        a <- x$total
        if(m == "units" & a == N){
            if(rm.if.all) NULL else paste0("No duplicate ", units.name)
        } else {
            b <- if(g) as.character(x[1, 3:(2+n)]) else NULL
            c <- if(g){
                     paste0(" (", paste0(paste0(gr, ":", b),
                                         collapse = ", "), ")")
                 } else NULL
            paste0(text, " ", a, c)
        }
    }
    r <- c(
        foo(m = "size", g = n>0, text = "Rows:"),
        foo("weight", n>0, "Weight:"),
        foo("units", n>0, paste0(capitalize(units.name), ":")),
        if("info" %in% attr) attr(dt, "info") else NULL
    )
    if(vector) return(r)
    s <- c(rep(sep, length.out = max(length(r)-1, 0)), "")
    R <- paste(r, s, sep = "", collapse = "")
    if(R == "") NULL else R
}

capitalize <- function(s){
    unlist(lapply(strsplit(s, ""), function(x){ x[1] <- toupper(x[1]);
        paste0(x, collapse = "")}))
}

## - ##' determine sequence of colors
get_grey <- function(grey = NULL, x = NULL){
    color_vec <- c("","rowcolor[gray]{.9}")
    if(dparam("grey.first")) color_vec <- rev(color_vec)
    if(is.null(grey)){
        NULL
    } else if(is.logical(grey)){
        if(grey){
            rep(color_vec, length.out = nrow(x))
        } else NULL
    } else if(is.character(grey) & length(grey) == 1){
        grey_var <- x[[grey]]
        if(is.null(grey_var)){
            warning(paste0("string '", grey,
                           "' passed to 'grey' is not",
                           "recognised as a column"))
            NULL
        } else {
            if(class(grey_var) == "factor") grey_var <- as.character(grey_var)
            ns <- rle(grey_var)$lengths
            rep(rep(color_vec, length.out = length(ns)), ns)
        }
    } else {
        if(length(grey) == nrow(x)){
            if(class(grey) == "factor") grey <- as.character(grey)
            ns <- rle(grey)$lengths
            rep(rep(color_vec, length.out = length(ns)), ns)
        } else {
            warning(paste0("argument passed to 'grey' not equal in ",
                           "length to the rows of object"))
            NULL
        }
    }
}

##' formatting
##'
##' formatting of dtables and data.frames
##' @param dt a dtable or such
##' @export
dable_format <- function(dt){
    da <- attr(dt, "part")
    DT <- as.data.frame(dt)
    attr(DT, "dattr") <- NULL
    ## things with dattr 'test' should be formatted as p-values
    pi <- which(da %in% "test")
    classy <- unlist(lapply(DT, class))
    indx <- which(classy %in% c("numeric", "integer"))
    ps <- intersect(pi, indx)
    if(length(ps) > 0){
        pdots <- dots
        pdots[['maybe.p']] <- TRUE
        Arg <- c(list("X" = DT[, ps, drop = FALSE],
                      "FUN" = dform.num.vec),
                 pdots)
        DT[ps] <- do.call(what = lapply, args = Arg)
        indx <- setdiff(indx, ps)
    }
    ## dots[['maybe.p']] <- FALSE ## ? why not have this here ?
    Arg <- c(list('X' = DT[, indx, drop = FALSE],
                  'FUN' = dformat_num),
             dots)
    DT[indx] <- do.call(what = lapply, args = Arg)
    ## now everything else can be formatted as text
    Arg <- c(list('X' = DT,
                  'FUN' = dformat_text),
             dots)
    DT[] <- do.call(what = lapply, args = Arg)
    attributes(DT) <- attributes(dt)
    DT
}
