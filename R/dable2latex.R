##' LaTeX code for a dable
##'
##' A wrapper for Hmisc::latex trying to create nice LaTeX code to present a
##' dable
##' @param dt object created by dable
##' @param format logical; format the variables in dt?
##' @param count character; what entity is presented in row header 'rows',
##'     'units' or 'weight'
##' @param kill character vector; columns to remove for presentation
##' @param grey character; which rows to make grey-ish in presentation
##' @param t2l logical (terms-to-label); replace terms with labels, if applicable
##' @param file character; argument for Hmisc::latex
##' @param where character; argument for Hmisc::latex
##' @param rowname character; column name to use as rowname in Hmisc::latex
##' @param row.group logical or NULL; use the grouping in guide? If NULL this
##'     will be algorithmically decided
##' @param rgroup character; argument for Hmisc::latex
##' @param n.rgroup integer; argument for Hmisc::latex
##' @param insert.bottom character or logical; text to be placed under the
##'     table. If TRUE, attr2text will be used to create this text
##' @param ... arguments passed to Hmisc::latex
##' @importFrom Hmisc latex latexTranslate
##' @export
datex <- function(dt,
                  format = TRUE,
                  count = "rows",
                  kill = NULL,
                  grey = "term",
                  t2l = TRUE,
                  file = "",
                  where = "hbt",
                  rowname = NULL,
                  row.group = NULL,
                  rgroup = NULL,
                  n.rgroup = NULL,
                  insert.bottom = TRUE,
                  ...){
    Dots <- list(...) ## Dots <- list()
    properties(format, nm = "format", class = "logical", length = 1, na.ok = FALSE)
    properties(file, nm = "file", class = "character", length = 1, na.ok = FALSE)
    properties(where, nm = "where", class = "character", length = 1, na.ok = FALSE)
    properties(count, nm = "count", class = "character", length = 1, na.ok = FALSE)
    properties(t2l, nm = "t2l", class = "logical", length = 1, na.ok = FALSE)
    one_of(count, nm = "count", set = c("rows", "units", "weight"))
    properties(rowname, nm = "rowname", class = c("NULL", "character"),
               length = 0:1, na.ok = FALSE)
    Guide <- attr(dt, "guide")
    Term <- with(Guide, ifelse(type == "surv", label, term))
    a <- align(dt$term, template = Term, group = Guide$group,
               outgroup = dpget("vtab.group.name"))
    dt <- dt[a$order,]
    grey <- get_grey(grey, dt)
    if(format) dt <- dable_format(dt, output = "latex")
    Ntxt <- attr2n(attributes(dt), count)
    BL <- attr(dt, "type") == "baseline"
    if(BL){
        ## if("Variable" %in% names(dt)){
        ##     rowname <- "Variable"
        ##     kill <- c(kill, "term")
        ## } else rowname <- "term"
        ## if(is.null(row.group)) row.group <- length(unique(a$group.rle$values)) > 1
        ## if(bl.method == "standard"){
        ##     if(all(c("p.info", "p") %in% names(dt))){
        ##         dt <- dable_fnote(dt, info = "p.info", fn.var = "p",
        ##                           info.attr = "info", format = TRUE)
        ##     }
        ##     if("Summary.info" %in% names(dt)){
        ##         dt <- dable_prune(dt, rm = "Summary.info", info = TRUE,
        ##                           info.attr = "info", info.unique = TRUE,
        ##                           split.unique = TRUE)
        ##     }
        ## }
        w <- paste0("[datex] you've called datex with a baseline table - ",
                    "it is better to use blatex or blatex_default")
        message(w)
        blatex(bl = dt, .blatex.theme.strict = 0, ...)
        return(invisible(NULL))
    }
    if(!is.null(rowname)){
        one_of(rowname, nm = "rowname", set = names(dt))
        Rowname <- dt[[rowname]]
        dt <- dable_prune(dt, rm = rowname)
        dup <- duplicated(Rowname)
        if(any(dup)){
            mj <- mumbojumbo(n = sum(dup), prefix = "$\\phantom{", suffix = "}$")
            Rowname[which(dup)] <- paste0(Rowname[which(dup)], mj)
        }
    } else {
        rowname <- "term"
        Rowname <- if(t2l){
                       decipher(dt[[rowname]], Guide, flexible = FALSE)
                   } else dt[[rowname]]
    }
    rg_ind <- if(!is.null(rgroup)){
                  FALSE
              } else if(is.null(row.group)){
                  length(unique(a$group.rle$values)) > 1
              } else isTRUE(row.group)
    ib <- if(is.null(insert.bottom) | isTRUE(insert.bottom)){
              paste0("{\\small\\begin{center}\\emph{",
                     attr2text(dt),
                     "}\\end{center}}")
          } else if(is.character(insert.bottom)){
              insert.bottom
          } else NULL
    DT <- dt
    for(k in unique(c(kill, rowname))) DT <- dable_prune(DT, rm = k)
    Part <- attr(DT, "part")
    Hh <- part2head(Part, names(DT), BL, Ntxt)
    names(DT) <- Hh$h
    cg <- rle(Hh$H)
    if("term" %in% names(DT) & t2l){
        DT$term <- decipher(DT$term, Guide, flexible = FALSE)
    }
    Args <- list(object = DT,
                 file = file,
                 where = where,
                 title = "",
                 rowname = Rowname,
                 cgroup = cg$values,
                 n.cgroup = cg$lengths,
                 rgroup = if(rg_ind) a$group.rle$values else rgroup,
                 n.rgroup = if(rg_ind) a$group.rle$lengths else n.rgroup,
                 rownamesTexCmd = grey,
                 insert.bottom = ib)
    if(is.null(Dots$label)) Args$label <- paste0("tab:", mumbojumbo(1, m = 10))
    do.call(what = Hmisc::latex, args = c(Args, Dots))
}

c_list <- function(x, y, overwrite = TRUE){
    properties(x, class = "list")
    properties(y, class = "list")
    nm_x <- names(x)
    nm_y <- names(y)
    set <- if(overwrite) nm_y else setdiff(nm_y, nm_x)
    for(s in set) x[[s]] <- y[[s]]
    x
}
## c_list(list(x=1,y=2,z=3), list(z=-3,u=-4), overwrite = TRUE)
## c_list(list(x=1,y=2,z=3), list(z=-3,u=-4), overwrite = FALSE)

##' Default LaTeX code for a baseline table
##'
##' A wrapper for Hmisc::latex trying to create nice LaTeX code to present a
##' baseline dable
##' @param bl object created by dable
##' @param format logical; format the variables in bl?
##' @param count character; what entity is presentet in row header 'rows',
##'     'units' or 'weight'
##' @param kill character vector; columns to remove for presentation
##' @param grey character; which rows to make grey-ish in presentation
##' @param file character; argument for Hmisc::latex
##' @param where character; argument for Hmisc::latex
##' @param row.group logical or NULL; use the grouping in guide? If NULL this
##'     will be algorithmically decided
##' @param insert.bottom character or logical; text to be placed under the
##'     table. If TRUE, attr2text will be used to create this text
##' @param ... arguments passed to Hmisc::latex
##' @export
blatex_default <- function(bl,
                           format = TRUE,
                           count = "rows",
                           kill = "term",
                           grey = "term",
                           file = "",
                           where = "hbt",
                           row.group = NULL,
                           insert.bottom = TRUE,
                           ...){
    Dots <- list(...) ## Dots <- list()

    ## check arguments:
    logi1(format)
    char1(list('file' = file, 'where' = where, 'count' = count))
    one_of(count, nm = "count", set = c("rows", "units", "weight"))
    logi1(row.group, null.ok = TRUE)
    char1(grey, null.ok = TRUE)
    properties(insert.bottom, class = c("logical", "character"), length = 1,
               na.ok = FALSE)

    ## order according to guide:
    A <- align_with_guide(d = bl)
    bl <- bl[A$order,]

    ## format:
    if(format) bl <- dable_format(bl, output = "latex")

    ## if NULL set row.group to TRUE if more than 1 group
    if(is.null(row.group)) row.group <- length(unique(A$group.rle$values)) > 1

    ## display p-value info as footnotes on the values
    if(all(c("p.info", "p") %in% names(bl))){
        bl <- dable_fnote(bl, info = "p.info", fn.var = "p",
                          info.attr = "info", format = TRUE)
    }

    ## display info on summary measures as text inserted below the table
    if("Summary.info" %in% names(bl)){
        bl <- dable_prune(bl, rm = "Summary.info", info = TRUE,
                          info.attr = "info", info.unique = TRUE,
                          split.unique = TRUE)
    }

    ## the rowname argument is only necessary when rgroups are used, but easier
    ## to always use them in this function. They have to be unique, so add some
    ## hidden noise if not:
    Rowname <- unique_latex_rownames(bl[["Variable"]])
    bl <- dable_prune(bl, rm = "Variable")

    ## establish what text to put below table, if any
    ib <- x_true_then_y_else_x(x = insert.bottom,
                               y = attr2text(bl),
                               latex = TRUE)

    ## create a copy of input and polish it for output
    BL <- bl
    for(k in unique(kill)) BL <- dable_prune(BL, rm = k)
    Hh <- part2head(part = attr(BL, "part"),
                    cnm = names(BL),
                    bl = TRUE,
                    ntxt = attr2n(attributes(bl), count))
    names(BL) <- Hh$h
    cg <- rle(Hh$H)

    ## create list of arguments
    Args <- list(object = BL,
                 file = file,
                 where = where,
                 title = "",
                 rowname = Rowname,
                 cgroup = cg$values,
                 n.cgroup = cg$lengths,
                 rgroup = if(row.group) A$group.rle$values else NULL,
                 n.rgroup = if(row.group) A$group.rle$lengths else NULL,
                 rownamesTexCmd = get_grey(grey, bl),
                 insert.bottom = ib)

    ## if no label given, create one
    if(is.null(Dots$label)) Args$label <- paste0("tab:", mumbojumbo(1, m = 10))

    ## function call
    do.call(what = Hmisc::latex, args = c(Args, Dots))
}

datex.bl <- blatex_default

##' LaTeX code for themed baseline tables
##'
##' LaTeX code for baseline table created with \code{baseline}
##' @param bl object created with baseline
##' @param ... arguments passed
##' @param .blatex.theme.strict numeric (mostly internal); how upset this
##'     function will be if the 'theme' attribute is missing from the
##'     bl-argument before passing the problem along to blatex_default: 0 = not
##'     all all, 1 = somewhat, cause a warning, 2 = very, cause an error
##' @export
blatex <- function(bl, ..., .blatex.theme.strict = 1){
    properties(.blatex.theme.strict, class = c("numeric", "integer"),
               length = 1, na.ok = FALSE)
    one_of(.blatex.theme.strict, set = 0:2)
    theme <- attr(bl, "theme")
    pass.dots <- c(list(bl = bl), list(...))
    if(is.null(theme)){
        s <- paste0("input 'bl' lacks the 'theme' attribute - ",
                    "you probably want to use the 'baseline' ",
                    "function to create bl")
        if(.blatex.theme.strict == 1) warning(s)
        if(.blatex.theme.strict == 2) stop(s)
        do.call(what = "blatex_default", args = pass.dots)
    } else {
        n <- theme$desc
        do.call(what = paste0("blatex", n),
                args = pass.dots)
    }
}

##' @rdname blatex
##' @details blatex0: alias for blatex_default
##' @export
blatex0 <- blatex_default

##' @rdname blatex
##' @details blatex1: alias for blatex_default
##' @export
blatex1 <- blatex_default

##' @rdname blatex
##' @details blatex2: code for baseline theme 2 (experimental)
##' @export
blatex2 <- function(bl, ...){
    g <- attr(bl, "guide")
    key <- setNames(g$label, nm = g$term)
    tmp <- setNames(paste0("XK1", key, "XK2"), nm = key)
    x1 <- decipher(bl$Variable, tmp, within = TRUE)
    x2 <- sub("XK1", "\\\\textbf{\\\\emph{", x1)
    x3 <- sub("XK2", "}}", x2)
    bl$Variable <- x3
    do.call(what = "blatex_default",
            args = c(list(bl = bl), list(...)))
}


align_with_guide <- function(d){
    Guide <- attr(d, "guide")
    Term <- with(Guide, ifelse(type == "surv", label, term))
    align(x = d$term,
          template = Term,
          group = Guide$group,
          outgroup = dpget("vtab.group.name"))
}

unique_latex_rownames <- function(x){
    dup <- duplicated(x)
    if(any(dup)){
        mj <- mumbojumbo(n = sum(dup), prefix = "$\\phantom{", suffix = "}$")
        x[which(dup)] <- paste0(x[which(dup)], mj)
    }
    x
}

x_true_then_y_else_x <- function(x, y, latex = FALSE){
    txt <- if(is.logical(x)){
               if(isTRUE(x)) y else NULL
           } else as.character(x)
    if(latex){
        if(!is.null(txt)){
            paste0("{\\small\\begin{center}\\emph{", txt, "}\\end{center}}")
        } else NULL
    } else txt
}


##' extract attributes
##'
##' extract tidy version of attributes from a dtable
##' @param dt a dable object
##' @export
dable_attr <- function(dt){
    a <- attributes(dt)
    n <- a$rows
    w <- a$weight
    u <- a$units
    R <- data.frame(
        measure = c(
            if(!is.null(n))   "rows"   else NULL,
            if(!is.null(w))   "weight" else NULL,
            if(!is.null(u))   "units"  else NULL
        ),
        total = dform.num.vec(c(n, w, u)),
        stringsAsFactors = FALSE
    )
    if(!is.null(a$gtab_rows)){
        gw <- a$gtab_weight
        tmp <- rbind(rows = a$gtab_rows,
                     ## what if weight is NULL here ?
                     weight = if(!is.null(gw)) dform.num.vec(gw) else NULL,
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
attr2text <- function(dt, attr = c("rows", "weight", "units", "info"),
                      units.name = NULL,
                      sep = ". ", vector = FALSE, rm.if.all = FALSE){
    if(is.null(units.name)) units.name <- dpget("units.name")
    if(nrow(dt) == 0) return("")
    da <- dable_attr(dt)
    gr <- setdiff(names(da), c("measure", "total"))
    n <- length(gr)
    foo <- function(m, g, text){
        if(!m %in% attr) return(NULL)
        N <- subset(da, da$measure == "rows")$total
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
        foo(m = "rows", g = n>0, text = "Rows:"),
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
get_grey <- function(grey = NULL, x = NULL, latex = TRUE){
    color_vec <- if(latex) c("","rowcolor[gray]{.9}") else c(FALSE, TRUE)
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
            if("factor" %in% class(grey_var)) grey_var <- as.character(grey_var)
            ns <- rle(grey_var)$lengths
            rep(rep(color_vec, length.out = length(ns)), ns)
        }
    } else {
        if(length(grey) == nrow(x)){
            if("factor" %in% class(grey)) grey <- as.character(grey)
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
##' formatting of dables and data.frames
##' @param dt a dable or such
##' @param output output format
##' @export
dable_format <- function(dt, output = dpget("output")){
    da <- attr(dt, "part")
    DT <- as.data.frame(dt)
    ## attr(DT, "dattr") <- NULL
    ## things with dattr 'test' should be formatted as p-values
    pi <- which(grepl("^test", da))
    classy <- unlist(lapply(DT, class))
    indx <- which(classy %in% c("numeric", "integer"))
    ps <- intersect(pi, indx)
    if(length(ps) > 0){
        Arg <- list("X" = DT[, ps, drop = FALSE],
                    "FUN" = dform.num.vec,
                    "p" = TRUE)
        DT[ps] <- do.call(what = lapply, args = Arg)
        indx <- setdiff(indx, ps)
    }
    Arg <- list('X' = DT[, indx, drop = FALSE],
                'FUN' = dform.num.vec)
    DT[indx] <- do.call(what = lapply, args = Arg)
    ## now everything else can be formatted as text
    Arg <- list('X' = DT,
                'FUN' = dform.text.vec,
                'output' = output)
    DT[] <- do.call(what = lapply, args = Arg)
    attributes(DT) <- attributes(dt)
    DT
}

##' prune dable
##'
##' remove columns by name or index
##' @title prune dable
##' @param x object
##' @param rm index or variable name to remove
##' @param info store the discarded information in attributes?
##' @param info.attr name of attribute to store discarded info (if \code{info = TRUE})
##' @param info.unique store only unique info (if \code{info = TRUE})
##' @param split.unique if \code{unique.info = TRUE}, also split into individual
##'     sentences before determining uniqueness?
##' @export
dable_prune <- function(x, rm = NULL, info = FALSE,
                         info.attr = "info", info.unique = TRUE,
                         split.unique = TRUE){
    if(is.null(rm) | !(rm %in% names(x))) return(x)
    d <- attr(x, "part")
    old_attr <- attributes(x)
    if(is.character(rm)){
        rm <- which(names(x) %in% rm)
    }
    ## if(!all(d[rm] == "meta")){
    ##     s <- paste0("Pruning non-meta stuff from dable object.")
    ##     message(s)
    ## }
    if(info){
        infot <- unlist(lapply(x[,rm], identity))
        if(info.unique){
            if(split.unique){
                tmp <- unlist(strsplit(as.character(infot),
                                       split = ".", fixed = TRUE))
                infot <- gsub("(^ )|( $)", "", tmp)
            }
            infot <- unique(infot)
        }
    }
    if(length(rm) > 0){
        r <- x[,-rm, drop = FALSE]
        names(r) <- names(x)[-rm]
        attr(r, "part") <- attr(x, "part")[-rm]
        attributes(r) <- concatenate_attributes(r, old_attr)
        if(info) attr(r, info.attr) <- c(attr(r, info.attr), infot)
    } else r <- x
    r
}

#-#' concatenate attributes
#-#' @param x atributes
#-#' @param a adders
concatenate_attributes <- function(x, a){
    haz <- attributes(x)
    add <- setdiff(names(a), names(haz))
    c(haz, a[add])
}

##' create (latex) footnotes from table variables
##'
##' make \code{unique(variable)} a part of attribute 'info' and
##'     place a footnote on another variable
##' @param dt a dable
##' @param info variable with the footnote info
##' @param fn.var variable to get footnotes
##' @param info.attr name of attribute to store info in
##' @param format format the dable?
##' @importFrom stats na.omit
##' @export
dable_fnote <- function(dt, info, fn.var,
                         info.attr = "info",
                         format = FALSE){
    if(length(info) != 1 | length(fn.var) != 1){
        stop("want 'info' and 'fn.var' to be length 1")
    }
    foo <- function(x){
        if(is.character(x)){
            if(sum(names(dt) %in% x) > 1){
                stop(paste0("'", x, "' identifies more than one variable"))
            }
        }
    }
    foo(info)
    foo(fn.var)
    if(format){
        if(class(dt[[fn.var]]) %in% c("numeric", "integer")){
            dt[[fn.var]] <- do.call(dform.num.vec,
                                    list(dt[[fn.var]]))
        } else {
            dt[[fn.var]] <- do.call(dform.text.vec,
                                    list(dt[[fn.var]]))
        }
    }
    infot   <- unique(as.character(stats::na.omit(unlist(dt[[info]]))))
    i.infot <- as.numeric(factor(dt[[info]], levels = infot))
    symb <- latex_symbols(n = max(i.infot, na.rm = TRUE),
                          pre = "$\\phantom{.}^{\\", suff = "}$")
    symb2 <- latex_symbols(n = max(i.infot, na.rm = TRUE),
                           pre = "$^{\\", suff = "}$")
    sym.infot <- paste0(symb, infot)
    attr(dt, info.attr) <- c(attr(dt, info.attr), sym.infot)
    fn_var <- dt[[fn.var]]
    new_var <- paste0(id_or_empty(fn_var), id_or_empty(symb2[i.infot]))
    new_var[is.na(fn_var) | fn_var == ""] <- ""
    dt[[fn.var]] <- new_var
    dable_prune(dt, rm = info)
}

#-#' --- itself or empty string if NA
id_or_empty <- function(s) ifelse(is.na(s), "", s)

#-#'  -- a variable to a 'footnote'
latex_symbols <- function(n, pre = "\\", suff  = "", start = 1){
    symb <- c("bot", "forall", "flat", "sharp", "top", "S", "bigstar", "Join",
               "clubsuit", "diamondsuit", "spadesuit",  "heartsuit",
               "dagger", "ast", "star", "circ", "ddagger", "bullet")
    greekl <- c("alpha", "beta", "gamma", "delta", "epsilon", "varepsilon",
                "zeta", "eta", "theta", "vartheta", "iota", "kappa", "lambda",
                "mu", "nu", "xi", "pi", "varpi", "rho", "varrho", "sigma",
                "varsigma", "tau", "upsilon", "phi", "varphi", "chi",
                "psi", "omega")
    greeku <- c("Gamma","Delta","Theta","Lambda","Xi","Pi","Sigma",
                "Upsilon","Phi","Psi","Omega")
    S <- c(symb, greekl, greeku)
    N <- length(S)
    if(start<1) stop(paste0("need 1<'start'<", N))
    if( (start-1+n) > N) stop("there are not enough latex symbols")
    paste0(pre, S[(start):(start - 1 + n)], suff)
}

attr2n <- function(attr, n){
    one_of(x = n, nm = "'n'", set = c("rows", "units", "weight"))
    Nnm <- if(!is.null(names(n))){
               names(n)
           } else {
               c(rows = "n", units = "u", weight = "w")[n]
           }
    test <- length(attr[["gtab_rows"]])
    if(test == 0){
        N <- attr[[n]]
        if(!is.null(N)){
            stats::setNames(object = paste(Nnm, "=", N),
                            nm = dpget("gtab.group.name"))
        } else {
            w <- paste0("[attr2n] no ", n, " info detected")
            warning(w)
            stats::setNames(object = paste("n =", attr[["rows"]]),
                            nm = dpget("gtab.group.name"))
        }
    } else {
        Ng <- attr[[paste0("gtab_", n)]]
        if(!is.null(Ng)){
            stats::setNames(paste(Nnm, "=", Ng), nm = names(Ng))
        } else {
            w <- paste0("[attr2n] no ", n, " info detected")
            warning(w)
            stats::setNames(paste("n =", attr[["gtab_rows"]]), nm = names(Ng))
        }
    }
}


part2head <- function(part, cnm, bl, ntxt){
    i_m <- which(grepl("^meta", part))
    i_d <- which(grepl("^desc", part))
    ## g_d <- sub("(desc:)(.*)", "\\2", part[i_d])
    i_c <- which(grepl("^comp", part))
    i_t <- which(grepl("^test", part))
    tmp <- gsub("(^meta)|(^desc:)|(^comp:)|(^test:)", "", part)
    H <- rep("", length(tmp))
    h <- cnm
    H[i_d] <- tmp[i_d]
    h[c(i_c, i_t)] <- tmp[c(i_c, i_t)]
    if(bl){
        H[i_c] <- dpget("comp.header")
        H[i_t] <- dpget("test.header")
        if(all(H[i_d] %in% names(ntxt))) h[i_d] <- ntxt[H[i_d]]
    } else {
        H[i_c] <- cnm[i_c]
        H[i_t] <- cnm[i_t]
    }
    list(H = H, h = h)
}

mumbojumbo <- function(n, prefix = "", suffix = "",
                       m = NULL, try.unique = TRUE){
    if(n==0) return(character(0))
    set1 <- c(letters, LETTERS)
    if(is.null(m)){
        z <- c(52,52^2,52^3,52^4,52^5)
        m <- which(n / z < 0.2)[1] ## this is not a good uniform boundary
        if(is.na(m)) stop("[mumbojumbo] woha!")
    }
    R <- rep(NA_character_, n)
    dummy <- 1
    threshold <- if (try.unique) 100 else 0
    while ((all(is.na(R)) | any(duplicated(R))) & dummy < threshold) {
        for (k in 1:n) {
            R[k] <- paste(sample(set1, size = m, replace = TRUE),
                collapse = "")
        }
        dummy <- dummy + 1
    }
    if (dummy == threshold & try.unique)
        warning("[mumbojumbo] uniqueness failed!")
    paste0(prefix, R, suffix)
}
