##' functions for flextable output
##'
##' @name dable2flextable
##' @param dt object from dable
##' @param format logical; format the variables in dt?
##' @param count character; what entity is presentet in row header 'rows',
##'     'units' or 'weight'
##' @param kill character vector; columns to remove for presentation
##' @param grey character; which rows to make grey-ish in presentation
##' @param t2l logical (terms-to-label); replace terms with labels, if applicable
##' @param row.group logical or NULL; use the grouping in guide? If NULL this
##'     will be algorithmically decided
##' @param fontsize numerical vector of length 3 for fontsize of header, body
##'     and footnotes
##' @param insert.bottom character or logical; text to be placed under the
##'     table. If TRUE, attr2text will be used to create this text
##' @importFrom flextable flextable set_table_properties merge_h
##'     add_footer_lines footnote as_paragraph bg add_header_row
##'     fp_border_default bold italic fontsize delete_rows border_remove
##'     hline_bottom hline align
NULL



if(FALSE){


    d <- test_data()
    g <- dguide(d, id = "id", vtab = test_vtab(), stab = test_stab(), catg.tol = 15)

    format = TRUE
    count = "rows"
    kill = NULL
    grey = "term"
    t2l = TRUE
    row.group = NULL
    insert.bottom = TRUE
    fontsize = c(11, 11, 9)

    ## dt <- dreal(d)
    ## dt <- dreal(d, guide = g)
    dt <- dreal(d, guide = g, gtab = "gender")


    ## import

}

##' @rdname dable2flextable
##' @details dextable: turn simple descriptive tables into flextables
##' @export
dextable <- function(dt, format = TRUE, count = "rows", kill = NULL,
                     t2l = TRUE, grey = "term", row.group = NULL,
                     insert.bottom = TRUE, fontsize = c(11, 11, 9)){
    properties(format, class = "logical", length = 1, na.ok = FALSE)
    properties(count, class = "character", length = 1, na.ok = FALSE)
    one_of(count, set = c("rows", "units", "weight"))
    if(!is.null(kill)) properties(kill, class = "character", na.ok = FALSE)
    if(!is.null(grey)) properties(grey, class = "character", length = 1, na.ok = FALSE)
    properties(t2l, class = "logical", length = 1, na.ok = FALSE)
    if(!is.null(row.group)) properties(row.group, class = "logical", length = 1, na.ok = FALSE)
    properties(fontsize, class = c("numeric", "integer"), length = 3, na.ok = FALSE)

    A <- align_with_guide(d = dt)
    dt <- dt[A$order,]
    if(format) dt <- dable_format(dt, output = "neutral")
    if(is.null(row.group)) row.group <- length(unique(A$group.rle$values)) > 1

    BL <- attr(dt, "type") == "baseline"
    if(BL) stop("use other function")

    ib <- x_true_then_y_else_x(x = insert.bottom, y = attr2text(dt))

    use_grey <- FALSE
    gindex <- rep(FALSE, nrow(dt))
    if(!is.null(grey)){
        use_grey <- TRUE
        gindex <- get_grey(grey, dt, latex = FALSE)
    }

    if(t2l & "term" %in% names(dt)){
        dt$term <- decipher(dt$term, attr(dt, "guide"))
    }

    dt2 <- dt
    rg_index <- NULL
    if(row.group){
        Agr <- A$sorted$group
        ## insert extra rows where grouping names can fit
        dt2 <- insertNA(dt, Agr)
        gindex <- insertNA(gindex, Agr)
        gindex[is.na(gindex)] <- FALSE
        ## find index of added rows and put group names there
        An <- A$group.rle$lengths
        Av <- A$group.rle$values
        n <- length(An)
        rg_index <- c(0,cumsum(An[-n])) + 1:n
        m <- ncol(dt2)
        for(i in seq_along(rg_index)) dt2[rg_index[i], 1:m] <- rep(Av[i], m)
        ## note: we write the grouping name to all columns of the row - that way
        ## we can later use 'flextable::merge_h' to collapse the row to a single
        ## cell
    }


    DT <- dt2
    for(k in unique(kill)) DT <- dable_prune(DT, rm = k)

    nm_DT <- names(DT)
    nm_DT[nm_DT == "term"] <- " "
    n_col <- ncol(DT)

    Hh <- part2head(part = attr(DT, "part"),
                    cnm = nm_DT, ## names(DT),
                    bl = TRUE,
                    ntxt = attr2n(attributes(DT), count))

    ## need to rename DT: flextable will NOT allow duplicate column names
    names(DT) <- LETTERS[1:n_col]

    ## initiate flextable
    ft <- flextable::flextable(DT)
    ft <- flextable::set_table_properties(ft, opts_pdf = list(tabcolsep = 3),
                                          layout = "autofit")
    ft <- flextable::add_header_row(ft, values = Hh$h)
    ft <- flextable::add_header_row(ft, values = Hh$H)
    ft <- flextable::add_header_row(ft, top = FALSE, values = nm_DT)
    ft <- flextable::delete_rows(ft, i = 3, part = "head")
    ft <- flextable::merge_h(ft, i = 1:2, part = "head")

    RL <- rle(Hh$H)
    n <- length(RL$lengths)
    ii <- cumsum(RL$lengths) + 1
    if(n>1){
        for(i in 2:n){
            ft <- flextable::align(ft, i = 1:2, j = ii[i-1]:(ii[i]-1),
                                   part = "head", align = "center")
        }
    }

    ## make row group rows into a single cell
    if(row.group){
        for(i in rg_index){
            ft <- flextable::merge_h(ft, i = i)
            ft <- flextable::bold(ft, i = rg_index, j = 1)
        }
    }

    ## add text below
    ft <- flextable::add_footer_lines(ft, values = paste0(ib, ". "))
    ft <- flextable::bg(ft, i = which(gindex), bg = "#EFEFEF")
    ft <- flextable::bold(ft, i = 1, part = "header")
    ft <- flextable::fontsize(ft, size = fontsize[1], part = "header")
    ft <- flextable::fontsize(ft, size = fontsize[2], part = "body")
    ft <- flextable::fontsize(ft, size = fontsize[3], part = "footer")
    ft <- flextable::fontsize(ft, i = 2, size = min(fontsize), part = "head")
    ft <- flextable::italic(ft, i = setdiff(1:nrow(DT), rg_index), j = 1)
    ft <- flextable::border_remove(ft)
    ft <- flextable::hline_bottom(ft)
    ft <- flextable::hline(ft, i = 2, j = 2:n_col, part = "head")
    ft <- flextable::hline(ft, i = 3, j = 1:n_col, part = "head")
    ft

}


##' @rdname dable2flextable
##' @details blextable: turn baseline tables into flextables
##' @export
blextable <- function(dt,
                      format = TRUE,
                      count = "rows",
                      kill = "term",
                      grey = "term",
                      row.group = NULL,
                      fontsize = c(11, 11, 9),
                      insert.bottom = TRUE){

    ## get current parameters (to be reset at end of function)
    dparameters <- dpget_all()

    ## check arguments:
    logi1(format)
    char1(count)
    one_of(count, nm = "count", set = c("rows", "units", "weight"))
    logi1(row.group, null.ok = TRUE)
    properties(grey, class = c("character", "logical"), length = 1,
               na.ok = FALSE, null.ok = TRUE)
    properties(insert.bottom, class = c("logical", "character"), length = 1,
               na.ok = FALSE)

    ## order according to guide:
    A <- align_with_guide(d = dt)
    dt <- dt[A$order,]

    ## format:
    if(format) dt <- dable_format(dt, output = "flextable")

    ## if NULL set row.group to TRUE if more than 1 group
    if(is.null(row.group)) row.group <- length(unique(A$group.rle$values)) > 1

    ## display info on summary measures as text inserted below the table
    if("desc.info" %in% names(dt)){
        dt <- dable_prune(dt, rm = "desc.info", info = TRUE,
                          info.attr = "info", info.unique = TRUE,
                          split.unique = TRUE)
    }

    ## establish what text to put below table, if any
    ib <- x_true_then_y_else_x(x = insert.bottom, y = attr2text(dt))

    test.info <- NULL
    if("test.info" %in% names(dt)){
        test.info <- dt$test.info
        dt <- dable_prune(dt, rm = "test.info")
    }

    comp.info <- NULL
    comp.same <- FALSE
    if("comp.info" %in% names(dt)){
        comp.info <- dt$comp.info
        dt <- dable_prune(dt, rm = "comp.info")
        ci_u <- unique(comp.info[!is.na(comp.info)])
        if(length(ci_u) == 1) comp.same <- TRUE
        if(comp.same) ci_u <- if(ci_u == .stddiff()) .stddiff_abbr() else ci_u
    }


    ## grey
    if(!is.null(grey)){
        gindex <- get_grey(grey, dt, latex = FALSE)
    }

    ## group
    dt2 <- dt
    if(row.group){
        Agr <- A$sorted$group
        ## insert extra rows where grouping names can fit
        dt2 <- insertNA(dt, Agr)
        ## need to update test.info and gindex
        test.info <- insertNA(test.info, Agr)
        gindex <- insertNA(gindex, Agr)
        gindex[is.na(gindex)] <- FALSE
        ## find index of added rows and put group names there
        An <- A$group.rle$lengths
        Av <- A$group.rle$values
        n <- length(An)
        rg_index <- c(0,cumsum(An[-n])) + 1:n
        m <- ncol(dt2)
        for(i in seq_along(rg_index)) dt2[rg_index[i], 1:m] <- rep(Av[i], m)
        ## note: we write the grouping name to all columns of the row - that way
        ## we can later use 'flextable::merge_h' to collapse the row to a single
        ## cell
    }

    ## create a copy of input and polish it for output
    DT <- dt2
    for(k in unique(kill)) DT <- dable_prune(DT, rm = k)
    Hh <- part2head(part = attr(DT, "part"),
                    cnm = names(DT),
                    bl = TRUE,
                    ntxt = attr2n(attributes(dt), count),
                    comp.header = if(comp.same) ci_u else dpget("comp.header"))
    Hh$H[Hh$H==""] <- " " ## else flextable col_keys fails
    names(DT) <- Hh$H

    ## initiate flextable
    ft <- flextable::flextable(DT)
    ft <- flextable::set_table_properties(ft, opts_pdf = list(tabcolsep = 3),
                                          layout = "autofit")

    ## make row group rows into a single cell
    if(row.group){
        for(i in rg_index){
            ft <- flextable::merge_h(ft, i = i)
        }
    }

    ## add text below
    ft <- flextable::add_footer_lines(ft, values = paste0(ib, ". "))

    ## add footnotes on p-values
    if(!is.null(test.info)){
        tests <- unique(stats::na.omit(test.info))
        test_nm <- dpget("test.header")
        j <- which(names(DT) == "Test")
        dummy <- FALSE
        for(i in seq_along(tests)){ ## i = 1
            t <- tests[i]
            indx <- which(!is.na(DT$Test) & DT$Test != "" &
                          !is.na(test.info) & test.info == t)
            ft <- flextable::footnote(ft, i = indx, j = j,
                                      value = flextable::as_paragraph(t),
                                      ref_symbols = letters[i], inline = dummy)
            dummy <- TRUE
        }
    }

    ## add footnotes on p-values
    if(!comp.same){
        comps <- unique(stats::na.omit(comp.info))
        comp_nm <- dpget("comp.header")
        j <- which(names(DT) == comp_nm)
        dummy <- FALSE
        for(i in seq_along(comps)){ ## i = 1
            t <- comps[i]
            indx <- which(!is.na(DT[[comp_nm]]) & DT[[comp_nm]] != "" &
                          !is.na(comp.info) & comp.info == t)
            ft <- flextable::footnote(ft, i = indx, j = j,
                                      value = flextable::as_paragraph(t),
                                      ref_symbols = letters[i], inline = dummy)
            dummy <- TRUE
        }
    }


    ## note on indentation;
    ##  - docx: spaces work
    ##  - latex: '\quad\quad'
    ##  - html: &nbsp &nbsp (?)
    ## dable.indent <- dparam("indent")
    ## iindx <- which(grepl(paste0("^", dable.indent), dt$Variable))

    ## grey
    ft <- flextable::bg(ft, i = which(gindex), bg = "#EFEFEF")

    ## remains: insert line in head (n = ... )
    ft <- flextable::add_header_row(ft, values = Hh$h, top = FALSE)
    ft <- flextable::hline(ft, i = 1,
                           border = flextable::fp_border_default(width = 0),
                           part = "header")

    ft <- flextable::bold(ft, i = 1, part = "header")
    ft <- flextable::italic(ft, i = 2, part = "header")

    ft <- flextable::fontsize(ft, size = fontsize[1], part = "header")
    ft <- flextable::fontsize(ft, size = fontsize[2], part = "body")
    ft <- flextable::fontsize(ft, size = fontsize[3], part = "footer")

    ## ft <- bold(ft, i = 1, part = "head")
    ft <- flextable::bold(ft, i = rg_index, j = 1)
    ft <- flextable::italic(ft, i = setdiff(1:nrow(DT), rg_index), j = 1)

    ## print(ft, preview = "docx")
    ## print(ft, preview = "html")
    ## print(ft, preview = "pdf")

    dpset(dparameters)
    ft
}


index_shift <- function(i, x){
    n <- sum(x)
    if(any(i<1) || any(i>n)) stop("index shift out of bounds")
    shift <- as.integer(cut(i, breaks = c(1, cumsum(x)),
                            include.lowest = TRUE, right = TRUE)) - 1L
    as.integer(i) + shift
}

addNArow <- function(x){
    rbind(x[NA, ][1, ], x)
}
addNA <- function(x) c(NA, x)

insertNA <- function(x, f){
  if(!is.factor(f)) f <- factor(f, levels = unique(f))
  isDF <- is.data.frame(x)
  Fnc <- if(isDF) addNArow else addNA
  What <- if(isDF) rbind else c
  r <- do.call(what = What,
          args = lapply(
            X = split(x, f = f),
            FUN = Fnc
          ))
  if(isDF) rownames(r) <- NULL else names(r) <- NULL
  r
}
