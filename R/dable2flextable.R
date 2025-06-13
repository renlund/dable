##' turn dable into a flextable
##'
##' A flextable can be used to create several types of output
##' @param dt object from dable
##' @param dt object created by dable
##' @param format logical; format the variables in dt?
##' @param size character; what entity is presentet in row header 'rows',
##'     'units' or 'weight'
##' @param kill character vector; columns to remove for presentation
##' @param grey character; which rows to make grey-ish in presentation
##' @param row.group logical or NULL; use the grouping in guide? If NULL this
##'     will be algorithmically decided
##' @param fontsize numerical vector of length 3 for fontsize of header, body
##'     and footnotes
##' @param insert.bottom character or logical; text to be placed under the
##'     table. If TRUE, attr2text will be used to create this text
##' @importFrom flextable flextable set_table_properties merge_h
##'     add_footer_lines footnote as_paragraph bg add_header_row
##'     fp_border_default bold italic fontsize
##' @export
flexdable <- function(dt,
                      format = TRUE,
                      size = "rows",
                      kill = "term",
                      grey = "term",
                      row.group = NULL,
                      fontsize = c(11, 11, 9),
                      insert.bottom = TRUE){

    ## get current parameters (to be reset at end of function)
    dparameters <- dpget_all()

    ## check arguments:
    logi1(format)
    char1(size)
    one_of(size, nm = "size", set = c("rows", "units", "weight"))
    logi1(row.group, null.ok = TRUE)
    properties(grey, class = c("character", "logical"), length = 1,
               na.ok = FALSE, null.ok = TRUE)
    properties(insert.bottom, class = c("logical", "character"), length = 1,
               na.ok = FALSE)

    ## order according to guide:
    A <- align_with_guide(d = dt)
    dt <- dt[A$order,]

    ## format:
    dpset("output", value = "not latex")
    if(format) dt <- dable_format(dt)

    ## if NULL set row.group to TRUE if more than 1 group
    if(is.null(row.group)) row.group <- length(unique(A$group.rle$values)) > 1

    ## display info on summary measures as text inserted below the table
    if("Summary.info" %in% names(dt)){
        dt <- dable_prune(dt, rm = "Summary.info", info = TRUE,
                          info.attr = "info", info.unique = TRUE,
                          split.unique = TRUE)
    }

    ## establish what text to put below table, if any
    ib <- x_true_then_y_else_x(x = insert.bottom, y = attr2text(dt))

    p.info <- NULL
    if("p.info" %in% names(dt)){
        p.info <- dt$p.info
        dt <- dable_prune(dt, rm = "p.info")
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
        ## need to update p.info and gindex
        p.info <- insertNA(p.info, Agr)
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
                    ntxt = attr2n(attributes(dt), size))
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
    if(!is.null(p.info)){
        tests <- unique(stats::na.omit(p.info))
        j <- which(names(DT) == "Test")
        dummy <- FALSE
        for(i in seq_along(tests)){ ## i = 1
            t <- tests[i]
            indx <- which(!is.na(DT$Test) & DT$Test != "" &
                          !is.na(p.info) & p.info == t)
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
