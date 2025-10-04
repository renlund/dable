## ----"setup", cache = FALSE, echo = FALSE, include = FALSE--------------------
library(knitr)
library(dable)
## devtools::load_all()
opts_chunk$set(include = TRUE,
               echo = TRUE,
               cache = FALSE)
opts_knit$set(eval.after = c('fig.cap', 'fig.scap'))
if(FALSE){
    setwd('c:/Users/henre358/My Drive/R/P_package/dable/vignettes')
    knitr::knit2pdf("dable-manual.rnw", clean = TRUE)
    shell.exec("dable-manual.pdf")
}
dpset_defaults(overwrite = TRUE, style = "latex") ## also in chunk below

## ----"set-latex-defaults"-----------------------------------------------------
dpset_defaults(overwrite = TRUE, style = "latex")

## ----"map", include = TRUE, echo = FALSE, fig.scap = scap, fig.cap = cap------
old <- par()$mar
par(mar = c(.1,.1,.1,.1))

NM <- c(
    "integer\nnumeric",           #1
    "factor\ncharacter\nlogical", #2
    "Date",                       #3
    "surv",                       #4
    "real",                       #5
    "bnry",                       #6
    "catg",                       #7
    "lcat",                       #8
    "date"                       #9
)
## transitions:
M <- matrix(data = 0, nrow = 9, ncol = 9)
M[4,1] <- "names of terms"
M[5,1] <- ""
M[6,1] <- ""
M[7,1] <- "few unique values"
M[6,2] <- "2 values*"
M[7,2] <- ""
M[8,2] <- "numerous unique values"
M[9,3] <- ""
## coordinates of boxes:
x1 <- .1
x2 <- .9
p <- matrix(data = 0, nrow = 9, ncol = 2)
a <- -.05; b <- 1.33
p[1,] <- c(x1, a + b * .6)
p[4,] <- c(x2, a + b * .75)
p[5,] <- c(x2, a + b * .6)
p[2,] <- c(x1, a + b * .35)
p[6,] <- c(x2, a + b * .45)
p[7,] <- c(x2, a + b * .35)
p[8,] <- c(x2, a + b * .25)
p[3,] <- c(x1, a + b * .1)
p[9,] <- c(x2, a + b * .1)
## relative position of arrows on connecting lines:
ap <- matrix(data = NA, nrow = 9, ncol = 9)
ap[4,1] <- .5
ap[5,1] <- .3
ap[6,1] <- .69
ap[7,1] <- .3
ap[6,2] <- .69
ap[7,2] <- .3
ap[8,2] <- .4
ap[9,3] <- .3
## curvature of connecting lines:
cu <- matrix(data = 0, nrow = 9, ncol = 9)
cu[4,1] <- -.1
cu[6,1] <- .1
cu[7,1] <- .1
cu[6,2] <- -.1
cu[8,2] <- .1
diagram::plotmat(
             M,
             pos = p,
             name = NM,
             curve = cu,
             arr.type = "simple",
             arr.pos = ap,
             box.type = "rect",
             box.size = rep(c(0.1, 0.06), c(3,6)),
             box.prop = .7,
             latex = TRUE
         )

par(mar = old)
rm(list=ls())
scap <- paste0("Possible mappings.")
cap <- paste0(scap,
              " For both numeric/integer and factor/character/logical, if ",
              "there are exactly 2 unique values corresponding to one of the ",
              "elements  of \\texttt{bnry.list}, they get type \\texttt{bnry}. ",
              " If number of nunique values of a numeric/integer variable ",
              "is \\texttt{real.tol} or less, the type is \\texttt{catg}. ",
              " If number of unique values of a factor/character variable ",
              "is strictly greater than  \\texttt{catg.tol}, ",
              "the type is \\texttt{lcat}. The \\texttt{surv} type is ",
              "determined through given names or pattern matching. ")

## ----"default-bnry.list"------------------------------------------------------
dpget("real.tol")
dpget("catg.tol")
dpget("bnry.list")

## ----"stab-example"-----------------------------------------------------------
test_stab()

## ----"default-affix"----------------------------------------------------------
dpget("surv.prefix") ## is prefix used? (else suffix)
dpget("surv.affix") ## what is the affix used?

## ----"test-data"--------------------------------------------------------------
d <- test_data()
str(d)

## ----"guide-default"----------------------------------------------------------
(g <- dguide(d))

## ----"vtab.group.name"--------------------------------------------------------
dpget("vtab.group.name")

## ----"stab.group.name"--------------------------------------------------------
dpget("stab.group.name")

## ----"guide-manual-change", eval = FALSE--------------------------------------
# g$type[g$term == "pid"] <- "lcat"

## ----"guide-parameter-change"-------------------------------------------------
subset(dguide(d, catg.tol = 10), subset = term == "pid") ## type now lcat

## ----"vtab-stab"--------------------------------------------------------------
(vt <- test_vtab())
(st <- test_stab())

## ----"add-grouping-to-stab"---------------------------------------------------
st$group <- "Outcomes"

## ----"guide-with-parameters"--------------------------------------------------
(g <- dguide(d, id = c("id", "pid"), vtab = vt, stab = st))

## ----"baseline-table-a-first-look", results = 'asis', message = FALSE, warning = FALSE----
b <- baseline(d, guide = g, gtab = "gender", part = c(T,T,T))
blatex(b, where = "!ht", caption = "Default baseline table",
       label = "tab:default-bsl", size = "small")

## ----"real-with-dable"--------------------------------------------------------
dable(d, type = "real", guide = g)

## ----"default-type-tables"----------------------------------------------------
dreal(d, guide = g)
dcatg(d, guide = g)
dbnry(d, guide = g)
dlcat(d, guide = g)
ddate(d, guide = g)
dsurv(d, guide = g)

## ----"default-type-tables-extra-arg"------------------------------------------
ddate(d, guide = g, date.format = "%Y-%m-%d")
dsurv(d, guide = g, time.unit = 365.25)

## ----"stratification"---------------------------------------------------------
(dt <- dreal(d, guide = g, gtab = "gender"))

## ----"attr-part"--------------------------------------------------------------
attr(dt, "part")

## ----"datex", results = 'asis'------------------------------------------------
datex(dt, label = "tab:datex",
      caption = "Default \\LaTeX\\, output for descriptive table.")

## ----"datex-with-less", results = 'asis'--------------------------------------
datex(dt, row.group = FALSE, lab = FALSE, label = "tab:datex-less",
      caption = "Descriptive table using less meta data.")

## ----"gtab_maker"-------------------------------------------------------------
gt <- gtab_maker("area", data = d, all = TRUE)
head(gt)

## ----"gtab-part"--------------------------------------------------------------
(dt <- dlcat(d, guide = g, gtab = gt))
attr(dt, "part") ## ordered as gtab

## ----"gtab-part-manipulate"---------------------------------------------------
dt <- dlcat(d, guide = g, gtab = gt, part = list("desc" = c(3,1,2)))
attr(dt, "part") ## order now permutated

## ----"desc-last"--------------------------------------------------------------
dt <- dlcat(d, guide = g, gtab = gt, part = "last")
attr(dt, "part") ## now only last group included

## ----"change-describer"-------------------------------------------------------
dreal(d, fnc = list(desc = "min_max"))

## ----"see-default"------------------------------------------------------------
dpget("real.desc")
dpget("real.comp")
dpget("real.test")

## ----"change-default", eval = FALSE-------------------------------------------
# dpset("real.desc", "name_of_new_function") ## no eval

## ----"weight", results = 'hide'-----------------------------------------------
dreal(d, guide = g, weight = "importance")

## ----"desc-and-comp", results = 'asis', warning = FALSE-----------------------
dt <- dreal(d, guide = g, gtab = "gender", part = c(TRUE, TRUE))
datex(dt, label = "tab:desc-n-comp", row.group = FALSE,
      caption = "Descriptive table with comparison.")

## ----"desc-and-comp-2", results = 'asis', warning = FALSE---------------------
dt <- dreal(d, guide = g, gtab = "gender",
            part = list(TRUE, list(c(2,1))))
datex(dt, label = "tab:desc-n-comp-2", row.group = FALSE,
      caption = "Descriptive table with comparison order changed.")

## ----"comp-across", results = 'asis'------------------------------------------
dt <- dbnry(d, guide = g, gtab = "region",
            part = list(FALSE, "across"))
datex(dt, label = "tab:across", row.group = FALSE,
      caption = "Comparison \\emph{across} the groups.")

## ----"comp-adjacent", results = 'asis'----------------------------------------
dt <- dbnry(d, guide = g, gtab = "region",
            part = list(FALSE, "adjacent"))
datex(dt, label = "tab:adjacent", row.group = FALSE,
      caption = "Comparison between \\emph{adjacent} groups.")

## ----"comp-list", results = 'asis'--------------------------------------------
dt <- dbnry(d, guide = g, gtab = "region",
            part = list(FALSE, list(c(1,3), c(5,2))))
datex(dt, label = "tab:comp-list", row.group = FALSE,
      caption = "Comparison between chosen groups.")

## ----"desc-test", results = 'asis', warning = FALSE, error = FALSE, message = FALSE----
dt <- dreal(d, guide = g, gtab = "country", part = c(TRUE,FALSE,TRUE))
datex(dt, label = "tab:desc-test", row.group = FALSE,
      caption = "Descriptive table with test.")

## ----"desc-test-2", results = 'asis', warning = FALSE, error = FALSE, message = FALSE----
dt <- dreal(d, guide = g, gtab = "country", part = list(TRUE,FALSE,c(2,3)))
datex(dt, label = "tab:desc-test-2", row.group = FALSE,
      caption = "Descriptive table with selected testing.")

## ----"my-describer"-----------------------------------------------------------
IQR <- function(x, ...){
    x <- quantile(x, probs = c(.25, .75), na.rm = TRUE)
    data.frame(IQR = x[2] - x[1])
}
dreal(d, fnc = list(desc = "IQR")) |> head(n = 3)

## ----"point-to-mean"----------------------------------------------------------
dreal(d, fnc = list(desc = "mean"), na.rm = TRUE) |> head(n = 3)

## ----"point-to-myMean"--------------------------------------------------------
myMean <- function(x, ...) mean(x, na.rm = TRUE, ...)
attr(myMean, "label") <- "The Mean Value"
dreal(d, fnc = list(desc = "myMean")) |> head(n = 3)

## ----"my-meta"----------------------------------------------------------------
myBnry <- function(x, ...){
    if(!is.factor(x)) x <- factor(x)
    data.frame(Level = levels(x),
               Count = as.integer(table(x)))
}
attr(myBnry, "meta") <- "Level"
dbnry(d, gtab = "gender", fnc = list(desc = "myBnry"))

## ----"surv-desc"--------------------------------------------------------------
Foo <- function(time, event, my.unit.of.time = 'time units', ... ){
    data.frame(Info = sprintf(paste0("%s events in %.2f ", my.unit.of.time),
                              sum(event), sum(time)))
}
dsurv(d, fnc = list(desc = "Foo"))
dsurv(d, fnc = list(desc = "Foo"), my.unit.of.time = "days")

## ----"my-comp", warning = FALSE-----------------------------------------------
Delta <- function(x, g, ...){
    if(!is.factor(g)) g <- factor(g)
    x_i <- g == levels(g)[1]
    y_i <- g == levels(g)[2]
    data.frame(MinDelta = min(x[x_i], na.rm = TRUE) - min(x[y_i], na.rm = TRUE),
               MaxDelta = max(x[x_i], na.rm = TRUE) - max(x[y_i], na.rm = TRUE))
}
dreal(d, gtab = "gender", part = c(F,T), fnc = list(desc=NULL, comp = "Delta"))

## ----"my-test", warning = FALSE-----------------------------------------------
(dt <- dreal(d, gtab = "gender", part = c(F,T,T),
            fnc = list(desc=NULL, comp = NULL, test = "Delta")))

## ----"part-keeps-track"-------------------------------------------------------
attr(dt, "part")

## ----"HR"---------------------------------------------------------------------
HR <- function(time, event, g, weight = NULL, ...){
    if(!is.factor(g)) g <- factor(g)
    mod <- survival::coxph(survival::Surv(time, event) ~ g, weight = weight)
    data.frame(HR = as.numeric(exp(mod$coefficients)[1]))
}
dsurv(d, gtab = "gender", part = c(F,T), fnc = list(desc=NULL, comp = "HR"))

## ----"the-dots"---------------------------------------------------------------
theDots <- function(x, ...){
    dots <- list(...)
    foo <- function(z) if(is.null(z)) "NULL" else z
    data.frame(name = names(dots),
               value = unlist(lapply(dots, foo)))
}
dlcat(d, guide = g, fnc = list(desc = "theDots"),
      pass_this_along = "this was passed by the user")

## ----"silly"------------------------------------------------------------------
silly <- function(x, ...){
    dots <- list(...)
    r <- paste0("'I'm %s (a %s type) but I prefer my to use my label: %s'")
    data.frame(Presentation = sprintf(r, dots$.term, dots$.type, dots$.label))
}
dlcat(d, guide = g, fnc = list(desc = "silly"))

## ----"baseline-theme-0", results = 'asis', message = FALSE, warning = FALSE----

sel <- subset(g, !group %in% c("Measures etc.", "Outcomes", "Covariates"))$term
d2 <- d[, sel]
g2 <- dguide(d2, id = c("id", "pid"), vtab = vt,
             elim.set = c("end", "region", "area"))
b <- baseline(d2, theme = 0, guide = g2, gtab = "gender", part = c(T,T,T))
blatex(b, caption = "Baseline table theme 0 (default)",
       label = "tab:bsl-0", size = "small")

## ----"baseline-theme-1", results = 'asis', message = FALSE, warning = FALSE----
b <- baseline(d2, theme = 1, guide = g2, gtab = "gender", part = c(T,T,T))
blatex(b, caption = "Baseline table theme 1.",
       label = "tab:bsl-1", size = "small")

## ----"baseline-theme-2", results = 'asis', message = FALSE, warning = FALSE----
b <- baseline(d2, theme = 2, guide = g2, gtab = "gender", part = c(T,T,T))
blatex(b, caption = "Baseline table theme 2.",
       label = "tab:bsl-2", size = "small")

## ----"adjusted-baseline-0", results = 'asis', message = FALSE-----------------
gt <- gtab_maker("gender", data = d2, all = TRUE, all.first = TRUE)
b <- baseline(d2, theme = 0, guide = g2, gtab = gt,
                   part = list(T, F, c(2,3)))
blatex(b, label = "tab:adj-bsl", caption = "Adjusted baseline table")

## ----"test-default-describers"------------------------------------------------
dable(d, type = "real", guide = g)
dable(d, type = "catg", guide = g)
dable(d, type = "bnry", guide = g)
dable(d, type = "date", guide = g)
dable(d, type = "surv", guide = g)
dable(d, type = "lcat", guide = g)

## ----"test-default-comparers"-------------------------------------------------
dable(d, type = "real", guide = g, gtab = "gender", part = c(F,T))
dable(d, type = "catg", guide = g, gtab = "gender", part = c(F,T))
dable(d, type = "bnry", guide = g, gtab = "gender", part = c(F,T))
dable(d, type = "date", guide = g, gtab = "gender", part = c(F,T))
dable(d, type = "surv", guide = g, gtab = "gender", part = c(F,T))
dable(d, type = "lcat", guide = g, gtab = "gender", part = c(F,T))

## ----"test-default-testers"---------------------------------------------------
dable(d, type = "real", guide = g, gtab = "gender", part = c(F,F,T))
dable(d, type = "catg", guide = g, gtab = "gender", part = c(F,F,T))
dable(d, type = "bnry", guide = g, gtab = "gender", part = c(F,F,T))
dable(d, type = "date", guide = g, gtab = "gender", part = c(F,F,T))
dable(d, type = "surv", guide = g, gtab = "gender", part = c(F,F,T))
dable(d, type = "lcat", guide = g, gtab = "gender", part = c(F,F,T))

## ----"test-default-baseline"--------------------------------------------------
baseline(d, guide = g, gtab = "gender", part = c(F,F,F))
baseline(d, guide = g, gtab = "gender", part = c(T,F,F))
baseline(d, guide = g, gtab = "gender", part = c(F,T,F))
baseline(d, guide = g, gtab = "gender", part = c(F,F,T))
baseline(d, guide = g, gtab = "gender", part = c(T,T,F))
baseline(d, guide = g, gtab = "gender", part = c(T,F,T))
baseline(d, guide = g, gtab = "gender", part = c(F,T,T))
baseline(d, guide = g, gtab = "gender", part = c(T,T,T))

