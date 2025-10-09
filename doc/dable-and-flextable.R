## ----"SETUP", include = FALSE-------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE,
                      comment = "#>")
library(flextable)
## library(dable)
devtools::load_all()
dpset_defaults(overwrite = TRUE)
if(FALSE){
    .cmp <- function() rmarkdown::render(input = "vignettes/dable-and-flextable.Rmd")
    .look <- function() shell.exec("dable-and-flextable.html")
}

## -----------------------------------------------------------------------------
d <- test_data()
g <- dguide(d, id = "id", vtab = test_vtab(), stab = test_stab(), catg.tol = 15)

## ----message = FALSE----------------------------------------------------------
dt <- baseline(d, theme = 0, guide = g, gtab = "area", time.unit = 365.25,
               part = list(T,T,T))
blextable(dt)

## ----message = FALSE----------------------------------------------------------
dt <- baseline(d, theme = 2, guide = g, gtab = "area", time.unit = 365.25,
               part = list(T,T,T))
blextable(dt)

## ----message = FALSE----------------------------------------------------------
dreal(d) |> dextable()
dreal(d, guide = g) |> dextable()
dreal(d, guide = g) |> dextable(row.group = FALSE)
dreal(d, guide = g) |> dextable(t2l = FALSE, row.group = FALSE)
dreal(d, guide = g, gtab = "gender", part = c(T,T,T)) |> dextable()
dreal(d, guide = g, gtab = "gender", part = c(T,T,F)) |> dextable()
dreal(d, guide = g, gtab = "gender", part = c(T,F,T)) |> dextable()
dreal(d, guide = g, gtab = "gender", part = c(F,T,F)) |> dextable()
dreal(d, guide = g, gtab = "gender", part = c(T,F,F)) |> dextable()
dreal(d, guide = g, gtab = "gender", part = c(F,T,F)) |> dextable()
dreal(d, guide = g, gtab = "gender", part = c(F,F,T)) |> dextable()
dreal(d, guide = g, gtab = "gender", part = c(F,F,F))
dreal(d, guide = g, gtab = "gender") |> dextable(row.group = FALSE)
dreal(d, guide = g, gtab = "gender") |> dextable(t2l = FALSE, row.group = FALSE)

