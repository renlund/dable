## ----"SETUP", include = FALSE-------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE,
                      comment = "#>")
library(dable)
dpset_defaults(overwrite = TRUE)
if(FALSE){
    .cmp <- function() rmarkdown::render(input = "vignettes/dable-and-flextable.Rmd")
    .look <- function() shell.exec("dable-and-flextable.html")
}

## -----------------------------------------------------------------------------
d <- test_data()
vt <- test_vtab()
st <- test_stab()
g <- dguide(d, id = "id", vtab = vt, stab = st, catg.tol = 15)

## -----------------------------------------------------------------------------
dt <- baseline(d, guide = g, gtab = "area", time.unit = 365.25,
               part = list(T,T,T))
flexdable(dt)

## -----------------------------------------------------------------------------
dt <- baseline(d, theme = 2, guide = g, gtab = "area", time.unit = 365.25,
               part = list(T,T,T))
flexdable(dt)

