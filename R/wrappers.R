##' Missing stats
##'
##' Count and percentage of missing
##' @param data data.frame
##' @param guide passed to \code{dable}
##' @param gtab passed to \code{dable}
##' @export
dmissing <- function(data, guide = NULL, gtab = NULL){
    settings <- dpget_all()
    on.exit(dpset(settings))
    dpset(list(real.desc.bl = "na_summary",
               catg.desc.bl = "na_summary",
               bnry.desc.bl = "na_summary",
               lcat.desc.bl = "na_summary",
               date.desc.bl = "na_summary",
               surv.desc.bl = "na_summary.surv"))
    dable(data = data,
          type = "baseline",
          guide = guide,
          gtab = gtab,
          part = c(TRUE, FALSE, FALSE))
}

na_summary <- function(x, ...){
    n <- length(x)
    m <- sum(is.na(x))
    p <- m / n
    ifelse(test = p < 0.01,
           yes = ifelse(test = m == 0,
                        yes = "",
                        no = paste0(m, " (<1)")),
           no = ifelse(test = p < 0.1,
                       yes = paste0(m, "  (", round(100*p), ")"),
                       no = paste0(m, " (", round(100*p), ")")))
}
attr(na_summary, "label") <- "Missing"
na_summary.surv <- function(time, event, ...){
    time[is.na(event)] <- NA
    na_summary(time)
}
attr(na_summary.surv, "label") <- "Missing"


if(FALSE){
    n <- 2*2*67
    d <- data.frame(id = 1:n,
                    gr = rep(c("midi", "maxi", "efti"), c(n/4, n/2, n/4)),
                    foo = rep(1:0, each = n/2),
                    bar = rep(LETTERS[1:2], n/2),
                    baz = as.Date("2001-01-01") + sample(100:10000, n),
                    ev.U = rbinom(n, 1, 0.1),
                    t.U = rexp(n, 1/50),
                    ev.XY = rbinom(n, 1, 0.1),
                    t.XY = rexp(n, 1/50))
    d$foo[sample(1:n, 10)] <- NA_integer_
    d$bar[sample(1:n, 1)] <- NA_character_
    d$baz[sample(1:n, 51)] <- as.Date(NA_character_)
    d$ev.U[sample(1:n, 2)] <- NA_integer_
    d$t.U[sample(1:n, 12)] <- NA_real_
    dmissing(d)
    dmissing(d, gtab = "gr")
}
