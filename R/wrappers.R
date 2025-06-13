##' Standardized differences
##'
##' Calculate the standardized differences between groups defined by gtab.
##' @param data data.frame
##' @param gtab grouping table
##' @param comp argument for 'comp' entry of list given as 'part' argument in
##'     \code{dable}. This can be given as 'across' (all groups compared to
##'     first group) or 'adjacent' (all groups compared to prior group). If
##'     given as TRUE it defaults to 'across'. Note; you can also specify a list
##'     of pairs for the indexes to be compared; e.g. if \code{gtab} defines 4
##'     groups \code{comp = list(c(2,1),c(4,3))} will have the second group
##'     compared to the first, and the fourth group compared to the third.
##' @param ... arguments passed to \code{dable}
##' @export
dstd <- function(data, gtab, comp, ...){
    settings <- dpget_all()
    on.exit(dpset(settings))
    dpset(list(real.desc.bl = "label_type",
               catg.desc.bl = "label_type",
               bnry.desc.bl = "label_type",
               lcat.desc.bl = "label_type",
               date.desc.bl = "label_type",
               surv.desc.bl = "label_type",
               real.comp.bl = "real.std",
               catg.comp.bl = "catg.std",
               bnry.comp.bl = "bnry.std",
               lcat.comp.bl = "catg.std",
               date.comp.bl = "date.std",
               surv.comp.bl = "surv.std"))
    dable(
        data = data,
        type = "baseline",
        gtab = gtab,
        part = list(
            desc = TRUE,
            comp = comp,
            test = FALSE
        ),
        ...
    )
}

label_type <- function(x, ...){
    dots <- list(...)
    l <- dots$.label
    t <- dots$.type
    data.frame(
        Variable = if(is.null(l)) NA_character_ else l,
        type = if(is.null(t)) NA_character_ else t
    )
}
attr(label_type, "meta") <- c("Variable", "type")

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
