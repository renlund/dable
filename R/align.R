##' ordering of vector
##'
##' Order a given vector, which may contain duplicates, according to the wanted
##'   order given by some other vector. Supercedes 'order_as' and
##'   'order_by_list' in a single function.
##' @param x the vector that needs ordering
##' @param template the wanted order
##' @param group possible grouping
##' @param all logical; should given elements not in template be kept?
##' @param outgroup group name of elements not in template (if kept)
##' @export
align <- function(x, template = NULL, group = NULL, all = TRUE, outgroup = ".Other"){
    properties(all, class = c("NULL", "logical"), length = 0:1, na.ok = FALSE)
    properties(outgroup, class = "character", length = 1, na.ok = FALSE)
    if(length(x) == 0){
        warning("zero length input makes no sense")
        return(as.list(NULL))
    }
    if(!is.null(group) & !is.null(template)){
        if(length(group) != length(template)){
            stop("template and group of the same length, please")}
    }
    if(is.null(template)) template = sort(unique(x))
    m <- match(x, template)
    distinct_m <- sort(unique(na.omit(m)))
    order <- rep(NA_integer_, length(x))
    dummy <- 0L
    for(d in distinct_m){
        i <- which(x %in% x[which(d==m)][1])
        n <- length(i)
        order[dummy + 1:n] <- i
        dummy <- dummy + n
    }
    if(any(is.na(m))){
        if(all){
            order[which(is.na(order))] <- which(is.na(m))
        } else {
            order <- order[!is.na(order)]
        }
    }
    z <- data.frame(x = x[order])
    if(is.null(group)){
        z$group <- outgroup
        list(order = order,
             sorted = z,
             group.rle = list(lengths = nrow(z),
                              values = outgroup))
    } else {
        tg <- data.frame(template = template, group = group)
        s <- merge(x = z, y = tg, all.x = TRUE,
                   by.x = "x", by.y = "template", sort = FALSE)
        if(all){
            s$group[is.na(s$group)] <- outgroup
        }
        Rle <- rle(s$group)
        class(Rle) <- "list"
        list(order = order,
             sorted = s,
             group.rle = Rle)
    }
}
