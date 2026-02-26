##' @title ordering a vector
##' @description Order a given vector, which may contain duplicates, according to the wanted
##'   order given by some other vector.
##' @param x the vector that needs ordering
##' @param template the wanted order
##' @param group possible grouping
##' @param all logical; should given elements not in template be kept?
##' @param outgroup group name of elements not in template (if kept)
##' @param exact logical; if TRUE matches for \code{x} in \code{template} is
##'     determined by \code{match}, if FALSE matches are determined by
##'     \code{gmatch}. The latter is based on \code{grep}.
##' @param ... arguments passed to \code{gmatch} (only relevant when \code{exact
##'     = FALSE})
##' @seealso \code{\link{gmatch}}
##' @examples
##' D <- data.frame(x = LETTERS[c(2,2,2,4,4,1,3,3)])
##' D$y <- paste0(D$x, ":", letters[c(1,2,3,1,2,1,1,2)])
##' print(D)
##' ordnung <- rev(LETTERS)
##' a <- align(D$x, template = ordnung)
##' print(D[a$order,])
##' @export
align <- function(x, template = NULL, group = NULL, all = TRUE,
                  outgroup = ".Other", exact = TRUE, ...){
    dots <- list(...) ## dots <- as.list(NULL)
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
    m <- if(exact){
             match(x, template)
         } else{
             do.call(what = gmatch,
                     args = c(list(x = x, table = template), dots))
         }
    distinct_m <- sort(unique(stats::na.omit(m)))
    order <- rep(NA_integer_, length(x))
    dummy <- 0L
    for(d in distinct_m){
        i <- which(d == m)
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
        z$template <- template[m][order]
        tg <- data.frame(template = template,
                         group = group)
        s <- merge(x = z, y = tg, all.x = TRUE,
                   by = "template", sort = FALSE)
        s$template <- NULL
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

##' @title grepl matching
##' @description \code{gmatch} returns a vector of the positions of (first)
##'     matches of its first argument in its second.
##' @details Similar to \code{match} but uses \code{grep} to determine matches.
##' @param x vector; the values to be matches
##' @param table vactor; the values to be matched against
##' @param right logical; if \code{TRUE} behaviour is like \code{match}, but if
##'     \code{FALSE} then it is the \code{table} elements that are used as the
##'     \code{pattern} argument in \code{grepl}, i.e. this reverses the
##'     direction of finding matches. Note however, that the function always
##'     returns the indices of the \code{table} argument. See example for
##'     possible use cases.
##' @param nomatch the value to be returned in the case when no match is
##'     found. Note that it is coerced to integer
##' @param ... arguments passed to \code{grep}
##' @return An integer vector of the same length as \code{x}, giving the indices
##'     of the elements in \code{table} which matched, or \code{nomatch}.
##' @examples
##' ## if matches are exact (and there is no risk of matching confusion) then
##' ## match and gmatch behave the same (even if right = FALSE)
##' x <- LETTERS[c(2,2,4,6,1,1,3)]
##' y <- template <- LETTERS[5:1]
##' match(x, y)
##' gmatch(x, table = y, right = TRUE)
##' gmatch(x, table = y, right = FALSE)
##' ## finding matches with gmatch could be useful if e.g. there might be a case
##' ## mismatch
##' x <- c("bar", "foo", "bar")
##' y <- c("Foo", "Bar")
##' match(x, y) ## this "fails"
##' match(x, tolower(y)) ## this works
##' gmatch(x, table = y, ignore.case = TRUE)
##' ## Sometimes there is a risk of "bad" matches
##' x <- c("bar", "foo", "bar")
##' y <- c("Foo (not Bar)", "Bar")
##' gmatch(x, table = y, ignore.case = TRUE) ## probably not what is wanted
##' ## to remedie this, one need to wrap the input in a regular expression
##' ## complex enough to handle the situation, here e.g. to make sure that only
##' ## the inital part of the string is matched
##' gmatch(sprintf("^%s", x), table = y, ignore.case = TRUE)
##' ## sometimes it is useful to be able to reverse the direction of the match
##' ## (even though some of these could possibly be handled with a more
##' ## complex regular expression), e.g.
##' x <- c("bar a", "foo x", "foo y", "bar b", "foo z")
##' y <- c("foo", "bar")
##' gmatch(x, table = y, right = FALSE)
##' @export
gmatch <- function(x, table, right = TRUE, nomatch = NA_integer_, ...){
    properties(x, class = "character")
    properties(table, class = "character")
    properties(right, class = "logical", length = 1, na.ok = FALSE)
    properties(nomatch, class = "integer", length = 1, na.ok = TRUE)
    dots <- list(...) ## dots <- as.list(NULL)
    n <- length(x)
    m <- length(table)
    if(n == 0){
        integer(0)
    } else if(m == 0){
        rep(nomatch, n)
    } else {
        r <- integer(n)
        if(right){
            for(i in 1:n){
                g <- do.call(what = grep,
                             args = c(list(pattern = x[i],
                                           x = table),
                                      dots))
                r[i] <- if(length(g) == 0) nomatch else g[1]
            }
        } else {
            x.val <- unique(x)
            for(i in seq_along(x.val)){
                j.find <- NA_integer_
                for(j in 1:m){
                    g <- do.call(what = grep,
                                 args = c(list(pattern = table[j],
                                               x = x.val[i]),
                                          dots))
                    if(length(g) != 0){
                        j.find <- j
                        break
                    }
                }
                r[which(x == x.val[i])] <- if(is.na(j.find)) nomatch else j.find
            }
        }
        r
    }
}
