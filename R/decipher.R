##' replace selected values
##'
##' Change values that corresponds to those of a key.
##' @param x Character (or factor) vector
##' @param key Named character vector; where (typically) the names corresponds
##'     to values appearing in x, and the values are those we want as
##'     replacements. However, if \code{flexible = TRUE}, then a check is
##'     performed to see if it makes more sense to invert the key. It is also
##'     possible to supply a data.frame with columns 'term' and 'label', if so a
##'     key is created from this information.
##' @param flexible Logical; if TRUE the key can be inverted
##' @param within Logical; if TRUE occurences of the keyed values within a
##'     string can be replaced.
##' @param within_word Logical; if 'within' is TRUE, one can use this parameter
##'     to choose if replacement can occur within words or not. E.g. if TRUE
##'     then replacing 'break' with 'pause' will turn 'breakfast' into
##'     'pausfast'. I imagine this is not typically what is wanted. If FALSE,
##'     replacement occurences must be encapsulated by beginning of line, space
##'     characters, punctuation characters or end of line, so that replacement
##'     does not take place within words.
##' @examples
##' x <- c("foo", "foo!", "A foo", "A foo and two bar",
##'        "football", "barely", "bar")
##' key <- c(foo = "yay", bar = "bohnanza")
##' data.frame(
##'   x = x,
##'   d0 = key[x], ## useful only if all values of s exists in the key
##'   d1 = decipher(x, key),
##'   d2 = decipher(x, key, within = TRUE)
##' )
##' x <- factor(c(1,3,2), levels = 1:3,
##'             labels = c("foo", "bar", "baz"))
##' key <- c(A = "foo", C = "baz")
##' str(data.frame(x = x, d = decipher(x, key)))
##' @return vector of same length and class as s (character or factor)
##' @export
decipher <- function (x, key, flexible = TRUE, within = FALSE,
                      within_word = FALSE) {
    properties(x, class = c("character", "factor"))
    if(inherits(key, "data.frame")){
        inclusion(names(key), nm = "names of 'key' argument",
                  include = c("term", "label"))
        key <- setNames(key$label, nm = key$term)
    }
    properties(key, class = "character", na.ok = FALSE)
    properties(names(key), nm = "names of key", class = "character",
               length = length(key), na.ok = FALSE)
    properties(flexible, class = "logical", length = 1, na.ok = FALSE)
    properties(within, class = "logical", length = 1, na.ok = FALSE)
    properties(within_word, class = "logical", length = 1, na.ok = FALSE)
    if(within_word & !within){
        s <- paste0("argument 'within_word' is hierarchically below 'within' ",
                    "so if the latter is FALSE, then so are both")
        warning(s)
    }
    w_patt <- function(x){
        if(within_word){
            x
        } else {
            paste0("(^|[[:punct:]]|[[:space:]])",
                   "(", x, ")",
                   "($|[[:punct:]]|[[:space:]])")
        }
    }
    if("factor" %in% class(x)){
        r <- factor(x = as.numeric(x),
                    labels = decipher(x = levels(x),
                                      key = key,
                                      flexible = flexible,
                                      within = within))
        return(r)
    }
    if(flexible){
        if(within){
            n1 <- sum(unlist(lapply(X = names(key),
                                    FUN = function(z) sum(grepl(w_patt(z), x)))))
            n2 <- sum(unlist(lapply(X = key,
                                    FUN = function(z) sum(grepl(w_patt(z), x)))))
        } else {
            n1 <- sum(names(key) %in% x)
            n2 <- sum(key %in% x)
        }
        if(n2 > n1) key <- stats::setNames(names(key), nm = key)
    }
    if(within){
        r <- x
        for (i in seq_along(key)) {
            r <- gsub(pattern = w_patt(names(key)[i]),
                      replacement = paste0("\\1", key[i], "\\3"),
                      x = r)
        }
        r
    } else {
        r <- key[as.character(x)]
        as.character(ifelse(is.na(r), x, r))
    }
}
