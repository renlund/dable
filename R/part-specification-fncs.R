part_spec <- function(part, gtab){
    if(!is.list(part)) part <- as.list(part)
    properties(part, nm = "'part' converted to a list (if not already)",
               class = "list", length = 0:3)
    el <- expand_list(part, n = 3, fill = FALSE)
    M <- length(gtab)
    list(desc_spec(el[[1]], m = M),
         comp_spec(el[[2]], m = M),
         test_spec(el[[3]], m = M, gtab = gtab))
}

expand_list <- function(l, n = 3, fill = FALSE){
    m <- length(l)
    if(m < n) for(i in 1:(n-m)) l <- c(l, list(fill))
    l
}


## desc ------------------------------------------------------------------------
desc_spec <- function(x, m){
    if(is.logical(x)) x <- if(!is.na(x) && x) "all" else "none"
    if(is.character(x)) x <- text2desc_spec(x, m)
    verify_desc_spec(x, m)
}

text2desc_spec <- function(x, m){
    properties(m, class = c("numeric", "integer"), length = 1, na.ok = FALSE)
    one_of(x, nm = "text description specification",
           set = c("all", "first", "last", "none"))
    if(x == "all"){
        1:m
    } else if(x == "first"){
        1
    } else if(x == "last"){
        m
    } else if(x == "none"){
        integer(0)
    }
}

verify_desc_spec <- function(x, m){
    properties(x, nm = "numeric description specification",
               class = c("numeric", "integer"),
               length = 0:m,
               na.ok = FALSE)
    if(!all(x %in% 0:m)){
        s <- paste0("numeric description specification must be integers in the ",
                    "range 1:", m)
        stop(s)
    }
    unique(x)
}

## comp ------------------------------------------------------------------------
comp_spec <- function(x, m){
    if(m <= 1) x <- FALSE
    if(is.logical(x)) x <- if(!is.na(x) && x) "across" else "none"
    if(is.character(x)) x <- text2comp_spec(x, m)
    verify_comp_spec(x, m)
}

text2comp_spec <- function(x, m){
    properties(m, class = c("numeric", "integer"), length = 1, na.ok = FALSE)
    one_of(x, nm = "text comparison specification",
           set = c("across", "adjacent", "none"))
    L <- list()
    if(x != "none"){
        if(m == 1) stop("1 group only, no comparisons possible")
        if(x == "across"){
            for(i in 2:m) L[[i-1]] <- c(1,i)
        } else if(x == "adjacent"){
            for(i in 1:(m-1)) L[[i]] <- c(i,i+1)
        }
        L
    } else L
}

verify_comp_spec <- function(x, m){
    properties(m, class = c("numeric", "integer"), length = 1, na.ok = FALSE)
    if(length(x) == 0){
        list()
    } else {
        if(m == 1) stop("1 group only, no comparisons possible")
        properties(x, "numeric comparison specification",
                   class = c("list", "numeric", "integer"))
        if(!is.list(x)) x <- list(x)
        foo <- function(z){
            properties(z, nm = "some numeric comparison specification",
                       class = c("integer", "numeric"),
                       length = 2, na.ok = FALSE)
            if(!(all(z %in% 1:m))){
                s <- paste0("numeric comparison specification must be pairs of ",
                            "integers in the range 1:", m)
                stop(s)
            }
            if(z[1] == z[2]){
                s <- paste0("numeric comparison specification must be pairs of ",
                            "unique integers")
                stop(s)
            }
        }
        lapply(x, FUN = foo)
        x
    }
}

## test ------------------------------------------------------------------------
test_spec <- function(x, m, gtab){
    if(m <= 1) x <- FALSE
    if(is.logical(x)) x <- if(!is.na(x) && x) "all" else "none"
    if(is.character(x)) x <- text2test_spec(x, m)
    verify_test_spec(x, m, gtab)
}

text2test_spec <- function(x, m){
    one_of(x, nm = "text test specification", set = c("all", "none"))
    if(x == "all") 1:m else integer(0)
}

## it could possible be useful to allow, say, -1 to mean test all but gtab[,1]
verify_test_spec <- function(x, m, gtab){
    properties(m, class = c("numeric", "integer"), length = 1, na.ok = FALSE)
    if(length(x) == 0){
        x
    } else {
        if(m == 1) stop("1 group only, no test possible")
        properties(x, class = c("integer", "numeric"), na.ok = FALSE)
        x <- unique(x)
        if(length(x) < 2){
            stop("need two or more groups for testing")
        }
        if( ! all(x %in% 1:m) ){
            s <- paste0("numeric test specification should be given as integers in the ",
                        "range 1:", m)
            stop(s)
        }
        equiv <- gtab_equiv2factor(gtab= gtab[, x, drop = FALSE],
                                   na.ok = TRUE, verbose = TRUE)
        if(!equiv) stop("bad test specification (fails gtab_equiv2factor) - ",
                        "overlapping groups")
        x
    }
}



if(FALSE){

    gt <- data.frame(c(T,F,F),c(F,T,F),c(F,F,T))

    part_spec(list(), gt)
    part_spec(c(T,F,T), gt)
    part_spec(list(T,NA,c(1,2)), gt)
    part_spec(list("first",NA,c(1,2)), gt)
    part_spec(list("all","adjacent","none"), gt)


    desc_spec(TRUE, 3)
    desc_spec(FALSE, 3)
    desc_spec(NA, 3)
    desc_spec("all", 3)
    desc_spec("first", 3)
    desc_spec("last", 3)
    desc_spec("none", 3)
    desc_spec(1:2, 3)

    comp_spec(TRUE, 3)
    comp_spec(FALSE, 3)
    comp_spec(FALSE, 1)


    comp_spec(NA, 3)
    comp_spec("across", 3)
    comp_spec("adjacent", 3)
    comp_spec("none", 3)
    comp_spec(1:2, 3)

    gt <- data.frame(c(T,F,F),c(F,T,F),c(F,F,T))
    test_spec(TRUE, 3, gt)
    test_spec(FALSE, 3, gt)
    test_spec(NA, 3, gt)
    test_spec("all", 3, gt)
    test_spec("none", 3, gt)
    test_spec(1:2, 3, gt)

}
