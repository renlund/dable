default.surv.affix <- function() dparam("surv.affix")
default.surv.prefix <- function() dparam("surv.prefix")
default.stab.group.name <- function() dparam("stab.group.name")

##' survival table (stab) functions
##'
##' @param s character; common name of surv variables (differing only in
##'     prefix/suffix)
##' @param affix character; a prefix or suffix
##' @param prefix logical; if TRUE indicates that a prefix is used, if FALSE
##'     suffix
##' @param stab data.frame, a "survival table" (stab) i.e. a data frame
##'     specifying the surv-pair variables. Must contain columns 'label', 'time'
##'     and 'event'. (Optionally it could contain 'group', especially if one
##'     might want to merge this information with a "variable table" (vtab).)
##' @param term character; variable name(s)
##' @param nm character vector; typically the names of variables in a data set
##' @param vtab data.frame; a "variable table" (vtab)
##' @param group.name character; name of grouping for surv variables
##' @name stab-fncs
NULL

#' @rdname stab-fncs
#' @details get_or_test_affix: test (and get default if missing) value of 'affix'
#' @export
get_or_test_affix <- function(affix = NULL){
    if(is.null(affix)) affix = default.surv.affix()
    properties(affix, class = "character", length = 2, na.ok = FALSE)
    properties(names(affix), nm = "names of affix", class = "character",
               length = 2, na.ok = FALSE)
    inclusion(names(affix), nm = "names of affix", include = c("time", "event"))
    if(names(affix)[1] != "time"){
        stop("first component of 'affix' should be named 'time'")
    }
    affix
}

#' @rdname stab-fncs
#' @details get_or_test_prefix: test (and get the default if missing) the
#'     logical option for if a prefix (rather than suffix) should be used
#' @export
get_or_test_prefix <- function(prefix = NULL){
    if(is.null(prefix)) prefix = default.surv.prefix()
    properties(prefix, class = "logical", length = 1, na.ok = FALSE)
    prefix
}

#' @rdname stab-fncs
#' @details surv_paste: paste a surv name and an affix
#' @export
surv_paste <- function(s, affix, prefix = NULL){
    properties(s, class = "character", na.ok = FALSE)
    properties(affix, class = "character", length = 1, na.ok = FALSE)
    prefix <- get_or_test_prefix(prefix)
    if(prefix) paste0(affix, s) else paste0(s, affix)
}

#' @rdname stab-fncs
#' @details surv_t: create time-component name from a surv name
#' @export
surv_t <- function(s, affix = NULL, prefix = NULL){
    if(length(s) == 0){
        character(0)
    } else {
        affix <- get_or_test_affix(affix)
        surv_paste(s, affix = affix["time"], prefix = prefix)
    }
}

#' @rdname stab-fncs
#' @details surv_e: create event-component name from a surv name
#' @export
surv_e <- function(s, affix = NULL, prefix = NULL){
    if(length(s) == 0){
        character(0)
    } else {
        affix <- get_or_test_affix(affix)
        surv_paste(s, affix = affix["event"], prefix = prefix)
    }
}

#' @rdname stab-fncs
#' @details surv_nm: create component names from a surv name
#' @export
surv_nm <- function(s, affix = NULL, prefix = NULL){
    properties(s, class = "character", length = 1, na.ok = FALSE)
    c("time" = surv_t(s, affix = affix, prefix = prefix),
      "event" = surv_e(s, affix = affix, prefix = prefix))
}

#' @rdname stab-fncs
#' @details check_stab: check that correct variables are included
check_stab <- function(stab){
    properties(stab, class = "data.frame")
    inclusion(names(stab), nm = "names of surv table",
              include = c("label", "time", "event"))
    n <- nrow(stab)
    if(n > 0){
        properties(stab$label, class = "character", length = n, na.ok = FALSE)
        properties(stab$time, class = "character", length = n, na.ok = FALSE)
        properties(stab$event, class = "character", length = n, na.ok = FALSE)
        stab
    } else {
        warning("stab has zero rows")
        NULL
    }
}

##' @rdname stab-fncs
##' @details which_surv_component: is 'term' the time- event component of a
##'     surv-variable pair (if not: NA_character_)
##' @export
which_surv_component <- function(term, stab){
    if(is.null(stab)){
        rep(NA_character_, length(term))
    } else {
        ifelse(test = term %in% stab$time,
               yes = "time",
               no = ifelse(test = term %in% stab$event,
                           yes = "event",
                           no = NA_character_))
    }
}

##' @rdname stab-fncs
##' @details is_surv_component: is 'term' a component of a surv-variable pair
##' @export
is_surv_component <- function(term, stab){
    !is.na(which_surv_component(term = term, stab = stab))
}

#' @rdname stab-fncs
#' @details verify_stab: check a surv-table; i.e. check if the component names
#'     exist in 'nm' (typically the variable names). Return an stab reduced to
#'     those components that exist in nm.
#' @export
verify_stab <- function(stab, nm, warn = TRUE){
    check_stab(stab)
    properties(nm, class = "character", na.ok = FALSE)
    properties(warn, class = "logical", length = 1, na.ok = FALSE)
    i_time  <- stab$time %in% nm
    i_event <- stab$event %in% nm
    i <- i_time & i_event
    if(warn && any(!i)){
        s1 <- paste0("time components missing for: ",
                     paste0(stab$label[!i_time], collapse = ", "), ".")
        s2 <- paste0("event components missing for: ",
                     paste0(stab$label[!i_event], collapse = ", "), ".")
        s <- paste0(if( any(!i_time) ) paste0(s1, "\n") else NULL,
                    if( any(!i_event) ) s2 else NULL)
        warning(s)
    }
    stab[i,]
}

#' @rdname stab-fncs
#' @details surv_search_string: create the search string to find the time and
#'     event components (if systematically named accoriding to defaults).
#' @export
surv_search_string <- function(){
    prefix = get_or_test_prefix()
    affix = get_or_test_affix()
    f <- gsub(".", "\\.", affix, fixed = TRUE)
    search_t <- surv_paste(s = f['time'],
                           affix = if(prefix) "^" else "$")
    search_e <- surv_paste(s = f['event'],
                           affix = if(prefix) "^" else "$")
    c("time" = search_t, "event" = search_e)
}

#' @rdname stab-fncs
#' @details create_stab: create survival table from surv names. Use name attribute
#'     of s as labels if such exist.
#' @export
create_stab <- function(s){
    properties(s, class = "character", na.ok = FALSE)
    nm <- names(s)
    if(!is.null(nm)) properties(nm, class = "character", na.ok = FALSE)
    data.frame(label = if(!is.null(nm)) nm else s,
               time = surv_t(s),
               event = surv_e(s))
}

#' @rdname stab-fncs
#' @details extract_stab_from_names: extract all candidate surv-components from
#'     'nm' (typically the variable names)
#' @export
extract_stab_from_names <- function(nm){
    ss <- surv_search_string()
    cand_t <- grepl(ss['time'], nm)
    ts <- sub(ss['time'], "", nm[cand_t])
    cand_e <- grepl(ss['event'], nm)
    es <- sub(ss['event'], "", nm[cand_e])
    r <- create_stab(intersect(ts, es))
    if(nrow(r) > 0) r else NULL
}

#' @rdname stab-fncs
#' @details stab2vtab: convert survival table to variable table
#' @export
stab2vtab <- function(stab, group.name = NULL){
    check_stab(stab)
    if(is.null(group.name)){
        group.name = default.stab.group.name()
    }
    properties(group.name, class = "character",
               length = 1, na.ok = FALSE)
    if(is.null(stab$group)){
        stab$group <- group.name
    }
    stab$label.time <- paste0(stab$label, " (time)")
    nm <- c("term", "label", "group")
    a <- stab[, c("event", "label", "group")]
    names(a) <- nm
    b <- stab[, c("time", "label.time", "group")]
    names(b) <- nm
    r <- rbind(a,b)
    n <- nrow(r)
    i <- shuffle(1:(n/2), (n/2+1):n)
    r[i, ]
}

#' @rdname stab-fncs
#' @details combine_vs_tab: create a variable table by combining a variable
#'     table and a surv table
#' @export
combine_vs_tab <- function(vtab, stab, group.name = NULL, stab.first = FALSE){
    Stab <- stab2vtab(stab = stab, group.name = group.name)
    nm <- c("term", "label", "group")
    if(any(Stab$term %in% vtab$term)){
        if( all(Stab$term %in% vtab$term) ){
            if(stab.first){
                Vtab <- subset(vtab, !(term %in% Stab$term))
                rbind(Vtab[, nm], Stab[, nm])
            } else {
                vtab
            }
        } else {
            s <- paste0("partially overlapping stab and vtab information\n",
                        " (no overlap or complete overlap is ok)")
            stop(s)
        }
    } else {
        rbind(vtab[, nm], Stab[, nm])
    }
}
