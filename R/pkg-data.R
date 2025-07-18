##' @description Generate a test data set
##' @details Generate a test data set with a number of different types of variables.
##' @title test data
##' @param n integer; number of rows (at least 10)
##' @param seed numeric; seed for random number generator
##' @importFrom stats rnorm rexp rbinom runif
##' @return A data.frame
##' @export
test_data <- function(n = 1000, seed = 1){
    properties(n, class = c("integer", "numeric"), length = 1, na.ok = FALSE)
    if(n < 10){
        message("n is so very small, let's use n = 10 instead")
        n = 10
    }
    set.seed(seed)

    ## some data
    d <- data.frame(
        id = 1:n,
        pid = sprintf("id%s", 1:n),
        status = "included",
        country = factor(sample(c("SW", "NO", "DE"), size = n, replace = TRUE),
                         levels = c("SW", "NO", "DE")),
        area = sample(c("rural", "urban"), size = n, replace = TRUE, prob = c(1,4)),
        offspring = sample(c(TRUE, FALSE), size = n, replace = TRUE, prob = c(3,1)),
        age = round(stats::rnorm(n, 55, 10)),
        male = sample(0:1, size = n, replace = TRUE, prob = c(.55,.45)),
        measA = stats::rexp(n, 1/40),
        measNA = NA_real_,
        forgotten = stats::runif(n, -1,1),
        importance = sample(seq(0.1, 6, by = .1), size = n, replace = TRUE),
        index = as.Date("2017-01-01") + stats::runif(n, min = 0, max = 729),
        posix = as.POSIXct(stats::runif(n, 1485000000, 1495000000)),
        ev.foo = stats::rbinom(n, 1, prob = .2),
        ev.bar = stats::rbinom(n, 1, prob = .3)
    )

    ## some random missing
    d$area[sample(1:n, size = 5)] <- NA_character_
    d$measA[sample(1:n, size = 7)] <- NA_real_
    d$index[sample(1:n, size = 1)] <- as.Date(NA_character_)

    ## some internal dependencies
    d <- within(
        data = d,
        expr = {
            region <- ifelse(
                country == "SW",
                yes = sample(LETTERS[c(1,2,3)], n, TRUE, 3:1),
                no = ifelse(country == "NO",
                            sample(LETTERS[c(1,3,4)], n, TRUE, c(2,1,3)),
                            sample(LETTERS[c(1,2,5)], n, TRUE, 1:3))
            )
            kids <- ifelse(test = offspring,
                           yes = 1L + rpois(n,1),
                           no = 0L)
            gender <- factor(ifelse(male == 1, "male", "female"),
                             levels = c("male", "female"))
            measM <- ifelse(male == 1,
                            yes = stats::rexp(n, 1/50),
                            no = NA_real_)
            measB <- ifelse(country != "NO",
                            yes = stats::rexp(n, 1/60),
                            no = NA_real_)
            end <- index + 365
            t.foo <- ifelse(ev.foo == 1, stats::runif(n, 1, 365), 365)
            t.bar <- ifelse(ev.bar == 1, stats::runif(n, 10, 365), 365)
        }
    )

    d
}

##' @describeIn test_data test_vtab: generate an 'vtab' that accompanies the data set
##'     generated by test_data. It contains information (label, group) for (almost)
##'     each term.
##' @param include.tte logical; include time-to-event variables? These are
##'     perhaps better kept track of in a 'stab'
##' @importFrom utils read.csv
##' @export
test_vtab <- function(include.tte = FALSE){
    meta <- utils::read.csv(text = "
term, label
id, ID (integer)
pid, ID
status, A constant
index, Index
posix, Unkown format
end, End
", strip.white = TRUE)
    meta$group = "Meta data"
    demo <- read.csv(text = "
term, label
age, Age
male, Male
gender, Gender
country, Country
area, Area
region, Region
", strip.white = TRUE)
    demo$group = "Demographics"
    etc <- read.csv(text = "
term, label
offspring, Offspring
kids, Kid count
measA, A measure
measB, Another measure
measM, Manly measure
measNA, Missing measure
", strip.white = TRUE)
    etc$group = "Measures etc."
    if(include.tte){
        outc <- read.csv(text = "
term, label
ev.foo, Foo
t.foo, Foo
ev.bar, Bar
t.bar, Bar
", strip.white = TRUE)
    outc$group = "Outcomes"
    } else outc <- NULL
    rbind(meta, demo, etc, outc)
}

##' @describeIn test_data test_stab: generate an 'stab' that accompanies the data set
##'     generated by test_data. It contains information (label, time, event) for the
##'     relevant variables
##' @export
test_stab <- function(){
    utils::read.csv(text = "
label, time, event
Foo, t.foo, ev.foo
Bar, t.bar, ev.bar
", strip.white = TRUE)
}
