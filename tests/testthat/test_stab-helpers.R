test_that("stab-helper functions works", {

    ## library("testthat")
    ## devtools::load_all()

    x <- c("time" = "A", "event" = "B")
    expect_equal(get_or_test_affix(x), x)
    expect_error(get_or_test_affix(x[2:1]))


    ## st <- data.frame(label = c("foo", "bar"),
    ##                  time = c("a", "b"),
    ##                  event = c("A", "B"))
    ## expect_equal(stab2slist(st),
    ##              list(foo = c(time = "a", event = "A"),
    ##                   bar = c(time = "b", event = "B")))


    st <- data.frame(lebel = c("foo", "bar", "baz", "quuz"),
                     time = c("a", "b", "c", "d"),
                     event = c("A", "B", "C", "D"))
    expect_error(check_stab(st))
    names(st)[1] <- "label"
    nm <- c(letters[3:4], LETTERS[c(2,4)])
    expect_warning(r <- verify_stab(stab = st, nm = nm))
    expect_equal(r, st[4,])

})
