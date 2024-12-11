test_that("gtab helpers works", {

    ## library("testthat")
    ## devtools::load_all()

    i <- c(1,2,1,2,2)
    d <- data.frame(foo = factor(i, labels = letters[1:2]))
    r <- data.frame(All = rep(TRUE, length(i)), a = i==1, b = i==2)

    expect_equal(create_gtab("foo", data = d, all = FALSE), r[, 2:3])
    expect_equal(create_gtab("foo", data = d, all = FALSE, rev = TRUE), r[, 3:2])
    expect_equal(create_gtab("foo", data = d, all = TRUE, all.first = TRUE), r)
    expect_equal(create_gtab("foo", data = d, all = TRUE, all.first = FALSE),
                 r[, c(2,3,1)])
    expect_equal(create_gtab("foo", data = d, all = TRUE, rev = TRUE), r[, 3:1])

    expect_equal(check_gtab(r, 5), r)
    expect_error(check_gtab(r, 4))
    r1 <- r
    r1$x <- 1L
    expect_error(check_gtab(r1))
    r2 <- r
    r2$a[3] <- NA
    expect_error(check_gtab(r2))


})
