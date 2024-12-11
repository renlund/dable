test_that("desc-real fncs work", {

    x <- c(2,3,7)
    w <- c(3,1,2)
    expect_equal(sd(rep(x, w)), d.sd(x, w))



})
