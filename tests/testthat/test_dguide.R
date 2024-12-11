test_that("dguide and related functions works", {

    ## library("testthat")
    ## devtools::load_all()

    x <- list(LETTERS[2:1], 2:1, c(2.1,4.5))
    expect_equal(dparam("bnry.list",x), x)
    expect_error(dparam("bnry.list",list(LETTERS[2:1], 2:1, c(NA,4.5))))
    expect_error(dparam("bnry.list",list(LETTERS[2:1], 3:1, c(1,4.5))))

    ## expect_equal(is_bnry.value(0:1, list(letters[1:2], 1:0)), TRUE)
    ## expect_equal(is_bnry.value(0:1, list(letters[1:2], c(0,1))), TRUE)
    ## expect_equal(is_bnry.value(0:1, list(letters[1:2], c(0,1.1))), FALSE)
    ## expect_equal(is_bnry.value(0:1, NULL), FALSE)

    expect_equal(value_type(as.Date("2001-01-01")),
                 list(type = "date", class = "Date", levels = NA))

    expect_identical(value_type(1:5),
                     list(type = "real", class = "integer", levels = NA))
    expect_identical(value_type(1:5, real.tol = 4),
                     list(type = "real", class = "integer", levels = NA))
    expect_identical(value_type(1:5, real.tol = 5),
                     list(type = "catg", class = "integer", levels = 1:5))

    expect_identical(value_type(LETTERS[c(1:6,5,5,1,2,2,2)]),
                 list(type = "catg", class = "character", levels = LETTERS[c(1:6)]))
    expect_identical(value_type(LETTERS[c(1:6,5,5,1,2,2,2)], catg.tol = 6),
                 list(type = "catg", class = "character", levels = LETTERS[c(1:6)]))
    expect_identical(value_type(LETTERS[c(1:6,5,5,1,2,2,2)], catg.tol = 5),
                 list(type = "lcat", class = "character", levels = LETTERS[c(1:6)]))

    expect_identical(value_type(c(0,0,0,1,1,1,NA,0,1)),
                 list(type = "real", class = "numeric", levels = NA))
    expect_equal(value_type(c(0,0,0,1,1,1,NA,0,1), real.tol = 2),
                 list(type = "catg", class = "numeric", levels = 0:1))
    expect_equal(value_type(c(0,0,0,1,1,1,NA,0,1),
                               bnry.list = list(letters[3:4], c(TRUE, FALSE), 1:0),
                               real.tol = 2),
                 list(type = "bnry", class = "numeric", levels = 0:1))

    data <- data.frame(id = 1:2,
                       z=factor(LETTERS[4:5]),
                       foo=c(3.4,4.1),
                       y = c(FALSE, TRUE),
                       x = c(2.2,3.4),
                       bar=0:1,
                       dd = as.Date("2000-01-01") + c(0L, 32L))
    expect_equal(
        term_type(term = names(data), data = data),
        structure(list(term = c("id", "z", "foo", "y", "x", "bar", "dd"),
                       type = c("real", "catg", "real", "bnry",
                                "real", "bnry", "date"),
                       class = c("integer", "factor", "numeric", "logical",
                                 "numeric", "integer", "Date")),
                  class = "data.frame",
                  row.names = c(NA, -7L),
                  levels = list(z = c("D", "E"),
                                y = c(FALSE,  TRUE),
                                bar = 0:1))
    )

    st <- data.frame(label = "The Foo", time = "foo", event = "bar")
    expect_equal(
        term_type(term = names(data), data = data, stab = st),
        structure(list(term = c("id", "z", "foo", "y", "x", "bar", "dd"),
                       type = c("real", "catg", "surv.t", "bnry",
                                "real", "surv.e", "date"),
                       class = c("integer", "factor", "numeric", "logical",
                                 "numeric", "integer", "Date")),
                  class = "data.frame",
                  row.names = c(NA, -7L),
                  levels = list(z = c("D", "E"),
                                y = c(FALSE,  TRUE)))
    )


    ## vt <- data.frame(term = c("id", "x", "dd", "y", "z"),
    ##                  label = c("ID", "The X", "The Date", "The Y","The Z"))
    ## r <- vt
    ## expect_equal(vstab_bind(vt), check_vtab(vt))
    ## expect_equal(vstab_bind(vt), r)

    ## vt2 <- vt
    ## vt2$group <- rep(c("A", "B"), c(3,2))
    ## r$group <- vt2$group
    ## expect_equal(vstab_bind(vt2), check_vtab(vt2))
    ## expect_equal(vstab_bind(vt2), r)

    ## st <- data.frame(label = "Holy Foo", time = "foo", event = "bar")
    ## r <- rbind(vt, data.frame(term = c("bar", "foo"),
    ##                           label = c("Holy Foo", "Holy Foo (time)")))
    ## expect_equal(vstab_bind(vt, stab = st), r)

    if(FALSE){

        data <- data.frame(id = 1:2,
                           z=factor(LETTERS[4:5]),
                           foo=c(3.4,4.1),
                           y = c(FALSE, TRUE),
                           x = c(2.2,3.4),
                           bar=0:1,
                           dd = as.Date("2000-01-01") + c(0L, 32L))
        vt <- data.frame(term = c("id", "x", "dd", "y", "z"),
                         label = c("ID", "The X", "The Date", "The Y","The Z"),
                         group = rep(LETTERS[1:2], c(3,2)))
        st <- data.frame(label = "Holy Foo", time = "foo", event = "bar",
                         group = "Survy-purvy")

        (g <- dguide(data))
        attributes(g)
        dguide(data, unit.id = "id")
        dguide(data, elim.set = "id")
        dguide(data, unit.id = "id", elim.set = "foo")

        dguide(data, vtab = vt)
        dguide(data, vtab = vt, stab = st)

        dguide(data, unit.id = "id", vtab = vtab)
        dguide(data, elim.set = "id", vtab = vtab)
        dguide(data, unit.id = "id", elim.set = "foo", vtab = vtab)

        vtab$group <- rep(c("A", "B"), each = 2)
        dguide(data, vtab = vtab)
        dguide(data, unit.id = "id", vtab = vtab)
        dguide(data, elim.set = "id", vtab = vtab)
        dguide(data, unit.id = "id", elim.set = "foo", vtab = vtab)

        stab <- data.frame(label = "Bar", time = "foo", event = "bar")
        dguide(data, vtab = vtab, stab = stab)

    }

})
