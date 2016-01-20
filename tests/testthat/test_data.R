context("Test input data processing")


## test data ##
test_that("test: data are processed fine", {
    ## skip on CRAN
    skip_on_cran()
    rm(list=ls())

    ## generate data
    x1 <- c(0,1,3)
    x2 <- c(2,5)
    names(x1) <- letters[1:3]
    names(x2) <- c('a', 'r')
    D1 <- dist(x1)
    D2 <- dist(x2)
    out <- vimes.data(D1, D2)

    ## check output shape
    expect_is(out, "list")
    expect_is(out[[1]], "dist")
    expect_is(out[[2]], "dist")

    ## check attributes
    expect_equal(attr(out, "N"), 4)
    expect_equal(attr(out, "labels"), rownames(as.matrix(out[[1]])))
    expect_equal(attr(out, "labels"), rownames(as.matrix(out[[2]])))

    ## round trip
    expect_equal(as.matrix(out[[1]])[labels(x1),labels(x1)], as.matrix(D1))
    expect_equal(as.matrix(out[[2]])[labels(x2),labels(x2)], as.matrix(D2))
})

