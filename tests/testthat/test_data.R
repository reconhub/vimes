context("Test input data processing")


## test data ##
test_that("test: data are processed fine", {
    ## skip on CRAN
    skip_on_cran()

    ## generate data
    x1 <- c(0,1,3)
    x2 <- c(2,5,7,12)
    names(x1) <- letters[1:3]
    names(x2) <- c('a', 'r', 'c')
    D1 <- dist(x1)
    D2 <- dist(x2)
    out <- vimes_data(D1, D2)

    ## check output shape
    expect_is(out, "list")
    expect_is(out, "vimes_data")
    expect_is(out[[1]], "dist")
    expect_is(out[[2]], "dist")

    ## check attributes
    expect_equal(attr(out, "N"), 2)
 
    ## round trip
    lab.common <- attr(out,"labels")
    expect_equal(as.matrix(out[[1]]), as.matrix(D1)[lab.common,lab.common])
    expect_equal(as.matrix(out[[2]]), as.matrix(D2)[lab.common,lab.common])
    expect_identical(vimes_data(out), out)
})

