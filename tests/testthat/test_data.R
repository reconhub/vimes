context("Test input data processing")



test_that("test: vimes.dist", {
    ## skip on CRAN
    skip_on_cran()
    rm(list=ls())

    ## generate data
    x1 <- c(0,1,3)
    x2 <- c(2,5,7,12)
    names(x1) <- letters[1:3]
    names(x2) <- c('a', 'r', 'c')
    D1 <- dist(x1)
    D2 <- dist(x2)
    out <- vimes.dist(D1, D2)

    ## check output shape
    expect_is(out, "list")
    expect_is(out, "vimes.input")
    expect_is(out[[1]], "dist")
    expect_is(out[[2]], "dist")

    ## check attributes
    expect_equal(attr(out, "N"), 2)
    ## expect_equal(attr(out, "labels"), rownames(as.matrix(out[[1]])))
    ## expect_equal(attr(out, "labels"), rownames(as.matrix(out[[2]])))

    ## round trip
    lab.common <- attr(out,"labels")
    expect_equal(as.matrix(out[[1]]), as.matrix(D1)[lab.common,lab.common])
    expect_equal(as.matrix(out[[2]]), as.matrix(D2)[lab.common,lab.common])
    expect_identical(vimes.dist(out), out)
})






test_that("test: vimes.data", {
    ## skip on CRAN
    skip_on_cran()
    rm(list=ls())

    ## generate data
    set.seed(1)
    x <- vimes.data(dates=sample(sim1$dates),
                    xy=sim1$xy,
                    dna=sim1$dna)

    ## check output form
    expect_is(x, "list")
    expect_equal(length(x), 3L)
    expect_is(x[[1]], "dist")
    expect_is(x[[2]], "dist")
    expect_is(x[[3]], "dist")
    expect_equal(attr(x,"N"), length(sim1$dates))

    ## check output values
    expect_equal(as.matrix(x[[1]])[1,4], 6)
    expect_equal(as.matrix(x[[1]])[5,4], 7)
    expect_equal(round(as.matrix(x[[2]])[1,2], 2), 8.7)
    expect_equal(round(as.matrix(x[[2]])[3,10], 2), 6.91)
    expect_equal(as.matrix(x[[3]])[2,3], 29)
    expect_equal(as.matrix(x[[3]])[1,2], 26)

})
