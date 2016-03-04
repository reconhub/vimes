context("Test non-exported functions")


## test vimes ##
test_that("test cutoff.ml", {
    ## skip on CRAN
    skip_on_cran()
    rm(list=ls())

    ## generate data
    set.seed(1)
    dat1 <- rnorm(30, c(0,1,6))
    d1 <- dist(dat1)
    f <- function(x) dgamma(x, shape=1, rate=2, log=TRUE)

    ## test output
    out <- cutoff.ml(d1, f)
    expect_equal(length(out), 1L)
    expect_equal(out, 0.3671218185)

})
