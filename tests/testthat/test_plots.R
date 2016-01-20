context("Test input data processing")


## test data ##
test_that("test plot.vimes.input", {
    ## skip on CRAN
    skip_on_cran()
    rm(list=ls())

    ## generate data
    set.seed(1)
    x1 <- rnorm(20)
    x2 <- runif(20)
    names(x1) <- sample(letters, 20)
    names(x2) <- sample(letters, 20)
    D1 <- dist(x1)
    D2 <- dist(x2)
    out <- vimes.data(D1, D2)
    out
    plot(out)

    ## check expected errors
    expect_error(plot(out, n=-1))
})

