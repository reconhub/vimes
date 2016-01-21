context("Test main functions")


## test data ##
test_that("test vimes.graph", {
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
    data <- vimes.data(D1, D2)
    out <- vimes.graph(data[[1]], cutoff=0)

    ## check output shape
    expect_is(out, "list")
    expect_is(out$graph, "igraph")
    expect_is(out$clusters, "list")
    expect_equal(out$cutoff, 0)
    expect_equal(length(out$clusters$membership), 4)

})

