context("Test main functions")


## test vimes.prune ##
test_that("test vimes.prune", {
    ## skip on CRAN
    skip_on_cran()
    rm(list=ls())

    ## generate data
    x1 <- c(0,1,3)
    x2 <- c(2,5,10)
    names(x1) <- letters[1:3]
    names(x2) <- c('a', 'b', 'r')
    D1 <- dist(x1)
    D2 <- dist(x2)
    data <- vimes.data(D1, D2)
    data.no.na <- vimes.data(D1, D2)
    out <- vimes.prune(data[[1]], cutoff=0)
    out.no.na <- vimes.prune(data.no.na[[1]], cutoff=10)

    ## check output shape
    expect_is(out, "list")
    expect_is(out$graph, "igraph")
    expect_is(out$clusters, "list")
    expect_equal(out$cutoff, 0)
    expect_equal(length(out$clusters$membership), 2)

    expect_is(out.no.na, "list")
    expect_is(out.no.na$graph, "igraph")
    expect_is(out.no.na$clusters, "list")
    expect_equal(out.no.na$cutoff, 10)
    expect_equal(length(out.no.na$clusters$membership), 2)

})






