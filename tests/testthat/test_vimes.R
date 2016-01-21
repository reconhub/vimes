

## test vimes ##
test_that("test vimes", {
    ## skip on CRAN
    skip_on_cran()
    rm(list=ls())

    ## generate data
    set.seed(2)
    dat1 <- rnorm(30, c(0,1,6))
    dat2 <- rnorm(30, c(0,0,1))
    dat3 <- rnorm(30, c(8,1,2))
    x <- lapply(list(dat1, dat2, dat3), dist)
    x <- vimes.data(x)

    ## analyse data
    res <- vimes(x, cutoff=c(2,4,2))
    plot(res$graph)

    ## tests
    expect_true(is.list(res))
    expect_is(res$graph, "igraph")
    expect_equal(res$clusters$K, 3)
    expect_true(is.list(res$separate.graphs))
    expect_equal(length(res$separate.graphs), length(x))
    for(e in res$separate.graphs){
        expect_is(e$graph, "igraph")
    }
})
