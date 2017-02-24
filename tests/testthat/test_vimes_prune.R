context("Test main functions")


test_that("test vimes_prune", {
    ## skip on CRAN
    skip_on_cran()

    ## generate data
    x1 <- c(0,1,3)
    x2 <- c(2,5,10)
    names(x1) <- letters[1:3]
    names(x2) <- c('a', 'b', 'r')
    D1 <- dist(x1)
    D2 <- dist(x2)
    data <- vimes_data(D1, D2)
    data_no_na <- vimes_data(D1, D2)
    out <- vimes_prune(data[[1]], cutoff=0)
    out_no_na <- vimes_prune(data_no_na[[1]], cutoff=10)

    ## check output shape
    expect_is(out, "list")
    expect_is(out$graph, "igraph")
    expect_is(out$clusters, "list")
    expect_equal(out$cutoff, 0)
    expect_equal(length(out$clusters$membership), 2)

    expect_is(out_no_na, "list")
    expect_is(out_no_na$graph, "igraph")
    expect_is(out_no_na$clusters, "list")
    expect_equal(out_no_na$cutoff, 10)
    expect_equal(length(out_no_na$clusters$membership), 2)

})






