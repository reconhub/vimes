

## test vimes ##
test_that("test vimes", {
    ## skip on CRAN
    skip_on_cran()

    ## generate data
    set.seed(2)
    dat1 <- rnorm(30, c(0,1,6))
    dat2 <- rnorm(30, c(0,0,1))
    dat3 <- rnorm(30, c(8,1,2))
    x <- lapply(list(dat1, dat2, dat3), dist)
    x <- vimes_data(x)

    log.dens <- list(f1 = function(x) dgamma(x,2,1, log = TRUE),
                     f2 = function(x) dgamma(x,2,1/2, log = TRUE),
                     f3 = function(x) dgamma(x,2,1, log = TRUE))

    ## analyse data
    res_bas <- vimes(x, cutoff = c(2,4,2))
    
    ## tests basic results
    expect_true(is.list(res_bas))
    expect_is(res_bas$graph, "igraph")
    expect_equal(res_bas$clusters$K, 3)
    expect_true(is.list(res_bas$separate_graphs))
    expect_equal(length(res_bas$separate_graphs), length(x))
    for(e in res_bas$separate_graphs){
        expect_is(e$graph, "igraph")
    }


})
