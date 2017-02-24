context("Test graph options")


## test data ##
test_that("test graph options", {
    ## skip on CRAN
    skip_on_cran()
    rm(list=ls())

    ## generate data
    out <- vimes_graph_opt(vertex_size = 3)

    ## check expected length
    expect_is(out, "list")
    expect_equal(out$vertex_size, 3)
    expect_error(vimes_graph_opt(foo = "bar"))
})

