---
title: "Detecting disease outbreaks using vimes"
author: "Thibaut Jombart"
date: "2017-02-28"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteIndexEntry{vimes: a quick demo.}
  \usepackage[utf8]{inputenc}
---



*vimes*: VIsualisation and Monitoring of EpidemicS 

=================================================

*vimes* provides tools for integrating various types of surveillance data for
 detecting disease outbreaks. This document provides an overview of the
 package's content.


Installing *vimes*
-------------
To install the development version from github:

```r
library(devtools)
install_github("reconhub/vimes")
```

The stable version can be installed from CRAN using:

```r
install.packages("vimes")
```

Then, to load the package, use:

```r
library("vimes")
```

```
## Error in library("vimes"): there is no package called 'vimes'
```


A short demo
------------------
Here is a short demonstration of the package using a dummy dataset.

We first simulate the data using 3 mixtures of 3 normal distributions, and
compute Euclidean distances between the observations for each mixture.  In
practice, each mixture would be a different data type (e.g. location, time of
onset of symptoms, genetic sequences of the pathogen):


```r
set.seed(2)
dat1 <- rnorm(30, c(0,1,6))
dat2 <- rnorm(30, c(0,0,1))
dat3 <- rnorm(30, c(8,1,2))
x <- lapply(list(dat1, dat2, dat3), dist)
```

The function `vimes_data` processes the data and ensures matching of the
individuals across different data sources:


```r
x <- vimes_data(x)
```

```
## Error in eval(expr, envir, enclos): could not find function "vimes_data"
```

```r
plot(x)
```

```
## Error in xy.coords(x, y, xlabel, ylabel, log): 'x' is a list, but does not have components 'x' and 'y'
```

We can now run `vimes` on the data:

```r
res <- vimes(x, cutoff = c(2,4,2))
```

```
## Error in eval(expr, envir, enclos): could not find function "vimes"
```

```r
names(res)
```

```
## Error in eval(expr, envir, enclos): object 'res' not found
```

```r
res$graph
```

```
## Error in eval(expr, envir, enclos): object 'res' not found
```

```r
res$clusters
```

```
## Error in eval(expr, envir, enclos): object 'res' not found
```

The main graph is:

```r
plot(res$graph, main="Main graph")
```

```
## Error in plot(res$graph, main = "Main graph"): object 'res' not found
```

```r
for(i in 1:3) {
plot(res$separate_graphs[[i]]$graph, main = paste("Graph from data", i))
}
```

```
## Error in plot(res$separate_graphs[[i]]$graph, main = paste("Graph from data", : object 'res' not found
```
