---
title: "Detecting disease outbreaks using vimes"
author: "Thibaut Jombart"
date: "2016-01-21"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteEngine{rmarkdown::render}
  %\VignetteIndexEntry{vimes: VIsualisation and Monitoring of EpidemicS.}
  \usepackage[utf8]{inputenc}
---



*vimes*: VIsualisation and Monitoring of EpidemicS 

=================================================
*vimes* provides tools for integrating various types of surveillance data for detecting disease outbreaks. This document provides an overview of the package's content.


Installing *vimes*
-------------
To install the development version from github:

```r
library(devtools)
install_github("thibautjombart/vimes")
```

The stable version can be installed from CRAN using:

```r
install.packages("vimes")
```

Then, to load the package, use:

```r
library("vimes")
```


A short demo
------------------
Here is a short demonstration of the package using a dummy dataset.

We first simulate the data using 3 mixtures of 3 normal distributions, and compute Euclidean distances between the observations for each mixture.
In practice, each mixture would be a different data type (e.g. location, time of onset of symptoms, genetic sequences of the pathogen):

```r
set.seed(2)
dat1 <- rnorm(30, c(0,1,6))
dat2 <- rnorm(30, c(0,0,1))
dat3 <- rnorm(30, c(8,1,2))
x <- lapply(list(dat1, dat2, dat3), dist)
```

The function `vimes.data` process the data and ensures matching of the individuals across different data sources:


```r
x <- vimes.data(x)
plot(x)
```

![plot of chunk vimesdata](figs/vimesdata-1.png)

We can now run `vimes` on the data:

```r
res <- vimes(x, cutoff=c(2,4,2))
res
```

```
## $graph
## IGRAPH UN-- 30 104 -- 
## + attr: layout_1 (g/n), layout_2 (g/n), layout_3 (g/n), layout
## | (g/n), color_1 (v/c), color_2 (v/c), color_3 (v/c), size_1
## | (v/n), size_2 (v/n), size_3 (v/n), label.family_1 (v/c),
## | label.family_2 (v/c), label.family_3 (v/c), label.color_1 (v/c),
## | label.color_2 (v/c), label.color_3 (v/c), name (v/c), color
## | (v/c), size (v/n), label.family (v/c), label.color (v/c),
## | weight_1 (e/n), weight_2 (e/n), weight_3 (e/n), label.color_1
## | (e/c), label.color_2 (e/c), label.color_3 (e/c), label.color
## | (e/c)
## + edges (vertex names):
## 
## $clusters
## $clusters$membership
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 
##  1  2  3  1  2  3  1  2  3  1  2  3  1  2  3  1  2  3  1  2  3  1  2  3  1 
## 26 27 28 29 30 
##  2  3  1  2  3 
## 
## $clusters$size
## [1] 10 10 10
## 
## $clusters$K
## [1] 3
## 
## $clusters$color
##         1         2         3 
## "#ff6666" "#666699" "#79d2a6" 
## 
## 
## $cutoff
## [1] 2 4 2
## 
## $separate.graphs
## $separate.graphs[[1]]
## $separate.graphs[[1]]$graph
## IGRAPH UNW- 30 177 -- 
## + attr: layout (g/n), name (v/c), color (v/c), size (v/n),
## | label.family (v/c), label.color (v/c), weight (e/n), label.color
## | (e/c)
## + edges (vertex names):
##  [1] 1--4  1--5  1--7  1--8  1--10 1--13 1--14 1--16 1--19 1--22 1--25
## [12] 1--26 1--28 2--5  2--7  2--8  2--10 2--11 2--13 2--14 2--17 2--19
## [23] 2--20 2--23 2--25 2--28 2--29 3--6  3--9  3--12 3--15 3--18 3--21
## [34] 3--24 3--27 3--30 4--7  4--8  4--10 4--13 4--14 4--16 4--22 4--25
## [45] 4--26 4--28 5--7  5--8  5--10 5--11 5--13 5--14 5--17 5--19 5--20
## [56] 5--23 5--25 5--28 5--29 6--9  6--12 6--15 6--18 6--21 6--24 6--27
## + ... omitted several edges
## 
## $separate.graphs[[1]]$clusters
## $separate.graphs[[1]]$clusters$membership
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 
##  1  1  2  1  1  2  1  1  2  1  1  2  1  1  2  1  1  2  1  1  2  1  1  2  1 
## 26 27 28 29 30 
##  1  2  1  1  2 
## 
## $separate.graphs[[1]]$clusters$size
## [1] 20 10
## 
## $separate.graphs[[1]]$clusters$K
## [1] 2
## 
## $separate.graphs[[1]]$clusters$color
##         1         2 
## "#ff6666" "#666699" 
## 
## 
## $separate.graphs[[1]]$cutoff
## [1] 2
## 
## 
## $separate.graphs[[2]]
## $separate.graphs[[2]]$graph
## IGRAPH UNW- 30 429 -- 
## + attr: layout (g/n), name (v/c), color (v/c), size (v/n),
## | label.family (v/c), label.color (v/c), weight (e/n), label.color
## | (e/c)
## + edges (vertex names):
##  [1] 1--2  1--3  1--4  1--5  1--6  1--7  1--8  1--9  1--10 1--11 1--12
## [12] 1--13 1--14 1--15 1--16 1--17 1--18 1--19 1--20 1--21 1--22 1--23
## [23] 1--24 1--25 1--26 1--27 1--28 1--29 1--30 2--3  2--4  2--5  2--6 
## [34] 2--7  2--8  2--9  2--10 2--11 2--12 2--13 2--14 2--15 2--16 2--17
## [45] 2--18 2--19 2--20 2--21 2--22 2--23 2--24 2--25 2--26 2--27 2--28
## [56] 2--29 2--30 3--4  3--5  3--6  3--7  3--8  3--9  3--10 3--11 3--12
## + ... omitted several edges
## 
## $separate.graphs[[2]]$clusters
## $separate.graphs[[2]]$clusters$membership
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 
##  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1 
## 26 27 28 29 30 
##  1  1  1  1  1 
## 
## $separate.graphs[[2]]$clusters$size
## [1] 30
## 
## $separate.graphs[[2]]$clusters$K
## [1] 1
## 
## $separate.graphs[[2]]$clusters$color
##         1 
## "#ff6666" 
## 
## 
## $separate.graphs[[2]]$cutoff
## [1] 4
## 
## 
## $separate.graphs[[3]]
## $separate.graphs[[3]]$graph
## IGRAPH UNW- 30 195 -- 
## + attr: layout (g/n), name (v/c), color (v/c), size (v/n),
## | label.family (v/c), label.color (v/c), weight (e/n), label.color
## | (e/c)
## + edges (vertex names):
##  [1] 1--4  1--7  1--10 1--13 1--16 1--22 1--25 1--28 2--3  2--5  2--6 
## [12] 2--9  2--12 2--14 2--15 2--17 2--18 2--21 2--26 2--27 2--30 3--5 
## [23] 3--6  3--8  3--9  3--11 3--12 3--14 3--15 3--17 3--18 3--20 3--21
## [34] 3--23 3--24 3--26 3--27 3--29 3--30 4--10 4--13 4--16 4--19 4--22
## [45] 4--28 5--6  5--8  5--9  5--11 5--12 5--14 5--15 5--17 5--18 5--20
## [56] 5--21 5--23 5--24 5--26 5--27 5--29 5--30 6--8  6--9  6--11 6--12
## + ... omitted several edges
## 
## $separate.graphs[[3]]$clusters
## $separate.graphs[[3]]$clusters$membership
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 
##  1  2  2  1  2  2  1  2  2  1  2  2  1  2  2  1  2  2  1  2  2  1  2  2  1 
## 26 27 28 29 30 
##  2  2  1  2  2 
## 
## $separate.graphs[[3]]$clusters$size
## [1] 10 20
## 
## $separate.graphs[[3]]$clusters$K
## [1] 2
## 
## $separate.graphs[[3]]$clusters$color
##         1         2 
## "#ff6666" "#666699" 
## 
## 
## $separate.graphs[[3]]$cutoff
## [1] 2
```

The main graph is:

```r
plot(res$graph, main="Main graph")
```

![plot of chunk res](figs/res-1.png)

```r
for(i in 1:3) {
plot(res$separate.graphs[[i]]$graph, main=paste("Graph from data", i))
}
```

![plot of chunk res](figs/res-2.png)![plot of chunk res](figs/res-3.png)![plot of chunk res](figs/res-4.png)
