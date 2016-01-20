---
title: "Detecting disease outbreaks using vimes"
author: "Thibaut Jombart"
date: "2016-01-20"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteEngine{knitr::rmarkdown}
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

