# glcm

[![R-CMD-check](https://github.com/azvoleff/glcm/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/azvoleff/glcm/actions/workflows/R-CMD-check.yml)
[![CI](https://github.com/azvoleff/glcm/actions/workflows/test-coverage.yml/badge.svg)](https://github.com/azvoleff/glcm/actions/workflows/test-coverage.yml)

## Overview

The `glcm` package enables calculating image textures derived from grey-level
co-occurrence matrics (GLCMs) in R. The texture calculation is coded in C++
to optimize computation time. The `glcm` function in the package can compute 
the following texture statistics: mean (using either of two definitions), 
variance (using either of two definitions), homogeneity, contrast, 
dissimilarity, entropy, second_moment, and, correlation. The window size,
shift, and grey-level quantization are user determined. See the help file for 
`glcm` (included in the package) for details.

## Package Installation

To install the latest stable version of `glcm` from 
[CRAN](https://cran.r-project.org/package=glcm), fire up R and run:

```R
install.packages('glcm')
```

## Installing `glcm` Development Version

If you want the very latest version of `glcm`, you can install the development 
version. Be aware this version might not install as it is not as well tested as 
the stable version.

**NOTE: If you are installing on Windows, you will need to install the 
appropriate version of [Rtools](https://cran.r-project.org/bin/windows/Rtools/) 
for your version of R (as `glcm` contains C++ code) before you follow the below 
steps.**

The easiest way to install the development version of the `glcm` package is 
using the 
[`devtools`](https://cran.r-project.org/package=devtools) package by Hadley
Wickham. After installing `devtools` from CRAN, type:

```R
library(devtools)
install_github('azvoleff/glcm')
```

## Author Contact Information

[Alex Zvoleff](mailto:azvoleff@conservation.org)  
Conservation International  
2011 Crystal Dr. Suite 600  
Arlington, VA 22202  
USA
