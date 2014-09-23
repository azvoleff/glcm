# glcm

[![Build Status](https://travis-ci.org/azvoleff/glcm.png)](https://travis-ci.org/azvoleff/glcm)

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

The easiest way to install the (development version) of the `glcm` package is 
using the 
[`devtools`](http://cran.r-project.org/web/packages/devtools/index.html) 
package by Hadley Wickham. After installing `devtools` from CRAN, type:

```R
install_github('glcm', username='azvoleff')
```

at the R prompt to install `glcm`.

If you are installing on Windows, you will first need to install the 
appropriate version of [Rtools](http://cran.r-project.org/bin/windows/Rtools/) 
for your version of R (as `glcm` contains C++ code).

## Installing `glcm` Development Version

If you want the very latest version of `glcm`, you can install the 
development version. Be aware this version might not install as it is not as 
well tested as the stable version. To install from the `glcm` development 
branch, run:

```R
library(devtools)
install_github('azvoleff/glcm', ref="development")
```

## Author Contact Information

[Alex Zvoleff](mailto:azvoleff@conservation.org)  
Postdoctoral Associate  
Tropical Ecology Assessment and Monitoring (TEAM) Network  
Conservation International  
2011 Crystal Dr. Suite 500  
Arlington, VA 22202  
USA
