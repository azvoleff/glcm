## Test environments
* local Win 11 install, (R 4.5.1)
* Check on rhub2: https://github.com/r-hub2/bossy-socialized-tapeworm-glcm/actions/runs/19286298311
* Check on win uilder


## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

Maintainer: 'Alex Zvoleff <azvoleff@conservation.org>'


## Changes versus version 1.6.5
On Sept 23 Kurt Hornik sent an email noting of the need to fix glcm due to "a recent upgrade of RcppArmadillo which now uses
Armadillo 15 by default". These changes address build issues due to that upgrade.