## Test environments
* local Win 10 Pro install, (R 3.6.2)
* rhub  (Ubuntu 16.04 r-release, Fedora r-devel, Debian r-devel)

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

Maintainer: 'Alex Zvoleff <azvoleff@conservation.org>'

New submission

Package was archived on CRAN

Possibly mis-spelled words in DESCRIPTION:
  GLCMs (5:10, 15:6)
  Haralick (13:53)

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2020-02-20 as check problems were not
    corrected in time.


## Changes versus version 1.6.4
On Jan 25 Prof. Ripley sent an email noting that glcm was one of several
packages in which "At least one of the datasets cannot be loaded without a
package declared in Suggests". This was the case for the "raster" package,
which was needed by glcm for several examples.

Unfortunately I was not able to address the above prior to 2/20 due to travel.
I have now added the raster package to IMPORTS to address the above issue. I am
resubmitting the package in hope of getting it back onto CRAN.
