## Test environments
* local Win 10 Pro install, (R 3.6.0)
* local Linux install (Ubuntu 18.10, r-devel)
* rhub  (Ubuntu 16.04 r-release, Fedora r-devel, Debian r-devel)

## R CMD check results

Duration: 2m 36.6s

0 errors √ | 0 warnings √ | 0 notes √

## Changes versus version 1.6.3
* Version 1.6.3 was throwing an error in CRAN tests when loading the example 
  data in the package. Following discussion on CRAN@R-project.org and 
  r-sig-geo, this version includes a fix to the way coordinate reference 
  systems (CRS) are specified for the example data used in the package. Recent 
  versions of proj require EPSG codes to be specified in lower case 
  ('epsg:4326' rather than 'EPSG:4326'). This submission corrects CRS referencs 
  in the example data to use all lower case.
