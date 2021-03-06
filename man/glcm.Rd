% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/glcm.R
\encoding{UTF-8}
\name{glcm}
\alias{glcm}
\title{Image texture measures from grey-level co-occurrence matrices (GLCM)}
\usage{
glcm(x, n_grey = 32, window = c(3, 3), shift = c(1, 1), statistics = 
c("mean", "variance", "homogeneity", "contrast", "dissimilarity", "entropy", 
"second_moment", "correlation"), min_x=NULL, max_x=NULL, na_opt="any", 
na_val=NA, scale_factor=1, asinteger=FALSE)
}
\arguments{
\item{x}{a \code{RasterLayer} or \code{matrix}}

\item{n_grey}{number of grey levels to use in texture calculation}

\item{window}{the window size to consider for texture calculation as a two 
element integer vector (number of rows, number of columns)}

\item{shift}{a list or matrix specifying the shift to use. See Details.}

\item{statistics}{A list of GLCM texture measures to calculate (see 
Details).}

\item{min_x}{minimum value of input \code{RasterLayer} (optional, 
\code{glcm} will calculate if not supplied). Useful when running \code{glcm} 
over blocks of a raster.}

\item{max_x}{maximum value of input \code{RasterLayer} (optional, 
\code{glcm} will calculate if not supplied). Useful when running \code{glcm} 
over blocks of a raster.}

\item{na_opt}{How to handle NA values in \code{x}. Can be set to "ignore", 
"any" or "center". If set to "any", all textures statistics for a given 
pixel will be set to NA if there are any NA values in the \code{window} 
around that pixel. If set to "center" this will only occur if the center 
value is an NA. If set to "ignore", NA values in \code{window} will be 
ignored.}

\item{na_val}{the value to use to fill NA values on edges of \code{x} where 
textures cannot be calculated due to the window falling outside of the 
image, and as necessary depending on the chosen \code{na_opt}.}

\item{scale_factor}{factor by which to multiply results.  Useful if rounding 
results to integers (see \code{asinteger} argument).}

\item{asinteger}{whether to round results to nearest integer. Can be used to 
save space by saving results as, for example, an 'INT2S' \code{raster}.}
}
\value{
A \code{RasterLayer} or \code{RasterStack} with the requested GLCM 
texture measures.
}
\description{
This function supports calculating texture statistics derived from 
grey-level co-occurrence matrices (GLCMs). The default textures are 
calculated using a 45 degree shift. See Details for other options.
}
\details{
The \code{statistics} parameter should be a list, and can include any (one 
or more) of the following: 'mean', 'mean_ENVI', 'variance', 'variance_ENVI', 
'homogeneity', 'contrast', 'dissimilarity', 'entropy', 'second_moment', 
and/or 'correlation'. By default all of the statistics except for 
"mean_ENVI" and "variance_ENVI" will be returned .

\code{shift} can be one of:
\enumerate{
  \item a two element integer vector giving the shift (Q in Gonzalez and 
  Woods, 2008), as (number of rows, number of columns).

 \item a list of integer vectors of length 2 specifying multiple (row, col) 
 shifts over which to calculate the GLCM textures. For example:
 \code{shift=list(c(1,1), c(-1,-1))}

 \item a matrix with two columns specifying, in rows, multiple (row, col) 
 shifts over which to calculate the GLCM textures. For example:
 \code{shift=matrix(c(1,1,-1,-1), byrow=TRUE, ncol=2)}
}

If multiple shifts are supplied, \code{glcm} will calculate each texture 
statistic using all the specified shifts, and return the mean value of the 
texture for each pixel. To calculate GLCM textures over "all directions" (in 
the terminology of commonly used remote sensing software), use: 
\code{shift=list(c(0,1), c(1,1), c(1,0), c(1,-1))}. This will calculate the 
average GLCM texture using shifts of 0 degrees, 45 degrees, 
90 degrees, and 135 degrees.
}
\examples{
# Calculate GLCM textures on a matrix
d <- matrix(seq(1:25), nrow=5, ncol=5, byrow=TRUE)

# Calculate using default 90 degree shift
glcm(d, statistics=c('variance'))

# Calculate over all directions
glcm(d, shift=list(c(0,1), c(1,1), c(1,0), c(1,-1)), 
     statistics=c('variance'))

\dontrun{
# Calculate GLCM textures on a raster
require(raster)
# Calculate using default 90 degree shift
textures_shift1 <- glcm(raster(L5TSR_1986, layer=1))
plot(textures_shift1)

# Calculate over all directions
textures_all_dir <- glcm(raster(L5TSR_1986, layer=1),
                         shift=list(c(0,1), c(1,1), c(1,0), c(1,-1)))
plot(textures_all_dir)
}
}
\references{
Lu, D., and M. Batistella. 2005. Exploring TM image texture and its 
relationships with biomass estimation in Rondônia, Brazilian Amazon.  Acta 
Amazonica 35:249--257.

Gonzalez, R. C. 2008. Digital image processing. 3rd ed. Prentice Hall, Upper 
Saddle River, N.J, pages 830--836.

Haralick, R. M., K. Shanmugam, and I. Dinstein. 1973. Textural features for 
image classification. IEEE Transactions on Systems, Man and Cybernetics 
SMC-3:610--621.

Pratt, W. K. 2007. Digital image processing: PIKS Scientific inside. 4th ed.
Wiley-Interscience, Hoboken, N.J pages 540--541, 563--566.
}
