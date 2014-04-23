#' Image texture measures from grey-level co-occurrence matrices (GLCM)
#'
#' This function supports calculating texture statistics derived from 
#' grey-level co-occurrence matrices (GLCMs) in R.
#'
#' The \code{statistics} parameter should be a list, and can include any (one 
#' or more) of the following: 'mean', 'mean_ENVI', 'variance', 'variance_ENVI', 
#' 'homogeneity', 'contrast', 'dissimilarity', 'entropy', 'second_moment', 
#' and/or 'correlation'. By default all of the statistics except for 
#' "mean_ENVI" and "variance_ENVI" will be returned .
#' @export
#' @encoding UTF-8
#' @import Rcpp
#' @usage glcm(x, n_grey = 32, window = c(3, 3), shift = c(1, 1), statistics = 
#' c("mean", "variance", "homogeneity", "contrast", "dissimilarity", "entropy", 
#' "second_moment", "correlation"), min_x=NULL, max_x=NULL, na_opt="any", 
#' na_val=NA, scale_factor=1, asinteger=FALSE)
#' @param x a \code{RasterLayer} or \code{matrix}
#' @param n_grey number of grey levels to use in texture calculation
#' @param window the window size to consider for texture calculation as a two 
#' element integer vector (number of rows, number of columns)
#' @param shift a two element integer vector giving the shift (Q in Gonzalez 
#' and Woods, 2008), as (number of rows, number of columns)
#' @param statistics A list of GLCM texture measures to calculate (see 
#' Details).
#' @param min_x minimum value of input \code{RasterLayer} (optional, 
#' \code{glcm} will calculate if not supplied). Useful when running \code{glcm} 
#' over blocks of a raster.
#' @param max_x maximum value of input \code{RasterLayer} (optional, 
#' \code{glcm} will calculate if not supplied). Useful when running \code{glcm} 
#' over blocks of a raster.
#' @param na_opt How to handle NA values in \code{x}. Can be set to "ignore", 
#' "any" or "center". If set to "any", all textures statistics for a given 
#' pixel will be set to NA if there are any NA values in the \code{window} 
#' around that pixel. If set to "center" this will only occur if the center 
#' value is an NA. If set to "ignore", NA values in \code{window} will be 
#' ignored.
#' @param na_val the value to use to fill NA values on edges of \code{x} where 
#' textures cannot be calculated due to the window falling outside of the 
#' image, and as necessary depending on the chosen \code{na_opt}.
#' @param scale_factor factor by which to multiply results.  Useful if rounding 
#' results to integers (see \code{asinteger} argument).
#' @param asinteger whether to round results to nearest integer. Can be used to 
#' save space by saving results as, for example, an 'INT2S' \code{raster}.
#' @return A \code{RasterLayer} or \code{RasterStack} with the requested GLCM 
#' texture measures.
#' @references
#' Lu, D., and M. Batistella. 2005. Exploring TM image texture and its 
#' relationships with biomass estimation in Rondônia, Brazilian Amazon.  Acta 
#' Amazonica 35:249--257.
#'
#' Gonzalez, R. C. 2008. Digital image processing. 3rd ed. Prentice Hall, Upper 
#' Saddle River, N.J, pages 830--836.
#'
#' Haralick, R. M., K. Shanmugam, and I. Dinstein. 1973. Textural features for 
#' image classification. IEEE Transactions on Systems, Man and Cybernetics 
#' SMC-3:610--621.
#'
#' Pratt, W. K. 2007. Digital image processing: PIKS Scientific inside. 4th ed.
#' Wiley-Interscience, Hoboken, N.J pages 540--541, 563--566.
#' @examples
#' \dontrun{
#' require(raster)
#' textures <- glcm(raster(L5TSR_1986, layer=1))
#' plot(textures)
#' }
glcm <- function(x, n_grey=32, window=c(3, 3), shift=c(1, 1),
                 statistics=c('mean', 'variance', 'homogeneity', 'contrast', 
                              'dissimilarity', 'entropy', 'second_moment', 
                              'correlation'), min_x=NULL, max_x=NULL, 
                 na_opt='any', na_val=NA, scale_factor=1, asinteger=FALSE) {
    if (length(window) != 2) {
        stop('window must be integer vector of length 2')
    } else if (length(shift) != 2) {
        stop('shift must be integer vector of length 2')
    } else if ((window[1] %% 2 == 0) || (window[2] %% 2 == 0)) {
        stop('both elements of window must be odd')
    } else if ((window[1] + abs(shift[1])) > nrow(x)) {
        stop("window[1] + abs(shift[1]) must be less than nrow(x)")
    } else if ((window[2] + abs(shift[2])) > ncol(x)) {
        stop("window[2] + abs(shift[2]) must be less than ncol(x)")
    } else if (class(statistics) != 'character') {
        stop('statistics must be a character vector')
    }
    avail_stats <- c('mean', 'mean_ENVI', 'variance', 'variance_ENVI', 
                     'homogeneity', 'contrast', 'dissimilarity', 'entropy', 
                     'second_moment', 'correlation')
    stat_check <- unlist(lapply(statistics, function(stat) stat %in% avail_stats))
    if (sum(stat_check) != length(stat_check)) {
        stop(paste('invalid statistic(s):',
                   paste(statistics[!stat_check], collapse=', ')))
    }
    if (!(na_opt %in% c('any', 'center', 'ignore'))) {
        stop('na_opt must be "any", "center", or "ignore"')
    }
    # Resample the image to the required number of grey levels
    if (class(x) == 'RasterLayer') {
        if (!require(raster)) {
            stop('raster package is required for handling raster objects')
        }
        if (is.null(min_x)) min_x <- cellStats(x, 'min')
        if (is.null(max_x)) max_x <- cellStats(x, 'max')
        x_cut <- raster::cut(x, breaks=seq(min_x, max_x, length.out=(n_grey + 1)),
                             include.lowest=TRUE, right=FALSE)
        x_cut <- raster::as.matrix(x_cut)
    } else if ('matrix' %in% class(x) && (length(dim(x)) == 2)) {
        if (is.null(min_x)) min_x <- min(x)
        if (is.null(max_x)) max_x <- max(x)
        x_cut <- matrix(findInterval(x, seq(min_x, max_x, length.out=(n_grey + 1)), all.inside=TRUE),
                          nrow=nrow(x))
    } else {
        stop('x must be a RasterLayer or two-dimensional matrix')
    }

    textures <- calc_texture(x_cut, n_grey, window, shift, statistics, na_opt, 
                             na_val)

    if (class(x) == 'RasterLayer') {
        if (dim(textures)[3] > 1) {
            textures <- stack(apply(textures, 3, raster, template=x))
        } else {
            textures <- raster(textures[, , 1], template=x)
        }
        names(textures) <- paste('glcm', statistics, sep='_')
    } else if ('matrix' %in% class(x)) {
        dimnames(textures) <- list(NULL, NULL, paste('glcm', statistics, sep='_'))
    } else {
        stop('unknown object returned from calc_texture')
    }
    if (scale_factor != 1) {
        textures <- textures * scale_factor
    }
    if (asinteger) {
        textures <- round(textures)
    }
    return(textures)
}
