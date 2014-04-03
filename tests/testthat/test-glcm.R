context("GLCM textures")

suppressMessages(library(raster))

# Make a function to get 2d matrix from 3d matrix returned by glcm
get_pkg_glcm_texture <- function(statistic, window, shift) {
    if (length(statistic) != 1) {
        stop('length of statistic must be equal to 1')
    }
    # Note the na_val=0 is needed to match ENVI output
    texture <- glcm(test_raster, 32, window, shift, statistic, na_val=0)
    return(getValues(texture))
}

# glcm(raster(L5TSR_1986, layer=1), window=c(3, 5))
#
# glcm(raster(L5TSR_1986, layer=1), window=c(5, 7))
#
# expect_equal(get_pkg_glcm_texture('mean_ENVI', c(3, 5), c(1, 1)),
#              expected=getValues(test_raster_ENVI_textures_3x3$mean),
#              tolerance=.000001)
#
# expect_equal(get_pkg_glcm_texture('mean_ENVI', c(5, 7), c(2, 3)),
#              expected=getValues(test_raster_ENVI_textures_5x7$mean),
#              tolerance=.000001)
#
# b <- glcm(test_raster, statistics=c('mean_ENVI', 'variance_ENVI', 
#                                     'homogeneity', 'contrast', 'dissimilarity', 
#                                     'entropy', 'second_moment', 'correlation'), 
#           window=c(5, 7), shift=c(2, 3), na_val=0)
# plot(b)
# plot(test_raster_ENVI_textures_5x7)
#
# b <- glcm(test_raster, statistics=c('mean_ENVI', 'variance_ENVI', 
#                                     'homogeneity', 'contrast', 'dissimilarity', 
#                                     'entropy', 'second_moment', 'correlation'), 
#           window=c(3, 3), shift=c(1, 1), na_val=0)
# plot(b)
# plot(test_raster_ENVI_textures_3x3)

# Test all statistics that are available in EXELIS ENVI match the textures 
# output by pkg
test_that("GLCM mean is correct", {
    expect_equal(get_pkg_glcm_texture('mean_ENVI', c(3, 3), c(1, 1)),
                 expected=getValues(test_raster_ENVI_textures_3x3$mean_ENVI),
                 tolerance=.000001)
    expect_equal(get_pkg_glcm_texture('mean_ENVI', c(5, 7), c(2, 3)),
                 expected=getValues(test_raster_ENVI_textures_5x7$mean_ENVI),
                 tolerance=.000001)
})

test_that("GLCM variance is correct", {
    expect_equal(get_pkg_glcm_texture('variance_ENVI', c(3, 3), c(1, 1)),
                 expected=getValues(test_raster_ENVI_textures_3x3$variance_ENVI),
                 tolerance=.000001)
    expect_equal(get_pkg_glcm_texture('variance_ENVI', c(5, 7), c(2, 3)),
                 expected=getValues(test_raster_ENVI_textures_5x7$variance_ENVI),
                 tolerance=.000001)
})

test_that("GLCM homogeneity is correct", {
    expect_equal(get_pkg_glcm_texture('homogeneity', c(3, 3), c(1, 1)),
                 expected=getValues(test_raster_ENVI_textures_3x3$homogeneity),
                 tolerance=.000001)
    expect_equal(get_pkg_glcm_texture('homogeneity', c(5, 7), c(2, 3)),
                 expected=getValues(test_raster_ENVI_textures_5x7$homogeneity),
                 tolerance=.000001)
})

test_that("GLCM contrast is correct", {
    expect_equal(get_pkg_glcm_texture('contrast', c(3, 3), c(1, 1)),
                 expected=getValues(test_raster_ENVI_textures_3x3$contrast),
                 tolerance=.000001)
    expect_equal(get_pkg_glcm_texture('contrast', c(5, 7), c(2, 3)),
                 expected=getValues(test_raster_ENVI_textures_5x7$contrast),
                 tolerance=.000001)
})

test_that("GLCM dissimilarity is correct", {
    expect_equal(get_pkg_glcm_texture('dissimilarity', c(3, 3), c(1, 1)),
                 expected=getValues(test_raster_ENVI_textures_3x3$dissimilarity),
                 tolerance=.000001)
    expect_equal(get_pkg_glcm_texture('dissimilarity', c(5, 7), c(2, 3)),
                 expected=getValues(test_raster_ENVI_textures_5x7$dissimilarity),
                 tolerance=.000001)
})

test_that("GLCM entropy is correct", {
    expect_equal(get_pkg_glcm_texture('entropy', c(3, 3), c(1, 1)),
                 expected=getValues(test_raster_ENVI_textures_3x3$entropy),
                 tolerance=.000001)
    expect_equal(get_pkg_glcm_texture('entropy', c(5, 7), c(2, 3)),
                 expected=getValues(test_raster_ENVI_textures_5x7$entropy),
                 tolerance=.000001)
})

test_that("GLCM second_moment is correct", {
    expect_equal(get_pkg_glcm_texture('second_moment', c(3, 3), c(1, 1)),
                 expected=getValues(test_raster_ENVI_textures_3x3$second_moment),
                 tolerance=.000001)
    expect_equal(get_pkg_glcm_texture('second_moment', c(5, 7), c(2, 3)),
                 expected=getValues(test_raster_ENVI_textures_5x7$second_moment),
                 tolerance=.000001)
})

test_that("GLCM correlation is correct", {
    expect_equal(get_pkg_glcm_texture('correlation', c(3, 3), c(1, 1)),
                 expected=getValues(test_raster_ENVI_textures_3x3$correlation),
                 tolerance=.000001)
    expect_equal(get_pkg_glcm_texture('correlation', c(5, 7), c(2, 3)),
                 expected=getValues(test_raster_ENVI_textures_5x7$correlation),
                 tolerance=.000001)
})

# Test that glcm run on a raster matches the output from running glcm directly 
# on a matrix
glcm_corr_matrix <- glcm(raster::as.matrix(test_raster), 32, c(3, 3), c(1, 1), 'correlation', na_val=0)
glcm_corr_matrix <- matrix(glcm_corr_matrix, nrow=nrow(glcm_corr_matrix))
test_that("GLCM run on a matrix works correctly", {
    expect_equal(glcm_corr_matrix,
                 expected=raster::as.matrix(test_raster_ENVI_textures_3x3$correlation),
                 tolerance=.000001)
})

glcm_corr_int <- round(glcm(test_raster, 32, c(3, 3), c(1, 1), 'correlation', na_val=0) * 1000)
test_that("GLCM scaling works correctly when run with scaling and rounding", {
    expect_equal(glcm(test_raster, 32, c(3, 3), c(1, 1), 'correlation', 
                      asinteger=TRUE, scale_factor=1000, na_val=0),
                 expected=glcm_corr_int,
                 tolerance=.000001)
})
