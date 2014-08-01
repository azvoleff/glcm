#include <RcppArmadillo.h>
using namespace arma;

// Define a pointer to a texture function that will be used to map selected 
// co-occurrence statistics to the below texture calculation functions.
typedef double (*pfunc)(mat, mat, mat, double, double);

double text_mean(mat pij, mat imat, mat jmat, double mean_haralick, double ENVI_mean) {
    // Defined as in Haralick as mean of mux and muy
    return(mean_haralick);
}

double text_mean_ENVI(mat pij, mat imat, mat jmat, double mean_haralick, double ENVI_mean) {
    // Defined as in EXELIS ENVI (as simple mean over processing window)
    return(ENVI_mean);
}
            
double text_variance(mat pij, mat imat, mat jmat, double mean_haralick, double ENVI_mean) {
    // Defined as in Haralick, 1973, page 619 (equation 4)
    return(accu(square(imat - mean_haralick) % pij));
}

double text_variance_ENVI(mat pij, mat imat, mat jmat, double mean_haralick, double ENVI_mean) {
    // Defined as in EXELIS ENVI
    return(accu(square(imat - ENVI_mean) % pij) - 1);
}

double text_homogeneity(mat pij, mat imat, mat jmat, double mean_haralick, double ENVI_mean) {
    // Defined as in Haralick, 1973, page 619 (equation 5)
    return(accu(pij / (1 + square(imat - jmat))));
}

double text_contrast(mat pij, mat imat, mat jmat, double mean_haralick, double ENVI_mean) {
    // Defined as in Haralick, 1973, page 619 (equation 2)
    return(accu(pij % square(imat - jmat)));
}

double text_dissimilarity(mat pij, mat imat, mat jmat, double mean_haralick, double ENVI_mean) {
    //TODO: Find source for dissimilarity
    return(accu(pij % abs(imat - jmat)));
}

double text_entropy(mat pij, mat imat, mat jmat, double mean_haralick, double ENVI_mean) {
    // Defined as in Haralick, 1973, page 619 (equation 9)
    mat pij_log(pij);
    pij_log = mat(pij);
    pij_log(find(pij_log)) = log(pij_log(find(pij_log)));
    return(-accu(pij % pij_log));
}

double text_second_moment(mat pij, mat imat, mat jmat, double mean_haralick, double ENVI_mean) {
    // Defined as in Haralick, 1973, page 619
    return(accu(square(pij)));
}

double text_correlation(mat pij, mat imat, mat jmat, double mean_haralick, double ENVI_mean) {
    // Defined as in Gonzalez and Woods, 2009, page 832, also follows ENVI 
    // convention of using mr and mc equal to the sum rather than (as in 
    // Haralick 1973, eqn 3 on page 619) the mean of the pij by rows and columns
    double sigc, sigr, mr, mc;
    mr = sum(linspace<colvec>(1, pij.n_cols, pij.n_cols) % sum(pij, 1));
    mc = sum(linspace<rowvec>(1, pij.n_rows, pij.n_rows) % sum(pij, 0));
    // Calculate sigr and sigc (measures of row and column std deviation)
    sigr = sqrt(sum(square(linspace<colvec>(1, pij.n_cols, pij.n_cols) - mr) % sum(pij, 1)));
    sigc = sqrt(sum(square(linspace<rowvec>(1, pij.n_rows, pij.n_rows) - mc) % sum(pij, 0)));
    return((accu(imat % jmat % pij) - mr * mc) / (sigr * sigc));
}

//' Calculates a glcm texture for use in the glcm.R script
//'
//' This function is called by the \code{\link{glcm}} function. It is 
//' not intended to be used directly.
//'
//' @param rast a matrix containing the pixels to be used in the texture
//' calculation
//' @param n_grey number of grey levels to use in texture calculation
//' @param window_dims 2 element list with row and column dimensions of the
//' texture window
//' @param shift a matrix where each row gives an (x, y) shift to use when 
//' computing co-occurrency matrices. Textures will be calculated for each 
//' shift, and the average over all shifts will be returned.
//' @param statistics a list of strings naming the texture statistics to 
//' calculate
//' @param na_opt one of "ignore", "center", or "any"
//' @param na_val what value to use to fill missing values on edges or where
//' necessary due to chosen na_opt value
//' @return a list of length equal to the length of the \code{statistics} input 
//' parameter, containing the selected textures measures
// [[Rcpp::export]]
arma::cube calc_texture(arma::mat rast,
        int n_grey, arma::rowvec window_dims, arma::mat shift,
        Rcpp::CharacterVector statistics, std::string na_opt, double na_val) {
    mat imat(n_grey, n_grey);
    mat jmat(n_grey, n_grey);
    mat base_window(window_dims(0), window_dims(1));
    mat offset_window(window_dims(0), window_dims(1));
    mat pij(n_grey, n_grey);
    mat base_ul(shift.n_rows, 2, fill::zeros);
    mat offset_ul(shift.n_rows, 2), center_coord(shift.n_rows, 2), 
        lr_coord(shift.n_rows, 2);
    double mean_haralick, ENVI_mean;

    // textures_temp mat will hold the texture statistics as calculated for 
    // each shift, with textures in rows and per-shift values of each texture 
    // in columns
    mat textures_temp(statistics.size(), shift.n_rows);

    // textures cube will hold the final calculated texture statistics 
    // (averaged over all shifts as required)
    cube textures(rast.n_rows, rast.n_cols, statistics.size());

    std::map<std::string, double (*)(mat, mat, mat, double, double)> stat_func_map;
    stat_func_map["mean"] = text_mean;
    stat_func_map["mean_ENVI"] = text_mean_ENVI;
    stat_func_map["variance"] = text_variance;
    stat_func_map["variance_ENVI"] = text_variance_ENVI;
    stat_func_map["homogeneity"] = text_homogeneity;
    stat_func_map["contrast"] = text_contrast;
    stat_func_map["dissimilarity"] = text_dissimilarity;
    stat_func_map["entropy"] = text_entropy;
    stat_func_map["second_moment"] = text_second_moment;
    stat_func_map["correlation"] = text_correlation;

    // Calculate the base upper left (ul) coords and offset upper left coords 
    // for each shift, as row, column with zero based indices.
    for(unsigned n=0; n < shift.n_rows; n++) {
        if (shift(n, 0) < 0) {
            base_ul(n, 0) = base_ul(n, 0) + abs(shift(n, 0));
        }
        if (shift(n, 1) < 0) {
            base_ul(n, 1) = base_ul(n, 1) + abs(shift(n, 1));
        }
        offset_ul.row(n) = base_ul.row(n) + shift.row(n);
        center_coord.row(n) = base_ul.row(n) + floor(window_dims / 2);
        // lr_coord gives the maximum lower right coordinate included in 
        // the combined area of the offset and base windows
        lr_coord.row(n) = window_dims + abs(shift.row(n)) - 1;
    }

    // Make base_ul and offset_ul relative to center_coord of 0, 0
    base_ul = base_ul - center_coord;
    offset_ul = offset_ul - center_coord;
    lr_coord = lr_coord - center_coord;

    // Rcpp::Rcout << "base_ul:" << std::endl;
    // base_ul.print();
    // Rcpp::Rcout << "offset_ul:" << std::endl;
    // offset_ul.print();
    // Rcpp::Rcout << "center_coord:" << std::endl;
    // center_coord.print();
    // Rcpp::Rcout << "lr_coord:" << std::endl;
    // lr_coord.print();
     
    // Make a matrix of i's and a matrix of j's to be used in the below matrix 
    // calculations. These matrices are the same shape as pij with the entries 
    // equal to the i indices of each cell (for the imat matrix, which is 
    // indexed over the rows) or the j indices of each cell (for the jmat 
    // matrix, which is indexed over the columns). Note that linspace<mat> 
    // makes a column vector.
    //imat = repmat(linspace<colvec>(1, pij.n_rows, pij.n_rows), 1, 
    //pij.n_cols);
    //jmat = repmat(linspace<rowvec>(1, pij.n_cols, pij.n_cols), pij.n_rows, 
    //1);
    imat = repmat(linspace<vec>(1, pij.n_rows, pij.n_rows), 1, pij.n_cols);
    jmat = trans(imat);

    // Rcpp::Rcout << "cube_size: (" << textures.n_rows << ", " <<textures.n_cols << ", " <<textures.n_slices << ")" << std::endl;
    // Rcpp::Rcout << "ctr_x_limit: " << (rast.n_rows - max(lr_coord.col(0))) << std::endl;
    // Rcpp::Rcout << "ctr_y_limit: " << (rast.n_cols - max(lr_coord.col(1))) << std::endl;
    for(unsigned ctr_x=max(center_coord.col(0)); ctr_x < (rast.n_rows - max(lr_coord.col(0))); ctr_x++) {
        for(unsigned ctr_y=max(center_coord.col(1)); ctr_y < (rast.n_cols - max(lr_coord.col(1))); ctr_y++) {
            for(unsigned shift_num=0; shift_num < shift.n_rows; shift_num++) {
                // // Debugging code for tracking the upper left corner coordinate 
                // // of base and offset matrices
                // umat dbg_base(rast.n_rows, rast.n_cols, fill::zeros);
                // for(unsigned i=0; i < dbg_base.n_elem; i++) {
                //     dbg_base(i) = i;
                // }
                // dbg_base(ctr_x + base_ul(shift_num, 0), ctr_y + base_ul(shift_num, 1)) = 555;
                // dbg_base(ctr_x + offset_ul(shift_num, 0), ctr_y + offset_ul(shift_num, 1)) = 999;
                // dbg_base.print("ul coords (555 is base, 999 is offset):");

                base_window = rast.submat(ctr_x + base_ul(shift_num, 0),
                                          ctr_y + base_ul(shift_num, 1),
                                          ctr_x + base_ul(shift_num, 0) + window_dims(0) - 1,
                                          ctr_y + base_ul(shift_num, 1) + window_dims(1) - 1);

                offset_window = rast.submat(ctr_x + offset_ul(shift_num, 0),
                                            ctr_y + offset_ul(shift_num, 1),
                                            ctr_x + offset_ul(shift_num, 0) + window_dims(0) - 1,
                                            ctr_y + offset_ul(shift_num, 1) + window_dims(1) - 1);
                pij.fill(0);

                if (na_opt == "any") {
                    // Return NA for all textures within this window if there are 
                    // any NA values within the base_window or the offset_window
                    bool na_flag=false;
                    for(unsigned i=0; i < base_window.n_elem; i++) {
                        if ((!arma::is_finite(base_window(i))) || (!arma::is_finite(offset_window(i)))) {
                            for(signed s=0; s < statistics.size(); s++) {
                                textures_temp(s, shift_num) = na_val;
                            }
                            na_flag = true;
                            break;
                        }
                    }
                    if (na_flag) continue;
                } else if (na_opt == "center") {
                    // Return NA for all textures within this window if the center 
                    // value is an NA
                    if (!arma::is_finite(base_window(center_coord(0), center_coord(1)))) {
                        for(signed s=0; s < statistics.size(); s++) {
                            textures_temp(s, shift_num) = na_val;
                        }
                        continue;
                    }
                }

                for(unsigned i=0; i < base_window.n_elem; i++) {
                    if (!(arma::is_finite(base_window(i)) || arma::is_finite(offset_window(i)))) {
                        // This will execute only if there is an NA in the window 
                        // AND na_opt is set to "ignore" or "center"
                        continue;
                    }
                    // Subtract one from the below indices to correct for row and 
                    // col indices starting at 0 in C++ versus 1 in R.
                    pij(base_window(i) - 1, offset_window(i) - 1)++;
                }
                pij = pij / base_window.n_elem;

                mean_haralick = (mean(linspace<colvec>(1, pij.n_rows, pij.n_rows) %
                            sum(pij, 1)) +
                        mean(linspace<rowvec>(1, pij.n_rows, pij.n_rows) %
                            sum(pij, 0))) / 2;
                ENVI_mean = mean(vectorise(base_window) - 1);

                // Loop over the selected statistics, using the stat_func_map map 
                // to map each selected statistic to the appropriate texture 
                // function.
                for(signed i=0; i < statistics.size(); i++) {
                    pfunc f = stat_func_map[Rcpp::as<std::string>(statistics(i))];
                    textures_temp(i, shift_num) = (*f)(pij, imat, jmat, mean_haralick, ENVI_mean);
                }
            }
            // Average the statistics and across the shifts
            for(signed i=0; i < statistics.size(); i++) {
                textures(ctr_x, ctr_y, i) = mean(textures_temp.row(i));
            }
        }
    }

    // The below loops fill in border areas with nan values in areas where 
    // textures cannot be calculated due to edge effects. This saves 
    // computation time by avoiding the need to use fill::zeros when the matrix 
    // is initialized at the beginning of the function.
    //
    
    // Fill nan values on left border
    for(unsigned row=0; row < rast.n_rows; row++) {
        for(unsigned col=0; col < max(center_coord.col(1)); col++) {
            for(signed i=0; i < statistics.size(); i++) {
                textures(row, col, i) = na_val;
            }
        }
    }

    // Fill nan values on top border
    for(unsigned row=0; row < max(center_coord.col(0)); row++) {
        for(unsigned col=0; col < rast.n_cols; col++) {
            for(signed i=0; i < statistics.size(); i++) {
                textures(row, col, i) = na_val;
            }
        }
    }

    // Fill nan values on right border
    for(unsigned row=0; row < rast.n_rows; row++) {
        for(unsigned col=(rast.n_cols - max(lr_coord.col(1))); col < rast.n_cols; col++) {
            for(signed i=0; i < statistics.size(); i++) {
                textures(row, col, i) = na_val;
            }
        }
    }

    // Fill nan values on bottom border
    for(unsigned row=(rast.n_rows - max(lr_coord.col(0))); row < rast.n_rows; row++) {
        for(unsigned col=0; col < rast.n_cols; col++) {
            for(signed i=0; i < statistics.size(); i++) {
                textures(row, col, i) = na_val;
            }
        }
    }

    return(textures);
}
