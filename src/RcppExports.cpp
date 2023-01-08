// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// calcCentroids
NumericMatrix calcCentroids(arma::mat data, arma::mat belongmatrix, double m);
RcppExport SEXP _geocmeans_calcCentroids(SEXP dataSEXP, SEXP belongmatrixSEXP, SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type data(dataSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type belongmatrix(belongmatrixSEXP);
    Rcpp::traits::input_parameter< double >::type m(mSEXP);
    rcpp_result_gen = Rcpp::wrap(calcCentroids(data, belongmatrix, m));
    return rcpp_result_gen;
END_RCPP
}
// calcBelongMatrix
NumericMatrix calcBelongMatrix(NumericMatrix centers, NumericMatrix data, double m, NumericVector sigmas);
RcppExport SEXP _geocmeans_calcBelongMatrix(SEXP centersSEXP, SEXP dataSEXP, SEXP mSEXP, SEXP sigmasSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type centers(centersSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type data(dataSEXP);
    Rcpp::traits::input_parameter< double >::type m(mSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type sigmas(sigmasSEXP);
    rcpp_result_gen = Rcpp::wrap(calcBelongMatrix(centers, data, m, sigmas));
    return rcpp_result_gen;
END_RCPP
}
// calcSWFCCentroids
NumericMatrix calcSWFCCentroids(arma::mat data, arma::mat wdata, arma::mat belongmatrix, double m, double alpha);
RcppExport SEXP _geocmeans_calcSWFCCentroids(SEXP dataSEXP, SEXP wdataSEXP, SEXP belongmatrixSEXP, SEXP mSEXP, SEXP alphaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type data(dataSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type wdata(wdataSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type belongmatrix(belongmatrixSEXP);
    Rcpp::traits::input_parameter< double >::type m(mSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    rcpp_result_gen = Rcpp::wrap(calcSWFCCentroids(data, wdata, belongmatrix, m, alpha));
    return rcpp_result_gen;
END_RCPP
}
// calcSFCMBelongMatrix
NumericMatrix calcSFCMBelongMatrix(NumericMatrix centers, NumericMatrix data, NumericMatrix wdata, double m, double alpha, NumericVector sigmas, NumericVector wsigmas);
RcppExport SEXP _geocmeans_calcSFCMBelongMatrix(SEXP centersSEXP, SEXP dataSEXP, SEXP wdataSEXP, SEXP mSEXP, SEXP alphaSEXP, SEXP sigmasSEXP, SEXP wsigmasSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type centers(centersSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type data(dataSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type wdata(wdataSEXP);
    Rcpp::traits::input_parameter< double >::type m(mSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type sigmas(sigmasSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type wsigmas(wsigmasSEXP);
    rcpp_result_gen = Rcpp::wrap(calcSFCMBelongMatrix(centers, data, wdata, m, alpha, sigmas, wsigmas));
    return rcpp_result_gen;
END_RCPP
}
// calcBelongMatrixNoisy
NumericMatrix calcBelongMatrixNoisy(NumericMatrix centers, NumericMatrix data, double m, double delta, NumericVector sigmas);
RcppExport SEXP _geocmeans_calcBelongMatrixNoisy(SEXP centersSEXP, SEXP dataSEXP, SEXP mSEXP, SEXP deltaSEXP, SEXP sigmasSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type centers(centersSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type data(dataSEXP);
    Rcpp::traits::input_parameter< double >::type m(mSEXP);
    Rcpp::traits::input_parameter< double >::type delta(deltaSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type sigmas(sigmasSEXP);
    rcpp_result_gen = Rcpp::wrap(calcBelongMatrixNoisy(centers, data, m, delta, sigmas));
    return rcpp_result_gen;
END_RCPP
}
// calcSFCMBelongMatrixNoisy
NumericMatrix calcSFCMBelongMatrixNoisy(NumericMatrix centers, NumericMatrix data, NumericMatrix wdata, double m, double alpha, double delta, NumericVector sigmas, NumericVector wsigmas);
RcppExport SEXP _geocmeans_calcSFCMBelongMatrixNoisy(SEXP centersSEXP, SEXP dataSEXP, SEXP wdataSEXP, SEXP mSEXP, SEXP alphaSEXP, SEXP deltaSEXP, SEXP sigmasSEXP, SEXP wsigmasSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type centers(centersSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type data(dataSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type wdata(wdataSEXP);
    Rcpp::traits::input_parameter< double >::type m(mSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< double >::type delta(deltaSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type sigmas(sigmasSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type wsigmas(wsigmasSEXP);
    rcpp_result_gen = Rcpp::wrap(calcSFCMBelongMatrixNoisy(centers, data, wdata, m, alpha, delta, sigmas, wsigmas));
    return rcpp_result_gen;
END_RCPP
}
// calcFGCMBelongMatrix
NumericMatrix calcFGCMBelongMatrix(NumericMatrix centers, NumericMatrix data, double m, double beta, NumericVector sigmas);
RcppExport SEXP _geocmeans_calcFGCMBelongMatrix(SEXP centersSEXP, SEXP dataSEXP, SEXP mSEXP, SEXP betaSEXP, SEXP sigmasSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type centers(centersSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type data(dataSEXP);
    Rcpp::traits::input_parameter< double >::type m(mSEXP);
    Rcpp::traits::input_parameter< double >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type sigmas(sigmasSEXP);
    rcpp_result_gen = Rcpp::wrap(calcFGCMBelongMatrix(centers, data, m, beta, sigmas));
    return rcpp_result_gen;
END_RCPP
}
// calcSFGCMBelongMatrix
NumericMatrix calcSFGCMBelongMatrix(NumericMatrix centers, NumericMatrix data, NumericMatrix wdata, double m, double alpha, double beta, NumericVector sigmas, NumericVector wsigmas);
RcppExport SEXP _geocmeans_calcSFGCMBelongMatrix(SEXP centersSEXP, SEXP dataSEXP, SEXP wdataSEXP, SEXP mSEXP, SEXP alphaSEXP, SEXP betaSEXP, SEXP sigmasSEXP, SEXP wsigmasSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type centers(centersSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type data(dataSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type wdata(wdataSEXP);
    Rcpp::traits::input_parameter< double >::type m(mSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< double >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type sigmas(sigmasSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type wsigmas(wsigmasSEXP);
    rcpp_result_gen = Rcpp::wrap(calcSFGCMBelongMatrix(centers, data, wdata, m, alpha, beta, sigmas, wsigmas));
    return rcpp_result_gen;
END_RCPP
}
// calcFGCMBelongMatrixNoisy
NumericMatrix calcFGCMBelongMatrixNoisy(NumericMatrix centers, NumericMatrix data, double m, double beta, double delta, NumericVector sigmas);
RcppExport SEXP _geocmeans_calcFGCMBelongMatrixNoisy(SEXP centersSEXP, SEXP dataSEXP, SEXP mSEXP, SEXP betaSEXP, SEXP deltaSEXP, SEXP sigmasSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type centers(centersSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type data(dataSEXP);
    Rcpp::traits::input_parameter< double >::type m(mSEXP);
    Rcpp::traits::input_parameter< double >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< double >::type delta(deltaSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type sigmas(sigmasSEXP);
    rcpp_result_gen = Rcpp::wrap(calcFGCMBelongMatrixNoisy(centers, data, m, beta, delta, sigmas));
    return rcpp_result_gen;
END_RCPP
}
// calcSFGCMBelongMatrixNoisy
NumericMatrix calcSFGCMBelongMatrixNoisy(NumericMatrix centers, NumericMatrix data, NumericMatrix wdata, double m, double alpha, double beta, double delta, NumericVector sigmas, NumericVector wsigmas);
RcppExport SEXP _geocmeans_calcSFGCMBelongMatrixNoisy(SEXP centersSEXP, SEXP dataSEXP, SEXP wdataSEXP, SEXP mSEXP, SEXP alphaSEXP, SEXP betaSEXP, SEXP deltaSEXP, SEXP sigmasSEXP, SEXP wsigmasSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type centers(centersSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type data(dataSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type wdata(wdataSEXP);
    Rcpp::traits::input_parameter< double >::type m(mSEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< double >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< double >::type delta(deltaSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type sigmas(sigmasSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type wsigmas(wsigmasSEXP);
    rcpp_result_gen = Rcpp::wrap(calcSFGCMBelongMatrixNoisy(centers, data, wdata, m, alpha, beta, delta, sigmas, wsigmas));
    return rcpp_result_gen;
END_RCPP
}
// focal_euclidean_mat_window
NumericMatrix focal_euclidean_mat_window(NumericMatrix mat, NumericMatrix window);
RcppExport SEXP _geocmeans_focal_euclidean_mat_window(SEXP matSEXP, SEXP windowSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type mat(matSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type window(windowSEXP);
    rcpp_result_gen = Rcpp::wrap(focal_euclidean_mat_window(mat, window));
    return rcpp_result_gen;
END_RCPP
}
// focal_euclidean_list
NumericMatrix focal_euclidean_list(List matrices, NumericMatrix window);
RcppExport SEXP _geocmeans_focal_euclidean_list(SEXP matricesSEXP, SEXP windowSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type matrices(matricesSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type window(windowSEXP);
    rcpp_result_gen = Rcpp::wrap(focal_euclidean_list(matrices, window));
    return rcpp_result_gen;
END_RCPP
}
// focal_euclidean_arr_window
NumericMatrix focal_euclidean_arr_window(arma::cube mat, arma::mat window);
RcppExport SEXP _geocmeans_focal_euclidean_arr_window(SEXP matSEXP, SEXP windowSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::cube >::type mat(matSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type window(windowSEXP);
    rcpp_result_gen = Rcpp::wrap(focal_euclidean_arr_window(mat, window));
    return rcpp_result_gen;
END_RCPP
}
// focal_adj_mean_arr_window
arma::cube focal_adj_mean_arr_window(arma::cube mat, arma::mat window);
RcppExport SEXP _geocmeans_focal_adj_mean_arr_window(SEXP matSEXP, SEXP windowSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::cube >::type mat(matSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type window(windowSEXP);
    rcpp_result_gen = Rcpp::wrap(focal_adj_mean_arr_window(mat, window));
    return rcpp_result_gen;
END_RCPP
}
// calc_jaccard_idx
double calc_jaccard_idx(arma::vec x, arma::vec y);
RcppExport SEXP _geocmeans_calc_jaccard_idx(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(calc_jaccard_idx(x, y));
    return rcpp_result_gen;
END_RCPP
}
// calc_jaccard_mat
NumericMatrix calc_jaccard_mat(NumericMatrix matX, NumericMatrix matY);
RcppExport SEXP _geocmeans_calc_jaccard_mat(SEXP matXSEXP, SEXP matYSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type matX(matXSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type matY(matYSEXP);
    rcpp_result_gen = Rcpp::wrap(calc_jaccard_mat(matX, matY));
    return rcpp_result_gen;
END_RCPP
}
// moranI_matrix_window
double moranI_matrix_window(NumericMatrix mat, NumericMatrix window);
RcppExport SEXP _geocmeans_moranI_matrix_window(SEXP matSEXP, SEXP windowSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type mat(matSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type window(windowSEXP);
    rcpp_result_gen = Rcpp::wrap(moranI_matrix_window(mat, window));
    return rcpp_result_gen;
END_RCPP
}
// local_moranI_matrix_window
NumericVector local_moranI_matrix_window(NumericMatrix mat, NumericMatrix window);
RcppExport SEXP _geocmeans_local_moranI_matrix_window(SEXP matSEXP, SEXP windowSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type mat(matSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type window(windowSEXP);
    rcpp_result_gen = Rcpp::wrap(local_moranI_matrix_window(mat, window));
    return rcpp_result_gen;
END_RCPP
}
// Elsa_categorical_matrix_window
NumericVector Elsa_categorical_matrix_window(IntegerMatrix mat, IntegerMatrix window, NumericMatrix dist);
RcppExport SEXP _geocmeans_Elsa_categorical_matrix_window(SEXP matSEXP, SEXP windowSEXP, SEXP distSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerMatrix >::type mat(matSEXP);
    Rcpp::traits::input_parameter< IntegerMatrix >::type window(windowSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type dist(distSEXP);
    rcpp_result_gen = Rcpp::wrap(Elsa_categorical_matrix_window(mat, window, dist));
    return rcpp_result_gen;
END_RCPP
}
// Elsa_fuzzy_matrix_window
NumericVector Elsa_fuzzy_matrix_window(arma::fcube mats, arma::mat window, NumericMatrix dist);
RcppExport SEXP _geocmeans_Elsa_fuzzy_matrix_window(SEXP matsSEXP, SEXP windowSEXP, SEXP distSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::fcube >::type mats(matsSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type window(windowSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type dist(distSEXP);
    rcpp_result_gen = Rcpp::wrap(Elsa_fuzzy_matrix_window(mats, window, dist));
    return rcpp_result_gen;
END_RCPP
}
// adj_spconsist_arr_window_globstd
double adj_spconsist_arr_window_globstd(arma::fcube data, arma::fcube memberships, arma::mat window, double mindist);
RcppExport SEXP _geocmeans_adj_spconsist_arr_window_globstd(SEXP dataSEXP, SEXP membershipsSEXP, SEXP windowSEXP, SEXP mindistSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::fcube >::type data(dataSEXP);
    Rcpp::traits::input_parameter< arma::fcube >::type memberships(membershipsSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type window(windowSEXP);
    Rcpp::traits::input_parameter< double >::type mindist(mindistSEXP);
    rcpp_result_gen = Rcpp::wrap(adj_spconsist_arr_window_globstd(data, memberships, window, mindist));
    return rcpp_result_gen;
END_RCPP
}
// vecmin
double vecmin(NumericVector x);
RcppExport SEXP _geocmeans_vecmin(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(vecmin(x));
    return rcpp_result_gen;
END_RCPP
}
// vecmax
double vecmax(NumericVector x);
RcppExport SEXP _geocmeans_vecmax(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(vecmax(x));
    return rcpp_result_gen;
END_RCPP
}
// power_mat
NumericMatrix power_mat(NumericMatrix x, double p);
RcppExport SEXP _geocmeans_power_mat(SEXP xSEXP, SEXP pSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type p(pSEXP);
    rcpp_result_gen = Rcpp::wrap(power_mat(x, p));
    return rcpp_result_gen;
END_RCPP
}
// calcEuclideanDistance2
NumericVector calcEuclideanDistance2(NumericMatrix y, NumericVector x);
RcppExport SEXP _geocmeans_calcEuclideanDistance2(SEXP ySEXP, SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type y(ySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(calcEuclideanDistance2(y, x));
    return rcpp_result_gen;
END_RCPP
}
// calcEuclideanDistance3
arma::mat calcEuclideanDistance3(arma::mat y, arma::mat x);
RcppExport SEXP _geocmeans_calcEuclideanDistance3(SEXP ySEXP, SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type y(ySEXP);
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(calcEuclideanDistance3(y, x));
    return rcpp_result_gen;
END_RCPP
}
// add_matrices_bycol
NumericMatrix add_matrices_bycol(NumericMatrix x, NumericMatrix y);
RcppExport SEXP _geocmeans_add_matrices_bycol(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(add_matrices_bycol(x, y));
    return rcpp_result_gen;
END_RCPP
}
// sub_matrices_bycol
NumericMatrix sub_matrices_bycol(NumericMatrix x, NumericMatrix y);
RcppExport SEXP _geocmeans_sub_matrices_bycol(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(sub_matrices_bycol(x, y));
    return rcpp_result_gen;
END_RCPP
}
// prod_matrices_bycol
NumericMatrix prod_matrices_bycol(NumericMatrix x, NumericMatrix y);
RcppExport SEXP _geocmeans_prod_matrices_bycol(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(prod_matrices_bycol(x, y));
    return rcpp_result_gen;
END_RCPP
}
// div_matrices_bycol
NumericMatrix div_matrices_bycol(NumericMatrix x, NumericMatrix y);
RcppExport SEXP _geocmeans_div_matrices_bycol(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(div_matrices_bycol(x, y));
    return rcpp_result_gen;
END_RCPP
}
// sqrt_matrix_bycol
NumericMatrix sqrt_matrix_bycol(NumericMatrix x);
RcppExport SEXP _geocmeans_sqrt_matrix_bycol(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(sqrt_matrix_bycol(x));
    return rcpp_result_gen;
END_RCPP
}
// pow_matrix_bycol
NumericMatrix pow_matrix_bycol(NumericMatrix x, float p);
RcppExport SEXP _geocmeans_pow_matrix_bycol(SEXP xSEXP, SEXP pSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< float >::type p(pSEXP);
    rcpp_result_gen = Rcpp::wrap(pow_matrix_bycol(x, p));
    return rcpp_result_gen;
END_RCPP
}
// rowmins_mat
NumericVector rowmins_mat(NumericMatrix x);
RcppExport SEXP _geocmeans_rowmins_mat(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rowmins_mat(x));
    return rcpp_result_gen;
END_RCPP
}
// max_mat
double max_mat(NumericMatrix x);
RcppExport SEXP _geocmeans_max_mat(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(max_mat(x));
    return rcpp_result_gen;
END_RCPP
}
// test_inferior_mat
LogicalMatrix test_inferior_mat(NumericMatrix mat, double t);
RcppExport SEXP _geocmeans_test_inferior_mat(SEXP matSEXP, SEXP tSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type mat(matSEXP);
    Rcpp::traits::input_parameter< double >::type t(tSEXP);
    rcpp_result_gen = Rcpp::wrap(test_inferior_mat(mat, t));
    return rcpp_result_gen;
END_RCPP
}
// vector_out_prod
NumericMatrix vector_out_prod(NumericVector x);
RcppExport SEXP _geocmeans_vector_out_prod(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(vector_out_prod(x));
    return rcpp_result_gen;
END_RCPP
}
// calcRobustSigmas
NumericVector calcRobustSigmas(NumericMatrix data, NumericMatrix belongmatrix, NumericMatrix centers, double m);
RcppExport SEXP _geocmeans_calcRobustSigmas(SEXP dataSEXP, SEXP belongmatrixSEXP, SEXP centersSEXP, SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type data(dataSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type belongmatrix(belongmatrixSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type centers(centersSEXP);
    Rcpp::traits::input_parameter< double >::type m(mSEXP);
    rcpp_result_gen = Rcpp::wrap(calcRobustSigmas(data, belongmatrix, centers, m));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_geocmeans_calcCentroids", (DL_FUNC) &_geocmeans_calcCentroids, 3},
    {"_geocmeans_calcBelongMatrix", (DL_FUNC) &_geocmeans_calcBelongMatrix, 4},
    {"_geocmeans_calcSWFCCentroids", (DL_FUNC) &_geocmeans_calcSWFCCentroids, 5},
    {"_geocmeans_calcSFCMBelongMatrix", (DL_FUNC) &_geocmeans_calcSFCMBelongMatrix, 7},
    {"_geocmeans_calcBelongMatrixNoisy", (DL_FUNC) &_geocmeans_calcBelongMatrixNoisy, 5},
    {"_geocmeans_calcSFCMBelongMatrixNoisy", (DL_FUNC) &_geocmeans_calcSFCMBelongMatrixNoisy, 8},
    {"_geocmeans_calcFGCMBelongMatrix", (DL_FUNC) &_geocmeans_calcFGCMBelongMatrix, 5},
    {"_geocmeans_calcSFGCMBelongMatrix", (DL_FUNC) &_geocmeans_calcSFGCMBelongMatrix, 8},
    {"_geocmeans_calcFGCMBelongMatrixNoisy", (DL_FUNC) &_geocmeans_calcFGCMBelongMatrixNoisy, 6},
    {"_geocmeans_calcSFGCMBelongMatrixNoisy", (DL_FUNC) &_geocmeans_calcSFGCMBelongMatrixNoisy, 9},
    {"_geocmeans_focal_euclidean_mat_window", (DL_FUNC) &_geocmeans_focal_euclidean_mat_window, 2},
    {"_geocmeans_focal_euclidean_list", (DL_FUNC) &_geocmeans_focal_euclidean_list, 2},
    {"_geocmeans_focal_euclidean_arr_window", (DL_FUNC) &_geocmeans_focal_euclidean_arr_window, 2},
    {"_geocmeans_focal_adj_mean_arr_window", (DL_FUNC) &_geocmeans_focal_adj_mean_arr_window, 2},
    {"_geocmeans_calc_jaccard_idx", (DL_FUNC) &_geocmeans_calc_jaccard_idx, 2},
    {"_geocmeans_calc_jaccard_mat", (DL_FUNC) &_geocmeans_calc_jaccard_mat, 2},
    {"_geocmeans_moranI_matrix_window", (DL_FUNC) &_geocmeans_moranI_matrix_window, 2},
    {"_geocmeans_local_moranI_matrix_window", (DL_FUNC) &_geocmeans_local_moranI_matrix_window, 2},
    {"_geocmeans_Elsa_categorical_matrix_window", (DL_FUNC) &_geocmeans_Elsa_categorical_matrix_window, 3},
    {"_geocmeans_Elsa_fuzzy_matrix_window", (DL_FUNC) &_geocmeans_Elsa_fuzzy_matrix_window, 3},
    {"_geocmeans_adj_spconsist_arr_window_globstd", (DL_FUNC) &_geocmeans_adj_spconsist_arr_window_globstd, 4},
    {"_geocmeans_vecmin", (DL_FUNC) &_geocmeans_vecmin, 1},
    {"_geocmeans_vecmax", (DL_FUNC) &_geocmeans_vecmax, 1},
    {"_geocmeans_power_mat", (DL_FUNC) &_geocmeans_power_mat, 2},
    {"_geocmeans_calcEuclideanDistance2", (DL_FUNC) &_geocmeans_calcEuclideanDistance2, 2},
    {"_geocmeans_calcEuclideanDistance3", (DL_FUNC) &_geocmeans_calcEuclideanDistance3, 2},
    {"_geocmeans_add_matrices_bycol", (DL_FUNC) &_geocmeans_add_matrices_bycol, 2},
    {"_geocmeans_sub_matrices_bycol", (DL_FUNC) &_geocmeans_sub_matrices_bycol, 2},
    {"_geocmeans_prod_matrices_bycol", (DL_FUNC) &_geocmeans_prod_matrices_bycol, 2},
    {"_geocmeans_div_matrices_bycol", (DL_FUNC) &_geocmeans_div_matrices_bycol, 2},
    {"_geocmeans_sqrt_matrix_bycol", (DL_FUNC) &_geocmeans_sqrt_matrix_bycol, 1},
    {"_geocmeans_pow_matrix_bycol", (DL_FUNC) &_geocmeans_pow_matrix_bycol, 2},
    {"_geocmeans_rowmins_mat", (DL_FUNC) &_geocmeans_rowmins_mat, 1},
    {"_geocmeans_max_mat", (DL_FUNC) &_geocmeans_max_mat, 1},
    {"_geocmeans_test_inferior_mat", (DL_FUNC) &_geocmeans_test_inferior_mat, 2},
    {"_geocmeans_vector_out_prod", (DL_FUNC) &_geocmeans_vector_out_prod, 1},
    {"_geocmeans_calcRobustSigmas", (DL_FUNC) &_geocmeans_calcRobustSigmas, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_geocmeans(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
