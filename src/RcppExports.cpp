// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// any_parallels__
bool any_parallels__(NumericVector start, NumericVector end);
RcppExport SEXP _voicer_any_parallels__(SEXP startSEXP, SEXP endSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type start(startSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type end(endSEXP);
    rcpp_result_gen = Rcpp::wrap(any_parallels__(start, end));
    return rcpp_result_gen;
END_RCPP
}
// any_parallels_
LogicalVector any_parallels_(List context, NumericVector continuation);
RcppExport SEXP _voicer_any_parallels_(SEXP contextSEXP, SEXP continuationSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type context(contextSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type continuation(continuationSEXP);
    rcpp_result_gen = Rcpp::wrap(any_parallels_(context, continuation));
    return rcpp_result_gen;
END_RCPP
}
// part_overlap__
bool part_overlap__(NumericVector start, NumericVector end);
RcppExport SEXP _voicer_part_overlap__(SEXP startSEXP, SEXP endSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type start(startSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type end(endSEXP);
    rcpp_result_gen = Rcpp::wrap(part_overlap__(start, end));
    return rcpp_result_gen;
END_RCPP
}
// part_overlap_
LogicalVector part_overlap_(List context, NumericVector continuation);
RcppExport SEXP _voicer_part_overlap_(SEXP contextSEXP, SEXP continuationSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type context(contextSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type continuation(continuationSEXP);
    rcpp_result_gen = Rcpp::wrap(part_overlap_(context, continuation));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_voicer_any_parallels__", (DL_FUNC) &_voicer_any_parallels__, 2},
    {"_voicer_any_parallels_", (DL_FUNC) &_voicer_any_parallels_, 2},
    {"_voicer_part_overlap__", (DL_FUNC) &_voicer_part_overlap__, 2},
    {"_voicer_part_overlap_", (DL_FUNC) &_voicer_part_overlap_, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_voicer(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
