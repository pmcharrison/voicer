#include <Rcpp.h>
#include <math.h>
#include <iostream>
using namespace Rcpp;

double mod(double x, int base);

bool exposed_outer_octaves__(NumericVector x, NumericVector y) {
  double y_bass = y[0];
  double y_treb = y[y.size() - 1];
  double y_int = y_treb - y_bass;
  bool y_outer_octave = (y_int > 0) && (mod(y_int, 12) == 0);
  if (y_outer_octave) {
    double x_bass = x[0];
    double x_treb = x[x.size() - 1];
    double treb_motion = y_treb - x_treb;
    double bass_motion = y_bass - x_bass;
    bool similar_motion = (treb_motion * bass_motion) > 0;
    if (similar_motion &&
        abs(treb_motion) > 2L &&
        abs(bass_motion) > 2L) {
      return(true);
    }
  }
  return(false);
}

// [[Rcpp::export]]
LogicalVector exposed_outer_octaves_(List elts,
                                     NumericVector elt,
                                     bool reverse = false) {
  int n = elts.size();
  LogicalVector res = LogicalVector(n);
  for (int i = 0; i < n; i ++) {
    NumericVector context;
    NumericVector continuation;
    if (!reverse) {
      context = as<NumericVector>(elts[i]);
      continuation = elt;
    } else {
      context = elt;
      continuation = as<NumericVector>(elts[i]);
    }
    res[i] = exposed_outer_octaves__(context, continuation);
  }
  return(res);
}
