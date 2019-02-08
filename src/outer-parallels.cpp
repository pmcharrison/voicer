#include <Rcpp.h>
#include <math.h>
#include <iostream>
using namespace Rcpp;

double mod(double x, int base);

// [[Rcpp::export]]
bool outer_parallels__(NumericVector x, NumericVector y) {
  if (x.size() < 2 || y.size() < 2) return(false);
  double x_bass = x[0];
  double x_treble = x[x.size() - 1];
  double x_int = x_treble - x_bass;
  double x_pc_int = mod(x_int, 12);
  if (x_pc_int == 0 || x_pc_int == 7) {
    double y_bass = y[0];
    double y_treble = y[y.size() - 1];
    double y_int = y_treble - y_bass;
    if (y_int == x_int && x_bass != y_bass) return(true);
  }
  return(false);
}

// [[Rcpp::export]]
LogicalVector outer_parallels_(List contexts, NumericVector continuation) {
  int n = contexts.size();
  LogicalVector res = LogicalVector(n);
  for (int i = 0; i < n; i ++) {
    NumericVector context = as<NumericVector>(contexts[i]);
    res[i] = outer_parallels__(context, continuation);
  }
  return(res);
}
