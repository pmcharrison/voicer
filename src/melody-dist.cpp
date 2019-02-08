#include <Rcpp.h>
#include <math.h>
#include <iostream>
using namespace Rcpp;

// [[Rcpp::export]]
double melody_dist__(NumericVector x, NumericVector y) {
  if (x.size() == 0 || y.size() == 0) return(0);
  double x_top = x[x.size() - 1];
  double y_top = y[y.size() - 1];
  double res = abs(y_top - x_top);
  return(res);
}

// [[Rcpp::export]]
NumericVector melody_dist_(List contexts, NumericVector continuation) {
  int n = contexts.size();
  NumericVector res = NumericVector(n);
  for (int i = 0; i < n; i ++) {
    NumericVector context = as<NumericVector>(contexts[i]);
    res[i] = melody_dist__(context, continuation);
  }
  return(res);
}
