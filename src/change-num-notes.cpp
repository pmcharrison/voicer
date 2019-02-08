#include <Rcpp.h>
#include <math.h>
#include <iostream>
using namespace Rcpp;

// [[Rcpp::export]]
int change_num_notes__(NumericVector x, NumericVector y) {
  int s1 = x.size();
  int s2 = y.size();
  return(abs(s1 - s2));
}

// [[Rcpp::export]]
IntegerVector change_num_notes_(List contexts, NumericVector continuation) {
  int n = contexts.size();
  IntegerVector res = IntegerVector(n);
  for (int i = 0; i < n; i ++) {
    NumericVector context = as<NumericVector>(contexts[i]);
    res[i] = change_num_notes__(context, continuation);
  }
  return(res);
}
