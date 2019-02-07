#include <Rcpp.h>
#include <math.h>
#include <iostream>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector vl_dist_(List context, NumericVector continuation) {
  Environment pkg = Environment::namespace_env("voicer");
  Function min_vls = pkg[".min_vls"];
  List vls = min_vls(context, continuation, "pitch");
  int n = vls.size();
  NumericVector res = NumericVector(n);
  for (int i = 0; i < n; i ++) {
    List vl = vls[i];
    double dist = vl["dist"];
    res[i] = dist;
  }
  return(res);
}
