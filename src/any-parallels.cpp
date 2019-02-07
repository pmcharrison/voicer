#include <Rcpp.h>
#include <math.h>
#include <iostream>
using namespace Rcpp;

double mod(double x, int base);

// takes minimal voice leadings as input
// [[Rcpp::export]]
bool any_parallels__(NumericVector start, NumericVector end) {
  int n = start.size();
  for (int i = 0; i < n - 1; i++) {
    for (int j = i + 1; j < n; j ++) {
      bool moved = end[i] - start[i] != 0;
      if (moved) { // if the lower note has moved
        int original_pc_int = mod(start[j] - start[i], 12);
        bool originally_fifth_or_octave =
          original_pc_int == 7 || original_pc_int == 0;
        if (originally_fifth_or_octave) { // if the two notes originally formed a (compound) fifth or octave
          bool parallel = (end[i] - start[i]) == (end[j] - start[j]);
          if (parallel) { // if the two notes moved by the same interval
            return(true);
          }
        }
      }
    }
  }
  return(false);
}

// takes pitch sets as input
// [[Rcpp::export]]
LogicalVector any_parallels_(List context, NumericVector continuation) {
  Environment pkg = Environment::namespace_env("voicer");
  Function min_vls = pkg[".min_vls"];
  List vls = min_vls(context, continuation, "pitch");
  int n = vls.size();
  LogicalVector res = LogicalVector(n);
  for (int i = 0; i < n; i ++) {
    List vl = vls[i];
    NumericVector start = as<NumericVector>(vl["start"]);
    NumericVector end = as<NumericVector>(vl["end"]);
    res[i] = any_parallels__(start, end);
  }
  return(res);
}
