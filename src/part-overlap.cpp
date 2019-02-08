#include <Rcpp.h>
#include <math.h>
#include <iostream>
using namespace Rcpp;

void print_numeric(NumericVector v){
  int n = v.length();
  for (int i = 0; i < n; i ++) {
    Rprintf("%f", v[i]);
    if (i < n - 1) Rprintf(", ");
  }
  Rprintf("\n");
}

// takes minimal voice leadings as input
// [[Rcpp::export]]
bool part_overlap__(NumericVector start, NumericVector end) {
  int n = start.size();
  for (int i = 0; i < n - 1; i ++) {
    if (end[i] > start[i + 1]) {
      return(true); // cross above
    }
    if (end[i + 1] < start[i]) {
      return(true); // cross below
    }
  }
  return(false);
}

// takes pitch sets as input
// [[Rcpp::export]]
LogicalVector part_overlap_(List context, NumericVector continuation) {
  Environment pkg = Environment::namespace_env("voicer");
  Function min_vls = pkg[".min_vls"];
  List vls = min_vls(context, continuation, "pitch");
  int n = vls.size();
  LogicalVector res = LogicalVector(n);
  for (int i = 0; i < n; i ++) {
    List vl = vls[i];
    NumericVector start = as<NumericVector>(vl["start"]);
    NumericVector end = as<NumericVector>(vl["end"]);
    res[i] = part_overlap__(start, end);
  }
  return(res);
}
