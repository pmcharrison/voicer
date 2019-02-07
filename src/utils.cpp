#include <Rcpp.h>
#include <math.h>

double mod(double x, int base) {
  return x - floor(x / base) * base;
}
