#include <Rcpp.h>
#include <cmath>

using namespace Rcpp;

// Mean Squared Distance
// [[Rcpp::export]]
NumericVector meanSquaredDistance(NumericVector x, NumericVector y, double mx, double my) {
  int n = x.size();
  double tmp_ri = 0;
  int i;

  for (int i = 0; i < n; i++) {
    double tx = x[i] - mx;
    double ty = y[i] - my;
    tmp_ri += pow(tx, 2.0) + pow(ty, 2.0);
  }
  return NumericVector::create(1 / (double)(n-1) * tmp_ri);
}
