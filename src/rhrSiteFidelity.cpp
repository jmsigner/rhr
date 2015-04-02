#include <Rcpp.h>
#include <cmath>

using namespace Rcpp;


// Random walk
// [[Rcpp::export]]
List simpleRandomWalk(float sx, float sy, NumericVector sinA, NumericVector cosA, NumericVector d) {

  int n = d.size();

  NumericVector rx(n + 1);
  NumericVector ry(n + 1);

  rx[0] = sx;
  ry[0] = sy;

  for (int i = 0; i < n; i++) {

    rx[i+1] = rx[i] + cosA[i] * d[i];
    ry[i+1] = ry[i] + sinA[i] * d[i];

  }

  return List::create(Named("rx") = rx,
                      Named("ry") = ry);
}

