#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector t2cpp3(NumericVector t, int interval) {

  int n = t.size();
  NumericVector included(n);
  included[0] = 1;

  int tdiff = 0;
  for (int i = 1; i < n; i++) {
    tdiff += t[i] - t[i-1];
    if (tdiff >= interval) {
      included[i] = 1;
      tdiff = 0;
    } else  {
      included[i] = 0;
    }
  }
  return included;
}
