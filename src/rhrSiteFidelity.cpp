#include <Rcpp.h>
#include <cmath>

using namespace Rcpp;


// Random walk 
// [[Rcpp::export]]
List simpleRandomWalk(NumericVector sx, NumericVector sy, NumericVector sinA, NumericVector cosA, NumericVector d) {

  int n = d.size();

  NumericVector rx(n);
  NumericVector ry(n);

  rx[0] = sx[0];
  ry[0] = sy[0];

  for (int i = 1; i < n; i++) {

    rx[i] = rx[i-1] + cosA[i-1] * d[i];
    ry[i] = ry[i-1] + sinA[i-1] * d[i];
   
  }
  
  return List::create(Named("rx") = rx,
		     Named("ry") = ry);
}

// Mean Squared Distancean
// [[Rcpp::export]]
NumericVector meanSquaredDistance(NumericVector x, NumericVector y, double mx, double my) {

  int n = x.size();
  double tmp_ri = 0;  
  int i;  // counting variable used within the loop

  for (int i = 0; i < n; i++) {
    double tx = x[i] - mx;
    double ty = y[i] - my;
    tmp_ri += pow(tx, 2.0) + pow(ty, 2.0);
  }
  
  return NumericVector::create(1/(double)(n-1) * tmp_ri);
}
