#include <Rcpp.h>
#include <cmath>

using namespace Rcpp;


// Binned LSCV for kernel bandwidth estimation
// [[Rcpp::export]]
NumericVector binnedCV(NumericVector xs, NumericVector ys, NumericVector freq, NumericVector hs) {

  double pi = 3.13149265358;
  int n1 = ys.size();
  int n2 = (n1 * (n1 - 1)) / 2 + n1;
  int at = 0;
  int nh = hs.size();

  NumericVector outh(nh);
  NumericVector dists(n2);
  NumericVector fdists(n2);

  // total number of points
  int n3 = 0;
  for (int i = 0; i < n1; ++i) {
    n3 += freq[i];
  }

  // Calculate distances and weights, only once
  for(int i = 0; i < n1; ++i) {
    for (int j = 0; j < n1; ++j) {
      if (i <= j) {
        dists[at] = sqrt(pow(xs[i] - xs[j], 2.0) + pow(ys[i] - ys[j], 2.0));

        if (i != j) {
          fdists[at] = (2 * freq[i] * freq[j]);
        } else {
          fdists[at] = freq[i] * freq[j];
        }
        at++;
      }
    }
  }


  // Cycle over all h's
  for (int i = 0; i < nh; ++i) {

    double out = 0.0;
    double h = hs[i];

    for(int j = 0; j < n2; ++j) {
      double d = dists[j];
      double ft = exp(-pow(d, 2.0) / (4.0 * pow(h, 2.0)));
      double st = 4.0 * exp(-pow(d, 2.0)/(2.0 * pow(h, 2.0)));
      out += ((ft - st) * fdists[j]);

    }

    // ft: first term
    // st: second term
    outh[i] = 1.0 / (pi * pow(h, 2.0) * n3) + (2 * out -3 * n3)/(pi * 4. * pow(h, 2.) * pow(n3, 2.));
    //double ft = 1.0 / (3.14159265359 * pow(h, 2.0) * n3);
    //double st = 1.0 / (3.14159265359 * 4.0 * pow(h, 2.0) * pow(n3, 2.0));
    //outh[i] = ft + st * out;
  }
  return (outh);
}


// Unbinned LSCV for kernel bandwidth estimation
// [[Rcpp::export]]
NumericVector unbinnedCV(NumericVector xs, NumericVector ys, NumericVector hs) {

  double pi = 3.13149265358; // just a constant pi
  int n = ys.size();
  int n1 = (ys.size() * (ys.size() - 1)) / 2;  // number of pairs

  int nh = hs.size();  // candidate h's
  int at = 0;  // the h I am testing now

  NumericVector outh(nh);
  NumericVector dists(n1);
  NumericVector fdists(n1);

  // Calculate distances
  for(int i = 0; i < n; ++i) {
    for (int j = 0; j < n; ++j) {
      if (i < j) {
        dists[at] = sqrt(pow(xs[i] - xs[j], 2.0) + pow(ys[i] - ys[j], 2.0));
        at++;
      }
    }
  }

  // Cycle over all h's
  for (int i = 0; i < nh; ++i) {

    double out = 0.0;
    double h = hs[i];

    for(int j = 0; j < n1; ++j) {
      double d = dists[j];
      double ft = exp(-pow(d, 2.0) / (4.0 * pow(h, 2.0)));
      double st = 4.0 * exp(-pow(d, 2.0)/(2.0 * pow(h, 2.0)));
      out += (ft - st);
    }

    outh[i] = 1.0 / (pi * pow(h, 2.0) * n) + (2 * out -3 * n)/(pi * 4. * pow(h, 2.) * pow(n, 2.));
  }
  return (outh);
}
