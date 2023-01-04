#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
double dist(double x1, double y1, double x2, double y2) {
  return pow(pow((x2 - x1), 2) + pow((y2 - y1), 2), 0.5);
}

// [[Rcpp::export]]
NumericVector min_dists(NumericVector x1, NumericVector y1, NumericVector x2, NumericVector y2) {
 NumericVector v (x1.length());
  for(int i=0; i<x1.length(); ++i){
    double m = dist(x1[i], y1[i], x2[0], y2[0]);
    for(int j=1; j<x2.length(); ++j){
      double d = dist(x1[i], y1[i], x2[j], y2[j]);
      if (d < m){
        m = d;
      }
    }
    v[i] = m;
  }
  return v;
}
