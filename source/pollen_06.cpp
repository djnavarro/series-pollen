#include <Rcpp.h>
using namespace Rcpp;

// function to be called from R
// [[Rcpp::export]]
NumericMatrix raster_data(int iter, int layers, int pixels, double zoom, double alpha) {
  
  NumericMatrix image(pixels, pixels); // initially zero
  NumericMatrix coeffs(9, layers);
  
  // set coefficients
  for(int i = 0; i < 9; i++) {
    for(int j = 0; j < layers; j++) {
      coeffs(i, j) = R::runif(-.3, .3);
    }
  }
  
  // set image matrix to zeros
  for(int r = 0; r < pixels; r++) {
    for(int c = 0; c < pixels; c++) {
      image(c, r) = 0;
    }
  }
  
  // iterate
  int layer;
  int variant;

  // convenience variables
  double s = 0;
  double u = .3;
  
  // offsets
  double x_sh = 0;
  double y_sh = 0;

  // indices for storing coordinates
  int x_ind;
  int y_ind;

  // values for current state
  double x = 0;
  double y = 0;
  double z = 0;
  
  // values for previous state
  double x_old = R::runif(-1, 1);
  double y_old = R::runif(-1, 1);
  double z_old = R::runif(-1, 1);
  
  // iterate...
  for(int it = 1; it < iter; it++) {
    
    layer = rand() % layers;   // which affine transform to use?
    variant = rand() % 2;      // which variant function to use?
    
    // coordinates after random transform
    x = x_old + coeffs(0, layer) * x_old + coeffs(1, layer) * y_old + coeffs(2, layer);
    y = y_old + coeffs(3, layer) * x_old + coeffs(4, layer) * y_old + coeffs(5, layer);
    z = z_old + coeffs(6, layer) * x_old + coeffs(7, layer) * y_old + coeffs(8, layer);

    // apply function to the transformed coordinates
    if(variant != 0) {
      s = x*x + y*y + z*z;
      x = x/s;
      y = y/s;
      z = z/s;
    } else {
      x = sin(x);
      y = sin(y);
      z = sin(z);
    } 
    
    // compute indices to be updated
    x_ind = int (x * pixels * zoom) + pixels/2;
    y_ind = int (y * pixels * zoom) + pixels/2;

    // store results if they fall within the range
    if(x_ind >= 0 & x_ind < pixels) {
      if(y_ind >= 0 & y_ind < pixels) {
        image(x_ind, y_ind) = alpha * z + (1- alpha) * image(x_ind, y_ind);
      }
    }
    
    // move new to old
    x_old = x;
    y_old = y;
    z_old = (z + z_old)/2; 
  }
  
  return image;
}


