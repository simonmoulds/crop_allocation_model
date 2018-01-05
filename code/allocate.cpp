// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <numeric>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector sum_interval(NumericVector x, int interval) {
  int n = x.size();
  if (n % interval != 0) {
    stop("The number of elements in x must be a multiple of the supplied interval");
  } 
  int nn = n / interval;
  NumericVector res(nn);
  for (int i = 0; i < nn; i++) {
    int start = i * interval;
    int end = start + (interval - 1);
    IntegerVector index = seq(start, end);
    NumericVector v = x[index];
    res[i] = sum(v);
  }
  return res;
}

// [[Rcpp::export]]
NumericVector calculate_changeC(NumericMatrix area, NumericMatrix area1, NumericMatrix yield, NumericVector demand) {

  int nrow = area.nrow(), ncol = area.ncol();
  NumericMatrix prod_change(nrow,ncol);
  NumericVector demand1 = clone(demand);
  
  for (int i = 0; i < nrow; i++) {
    for (int j = 0; j < ncol; j++) {
      prod_change(i,j) = (area1(i,j) - area(i,j)) * yield(i,j);
    }
  }

  NumericVector total_prod_change = colSums(prod_change);
  for (int i = 0; i < ncol; i++) {
    demand1[i] = demand[i] - total_prod_change[i];
  }

  return demand1;
}

// [[Rcpp::export]]
NumericMatrix get_total_areaC(NumericMatrix x, int n_season, int n_input) {

  int nrow = x.nrow(), ncol = x.ncol();
  int n_cell = nrow / (n_season * n_input);
  int n_cell_input = n_cell * n_input;
  
  NumericMatrix out(n_season,ncol);
  for (int i = 0; i < n_season; i++) {
    NumericMatrix xx(n_cell_input,ncol);
    for (int j = 0; j < n_cell; j++) {
      for (int k = 0; k < n_input; k++) {
	int ix1 = j * n_input + k;
	int ix2 = j * n_season * n_input + (i * n_input) + k;
	xx(ix1,_) = x(ix2,_);
      }
    }
    out(i,_) = colSums(xx);
  }
  return out;
}

// [[Rcpp::export]]
NumericVector subcolumn(NumericMatrix x, int row1, int row2, int col) {
  if (col >= x.ncol()) {
    stop("stop");
  }

  if (row2 >= x.nrow()) {
    stop("stop");
  }

  if (row1 > row2) {
    stop("stop");
  }
  
  NumericVector xx = x(_,col);
  return xx[Range(row1, row2)];
}

// [[Rcpp::export]]
IntegerVector order_(NumericVector x, bool descending) {
  if (is_true(any(duplicated(x)))) {
    Rf_warning("There are duplicates in 'x'; order not guaranteed to match that of R's base::order");
  }
  NumericVector sorted = clone(x).sort(descending);
  return match(sorted, x);
}

// [[Rcpp::export]]
NumericMatrix intensify(NumericMatrix area, NumericMatrix suit, NumericMatrix yield, NumericVector demand, double target_intensification, LogicalVector crop_ix_logical, int n_cell, int n_season, int n_input, NumericVector cell_area, double fact, int no_chng_count_max) {

  // function to intensify production
  IntegerVector a = seq(0, demand.size()-1);
  IntegerVector crop_ix_tmp = a[crop_ix_logical];
  int crop_ix = crop_ix_tmp[0];
  double target_demand = demand[crop_ix] * target_intensification;
  NumericMatrix area1 = clone(area);

  // do we need to put this in the do { ... } loop?
  // what is a better way of doing this?
  NumericVector max_suit(n_cell);
  for (int i = 0; i < n_cell; i++) {
    int start_row_ix = i * n_season * n_input;
    int end_row_ix = start_row_ix + (n_season * n_input) - 1;
    NumericVector suit_v = subcolumn(suit, start_row_ix, end_row_ix, crop_ix);
    NumericVector area_v = subcolumn(area, start_row_ix, end_row_ix, crop_ix);
    double max_suit_v = Rcpp::max(suit_v);
    double max_area_v = Rcpp::max(area_v);
    if ((max_suit_v > 0) && (max_area_v > 0)) {
      NumericVector a = suit_v[suit_v > 0];
      max_suit[i] = a[0];
    } else {
      max_suit[i] = 0;
    }
  }

  // https://stackoverflow.com/questions/21609934/ordering-permutation-in-rcpp-i-e-baseorder
  IntegerVector ordr = order_(max_suit, true);
  int index = 0;
  double total_prod_chng = 0;
  int no_chng_count = 0;

  do {

    int ix = ordr[index];
    
    for (int i = 0; i < n_season; i++) {

      int start_row_ix = ix * n_season * n_input + i * n_input;
      int end_row_ix = start_row_ix + (n_input - 1);

      for (int j = 0; j < (n_input - 1); j++) {

	int hi_ix = end_row_ix - (j+1);
	int lo_ix = hi_ix + 1;

	double higher_intensity_area = area1(hi_ix,crop_ix);
	double lower_intensity_area = area1(lo_ix,crop_ix);

	double prod_chng, area_chng;
	
	if (lower_intensity_area > 0) {
	  
	  double area_chng0 = Rcpp::min(NumericVector::create(lower_intensity_area, (cell_area[ix] * fact)));
	  double prod_chng0 = area_chng0 * (yield(hi_ix,crop_ix) - yield(lo_ix,crop_ix));

	  if (prod_chng0 > 0) {

	    if (prod_chng0 < target_demand) {
	      prod_chng = prod_chng0;
	      area_chng = area_chng0;
	    } else {
	      prod_chng = target_demand;
	      area_chng = prod_chng / (yield(hi_ix,crop_ix) - yield(lo_ix,crop_ix));
	    }

	  } else {
	    prod_chng = 0;
	    area_chng = 0;
	  }
	  
	} else {
	  prod_chng = 0;
	  area_chng = 0;
	}

	// Rprintf("%f\n", prod_chng);
	
	area1(hi_ix,crop_ix) += area_chng;
	area1(lo_ix,crop_ix) -= area_chng;

	target_demand -= prod_chng;
	// Rprintf("%f\n", target_demand);
	
	total_prod_chng += prod_chng;
	
      }
    }

    if (index == (n_cell - 1)) {
      index = 0;
      if (total_prod_chng == 0) {
	no_chng_count++;
      }
      total_prod_chng = 0;
    } else {
      index++;
    }

  } while ((target_demand > 0) && (no_chng_count <= no_chng_count_max));
      
  return area1;

}

//                 area_mat[hi_ix,crop_ix] = area_mat[hi_ix,crop_ix] + area_chng
//                 area_mat[lo_ix,crop_ix] = area_mat[lo_ix,crop_ix] - area_chng

//                 dmd[crop_ix] = dmd[crop_ix] - prod_chng
//                 target_dmd = target_dmd - prod_chng
//                 total_prod_chng = total_prod_chng + prod_chng
                
//             }
//         }

//         if (index == n_cell) {
//             index = 0
//             if (total_prod_chng == 0) {
//                 no_chng_count = no_chng_count + 1
//             }
//             total_prod_chng = 0
//         }
        
//         ## continue until target is met or we know that
//         ## the target cannot be met
//         if ((target_dmd == 0) || no_chng_count > no_chng_count_max) {
//             if (target_dmd == 0) print("Target demand met")
//             if (no_chng_count > no_chng_count_max) print("Cannot meet target demand: returning early")
//             break()
//         }
//     }
//     area_mat
// }
			
// [[Rcpp::export]]
NumericMatrix allocate_expansionC(NumericMatrix area, NumericMatrix suit, NumericMatrix nb, int crop_ix, double cropland_area, double cell_area, double fact, double rand_min, double rand_max) {

  // Function to allocate cropland expansion for the case where a cell
  // contains unallocated cropland. The function is stochastic, and only
  // allocates change if the suitability of the cell to a particular crop
  // input level exceeds a random number drawn from a uniform distribution
  // with limits specified by arguments 'rand_min' and 'rand_max'.
  
  int nrow = area.nrow(), ncol=area.ncol();
  double alloc_area = Rcpp::sum(area);		       // total area of all crops
  double total_crop_area = Rcpp::sum(area(_,crop_ix)); // total area of current crop
  NumericMatrix area1 = clone(area);
    
  if ((alloc_area >= 0) && (alloc_area < cropland_area)) {

    double ar = Rcpp::min(NumericVector::create((cell_area * fact), (cropland_area - alloc_area)));
    double rnd = Rcpp::runif(1, rand_min, rand_max)[0];

    for (int i = 0; i < nrow; i++) {

      double chng;
      if (total_crop_area > 0.0) { // proportional if the crop is already grown in the cell
	chng = ar * (area1(i,crop_ix) / total_crop_area);
      } else {			 
	chng = ar / nrow;	 // TODO: make proportional to intensity of other crops in the cell
      }

      if (chng >= 0) {

	// only make change if suitability exceeds random number
	if ((suit(i,crop_ix) > rnd) && (nb(i,crop_ix) > 0)) { 
	  area1(i,crop_ix) += chng;
	}
      }
    }
  }
  return area1;
}

// [[Rcpp::export]]
NumericMatrix allocate_contractionC(NumericMatrix area, NumericMatrix suit, NumericMatrix nb, int crop_ix, double cell_area, double fact, double rand_min, double rand_max) {

  // Function to deallocate cropland that is currently allocated to a crop
  // type with falling demand.

  int nrow = area.nrow(), ncol=area.ncol();
  double alloc_area = sum(area(_,crop_ix));
  NumericMatrix area1 = clone(area);	    

  if (alloc_area > 0) {

    double ar = cell_area * fact;
    double totchng = Rcpp::min(NumericVector::create(ar, alloc_area));

    double rnd = runif(1, rand_min, rand_max)[0];
    
    for (int i = 0; i < nrow; i++) {

      double newarea = area1(i,crop_ix);
      if (newarea > 0) {
	double potchng = totchng * (newarea / alloc_area);
	double chng = Rcpp::max(NumericVector::create(0.0, potchng));
	newarea = Rcpp::max(NumericVector::create(0.0, (newarea - chng)));
	
        // only make change if suit is less than random number and nb greater than zero
	if ((suit(i,crop_ix) < rnd)) { // && (nb(i,crop_ix) > 0)) {
          area1(i,crop_ix) = newarea;
	}
      }
    }
  }
  return area1;
}

// [[Rcpp::export]]
NumericMatrix allocate_crop_conversionC(NumericMatrix area, NumericMatrix suit, NumericMatrix nb, IntegerVector decr_ix, int crop_ix, double cell_area, double fact, double rand_min, double rand_max) {
  
  int nrow = area.nrow(), ncol = area.ncol();
  int n_decr = decr_ix.size();
  NumericMatrix area1 = clone(area);
  NumericVector total_area = colSums(area);
  NumericVector decr_area = total_area[decr_ix];
  double total_decr_area = sum(decr_area);
  
  if (total_decr_area > 0) { 

    double ar = min(NumericVector::create(cell_area * fact, total_decr_area));  
    double rnd = runif(1, rand_min, rand_max)[0];

    // make changes at aggregate level to begin with
    NumericVector total_area_chng(ncol);
    for (int i = 0; i < n_decr; i++) {
      int ii = decr_ix[i];
      double a = (ar * total_area[ii] / total_decr_area);
      if (a > 1.0) {
        total_area_chng[ii] = a; // change is proportional
      }
    }
      
    for (int i = 0; i < nrow; i++) {
      double diff = 0;

      // only make change if suitability exceeds random number
      if ((suit(i,crop_ix) > rnd) && (nb(i,crop_ix) > 0)) { 
	
	for (int j = 0; j < n_decr; j++) {
	  
	  int jj = decr_ix[j];
	  
	  if ((total_area[jj] > 0) && (area1(i,jj) > 0) && (total_area_chng[jj] > 0)) {
	    double oldarea = area1(i,jj);
	    double potchng = total_area_chng[jj] * oldarea / total_area[jj];	    
	    double newarea = Rcpp::max(NumericVector::create(0.0, (oldarea - potchng)));
	    area1(i,jj) = newarea;
	    double chng = Rcpp::max(NumericVector::create(0.0, (oldarea - newarea)));
	    diff += chng;
	  }
	}
      }
      area1(i,crop_ix) += abs(diff);
    }
  }
  return area1;
}

// [[Rcpp::export]]
NumericMatrix allocate_intensificationC(NumericMatrix area, NumericMatrix suit, NumericMatrix nb, int crop_ix, double cell_area, double fact, double rand_min, double rand_max) {

  // Function to model agricultural intensification - i.e. transition from
  // a lower intensity to a higher intensity

  int nrow = area.nrow(), ncol=area.ncol();
  double total_area = sum(area(_,crop_ix));
  NumericMatrix area1 = clone(area);
  NumericVector crop_area = area1(_,crop_ix);

  if ((total_area > 0) && (nrow > 1)) {

    double rnd = runif(1, rand_min, rand_max)[0];

    for (int i = 0; i < (nrow-1); i++) {

      int hi_ix = nrow - (i + 2);
      int lo_ix = hi_ix + 1;

      double higher_intensity_area = crop_area[hi_ix];
      double lower_intensity_area = crop_area[lo_ix];

      double ar = min(NumericVector::create(cell_area * fact, lower_intensity_area)); 	
      if (lower_intensity_area > 0) {
	double target_suit = suit(hi_ix,crop_ix); // transition from low intensity to high intensity
	double target_nb = nb(hi_ix,crop_ix);
	if ((target_suit > rnd) && (target_nb > 0)) {
 	  higher_intensity_area += ar;
	  lower_intensity_area -= ar;
	}
	area1(hi_ix,crop_ix) = higher_intensity_area; 
	area1(lo_ix,crop_ix) = lower_intensity_area;
      }
    }
  }
  return area1;
}


// [[Rcpp::export]]
List allocate_incr(NumericMatrix crop_area0, NumericMatrix crop_yield, NumericMatrix crop_suit, NumericMatrix crop_nb, NumericMatrix total_crop_area, NumericVector cropland_area, NumericVector cell_area, NumericVector demand0, int crop_ix, IntegerVector decr_ix, int n_season, int n_input, double fact, double rand_min, double rand_max, double tol, int maxiter) {

  // make deep copies of variables that will be changed
  NumericMatrix crop_area = clone(crop_area0);
  NumericVector demand = clone(demand0);

  int n_cell = cell_area.size(), n_decr = decr_ix.size(), n_season_input = n_season * n_input;  
  int counter = 0;
  
  do {

    counter++; // advance counter

    // randomly select cell and get the index of the first row of data in crop_area matrix
    int ix = (sample(n_cell, 1) - 1)[0];
    int start_row_ix = ix * n_season_input;
    int end_row_ix = start_row_ix + n_season_input - 1;

    NumericMatrix tmp_crop_area = crop_area(Range(start_row_ix, end_row_ix),_);
    NumericMatrix tmp_crop_suit = crop_suit(Range(start_row_ix, end_row_ix),_);
    NumericMatrix tmp_crop_nb = crop_nb(Range(start_row_ix, end_row_ix),_);
    NumericMatrix tmp_crop_yield = crop_yield(Range(start_row_ix, end_row_ix),_);
   
    // create a copy of tmp_crop_area so we can calculate the change
    // in crop area once allocation has taken place
    NumericMatrix tmp_crop_area0 = clone(tmp_crop_area);

    double tmp_cropland_area = cropland_area[ix];
    double tmp_cell_area = cell_area[ix];

    // calculate total allocated area in each season and, seperately,
    // the total area of crops with falling demand
    NumericVector a = rowSums(tmp_crop_area);
    NumericVector total_season_area = sum_interval(a, n_input);

    NumericMatrix tmp_decr_crop_area(n_season_input,n_decr);
    for (int i = 0; i < n_decr; i++) {
      int ii = decr_ix[i];
      tmp_decr_crop_area(_,i) = tmp_crop_area(_,ii);
    }

    NumericVector b = rowSums(tmp_decr_crop_area);
    NumericVector total_decr_season_area = sum_interval(b, n_input);

    // total decreasing area across all seasons
    double total_decr_area = sum(total_decr_season_area);

    // loop through seasons to perform allocation
    for (int k = 0; k < n_season; k++) {

      // find out whether the crop is grown in the current growing
      // season - only continue if this is the case
      IntegerVector season_ix = seq(k * n_input, k * n_input + (n_input - 1));
      bool flag = total_crop_area(k,crop_ix) > 0;

      if (flag) {

	NumericMatrix season_area = tmp_crop_area(Range(k * n_input, k * n_input + (n_input - 1)),_);
	NumericMatrix season_suit = tmp_crop_suit(Range(k * n_input, k * n_input + (n_input - 1)),_);
	NumericMatrix season_nb = tmp_crop_nb(Range(k * n_input, k * n_input + (n_input - 1)),_);

	// working copy
	NumericMatrix season_area2 = clone(season_area);

	// annual crops are always dealt with first, so k == 0
	// means annual
	if (k == 0) {

	  // 1 crop conversion
	  if ((n_decr > 0) && (total_decr_area > 0)) {

	    if (n_season > 1) {

	      // work out the maximum amount by which the crop can
	      // increase
	      double max_change = 0;
	      for (int i = 0; i < n_season; i++) {
		double max_change_new = tmp_cropland_area - (total_season_area[i] - total_decr_season_area[i]);
		if (max_change_new > max_change) {
		  max_change = max_change_new;
		}
	      }

	      // adjust fact to ensure the amount of change does not exceed max_change
	      double fact2;
	      if (fact > (max_change / tmp_cell_area)) {
		fact2 = max_change / tmp_cell_area;
	      } else {
		fact2 = fact;
	      }

	      // identify the season with the maximum area of crops
	      // with falling demand
	      int max_decr_season_ix = which_max(total_decr_season_area);

	      // add perennial crops with falling area to annual matrix
	      for (int i = 0; i < n_input; i++) {
		int ii = max_decr_season_ix * n_input + i;
		for (int j = 0; j < n_decr; j++) {
		  int jj = decr_ix[j];
		  season_area2(i,jj) = tmp_crop_area(ii,jj);
		}
	      }

	      // run crop conversion algorithm
	      season_area2 =
	      	allocate_crop_conversionC(season_area2,  // area
	      				  season_suit,   // suitability
					  season_nb,	 // neighbourhood
	      				  decr_ix,       // decr_ix
	      				  crop_ix,       // crop_ix
	      				  tmp_cell_area, // cell_area
	      				  fact2,         // fact
	      				  rand_min,      // rand_min
	      				  rand_max);     // rand_max

	      double change = sum(season_area2(_,crop_ix)) - sum(season_area(_,crop_ix));

	      // update crop area in main crop_area matrix
	      for (int i = 0; i < n_season; i++) {

		double xx = total_decr_season_area[i];

		// adjust change so that it doesn't exceed the total
		// decreasing crop area
		double change2;
		if (change > xx) {
		  change2 = xx;
		} else {
		  change2 = change;
		}

		// update crop area matrix with new areas for crops
		// with falling demand (don't update current crop
		// area yet)
		for (int j = 0; j < n_input; j++) {
		  int ij = i * n_input + j;
		  for (int kk = 0; kk < n_decr; kk++) {
		    int kkk = decr_ix[kk];
		    if (tmp_crop_area(ij,kkk) > 0) { // TODO: tidy this section up a bit
		      double updated_area = tmp_crop_area(ij,kkk) - (tmp_crop_area(ij,kkk) / xx) * change2;
		      updated_area = Rcpp::max(Rcpp::NumericVector::create(0, updated_area));
		      
		      tmp_crop_area(ij,kkk) = updated_area;
		    }
		  }
		}
	      }

	    } else {

	      season_area2 =
	      	allocate_crop_conversionC(season_area2,  // area
	      				  season_suit,   // suitability
					  season_nb,
	      				  decr_ix,       // decr_ix
	      				  crop_ix,       // crop_ix
	      				  tmp_cell_area, // cell_area
	      				  fact,          // fact
	      				  rand_min,      // rand_min
	      				  rand_max);     // rand_max

	      for (int i = 0; i < n_input; i++) {
		int ii = k * n_input + i;
		tmp_crop_area(ii,_) = season_area2(i,_);
	      }
	    }
	  }

	  // 2 cropland expansion

	  // if there is more than one growing season we need to
	  // consider the maximum area of crops that are grown
	  // simultaneously
	  if (n_season > 1) {

	    // find the season (other than annual) with the largest
	    // area of crops
	    NumericVector a = clone(total_season_area);
	    a.erase(0); // this removes the first value (i.e. annual)
	    int max_season_ix = which_max(a) + 1;

	    // add perennial crops to annual crops (all except crop
	    // under consideration)
	    NumericVector b = season_area2(_,crop_ix);
	    for (int i = 0; i < n_input; i++) {
	      int ii = max_season_ix * n_input + i; // row index
	      season_area2(i,_) = tmp_crop_area(ii,_);
	    }
	    season_area2(_,crop_ix) = b;
	  }

	  season_area2 =
	    allocate_expansionC(season_area2,      // area
	  			season_suit,       // suit
				season_nb,
	  			crop_ix,           // crop_ix
	  			tmp_cropland_area, // cropland_area
	  			tmp_cell_area,     // cell_area
	  			fact,          // fact
	  			rand_min,      // rand_min
	  			rand_max);     // rand_max

	  // 3 cropland intensification
	  season_area2 =
	    allocate_intensificationC(season_area2,  // area
	  			      season_suit,   // suit
				      season_nb,
	  			      crop_ix,       // crop_ix
	  			      tmp_cell_area, // cell_area
	  			      fact,          // fact
	  			      rand_min,      // rand_min
	  			      rand_max);     // rand_max
	  			      // false);        //

	  // 4 update crop matrix
	  for (int i = 0; i < n_input; i++) {
	    int ii = season_ix[i];
	    double new_area = season_area2(i,crop_ix);
	    tmp_crop_area(ii,crop_ix) = Rcpp::max(NumericVector::create(0, new_area));
	  }

	} else {

	  if (n_decr > 0) {	  
	    season_area2 =
	      allocate_crop_conversionC(season_area2,  // area
	  				season_suit,   // suitability
					season_nb,
	  				decr_ix,       // decr_ix
	  				crop_ix,       // crop_ix
	  				tmp_cell_area, // cell_area
	  				fact,          // fact
	  				rand_min,      // rand_min
	  				rand_max);     // rand_max
	  }

	  season_area2 =
	    allocate_expansionC(season_area2,      // area
	  			season_suit,       // suit
				season_nb,
	  			crop_ix,           // crop_ix
	  			tmp_cropland_area, // cropland_area
	  			tmp_cell_area,     // cell_area
	  			fact,          // fact
	  			rand_min,      // rand_min
	  			rand_max);     // rand_max

	  season_area2 =
	    allocate_intensificationC(season_area2,  // area
	  			      season_suit,   // suit
				      season_nb,
	  			      crop_ix,       // crop_ix
	  			      tmp_cell_area, // cell_area
	  			      fact,          // fact
	  			      rand_min,      // rand_min
	  			      rand_max);     // rand_max
	  			      // false);        // deintensify

	  for (int i = 0; i < n_input; i++) {
	    int ii = season_ix[i];
	    tmp_crop_area(ii,_) = season_area2(i,_);
	  }
	}
      }
    }

    // update main crop matrix
    for (int i = 0; i < n_season_input; i++) {
      int ii = start_row_ix + i;
      crop_area(ii,_) = tmp_crop_area(i,_);
    }

    // update demand matrix
    demand = calculate_changeC(tmp_crop_area0, // area
			       tmp_crop_area,  // area1
			       tmp_crop_yield, // yield
			       demand);        // demand

    if (n_decr > 0) {
      std::vector<int> erase_ix;
      for (int i = 0; i < n_decr; i++) {
	int ii = decr_ix[i];
	double abs_decr_demand = abs(demand[ii]);
	if ((abs_decr_demand < tol) || (demand[ii] > 0)) {
	  erase_ix.push_back(i);
	}
      }

      int n_erase = erase_ix.size();
      if (n_erase > 0) {
	// backwards loop to ensure that deleting one element does
	// not change the index of subsequent elements to be deleted
	for (int i = (n_erase - 1); i >= 0; i--) {
	  decr_ix.erase(i);
	}
      }  
      n_decr = decr_ix.size();
    }

  } while ((demand[crop_ix] > tol) && (counter < maxiter));

  Rprintf("%f\n", demand[crop_ix]);  
  Rprintf("%d\n", counter);

  NumericMatrix total_crop_area_new = get_total_areaC(crop_area, n_season, n_input);
  return List::create(Named("demand") = demand, _["area"] = crop_area, _["total_area"] = total_crop_area_new);  
}

// [[Rcpp::export]]
List allocate_decr(NumericMatrix crop_area0, NumericMatrix crop_yield, NumericMatrix crop_suit, NumericMatrix crop_nb, NumericMatrix total_crop_area, NumericVector cropland_area, NumericVector cell_area, NumericVector demand0, int crop_ix, IntegerVector decr_ix, int n_season, int n_input, double fact, double rand_min, double rand_max, double tol, int maxiter) {

  // make deep copies of variables that will be changed
  NumericMatrix crop_area = clone(crop_area0);
  NumericVector demand = clone(demand0);
  int n_cell = cell_area.size(), n_season_input = n_season * n_input, ncol=crop_area0.ncol();
  
  int counter = 0;
  int ix, start_row_ix, end_row_ix;
  double tmp_cell_area;

  do {

    counter++; // advance counter

    ix = (sample(n_cell, 1) - 1)[0];
    start_row_ix = ix * n_season_input;
    end_row_ix = start_row_ix + n_season_input - 1;

    NumericMatrix tmp_crop_area = crop_area(Range(start_row_ix, end_row_ix),_);
    NumericMatrix tmp_crop_suit = crop_suit(Range(start_row_ix, end_row_ix),_);
    NumericMatrix tmp_crop_nb = crop_nb(Range(start_row_ix, end_row_ix),_);
    NumericMatrix tmp_crop_yield = crop_yield(Range(start_row_ix, end_row_ix),_);

    NumericMatrix tmp_crop_area0 = clone(tmp_crop_area);
    tmp_cell_area = cell_area[ix];

    // loop through seasons to perform allocation
    for (int k = 0; k < n_season; k++) {

      IntegerVector season_ix = seq(k * n_input, k * n_input + (n_input - 1));
      bool flag = total_crop_area(k,crop_ix) > 0;

      if (flag) {

    	NumericMatrix season_area = tmp_crop_area(Range(k * n_input, k * n_input + (n_input - 1)),_);
    	NumericMatrix season_suit = tmp_crop_suit(Range(k * n_input, k * n_input + (n_input - 1)),_);
	NumericMatrix season_nb = tmp_crop_nb(Range(k * n_input, k * n_input + (n_input - 1)),_);
    	NumericMatrix season_area2 = clone(season_area); // work on a copy

    	// untested
    	season_area2 =
    	  allocate_contractionC(season_area2,      // area
    				season_suit,       // suit
				season_nb,
    				crop_ix,           // crop_ix
    				tmp_cell_area,     // cell_area
    				fact,              // fact
    				rand_min,          // rand_min
    				rand_max);         // rand_max

    	// update crop matrix - TODO: put in function
    	for (int i = 0; i < n_input; i++) {
    	  int ii = season_ix[i];
    	  double new_area = season_area2(i,crop_ix);
          tmp_crop_area(ii,crop_ix) = Rcpp::max(NumericVector::create(0.0, new_area));
    	}
      }
    }
	  
    // update main crop matrix - TODO: put in function
    for (int i = 0; i < n_season_input; i++) {
      int ii = start_row_ix + i;
      crop_area(ii,_) = tmp_crop_area(i,_);
    }

    // update demand matrix
    demand = calculate_changeC(tmp_crop_area0, // area
    			       tmp_crop_area,  // area1
    			       tmp_crop_yield, // yield
    			       demand);        // demand
    
  } while ((std::abs(demand[crop_ix]) > tol) && (counter < maxiter) && (demand[crop_ix] < 0));

  Rprintf("%f\n", demand[crop_ix]);  
  Rprintf("%d\n", counter);

  NumericMatrix total_crop_area_new = get_total_areaC(crop_area, n_season, n_input);

  return List::create(Named("demand") = demand, _["area"] = crop_area, _["total_area"] = total_crop_area_new);

}

// [[Rcpp::export]]
List allocate(NumericMatrix crop_area0, NumericMatrix crop_yield, NumericMatrix crop_suit, NumericMatrix crop_nb, NumericMatrix total_crop_area, NumericVector cropland_area, NumericVector cell_area, NumericVector demand0, LogicalVector crop, LogicalVector decr, int n_season, int n_input, double fact, double rand_min, double rand_max, double tol, int maxiter) {

  if (crop.size() != decr.size()) {
    stop("Logical vectors 'crop' and 'decr' must have the same size");
  }
      
  // check whether crop has decreasing or increasing demand
  IntegerVector v = seq(0, decr.size()-1);
  IntegerVector decr_ix = v[decr];

  IntegerVector crop_ix_tmp = v[crop];
  if (crop_ix_tmp.size() != 1) {
    stop("Logical vector 'crop' must refer to one and only one crop type");
  }
  int crop_ix = crop_ix_tmp[0];
  
  bool test = std::find(decr_ix.begin(), decr_ix.end(), crop_ix) != decr_ix.end();
  if (test) {
    return allocate_decr(crop_area0, crop_yield, crop_suit, crop_nb, total_crop_area, cropland_area, cell_area, demand0, crop_ix, decr_ix, n_season, n_input, fact, rand_min, rand_max, tol, maxiter);
    
  } else {
    return allocate_incr(crop_area0, crop_yield, crop_suit, crop_nb, total_crop_area, cropland_area, cell_area, demand0, crop_ix, decr_ix, n_season, n_input, fact, rand_min, rand_max, tol, maxiter);    
  }
  
}

