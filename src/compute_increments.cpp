#include <Rcpp.h>
using namespace Rcpp;
#include <algorithm>

namespace {

inline bool is_missing(double value) {
  // R's is.na() considers both NA_real_ and NaN missing.
  return R_IsNA(value) || R_IsNaN(value);
}

}

// [[Rcpp::export]]
Rcpp::NumericVector compute_increment_values_cpp(
    const Rcpp::NumericVector& values,
    const double reset_drop,
    const double min_pos,
    const double max_gap) {

  const R_xlen_t n = values.size();
  Rcpp::NumericVector increments(n, NA_REAL);

  if (n < 2) {
    Rcpp::stop(
      "compute_increment_values_cpp: values must contain at least two points"
    );
  }

  double last_max = values[0];
  int gap_length = 0;

  // R starts at 1; C++ starts at 0. Therefore i = 1 is R's second row.
  for (R_xlen_t i = 1; i < n; ++i) {
    const bool previous_missing = is_missing(values[i - 1]);
    const bool current_missing = is_missing(values[i]);

    if (previous_missing) {
      ++gap_length;
    } else {
      gap_length = 0;
    }

    // A valid observation immediately after an oversized gap establishes
    // a new baseline but does not receive an increment.
    if (previous_missing && gap_length > max_gap) {
      if (!current_missing) {
        last_max = values[i];
      }
      continue;
    }

    // No increment can be calculated when either side is missing.
    if (previous_missing || current_missing) {
      continue;
    }

    const double difference = values[i] - values[i - 1];

    // A sufficiently large drop establishes a new baseline.
    if (difference <= -reset_drop) {
      last_max = values[i];
      increments[i] = 0.0;
      continue;
    }

    if (is_missing(last_max)) {
      // This preserves the current function's failure for an unresolved
      // leading-NA baseline. Its intended behavior should be decided in R.
      Rcpp::stop("missing value where TRUE/FALSE needed");
    }

    const double baseline = std::max(last_max, values[i - 1]);
    const double addition = values[i] - baseline;

    if (addition >= min_pos) {
      increments[i] = addition;
      last_max = values[i];
    } else {
      increments[i] = 0.0;
    }
  }

  return increments;
}