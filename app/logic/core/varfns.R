# Dependencies -----------------------------------------------------------------

box::use(
  config,
)

# Code -------------------------------------------------------------------------

#' Get administrative division level names
#' @export
admin_mapping <- function() {
  unlist(config$get("admin_mapping"))
}

#' Map scenario identifiers to display names
#' @export
scenario_mapping <- function() {
  unlist(config$get("scenario_mapping"))
}

#' Map impact metric columns to display names
#' @export
impact_mapping <- function() {
  unlist(config$get("impact_mapping"))
}

#' Get ordered risk strata levels (factor ordering)
#' @return Character vector of risk levels from lowest to highest risk
#' @export
risk_levels <- function() {
  config$get("risk_strata")$levels
}

#' Get risk strata thresholds for classification
#' @return Named numeric vector: level name -> upper prevalence threshold
#' @export
risk_thresholds <- function() {
  unlist(config$get("risk_strata")$thresholds)
}

#' Get high-risk strata names
#' @return Character vector of strata considered high risk
#' @export
high_risk_strata <- function() {
  config$get("risk_strata")$high_risk
}

#' Get risk strata color palette
#' @return Named character vector: level name -> hex color
#' @export
risk_colors <- function() {
  unlist(config$get("risk_strata")$colors)
}

#' Calculate risk stratum from prevalence rate
#'
#' Central function for risk classification.
#'
#' @param prevalence_rate Numeric vector of prevalence rates
#' @return Character vector of risk strata names
#' @export
classify_risk <- function(prevalence_rate) {
  thresholds <- risk_thresholds()
  levels <- risk_levels()

  # Sort thresholds in ascending order
  sorted_idx <- order(thresholds)
  sorted_names <- names(thresholds)[sorted_idx]
  sorted_thresholds <- thresholds[sorted_idx]

  # Initialize with highest risk
  result <- rep(levels[length(levels)], length(prevalence_rate))

  # Apply thresholds from highest to lowest
  for (i in rev(seq_along(sorted_thresholds))) {
    result[prevalence_rate <= sorted_thresholds[i]] <- sorted_names[i]
  }

  result
}
