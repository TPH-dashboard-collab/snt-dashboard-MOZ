box::use(
  checkmate[qtest],
  data.table[is.data.table, setDT],
  logger[log_debug, log_warn],
)

# fmt: skip
box::use(
  app/logic/core/logging[log_ns_builder, log_ns_fn],
)

#' Calculate Hamming distance between two boolean vectors
#'
#' The Hamming distance is the count of positions where two vectors differ. For
#' intervention combinations, this represents how many interventions differ
#' between the target and candidate combinations.
#'
#' @param target Logical vector (target intervention combination)
#' @param candidate Logical vector (available intervention combination)
#'
#' @return Integer representing the number of differing positions
#'
#' @details
#' Distance of 0 = exact match
#' Distance of 1 = one intervention differs
#' Distance of n = n interventions differ
#'
#' @examples
#' \dontrun{
#' target <- c(TRUE, FALSE, TRUE, FALSE)
#' candidate <- c(TRUE, TRUE, TRUE, FALSE)
#' calculate_hamming_distance(target, candidate)
#' # Returns: 1 (second position differs)
#' }
#'
#' @export
calculate_hamming_distance <- function(target, candidate) {
  stopifnot(
    "target must be a logical vector" = qtest(target, "B+")
  )
  stopifnot(
    "candidate must be a logical vector" = qtest(candidate, "B+")
  )
  stopifnot(
    "target and candidate must have same length" = length(target) ==
      length(candidate)
  )
  # The Hamming distance for logical vectors is rather trivial.
  sum(target != candidate)
}

#' Create boolean intervention vector from selection list
#'
#' Converts two character vectors of selected intervention names into a named
#' list compatible with database columns. This is used for comparing user
#' selections against available combinations.
#'
#' @param selected_interventions Character vector of intervention column names
#'   that are selected (e.g., c("deployed_int_CM", "deployed_int_STD_Nets"))
#' @param all_intervention_cols Character vector of all possible intervention
#'   column names in order
#'
#' @return Named list of TRUE/FALSE values for each intervention
#'
#' @examples
#' \dontrun{
#' all_cols <- c("deployed_int_CM", "deployed_int_IRS", "deployed_int_STD_Nets")
#' selected <- c("deployed_int_CM", "deployed_int_STD_Nets")
#'
#' create_intervention_list(selected, all_cols)
#' # Returns:
#' # list(deployed_int_CM = TRUE,
#' #      deployed_int_IRS = FALSE,
#' #      deployed_int_STD_Nets = TRUE)
#' }
#'
#' @export
create_intervention_list <- function(
  selected_interventions,
  all_intervention_cols
) {
  stopifnot(
    "selected_interventions must be a character vector" = qtest(
      selected_interventions,
      "S*"
    )
  )
  stopifnot(
    "all_intervention_cols must be a non-empty character vector" = qtest(
      all_intervention_cols,
      "S+"
    )
  )

  # Initialize all to FALSE
  result <- rep(FALSE, length(all_intervention_cols))
  names(result) <- all_intervention_cols

  # Set selected interventions to TRUE
  result[selected_interventions] <- TRUE

  as.list(result)
}

#' Find best matching intervention combination in data pool
#'
#' Given a target intervention combination, finds the closest available
#' combination in the data pool using Hamming distance. If multiple
#' combinations have the same distance, uses priority weighting as a
#' tiebreaker.
#'
#' @param district_name String name of district to match for
#' @param target_interventions Named list of TRUE/FALSE for each intervention
#'   (output from `create_intervention_list`)
#' @param int_data_pool_dt data.table containing available intervention
#'   combinations. Must have columns: admin_2, scenario_name, and all
#'   intervention columns (deployed_int_*)
#' @param priority_weights Optional named numeric vector for tiebreaking.
#'   Higher values indicate more important interventions. If NULL, first match
#'   is returned.
#'
#' @return List with components:
#' \describe{
#'   \item{matched_data}{data.table row with best matching combination}
#'   \item{distance}{Integer Hamming distance to target}
#'   \item{exact_match}{Logical: TRUE if distance is 0}
#'   \item{original_target}{The target intervention vector provided}
#'   \item{matched_interventions}{Named list of TRUE/FALSE for matched combo}
#' }
#'
#' @details
#' Algorithm:
#' 1. Filter data pool to specified district
#' 2. Calculate Hamming distance for each available combination
#' 3. Find minimum distance
#' 4. If multiple matches, use priority weighting:
#'    - Score = sum(priority * intervention_present)
#'    - Select combination with highest score
#' 5. Return match details
#'
#' If no data available for district, returns empty result with distance = Inf
#'
#' @export
find_best_match <- function(
  district_name,
  target_interventions,
  int_data_pool_dt,
  priority_weights = NULL,
  ...
) {
  stopifnot(
    "district_name must be a non-empty string" = qtest(district_name, "S1") &&
      district_name != ""
  )
  stopifnot(
    "target_interventions must be a list" = qtest(target_interventions, "l")
  )
  stopifnot(
    "int_data_pool_dt must be a data.table" = is.data.table(int_data_pool_dt)
  )
  if (!is.null(priority_weights)) {
    stopifnot(
      "priority_weights must be a named numeric vector" = qtest(
        priority_weights,
        "N+"
      ) &&
        !is.null(names(priority_weights))
    )
  }
  args <- list(...)
  log_ns <- log_ns_builder(args, as.character(match.call()[[1]]))

  # STEP 1: Filter to available combinations for this district
  available_combos <- int_data_pool_dt[admin_2 == district_name]

  if (nrow(available_combos) == 0) {
    log_warn(paste0("No data available for district: ", district_name))
    return(list(
      matched_data = setDT(data.frame()),
      distance = Inf,
      exact_match = FALSE,
      original_target = target_interventions,
      matched_interventions = list()
    ))
  }

  # STEP 2: Calculate Hamming distance for each available combo
  intervention_cols <- names(target_interventions)

  # Validate all intervention columns exist in data
  missing_cols <- setdiff(intervention_cols, names(available_combos))
  if (length(missing_cols) > 0) {
    stop(paste0(
      "Missing intervention columns in data pool: ",
      paste(missing_cols, collapse = ", ")
    ))
  }

  distances <- numeric(nrow(available_combos))
  target_vec <- as.logical(unlist(target_interventions))

  for (i in seq_len(nrow(available_combos))) {
    candidate_vec <- as.logical(
      unlist(available_combos[i, intervention_cols, with = FALSE])
    )
    distances[i] <- calculate_hamming_distance(target_vec, candidate_vec)
  }

  # STEP 3: Find minimum distance
  min_distance <- min(distances)
  best_match_indices <- which(distances == min_distance)
  best_matches <- available_combos[best_match_indices]

  log_debug(
    paste0(
      "Found ",
      length(best_match_indices),
      " combination(s) with distance ",
      min_distance,
      " for ",
      district_name
    ),
    namespace = log_ns_fn(log_ns)
  )

  # STEP 4: If multiple matches, use priority tiebreaker
  if (nrow(best_matches) > 1 && !is.null(priority_weights)) {
    log_debug(
      paste0(
        "Multiple matches found. Using priority tiebreaker."
      ),
      namespace = log_ns_fn(log_ns)
    )

    # <Score = sum(priority * intervention_present)>
    scores <- numeric(nrow(best_matches))
    for (i in seq_len(nrow(best_matches))) {
      intervention_vec <- as.logical(
        unlist(best_matches[i, intervention_cols, with = FALSE])
      )
      # Only score interventions that have defined priorities
      valid_priorities <- priority_weights[intervention_cols[intervention_vec]]
      valid_priorities <- valid_priorities[!is.na(valid_priorities)]
      scores[i] <- sum(valid_priorities)
    }
    best_match <- best_matches[which.max(scores)]

    log_debug(
      paste0(
        "Selected match with priority score ",
        max(scores)
      ),
      namespace = log_ns_fn(log_ns)
    )
  } else {
    best_match <- best_matches[1]
  }

  # STEP 5: Return match info
  matched_interventions_list <- as.list(
    best_match[, intervention_cols, with = FALSE]
  )

  result <- list(
    matched_data = best_match,
    distance = min_distance,
    exact_match = (min_distance == 0),
    original_target = target_interventions,
    matched_interventions = matched_interventions_list
  )

  if (result$exact_match) {
    log_debug(
      paste0(
        "Exact match found for ",
        district_name,
        " (scenario: ",
        best_match$scenario_name,
        ")"
      ),
      namespace = log_ns_fn(log_ns)
    )
  } else {
    log_debug(
      paste0(
        "Best match for ",
        district_name,
        " has distance ",
        min_distance,
        " (scenario: ",
        best_match$scenario_name,
        ")"
      ),
      namespace = log_ns_fn(log_ns)
    )
  }

  result
}
