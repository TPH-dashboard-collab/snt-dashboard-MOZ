box::use(
  checkmate[qtest],
  data.table[setDT],
)

#' Extract interventions from column names
#'
#' This function takes a data frame and extracts the intervention names from
#' columns that follow the pattern "deployed_int_X" where X is the intervention
#' name. For example, "deployed_int_ITN" would be converted to "ITN".
#'
#' @param dt A data.frame containing columns with names in the format
#'   "deployed_int_X" where X represents different interventions. Must include
#'   at least one column with a name following the pattern "deployed_int_X".
#'
#' @returns
#' A character vector containing the extracted intervention names.
#'
#' @examplesIf interactive()
#' # Create example data.table
#' dt <- data.table(
#'   deployed_int_ITN = c(TRUE, FALSE),
#'   deployed_int_IRS = c(FALSE, TRUE),
#'   other_column = 1:2
#' )
#'
#' # Extract intervention names
#' interventions <- colnames_to_interventions(dt)
#' # Returns: c("ITN", "IRS")
#' @export
colnames_to_interventions <- function(dt) {
  # Input validation
  # Data frame with at least one column
  stopifnot(
    "dt must be a non-empty data frame" = qtest(dt, "D+") && nrow(dt) > 0
  )

  # Ensure we have a data.table
  setDT(dt)

  # Extract column names that match the pattern "^deployed_int_(.*)"
  pattern <- "^deployed_int_.*"
  deployed <- grep(
    pattern = pattern, x = colnames(dt), value = TRUE
  )

  if (length(deployed) == 0) {
    stop(
      "No intervention columns found matching pattern '", pattern, "'. ",
      "Expected columns like 'deployed_int_ITN', 'deployed_int_IRS', etc."
    )
  }

  # Capture the intervention name via the pattern "^deployed_int_(.*)" and
  # return it ("\\1")
  deployed <- gsub(
    pattern = "^deployed_int_(.*)", replacement = "\\1", x = deployed
  )

  # Validate that no empty intervention names were extracted
  empty_interventions <- which(deployed == "")
  if (length(empty_interventions) > 0) {
    warning(
      "Found ",
      length(empty_interventions),
      " column(s) with empty intervention names. ",
      "This may cause issues downstream"
    )
    deployed <- deployed[deployed != ""]
  }

  # Check for duplicate intervention names
  if (length(deployed) > length(unique(deployed))) {
    duplicate_names <- deployed[duplicated(deployed)]
    warning(
      "Duplicate intervention names found: ",
      paste(unique(duplicate_names), collapse = ", "),
      ". This may cause issues downstream"
    )
  }

  # Final validation - ensure we still have interventions after filtering
  if (length(deployed) == 0) {
    stop("No valid intervention names could be extracted")
  }

  # Return the unique intervention names in original order
  return(unique(deployed))
}
