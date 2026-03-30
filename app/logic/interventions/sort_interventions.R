box::use(
  checkmate[qtest],
  data.table[data.table, is.data.table, setDT, setorder, setorderv],
)

#' Sort intervention combinations by complexity and internal components
#'
#' This function sorts intervention combinations based on the number of
#' components and optionally sorts the components within each combination
#' alphabetically. When given a data.table, it sorts the entire table based on
#' the specified intervention column. The given data.table is modified in place.
#'
#' @param data Input data, either a character vector of interventions or a data
#'   frame/data.table
#' @param interventions If data is a data.table, the column name containing
#'   interventions. If data is a vector, this parameter is ignored.
#' @param split_pattern The pattern to split combinations on (default: " \\+ ")
#' @param combine_pattern The pattern to combine interventions with (default:
#'   " + ")
#' @param special_case A string to always place first (default:
#'   "No intervention")
#' @param alphabetical Logical, whether to sort alphabetically within same
#'   component count
#' @param reverse Logical, whether to sort from most to least complex
#' @param sort_components Logical, whether to sort components within each
#'   intervention string
#' @param reverse_components Logical, whether to reverse sort components within
#'   each intervention string
#'
#' @return Either a sorted character vector or a sorted data.table, depending on
#'   input
#'
#' @examplesIf interactive()
#' # Sort vector with components sorted alphabetically
#' interventions <- c("vaccine + SMC", "ITN", "IRS + SMC + vaccine")
#' sort_interventions(interventions, sort_components = TRUE)
#' # Returns: c("ITN", "SMC + vaccine", "IRS + SMC + vaccine")
#'
#' # Sort vector with components sorted reverse alphabetically
#' sort_interventions(interventions,
#'   sort_components = TRUE,
#'   reverse_components = TRUE
#' )
#' # Returns: c("ITN", "vaccine + SMC", "vaccine + SMC + IRS")
#' @export
sort_interventions <- function(data,
                               interventions = NULL,
                               split_pattern = " \\+ ",
                               combine_pattern = " + ",
                               special_case = "No intervention",
                               alphabetical = FALSE,
                               reverse = FALSE,
                               sort_components = FALSE,
                               reverse_components = FALSE) {
  stopifnot(
    "data must be a non-empty data frame or character vector" = qtest(
      data, "S+"
    ) ||
      (qtest(data, "D+") && nrow(data) > 0)
  )
  stopifnot(
    "interventions must be a string or NULL" = qtest(interventions, "0") ||
      (qtest(interventions, "S1") && interventions != "")
  )
  stopifnot(
    "split_pattern must be a string" = qtest(split_pattern, "S1") &&
      split_pattern != ""
  )
  stopifnot(
    "combine_pattern must be a string" = qtest(combine_pattern, "S1") &&
      combine_pattern != ""
  )
  stopifnot(
    "special_case must be a string" = qtest(special_case, "S1") &&
      special_case != ""
  )
  stopifnot(
    "alphabetical must be a boolean" = qtest(alphabetical, "B1") &&
      alphabetical != ""
  )
  stopifnot(
    "reverse must be a boolean" = qtest(reverse, "B1") &&
      reverse != ""
  )
  stopifnot(
    "sort_components must be a boolean" = qtest(sort_components, "B1") &&
      sort_components != ""
  )
  stopifnot(
    "reverse_components must be a boolean" = qtest(reverse_components, "B1") &&
      reverse_components != ""
  )


  # Check if input is a data frame or data.table
  is_dt <- is.data.frame(data) || is.data.table(data)

  if (is_dt) {
    setDT(data)
    # Validate column name if data.table input
    if (is.null(interventions) || !interventions %in% names(data)) {
      stop(
        paste0(
          "When providing a data.table, 'interventions' must specify valid ",
          "column names"
        )
      )
    }

    dt <- data
    intervention_vector <- dt[[interventions]]
  } else {
    # Convert vector input to data.table
    dt <- data.table(
      original = data,
      original_index = seq_along(data)
    )
    intervention_vector <- data
  }

  # Function to sort components within an intervention string
  sort_intervention_components <- function(intervention_str) {
    if (intervention_str == special_case) {
      return(intervention_str)
    } else {
      # Split string into components and sort
      sorted_components <- sort(strsplit(intervention_str, split_pattern)[[1]])

      # Recombine with original separator
      return(paste(sorted_components, collapse = combine_pattern))
    }
  }

  # Sort components within intervention strings if requested
  if (sort_components) {
    intervention_vector <- sapply(
      intervention_vector,
      sort_intervention_components
    )
    if (is_dt) {
      dt[, (interventions) := intervention_vector]
    } else {
      dt$original <- intervention_vector
    }
  }

  # Create component count column
  component_counts <- sapply(
    strsplit(intervention_vector, split_pattern),
    length
  )

  # Handle special case
  if (!is.null(special_case)) {
    component_counts[intervention_vector == special_case] <- 0
  }

  if (is_dt) {
    # Add component count to original data.table
    dt[, temp_n_components := component_counts]
    dt[, temp_index := .I]

    # Apply sorting
    if (alphabetical) {
      if (reverse) {
        setorderv(
          dt,
          cols = c("temp_n_components", interventions, "temp_index"),
          order = c(-1, 1, 1)
        )
      } else {
        setorderv(
          dt,
          cols = c("temp_n_components", interventions, "temp_index"),
          order = c(1, 1, 1)
        )
      }
    } else {
      if (reverse) {
        setorder(dt, -temp_n_components, temp_index)
      } else {
        setorder(dt, temp_n_components, temp_index)
      }
    }

    # Remove temporary columns
    dt[, c("temp_n_components", "temp_index") := NULL]

    return(dt)
  } else {
    # For vector input, add component counts and sort
    dt[, n_components := component_counts]

    if (alphabetical) {
      if (reverse) {
        setorder(dt, -n_components, original, original_index)
      } else {
        setorder(dt, n_components, original, original_index)
      }
    } else {
      if (reverse) {
        setorder(dt, -n_components, original_index)
      } else {
        setorder(dt, n_components, original_index)
      }
    }

    return(dt$original)
  }
}
