box::use(
  checkmate[qtest],
  glue[glue_sql],
)

#' Generate counterfactual intervention combinations
#'
#' For a named logical vector of active interventions, generates counterfactual
#' combinations by switching off one active intervention at a time.
#' Base interventions (configured in config.yml) cannot be switched off.
#'
#' @param target Named logical vector of intervention booleans where
#'   TRUE indicates the intervention is deployed.
#' @param base_interventions Character vector of intervention column names that
#'   cannot be switched off. Must be provided (typically from config$get("base_interventions")).
#'
#' @return A named list of named logical vectors. The first element ("original")
#'   contains the input combination. Subsequent elements are counterfactuals
#'   where one active non-base intervention has been switched to FALSE.
#'   List names indicate which intervention was switched off (or "original").
#'
#' @examples
#' \dontrun{
#' target <- c(
#'   deployed_int_CM = TRUE,
#'   deployed_int_IRS = TRUE,
#'   deployed_int_STD_Nets = TRUE,
#'   deployed_int_Vaccine = FALSE
#' )
#' combos <- generate_counterfactual_combos(target, base_interventions = c("deployed_int_CM"))
#' # Returns list with:
#' # $original: all original values
#' # $deployed_int_IRS: IRS set to FALSE
#' # $deployed_int_STD_Nets: STD_Nets set to FALSE
#' }
#'
#' @export
generate_counterfactual_combos <- function(
  target,
  base_interventions
) {
  stopifnot(
    "target must be a named logical vector" = qtest(target, "B+") &&
      !is.null(names(target)),
    "base_interventions must be a character vector" = qtest(
      base_interventions,
      "S*"
    )
  )

  # Start with original combination
  result <- list(original = target)

  # Find active interventions (TRUE) that are not base interventions
  active_interventions <- names(target)[target == TRUE]
  switchable_interventions <- setdiff(active_interventions, base_interventions)

  # Generate one counterfactual per switchable intervention
  for (interv in switchable_interventions) {
    counterfactual <- target
    counterfactual[interv] <- FALSE
    result[[interv]] <- counterfactual
  }

  return(result)
}

#' Build SQL WHERE clause for intervention combinations
#'
#' Constructs a SQL WHERE clause that matches any of the provided
#' intervention boolean combinations using OR logic. Each combination
#' is expressed as a conjunction (AND) of column=value conditions,
#' and combinations are joined with OR.
#'
#' @param combos Named list of named logical vectors representing
#'   intervention combinations. Typically the output of
#'   `generate_counterfactual_combos()`.
#' @param con Database connection object for SQL escaping (e.g., from pool::dbPool).
#'
#' @return Character string containing SQL WHERE clause fragment.
#'   Returns empty string if combos is empty.
#'   Format: `((col1=1 AND col2=0 AND ...) OR (col1=1 AND col2=1 AND ...) OR ...)`
#'
#' @examples
#' \dontrun{
#' library(pool)
#' con <- dbPool(RSQLite::SQLite(), dbname = "data.sqlite")
#' combos <- list(
#'   original = c(deployed_int_CM = TRUE, deployed_int_IRS = TRUE),
#'   deployed_int_IRS = c(deployed_int_CM = TRUE, deployed_int_IRS = FALSE)
#' )
#' clause <- build_combo_where_clause(combos, con)
#' # Returns: "((deployed_int_CM=1 AND deployed_int_IRS=1) OR (deployed_int_CM=1 AND deployed_int_IRS=0))"
#' }
#'
#' @export
build_combo_where_clause <- function(combos, con) {
  stopifnot("combos must be a list" = is.list(combos))
  stopifnot("con must be provided" = !missing(con))

  if (length(combos) == 0) {
    return("")
  }

  # Build one AND clause per combination
  combo_clauses <- lapply(combos, function(combo) {
    # Convert logical to integer (TRUE -> 1, FALSE -> 0)
    values <- as.integer(combo)
    cols <- names(combo)

    # Build individual column=value conditions
    conditions <- mapply(
      function(col, val) {
        glue_sql("{`col`}={val}", .con = con)
      },
      cols,
      values,
      SIMPLIFY = FALSE,
      USE.NAMES = FALSE
    )

    # Join with AND
    paste0("(", paste(conditions, collapse = " AND "), ")")
  })

  # Join all combinations with OR
  clause <- paste0("(", paste(combo_clauses, collapse = " OR "), ")")

  return(clause)
}
