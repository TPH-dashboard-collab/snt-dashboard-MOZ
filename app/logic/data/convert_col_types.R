# Dependencies -----------------------------------------------------------------

box::use(
  checkmate[qtest],
  data.table[copy, is.data.table, setDT],
)

# Code -------------------------------------------------------------------------

#' Convert Column Types Based on Regex Patterns
#'
#' Applies conversion functions to columns matching specified regex patterns.
#'
#' @param dt A data.table or data.frame. If not a data.table, it will be
#'   converted.
#' @param rules Named list where names are regex patterns and values are
#'   functions (e.g., as.integer) to apply to matching columns
#' @param copy Logical indicating whether to create a copy of the data.table to
#'   avoid modifying by reference
#' @return A data.table with columns converted according to the rules (modified
#'   by reference unless copy=TRUE)
#' @export
convert_col_types <- function(dt, rules, copy = FALSE) {
  stopifnot("dt must not be empty" = qtest(dt, "D+") && nrow(dt) > 0)
  stopifnot("rules must be a non-empty named list" = qtest(rules, "L+"))
  stopifnot("copy must be a boolean" = qtest(copy, "B1"))

  if (!is.data.table(dt)) {
    setDT(dt)
  }

  # Create a copy of the data.table if requested
  if (copy) dt <- copy(dt)

  # Loop through each conversion rule
  for (col_pattern in names(rules)) {
    # Find columns matching the pattern (regex)
    target_cols <- grep(col_pattern, names(dt), value = TRUE)

    if (length(target_cols) > 0) {
      # Get the conversion function for this pattern
      convert_func <- rules[[col_pattern]]

      # Apply conversion to matching columns
      dt[, (target_cols) := lapply(.SD, convert_func), .SDcols = target_cols]
    }
  }

  return(dt)
}
