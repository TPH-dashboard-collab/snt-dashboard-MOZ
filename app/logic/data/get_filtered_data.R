box::use(
  checkmate[qtest],
  data.table[`%chin%`, setDT],
)

# fmt: skip
box::use(
  app/logic/interventions/colnames_to_interventions[colnames_to_interventions],
  app/logic/interventions/get_intervention_combo[get_intervention_combo],
)

#' Add intervention combination column to data.table
#'
#' This function adds an "intervention_combo" column to a data.table that
#' contains a human-readable string representing the combination of active
#' interventions. The function identifies intervention columns by their
#' "deployed_int_" prefix and combines active interventions with " + "
#' separator.
#'
#' @param dt A data.table containing intervention columns.  Must include
#'   columns:
#'   * At least one column following the pattern "deployed_int_X" where X
#'     represents different interventions. The column should contain boolean
#'     values (TRUE/FALSE) indicating whether each intervention is active.
#'
#' @return The input data.table with an additional "intervention_combo" column
#'   containing strings like "ITN + IRS" or "No intervention" if no
#'   interventions are active. The function modifies the data.table in-place.
#' @export
add_intervention_combo_column <- function(dt) {
  # Input validation
  # Data frame with at least one column and no missing value in any column
  stopifnot(
    "dt must be a non-empty data frame" = qtest(dt, "D+") && nrow(dt) > 0
  )

  # Ensure we have a data.table
  setDT(dt)

  # Generate intervention combinations if not already present
  if (!"intervention_combo" %chin% colnames(dt)) {
    # Find all intervention columns by matching pattern "^deployed_int_.*"
    int_cols <- grep(
      pattern = "^deployed_int_.*",
      x = colnames(dt),
      value = TRUE
    )

    # Validate that intervention columns exist
    if (length(int_cols) == 0) {
      warning(
        "No intervention columns found matching pattern '^deployed_int_.*'"
      )
      dt[, intervention_combo := "No intervention"]
      return(dt)
    }

    # Validate that all intervention columns exist in the data.table
    missing_cols <- setdiff(int_cols, colnames(dt))
    if (length(missing_cols) > 0) {
      stop(sprintf(
        "Intervention columns not found in data.table: %s",
        paste(missing_cols, collapse = ", ")
      ))
    }

    tryCatch(
      {
        # Combine individual interventions into a single string using mapply
        dt[,
          intervention_combo := do.call(
            mapply,
            # c() combines three parts into a single list of arguments for
            # mapply:
            c(
              # 1. The function to apply (get_intervention_combo)
              list(FUN = get_intervention_combo),

              # 2. Convert column names to list - this allows variable
              #    number of columns. If cols = c("col1", "col2"), this
              #    becomes list("col1", "col2")
              as.list(cols),

              # 3. Additional arguments that stay constant for each mapply call
              list(
                MoreArgs = list(
                  int_names = colnames_to_interventions(dt)
                )
              )
            )
          ),
          # env argument handles the variable substitution in data.table.
          # as.list(int_cols) ensures proper handling of column names as symbols
          env = list(cols = as.list(int_cols))
        ]
      },
      error = function(e) {
        warning(
          sprintf("Error generating intervention combinations: %s", e$message)
        )
        stop()
      }
    )
  }

  return(dt)
}
