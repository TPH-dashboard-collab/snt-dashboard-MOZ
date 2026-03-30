box::use(
  checkmate[qtest, qtestr],
)

#' Generate a combined intervention string
#'
#' This function takes boolean values for different interventions and their
#' corresponding names, and generates a string that represents the combination
#' of active interventions.
#'
#' @param ... Boolean values indicating whether each intervention is active
#'   (TRUE) or not (FALSE)
#' @param int_names A character vector of intervention names, corresponding to
#'   the boolean values
#' @param no_int_name String used when no intervention combination found.
#'   Defaults to "No intervention".
#'
#' @return A string representing the combination of active interventions, or
#'   "No intervention" if none are active
#'
#' @examplesIf interactive()
#' get_intervention_combo(
#'   TRUE, FALSE, TRUE,
#'   int_names = c("ITN", "IRS", "Vaccine")
#' )
#' # Returns: "ITN + Vaccine"
#'
#' get_intervention_combo(
#'   FALSE, FALSE, FALSE,
#'   int_names = c("ITN", "IRS", "Vaccine")
#' )
#' # Returns: "No intervention"
#' @export
get_intervention_combo <- function(...,
                                   int_names,
                                   no_int_name = "No intervention") {
  stopifnot(
    "int_names must be a non-empty character vector" = qtest(int_names, "S+")
  )
  stopifnot(
    "no_int_name must be a non-empty string" = qtest(no_int_name, "S1") &&
      no_int_name != ""
  )

  # Convert the input boolean values to a list
  cols <- list(...)
  # Check if args are empty
  stopifnot("... must be booleans" = qtest(cols, "L+"))
  # Check if args are boolean
  stopifnot("... must be booleans" = qtestr(cols, "B+"))

  # Initialize an empty vector to store active intervention names
  combo <- c()

  # Iterate through the boolean values
  for (i in seq_along(cols)) {
    # If the intervention is active (TRUE), add its name to the combo vector
    if (cols[[i]]) combo <- c(combo, int_names[[i]])
  }

  # Check if any interventions are active
  if (length(combo) > 0) {
    # If there are active interventions, combine their names with " + "
    # separator
    return(paste0(combo, collapse = " + "))
  } else {
    # If no interventions are active, return "No intervention"
    return(no_int_name)
  }
}
