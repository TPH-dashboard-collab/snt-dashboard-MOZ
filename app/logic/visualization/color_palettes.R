# fmt: skip
box::use(
  RColorBrewer[brewer.pal],
  app/logic/core/varfns[risk_colors],
  checkmate[qtest],
  grDevices[colorRampPalette],
  leaflet[colorFactor],
  stats[setNames],
  viridis[viridis],
)

#' Get standard malaria risk strata color palette
#'
#' Returns a named vector of colors for the four standard malaria risk strata
#' categories defined in config.yml.
#'
#' @return Named character vector with risk strata as names and hex colors as
#'   values
#' @export
get_risk_colors <- function() {
  risk_colors()
}

#' Create a custom blue color palette
#'
#' This function generates a color palette based on the "Blues" palette from
#' RColorBrewer. It removes the lightest shades for better readability and can
#' interpolate to create more colors if needed.
#'
#' @param n The number of colors to generate in the palette
#'
#' @return A vector of color codes in hexadecimal format
#'
#' @examplesIf interactive()
#' # Generate a palette with 5 colors
#' five_color_palette <- create_blue_palette(5)
#'
#' # Generate a palette with 10 colors
#' ten_color_palette <- create_blue_palette(10)
#' @export
create_blue_palette <- function(n) {
  # n should be an integer > 0, not NA
  stopifnot("n must be an integer > 0" = qtest(n, "X+[0,)"))

  # Get the "Blues" palette from RColorBrewer
  # This palette has 9 predefined shades of blue
  blues <- brewer.pal(9, "Blues")

  # Remove the first two colors from the palette. These are typically very light
  # shades that may be hard to read on a white background
  blues <- blues[seq(3, length(blues))]

  # Create a function that can interpolate between the selected blues
  blue_ramp <- colorRampPalette(blues)

  # Generate 'n' colors using the color ramp function
  # If n <= length(blues), this will select a subset
  # If n > length(blues), this will interpolate to create new colors
  return(blue_ramp(n))
}

#' Create a custom color palette with special handling for "No intervention"
#'
#' @param intervention_combos Character vector of intervention combinations
#' @param no_intervention_color Color to use for "No intervention" (default:
#'   "grey85")
#' @param palette_name Name of the palette to use for other interventions
#'   (default: "viridis")
#'
#' @return A function that maps intervention combinations to colors
#' @export
create_intervention_palette <- function(intervention_combos,
                                        no_intervention_color = "grey85",
                                        palette_name = "viridis") {
  stopifnot(
    "intervention_combos must be a character vector without NAs" =
      qtest(intervention_combos, "S+")
  )
  stopifnot(
    "no_intervention_color must be a single string" =
      qtest(no_intervention_color, "S1") && no_intervention_color != ""
  )
  stopifnot(
    "palette_name must be a single string" =
      qtest(palette_name, "S+") && palette_name != ""
  )


  # Get unique interventions
  unique_interventions <- unique(intervention_combos)

  # Check if "No intervention" is present
  has_no_intervention <- "No intervention" %in% unique_interventions

  if (has_no_intervention) {
    # Remove "No intervention" from the list that will use viridis colors
    other_interventions <- setdiff(unique_interventions, "No intervention")

    # Create viridis colors for other interventions
    n_colors <- length(other_interventions)
    other_colors <- viridis(n_colors)

    # Combine colors into a named vector
    all_colors <- c(
      setNames(no_intervention_color, "No intervention"),
      setNames(other_colors, other_interventions)
    )

    # Create the color factor function
    pal <- function(x) {
      unname(all_colors[x])
    }
  } else {
    # If no "No intervention" present, use regular viridis palette
    pal <- colorFactor(
      palette = palette_name,
      levels = unique_interventions
    )
  }

  return(pal)
}
