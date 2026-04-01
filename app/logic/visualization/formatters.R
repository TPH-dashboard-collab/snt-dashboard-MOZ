box::use(
  scales[comma]
)

#' Format large numbers in human-readable form
#'
#' @param x Numeric value to format
#' @param prefix Prefix to add (default: "$")
#' @param threshold Minimum value for abbreviation (default: 1e6)
#' @return Formatted string (e.g., "$1.5M", "$250K", "1,234")
#' @export
format_large_number <- function(x, prefix = "$", threshold = 1e6) {
  vapply(
    x,
    function(val) {
      if (is.na(val) || !is.finite(val)) {
        return("N/A")
      }

      abs_val <- abs(val)
      sign_str <- if (val < 0) "-" else ""

      if (abs_val >= 1e9) {
        formatted <- sprintf("%.1fB", abs_val / 1e9)
        paste0(sign_str, prefix, formatted)
      } else if (abs_val >= threshold && abs_val >= 1e6) {
        formatted <- sprintf("%.1fM", abs_val / 1e6)
        paste0(sign_str, prefix, formatted)
      } else if (abs_val >= threshold && abs_val >= 1e3) {
        formatted <- sprintf("%.0fK", abs_val / 1e3)
        paste0(sign_str, prefix, formatted)
      } else {
        paste0(sign_str, prefix, comma(round(abs_val)))
      }
    },
    character(1)
  )
}
