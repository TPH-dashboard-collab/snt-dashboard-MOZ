#' Format numbers with magnitude suffixes
#'
#' Formats large numbers using magnitude suffixes for readability:
#' - "B" for billions (>= 1 billion)
#' - "M" for millions (>= 1 million)
#' - "k" for thousands (>= 1 thousand)
#' - Plain numbers with comma separators for values < 1000
#'
#' Numbers with suffixes display up to 2 decimal places (e.g., "1.28 M",
#' "1.10 B").
#'
#' @param x A numeric vector of values to format.
#'
#' @return A character vector of formatted numbers.
#'
#' @examples
#' format_number(1500)       # "1.50 k"
#' format_number(1280000)    # "1.28 M"
#' format_number(1100000000) # "1.10 B"
#' format_number(999)        # "999"
#'
#' @export
format_number <- function(x) {
  ifelse(
    x >= 1e9,
    paste(
      trimws(format(
        round(x / 1e9, 2),
        nsmall = 2,
        big.mark = ",",
        scientific = FALSE
      )),
      "B"
    ),
    ifelse(
      x >= 1e6,
      paste(
        trimws(format(
          round(x / 1e6, 2),
          nsmall = 2,
          big.mark = ",",
          scientific = FALSE
        )),
        "M"
      ),
      ifelse(
        x >= 1e3,
        paste(
          trimws(format(
            round(x / 1e3, 2),
            nsmall = 2,
            big.mark = ",",
            scientific = FALSE
          )),
          "k"
        ),
        format(round(x), nsmall = 0, big.mark = ",")
      )
    )
  )
}
