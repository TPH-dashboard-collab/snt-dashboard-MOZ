box::use(
  checkmate[qtest],
  config,
  logger[
    layout_glue_generator,
    log_layout,
    log_threshold,
  ],
  utils[tail],
)

#' Set up logging
#'
#' Configures the logger layout and threshold based on the config.yml values.
#' Sets up the "__gutter" namespace used to hide filtered log messages.
#'
#' @export
setup_logging <- function() {
  logger_layout <- layout_glue_generator(
    format = "{level} [{format(time, \"%Y-%m-%d %H:%M:%S\")}] {ns}: {msg}"
  )
  log_layout(logger_layout)
  log_threshold(config$get("rhino_log_level"))

  # "__gutter" namespace swallows log messages filtered out by SNT_DEBUG

  log_threshold("ERROR", namespace = "__gutter")
}

#' Build Logging Namespace
#'
#' Builds a logging namespace from function arguments and the function name. If
#' the args list contains a 'log_ns' element, it will be used as the base
#' namespace and concatenated with the function name. Otherwise, only the
#' function name is used as the namespace.
#'
#' @param args A named list of function arguments
#' @param fn_name Character string or vector representing the function name. If
#'   a vector (e.g., from `as.character(match.call()[[1]])` when called via `$`
#'   notation), the last element is used as the function name.
#'
#' @return A character string representing the logging namespace
#'
#' @export
log_ns_builder <- function(args, fn_name) {
  stopifnot("args must be a list" = qtest(args, "L"))
  # Handle character vectors from as.character(match.call()[[1]]) when called
  # via $ notation (e.g., module$func returns c("$", "module", "func"))
  if (length(fn_name) > 1) {
    fn_name <- tail(fn_name, 1)
  }
  stopifnot("fn_name must be a string" = qtest(fn_name, "S1") && fn_name != "")

  log_ns_concat(
    if ("log_ns" %in% names(args)) {
      args[["log_ns"]]
    } else {
      NULL
    },
    fn_name
  )
}

#' Concatenate Logging Namespace Components
#'
#' Concatenates logging namespace components with colons as separators. If the
#' old namespace is NULL, only the new component is returned. Otherwise, the
#' components are joined with colons.
#'
#' @param old Character string or NULL representing the existing namespace
#' @param new Character string representing the new namespace component
#'
#' @return A character string representing the concatenated namespace
#'
#' @examplesIf interactive()
#' log_ns_concat(NULL, "my_function")
#' # Returns: "my_function"
#'
#' log_ns_concat("module", "my_function")
#' # Returns: "module:my_function"
#'
#' log_ns_concat("app:module", "my_function")
#' # Returns: "app:module:my_function"
log_ns_concat <- function(old = NULL, new) {
  stopifnot("new must be a string" = qtest(new, "S1") && new != "")
  if (!is.null(old)) {
    stopifnot("old must be a string or NULL" = qtest(old, "S1") || is.null(old))
  }

  new_ns <- if (is.null(old)) {
    new
  } else {
    paste(c(old, new), collapse = ":")
  }
  new_ns
}

#' Filter Logging Namespace Based on Debug Pattern
#'
#' Filters logging namespaces based on the SNT_DEBUG environment variable. If
#' the namespace matches the SNT_DEBUG regexp pattern (set as an environment
#' variable), the namespace is returned. Otherwise, "__gutter" is returned,
#' which is a special namespace name that is ignored by logging systems.
#'
#' @param ns Character string representing the logging namespace to filter
#'
#' @return A character string - either the original namespace if it matches the
#'   pattern, or "__gutter" if it doesn't
#'
#' @examplesIf interactive()
#' # If SNT_DEBUG is set to "module"
#' log_ns_fn("module:my_function")
#' # Returns: "module:my_function"
#'
#' log_ns_fn("other:module:my_function")
#' # Returns: "__gutter"
#'
#' @export
log_ns_fn <- function(ns) {
  stopifnot("ns must be a string" = qtest(ns, "S1"))

  pattern <- Sys.getenv("SNT_DEBUG", unset = NA)
  if (is.na(pattern)) {
    pattern <- ""
  }
  if (grepl(pattern = pattern, x = ns)) {
    ns
  } else {
    "__gutter"
  }
}
