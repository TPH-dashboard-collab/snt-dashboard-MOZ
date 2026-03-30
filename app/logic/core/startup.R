box::use(
  config,
  data.table[setDT],
  logger[log_debug, log_error],
  rmapshaper[ms_simplify],
  sf[st_read, st_transform],
)

# fmt: skip
box::use(
  app/logic/core/logging[log_ns_fn],
  app/logic/data/data_connection[make_data_connection],
)

#' Create a data connection from config
#'
#' Reads `db_type` from config and constructs the appropriate connection.
#' Supports SQLite and Supabase (when available on feat-rest-api).
#'
#' @param log_ns Character. Logging namespace.
#' @return A DataConnection R6 object.
#' @export
init_data_connection <- function(log_ns = "startup") {
  data_con_type <- config$get("db_type")

  data_con_args <- switch(
    data_con_type,
    SQLite = list(
      type = "SQLite",
      db_path = config$get("db_path"),
      log_ns = log_ns
    ),
    Supabase = list(
      type = "Supabase",
      log_ns = log_ns,
      view_prefix = config$get("view_prefix")
    ),
    stop("Unknown db_type: ", data_con_type)
  )

  log_debug(
    "Creating {data_con_type} data connection",
    namespace = log_ns_fn(log_ns)
  )

  do.call(what = make_data_connection, args = data_con_args)
}

#' Load and process the country shapefile from config
#'
#' Reads the shapefile, transforms to WGS84, simplifies geometry, and converts
#' to data.table. Country-specific column renames should be applied in main.R
#' after calling this function.
#'
#' @param log_ns Character. Logging namespace.
#' @return A data.table with the processed shapefile data.
#' @export
load_country_map <- function(log_ns = "startup") {
  shapefile_dir <- config$get("shapefile_dir")

  if (!file.exists(shapefile_dir)) {
    log_error(
      "Shapefile directory '{shapefile_dir}' not found.",
      namespace = log_ns_fn(log_ns)
    )
    stop("Shapefile directory not found.")
  }

  country_map <- tryCatch(
    {
      st_read(shapefile_dir) |>
        # NOTE 2024-10-07: Transform to WGS84. This is probably not always
        #   necessary.
        st_transform(4326)
    },
    error = function(e) {
      log_error(
        "Failed to read or transform shapefile: {e$message}",
        namespace = log_ns_fn(log_ns)
      )
      stop("Shapefile processing error.")
    }
  )

  # Simplify the geometry to reduce data size and improve rendering speed
  country_map <- ms_simplify(country_map)

  # Ensure the map is a data table
  setDT(country_map)

  log_debug("Shapefile processed and loaded.", namespace = log_ns_fn(log_ns))

  country_map
}
