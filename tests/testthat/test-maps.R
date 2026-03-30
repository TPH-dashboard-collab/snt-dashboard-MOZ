box::use(
  data.table[data.table],
  sf[st_as_sf, st_bbox, st_sfc, st_polygon],
  testthat[expect_error, expect_s3_class, test_that],
)

# fmt: skip
box::use(
  app/logic/visualization/maps[add_intervention_map, add_impact_map],
)

# Helper: create minimal sf data for map tests
make_sf_data <- function() {
  # Create two simple square polygons
  poly1 <- st_polygon(list(matrix(
    c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
    ncol = 2,
    byrow = TRUE
  )))
  poly2 <- st_polygon(list(matrix(
    c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0),
    ncol = 2,
    byrow = TRUE
  )))
  geom <- st_sfc(poly1, poly2, crs = 4326)
  dt <- data.table(
    admin_2 = c("District A", "District B"),
    intervention_combo = c("ITN + IRS", "ITN"),
    impact_value = c(100, 200)
  )
  st_as_sf(dt, geometry = geom)
}

make_bounds <- function() {
  list(xmin = 0, xmax = 2, ymin = 0, ymax = 1)
}

# --- add_intervention_map tests ---

test_that("add_intervention_map returns a leaflet object", {
  sf_data <- make_sf_data()
  pal <- function(x) ifelse(x == "ITN + IRS", "blue", "red")
  result <- add_intervention_map(sf_data, pal, make_bounds())
  expect_s3_class(result, "leaflet")
})

test_that("add_intervention_map works with intervention_col", {
  sf_data <- make_sf_data()
  sf_data$deployed <- c(TRUE, FALSE)
  pal <- function(x) ifelse(x, "cornflowerblue", "lightgrey")
  result <- add_intervention_map(
    sf_data,
    pal,
    make_bounds(),
    intervention_col = "deployed"
  )
  expect_s3_class(result, "leaflet")
})

test_that("add_intervention_map rejects invalid inputs", {
  sf_data <- make_sf_data()
  pal <- function(x) "blue"

  # NULL data
  expect_error(add_intervention_map(NULL, pal, make_bounds()))
  # Non-function color_pal
  expect_error(add_intervention_map(sf_data, "blue", make_bounds()))
  # Missing map_bounds keys
  expect_error(add_intervention_map(sf_data, pal, list(xmin = 0)))
  # Invalid intervention_col
  expect_error(
    add_intervention_map(
      sf_data,
      pal,
      make_bounds(),
      intervention_col = "nonexistent"
    )
  )
})

# --- add_impact_map tests ---

test_that("add_impact_map returns a leaflet object", {
  sf_data <- make_sf_data()
  pal <- function(x) "blue"
  result <- add_impact_map(sf_data, pal, make_bounds(), "impact_value")
  expect_s3_class(result, "leaflet")
})

test_that("add_impact_map rejects invalid inputs", {
  sf_data <- make_sf_data()
  pal <- function(x) "blue"

  # Missing col in data
  expect_error(add_impact_map(sf_data, pal, make_bounds(), "nonexistent"))
  # Non-function color_pal
  expect_error(add_impact_map(sf_data, "blue", make_bounds(), "impact_value"))
  # NULL data
  expect_error(add_impact_map(NULL, pal, make_bounds(), "impact_value"))
})
