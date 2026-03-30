box::use(
  data.table[data.table],
  testthat[expect_equal, expect_error, expect_true, test_that],
)

# fmt: skip
box::use(
  app/logic/core/utils[
    aggregate_impact,
    aggregate_metric,
    calculate_relative_reduction,
    nvec_get_name,
    nvec_get_val,
  ],
)

test_that("nvec_get_val works", {
  # Test with a simple named vector
  named_vec <- c(a = 1, b = 2, c = 3)

  # Test getting existing values
  expect_equal(nvec_get_val(named_vec, "a"), 1)
  expect_equal(nvec_get_val(named_vec, "b"), 2)
  expect_equal(nvec_get_val(named_vec, "c"), 3)

  # Test getting non-existent name (should error)
  expect_error(nvec_get_val(named_vec, "d"))

  # Test with character vector
  char_vec <- c(first = "hello", second = "world")
  expect_equal(nvec_get_val(char_vec, "first"), "hello")
  expect_equal(nvec_get_val(char_vec, "second"), "world")

  # Test with mixed types
  mixed_vec <- c(num = 42, char = "test", logical = TRUE)
  expect_equal(nvec_get_val(mixed_vec, "num"), "42")
  expect_equal(nvec_get_val(mixed_vec, "char"), "test")
  expect_equal(nvec_get_val(mixed_vec, "logical"), "TRUE")
})

test_that("nvec_get_name works", {
  # Test with a simple named vector
  named_vec <- c(a = 1, b = 2, c = 3)

  # Test getting names by values
  expect_equal(nvec_get_name(named_vec, 1), "a")
  expect_equal(nvec_get_name(named_vec, 2), "b")
  expect_equal(nvec_get_name(named_vec, 3), "c")

  # Test with character vector
  char_vec <- c(first = "hello", second = "world")
  expect_equal(nvec_get_name(char_vec, "hello"), "first")
  expect_equal(nvec_get_name(char_vec, "world"), "second")

  # Test with duplicate values (should return first match)
  dup_vec <- c(a = 1, b = 2, c = 1)
  expect_equal(nvec_get_name(dup_vec, 1), "a")

  # Test with logical values
  log_vec <- c(x = TRUE, y = FALSE)
  expect_equal(nvec_get_name(log_vec, TRUE), "x")
  expect_equal(nvec_get_name(log_vec, FALSE), "y")
})

# --- aggregate_metric tests ---

test_that("aggregate_metric returns correct structure", {
  dt <- data.table(
    year = rep(2026, 6),
    scenario_name = rep(c("BAU", "NSP"), each = 3),
    seed = rep(1, 6),
    EIR_CI = rep(c("EIR_lci", "EIR_mean", "EIR_uci"), 2),
    nHost = rep(1000, 6),
    incidenceRate = c(0.1, 0.2, 0.3, 0.05, 0.15, 0.25)
  )
  result <- aggregate_metric(dt, "incidenceRate")

  expect_true("metric_mean" %in% names(result))
  expect_true("metric_lci" %in% names(result))
  expect_true("metric_uci" %in% names(result))
  expect_equal(nrow(result), 2) # two scenarios
})

test_that("aggregate_metric errors on missing metric column", {
  dt <- data.table(
    year = 2026,
    scenario_name = "BAU",
    seed = 1,
    EIR_CI = "EIR_mean",
    nHost = 1000
  )
  expect_error(aggregate_metric(dt, "nonexistent"), "not found")
})

test_that("aggregate_metric rejects invalid inputs", {
  expect_error(aggregate_metric(NULL, "x"))
  dt <- data.table(
    year = 2026,
    scenario_name = "BAU",
    seed = 1,
    EIR_CI = "EIR_mean",
    nHost = 1000,
    x = 1
  )
  expect_error(aggregate_metric(dt, 123))
})

# --- calculate_relative_reduction tests ---

test_that("calculate_relative_reduction returns correct columns", {
  dt <- data.table(
    year = rep(2026, 2),
    scenario_name = c("BAU", "NSP"),
    metric_lci = c(0.08, 0.04),
    metric_mean = c(0.10, 0.05),
    metric_uci = c(0.12, 0.06)
  )
  result <- calculate_relative_reduction(dt, "BAU")

  expect_true("relative_reduction_mean" %in% names(result))
  expect_true("relative_reduction_lci" %in% names(result))
  expect_true("relative_reduction_uci" %in% names(result))
})

test_that("calculate_relative_reduction: reference has zero reduction", {
  dt <- data.table(
    year = rep(2026, 2),
    scenario_name = c("BAU", "NSP"),
    metric_lci = c(0.08, 0.04),
    metric_mean = c(0.10, 0.05),
    metric_uci = c(0.12, 0.06)
  )
  result <- calculate_relative_reduction(dt, "BAU")
  bau_row <- result[result$scenario_name == "BAU", ]
  expect_equal(bau_row$relative_reduction_mean, 0)
})

test_that("calculate_relative_reduction rejects missing reference", {
  dt <- data.table(
    year = 2026,
    scenario_name = "NSP",
    metric_lci = 0.04,
    metric_mean = 0.05,
    metric_uci = 0.06
  )
  expect_error(calculate_relative_reduction(dt, "BAU"))
})

# --- aggregate_impact tests ---

test_that("aggregate_impact returns correct structure for single year", {
  dt <- data.table(
    year = rep(2026, 6),
    scenario_name = rep(c("BAU", "NSP"), each = 3),
    admin_1 = "Region1",
    admin_2 = "District1",
    seed = 1,
    EIR_CI = rep(c("EIR_lci", "EIR_mean", "EIR_uci"), 2),
    cum_nUncomp = c(100, 200, 300, 50, 150, 250),
    cum_nSevere = c(10, 20, 30, 5, 15, 25),
    cum_tUncomp = c(80, 160, 240, 40, 120, 200),
    cum_tSevere = c(8, 16, 24, 4, 12, 20),
    cum_expectedDirectDeaths = c(1, 2, 3, 0.5, 1.5, 2.5)
  )
  result <- aggregate_impact(dt, year_start = 2026, year_end = 2026)

  expect_true("scenario_name" %in% names(result))
  expect_true("cum_nUncomp_mean" %in% names(result))
  expect_equal(nrow(result), 2)
})

test_that("aggregate_impact validates year range", {
  dt <- data.table(
    year = 2026,
    scenario_name = "BAU",
    admin_1 = "R1",
    admin_2 = "D1",
    seed = 1,
    EIR_CI = "EIR_mean",
    cum_nUncomp = 100,
    cum_nSevere = 10,
    cum_tUncomp = 80,
    cum_tSevere = 8,
    cum_expectedDirectDeaths = 1
  )
  expect_error(aggregate_impact(dt, year_start = 2030, year_end = 2030))
})

test_that("aggregate_impact rejects invalid inputs", {
  expect_error(aggregate_impact(NULL))
})
