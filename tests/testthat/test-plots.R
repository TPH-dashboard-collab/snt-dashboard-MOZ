box::use(
  data.table[data.table],
  ggplot2[aes, geom_point, ggplot],
  testthat[
    expect_equal,
    expect_error,
    expect_gt,
    expect_s3_class,
    expect_true,
    expect_type,
    test_that,
  ],
)

# fmt: skip
box::use(
  app/logic/visualization/plotly_custom[build_interactive_plot],
  app/logic/visualization/plots[
    costs_averted_plot,
    costs_plot,
    yearlyMetricReduction_plot,
  ],
)

test_that("build_interactive_plot returns a plotly object", {
  g <- ggplot(mtcars, aes(x = mpg, y = wt)) +
    geom_point()

  p <- build_interactive_plot(g, data = mtcars)
  expect_s3_class(p, "plotly")
})

test_that("layout meta includes csv_name and full_data", {
  g <- ggplot(mtcars, aes(x = mpg, y = wt)) +
    geom_point()

  p <- build_interactive_plot(g, data = mtcars, csv_name = "custom.csv")
  meta <- p$x$layout$meta

  expect_type(meta, "list")
  expect_equal(meta$csv_name, "custom.csv")
  expect_true(is.data.frame(meta$full_data))
})

test_that("CSV is non-empty file", {
  # Create a plot object
  g <- ggplot(mtcars, aes(x = mpg, y = wt)) +
    geom_point()
  p <- build_interactive_plot(g, data = mtcars)

  # Write the metadata to a temporary CSV
  meta <- p$x$layout$meta
  tmp_csv <- tempfile(fileext = ".csv")
  write.csv(meta$full_data, tmp_csv, row.names = FALSE)

  # Check that the file exists and is non-empty
  expect_true(file.exists(tmp_csv))
  expect_gt(file.info(tmp_csv)$size, 0)
})

test_that("CSV has expected dataframe", {
  # Create a plot object
  g <- ggplot(mtcars, aes(x = mpg, y = wt)) +
    geom_point()
  p <- build_interactive_plot(g, data = mtcars)

  # Write the metadata to a temporary CSV
  meta <- p$x$layout$meta
  tmp_csv <- tempfile(fileext = ".csv")
  write.csv(meta$full_data, tmp_csv, row.names = FALSE)

  # Read CSV as a dataframe
  csv_data <- as.data.frame(read.csv(tmp_csv))

  # Rownames are removed because read.csv
  # authomatically creates character row names
  rownames(csv_data) <- NULL
  rownames(mtcars) <- NULL

  # Compare exact values
  expect_equal(csv_data, mtcars)
})

# --- yearlyMetricReduction_plot tests ---

test_that("yearlyMetricReduction_plot returns a plotly object", {
  dt <- data.table(
    year = rep(c(2025, 2026), each = 2),
    scenario_name = rep(c("BAU", "NSP"), 2),
    relative_reduction_mean = c(0, 0.3, 0, 0.4),
    relative_reduction_lci = c(0, 0.2, 0, 0.3),
    relative_reduction_uci = c(0, 0.4, 0, 0.5)
  )
  mapping <- c("BAU" = "Reference", "NSP" = "New Strategy")
  result <- yearlyMetricReduction_plot(dt, "BAU", mapping, opts = list())
  expect_s3_class(result, "plotly")
})

test_that("yearlyMetricReduction_plot rejects invalid inputs", {
  expect_error(yearlyMetricReduction_plot(NULL, "BAU", c(a = "b"), list()))
  dt <- data.table(
    year = 2026,
    scenario_name = "BAU",
    relative_reduction_mean = 0,
    relative_reduction_lci = 0,
    relative_reduction_uci = 0
  )
  expect_error(yearlyMetricReduction_plot(dt, 123, c(a = "b"), list()))
})

# --- costs_plot tests ---

test_that("costs_plot returns a plotly object", {
  dt <- data.table(
    scenario_name = c("NSP1", "NSP2"),
    pretty_name = c("Strategy 1", "Strategy 2"),
    total_cost_mean = c(5000000, 8000000),
    total_cost_lci = c(4000000, 7000000),
    total_cost_uci = c(6000000, 9000000)
  )
  result <- costs_plot(dt)
  expect_s3_class(result, "plotly")
})

test_that("costs_plot rejects empty data", {
  expect_error(costs_plot(NULL))
})

# --- costs_averted_plot tests ---

test_that("costs_averted_plot returns a plotly object", {
  dt <- data.table(
    scenario_name = c("NSP1", "NSP2"),
    pretty_name = c("Strategy 1", "Strategy 2"),
    cost_per_averted_mean = c(25, 40),
    cost_per_averted_lci = c(20, 35),
    cost_per_averted_uci = c(30, 45)
  )
  result <- costs_averted_plot(dt)
  expect_s3_class(result, "plotly")
})

test_that("costs_averted_plot rejects empty data", {
  expect_error(costs_averted_plot(NULL))
})
