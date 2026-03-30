box::use(
  data.table[data.table],
  testthat[
    expect_equal,
    expect_error,
    expect_s3_class,
    test_that
  ],
)

# fmt: skip
box::use(
  app/logic/interventions/sort_interventions[sort_interventions],
)

# Tests for basic vector sorting -----------------------------------------------

test_that("sort_interventions sorts vector by component count", {
  interventions <- c("ITN", "ITN + IRS", "ITN + IRS + SMC")

  result <- sort_interventions(interventions)

  expect_equal(length(result), 3)
  expect_equal(result[1], "ITN")
  expect_equal(result[2], "ITN + IRS")
  expect_equal(result[3], "ITN + IRS + SMC")
})

test_that("sort_interventions reverses sorting when reverse = TRUE", {
  interventions <- c("ITN", "ITN + IRS", "ITN + IRS + SMC")

  result <- sort_interventions(interventions, reverse = TRUE)

  expect_equal(result[1], "ITN + IRS + SMC")
  expect_equal(result[2], "ITN + IRS")
  expect_equal(result[3], "ITN")
})

test_that("sort_interventions sorts alphabetically when alphabetical = TRUE", {
  interventions <- c("IRS + ITN", "SMC + ITN", "ITN + SMC")

  result <- sort_interventions(interventions, alphabetical = TRUE)

  # Should sort by component count first, then alphabetically
  expect_equal(result[1], "IRS + ITN")
  expect_equal(result[2], "ITN + SMC")
  expect_equal(result[3], "SMC + ITN")
})

test_that("sort_interventions sorts components within strings", {
  interventions <- c("IRS + ITN", "SMC + ITN + IRS", "ITN")

  result <- sort_interventions(interventions, sort_components = TRUE)

  expect_equal(result[1], "ITN")
  expect_equal(result[2], "IRS + ITN")
  expect_equal(result[3], "IRS + ITN + SMC")
})

test_that("sort_interventions handles special case", {
  interventions <- c("ITN + IRS", "No intervention", "ITN")

  result <- sort_interventions(interventions)

  expect_equal(result[1], "No intervention")
  expect_equal(result[2], "ITN")
  expect_equal(result[3], "ITN + IRS")
})

test_that("sort_interventions preserves order for same component count", {
  interventions <- c("ITN + IRS", "SMC + ITN", "IRS + SMC")

  result <- sort_interventions(interventions)

  # Should preserve original order for same component count
  expect_equal(result, interventions)
})

# Tests for data.table sorting -------------------------------------------------

test_that("sort_interventions sorts data.table by intervention column", {
  dt <- data.table(
    id = 1:3,
    interventions = c("ITN + IRS + SMC", "ITN", "ITN + IRS"),
    value = c(10, 20, 30)
  )

  result <- sort_interventions(dt, interventions = "interventions")

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 3)
  expect_equal(result$interventions[1], "ITN")
  expect_equal(result$interventions[2], "ITN + IRS")
  expect_equal(result$interventions[3], "ITN + IRS + SMC")

  # Other columns should maintain their associations
  expect_equal(result$id[1], 2)
  expect_equal(result$value[1], 20)
})

test_that("sort_interventions sorts data.table with reverse = TRUE", {
  dt <- data.table(
    id = 1:3,
    interventions = c("ITN", "ITN + IRS", "ITN + IRS + SMC")
  )

  result <- sort_interventions(
    dt,
    interventions = "interventions",
    reverse = TRUE
  )

  expect_equal(result$interventions[1], "ITN + IRS + SMC")
  expect_equal(result$interventions[2], "ITN + IRS")
  expect_equal(result$interventions[3], "ITN")
})

test_that("sort_interventions sorts data.table alphabetically", {
  dt <- data.table(
    id = 1:3,
    interventions = c("IRS + ITN", "SMC + ITN", "ITN + SMC")
  )

  result <- sort_interventions(
    dt,
    interventions = "interventions",
    alphabetical = TRUE
  )

  expect_equal(result$interventions[1], "IRS + ITN")
  expect_equal(result$interventions[2], "ITN + SMC")
  expect_equal(result$interventions[3], "SMC + ITN")
})

test_that("sort_interventions modifies data.table in place", {
  dt <- data.table(
    id = 1:3,
    interventions = c("ITN + IRS + SMC", "ITN", "ITN + IRS")
  )

  original_address <- data.table::address(dt)
  result <- sort_interventions(dt, interventions = "interventions")

  # Should modify in place
  expect_equal(data.table::address(result), original_address)
})

# Tests for edge cases ---------------------------------------------------------

test_that("sort_interventions handles empty components", {
  interventions <- c("ITN", "ITN + ", "ITN + IRS")

  result <- sort_interventions(interventions)

  # Empty components should be counted
  expect_equal(length(result), 3)
})

test_that("sort_interventions handles single element vector", {
  interventions <- c("ITN + IRS")

  result <- sort_interventions(interventions)

  expect_equal(length(result), 1)
  expect_equal(result[1], "ITN + IRS")
})

test_that("sort_interventions handles all same component count", {
  interventions <- c("ITN + IRS", "IRS + SMC", "SMC + ITN")

  result <- sort_interventions(interventions)

  # Should preserve original order
  expect_equal(result, interventions)
})

test_that("sort_interventions handles special case only", {
  interventions <- c("No intervention", "No intervention")

  result <- sort_interventions(interventions)

  expect_equal(result, c("No intervention", "No intervention"))
})

test_that("sort_interventions handles data.frame input", {
  df <- data.frame(
    id = 1:3,
    interventions = c("ITN + IRS + SMC", "ITN", "ITN + IRS")
  )

  result <- sort_interventions(df, interventions = "interventions")

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 3)
  expect_equal(result$interventions[1], "ITN")
})

# Parameter validation tests ---------------------------------------------------

test_that("sort_interventions validates data parameter", {
  expect_error(
    sort_interventions(NULL),
    "data must be a non-empty data frame"
  )

  expect_error(
    sort_interventions(data.frame()),
    "data must be a non-empty data frame"
  )
})

test_that("sort_interventions validates interventions parameter", {
  dt <- data.table(interventions = c("ITN", "IRS"))

  expect_error(
    sort_interventions(dt, interventions = 123),
    "interventions must be a string or NULL"
  )

  expect_error(
    sort_interventions(dt, interventions = ""),
    "interventions must be a string or NULL"
  )
})

test_that("sort_interventions validates split_pattern parameter", {
  expect_error(
    sort_interventions(c("ITN + IRS"), split_pattern = 123),
    "split_pattern must be a string"
  )

  expect_error(
    sort_interventions(c("ITN + IRS"), split_pattern = ""),
    "split_pattern must be a string"
  )
})

test_that("sort_interventions validates combine_pattern parameter", {
  expect_error(
    sort_interventions(c("ITN + IRS"), combine_pattern = 123),
    "combine_pattern must be a string"
  )

  expect_error(
    sort_interventions(c("ITN + IRS"), combine_pattern = ""),
    "combine_pattern must be a string"
  )
})

test_that("sort_interventions validates special_case parameter", {
  expect_error(
    sort_interventions(c("ITN + IRS"), special_case = 123),
    "special_case must be a string"
  )

  expect_error(
    sort_interventions(c("ITN + IRS"), special_case = ""),
    "special_case must be a string"
  )
})

test_that("sort_interventions validates alphabetical parameter", {
  expect_error(
    sort_interventions(c("ITN + IRS"), alphabetical = "not boolean"),
    "alphabetical must be a boolean"
  )
})

test_that("sort_interventions validates reverse parameter", {
  expect_error(
    sort_interventions(c("ITN + IRS"), reverse = "not boolean"),
    "reverse must be a boolean"
  )
})

test_that("sort_interventions validates sort_components parameter", {
  expect_error(
    sort_interventions(c("ITN + IRS"), sort_components = "not boolean"),
    "sort_components must be a boolean"
  )
})

test_that("sort_interventions validates reverse_components parameter", {
  expect_error(
    sort_interventions(c("ITN + IRS"), reverse_components = "not boolean"),
    "reverse_components must be a boolean"
  )
})

test_that("sort_interventions errors when interventions column not found", {
  dt <- data.table(wrong_column = c("ITN", "IRS"))

  expect_error(
    sort_interventions(dt, interventions = "interventions"),
    "When providing a data.table, 'interventions' must specify valid column names"
  )
})

test_that("sort_interventions errors when interventions is NULL for data.table", {
  dt <- data.table(interventions = c("ITN", "IRS"))

  expect_error(
    sort_interventions(dt, interventions = NULL),
    "When providing a data.table, 'interventions' must specify valid column names"
  )
})

# Tests for complex scenarios --------------------------------------------------

test_that("sort_interventions handles mixed case scenarios", {
  interventions <- c(
    "No intervention",
    "ITN",
    "IRS + ITN",
    "SMC + ITN + IRS",
    "IRS"
  )

  result <- sort_interventions(interventions)

  expect_equal(result[1], "No intervention")
  expect_equal(result[2], "ITN")
  expect_equal(result[3], "IRS")
  expect_equal(result[4], "IRS + ITN")
  expect_equal(result[5], "SMC + ITN + IRS")
})

test_that("sort_interventions handles reverse alphabetical", {
  interventions <- c("IRS + ITN", "SMC + ITN", "ITN + SMC")

  result <- sort_interventions(
    interventions,
    alphabetical = TRUE,
    reverse = TRUE
  )

  expect_equal(result[1], "IRS + ITN")
  expect_equal(result[2], "ITN + SMC")
  expect_equal(result[3], "SMC + ITN")
})

test_that("sort_interventions handles all parameters together", {
  dt <- data.table(
    id = 1:5,
    interventions = c(
      "No intervention",
      "IRS + ITN",
      "SMC",
      "SMC + ITN + IRS",
      "ITN"
    ),
    value = 1:5
  )

  result <- sort_interventions(
    dt,
    interventions = "interventions",
    sort_components = TRUE,
    alphabetical = TRUE,
    reverse = FALSE
  )

  expect_equal(result$interventions[1], "No intervention")
  expect_equal(result$interventions[2], "ITN")
  expect_equal(result$interventions[3], "SMC")
  expect_equal(result$interventions[4], "IRS + ITN")
  expect_equal(result$interventions[5], "IRS + ITN + SMC")
})

test_that("sort_interventions handles large dataset", {
  interventions <- rep(c("ITN", "ITN + IRS", "ITN + IRS + SMC"), 1000)

  result <- sort_interventions(interventions)

  expect_equal(length(result), 3000)
  expect_equal(result[1:1000], rep("ITN", 1000))
  expect_equal(result[1001:2000], rep("ITN + IRS", 1000))
  expect_equal(result[2001:3000], rep("ITN + IRS + SMC", 1000))
})

test_that("sort_interventions handles complex component names", {
  interventions <- c(
    "ITN Long Name",
    "IRS + ITN Long Name + Very Long SMC Name",
    "IRS"
  )

  result <- sort_interventions(interventions)

  expect_equal(result[1], "ITN Long Name")
  expect_equal(result[2], "IRS")
  expect_equal(result[3], "IRS + ITN Long Name + Very Long SMC Name")
})
