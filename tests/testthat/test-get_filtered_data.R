box::use(
  data.table[
    address,
    data.table
  ],
  testthat[
    expect_equal,
    expect_error,
    expect_s3_class,
    expect_true,
    expect_warning,
    test_that
  ],
)

# fmt: skip
box::use(
  app/logic/data/get_filtered_data[add_intervention_combo_column],
)

# Tests for add_intervention_combo_column --------------------------------------

test_that("add_intervention_combo_column adds combo column correctly", {
  dt <- data.table(
    deployed_int_ITN = c(TRUE, TRUE, FALSE),
    deployed_int_IRS = c(TRUE, FALSE, TRUE),
    deployed_int_SMC = c(FALSE, FALSE, FALSE),
    other_column = c(1, 2, 3)
  )

  result <- add_intervention_combo_column(dt)

  # Should add intervention_combo column
  expect_true("intervention_combo" %in% colnames(result))

  # Check that combinations are correct
  expect_equal(result$intervention_combo[1], "ITN + IRS")
  expect_equal(result$intervention_combo[2], "ITN")
  expect_equal(result$intervention_combo[3], "IRS")
})

test_that("add_intervention_combo_column handles no active interventions", {
  dt <- data.table(
    deployed_int_ITN = c(FALSE, FALSE),
    deployed_int_IRS = c(FALSE, FALSE),
    deployed_int_SMC = c(FALSE, FALSE)
  )

  result <- add_intervention_combo_column(dt)

  # Should return "No intervention" when all are FALSE
  expect_equal(result$intervention_combo[1], "No intervention")
  expect_equal(result$intervention_combo[2], "No intervention")
})

test_that("add_intervention_combo_column handles all interventions active", {
  dt <- data.table(
    deployed_int_ITN = c(TRUE),
    deployed_int_IRS = c(TRUE),
    deployed_int_SMC = c(TRUE),
    deployed_int_PBO = c(TRUE)
  )

  result <- add_intervention_combo_column(dt)

  # Should combine all interventions
  expect_equal(result$intervention_combo[1], "ITN + IRS + SMC + PBO")
})

test_that("add_intervention_combo_column modifies data.table in place", {
  dt <- data.table(
    deployed_int_ITN = c(TRUE, FALSE),
    deployed_int_IRS = c(FALSE, TRUE)
  )

  original_address <- address(dt)
  result <- add_intervention_combo_column(dt)

  # Should modify in place
  expect_equal(address(result), original_address)

  # Original dt should also have the new column
  expect_true("intervention_combo" %in% colnames(dt))
})

test_that("add_intervention_combo_column skips if column already exists", {
  dt <- data.table(
    deployed_int_ITN = c(TRUE, FALSE),
    deployed_int_IRS = c(FALSE, TRUE),
    intervention_combo = c("existing1", "existing2")
  )

  result <- add_intervention_combo_column(dt)

  # Should not overwrite existing column
  expect_equal(result$intervention_combo, c("existing1", "existing2"))
})

test_that("add_intervention_combo_column handles single intervention column", {
  dt <- data.table(
    deployed_int_ITN = c(TRUE, FALSE, TRUE),
    other_column = c(1, 2, 3)
  )

  result <- add_intervention_combo_column(dt)

  expect_equal(result$intervention_combo[1], "ITN")
  expect_equal(result$intervention_combo[2], "No intervention")
  expect_equal(result$intervention_combo[3], "ITN")
})

test_that("add_intervention_combo_column handles mixed intervention states", {
  dt <- data.table(
    deployed_int_ITN = c(TRUE, FALSE, TRUE, FALSE),
    deployed_int_IRS = c(FALSE, TRUE, TRUE, FALSE),
    deployed_int_SMC = c(FALSE, FALSE, TRUE, FALSE),
    admin = c("A", "B", "C", "D")
  )

  result <- add_intervention_combo_column(dt)

  expect_equal(result$intervention_combo[1], "ITN")
  expect_equal(result$intervention_combo[2], "IRS")
  expect_equal(result$intervention_combo[3], "ITN + IRS + SMC")
  expect_equal(result$intervention_combo[4], "No intervention")
})

test_that("add_intervention_combo_column handles data.frame input", {
  dt <- data.frame(
    deployed_int_ITN = c(TRUE, FALSE),
    deployed_int_IRS = c(FALSE, TRUE)
  )

  result <- add_intervention_combo_column(dt)

  # Should convert to data.table and add column
  expect_s3_class(result, "data.table")
  expect_true("intervention_combo" %in% colnames(result))
})

test_that("add_intervention_combo_column warns when no intervention columns found", {
  dt <- data.table(
    other_column1 = c(1, 2, 3),
    other_column2 = c("a", "b", "c")
  )

  expect_warning(
    result <- add_intervention_combo_column(dt),
    "No intervention columns found matching pattern"
  )

  # Should still add intervention_combo column with "No intervention"
  expect_true("intervention_combo" %in% colnames(result))
  expect_equal(result$intervention_combo[1], "No intervention")
})

test_that("add_intervention_combo_column errors on empty data.table", {
  dt <- data.table(
    deployed_int_ITN = logical(0),
    deployed_int_IRS = logical(0)
  )

  expect_error(
    add_intervention_combo_column(dt),
    "dt must be a non-empty data frame"
  )
})

test_that("add_intervention_combo_column errors on NULL input", {
  expect_error(
    add_intervention_combo_column(NULL),
    "dt must be a non-empty data frame"
  )
})

test_that("add_intervention_combo_column handles single row", {
  dt <- data.table(
    deployed_int_ITN = TRUE,
    deployed_int_IRS = FALSE,
    deployed_int_SMC = TRUE
  )

  result <- add_intervention_combo_column(dt)

  expect_equal(result$intervention_combo[1], "ITN + SMC")
})

test_that("add_intervention_combo_column handles many rows", {
  dt <- data.table(
    deployed_int_ITN = rep(c(TRUE, FALSE), 500),
    deployed_int_IRS = rep(c(FALSE, TRUE), 500),
    id = 1:1000
  )

  result <- add_intervention_combo_column(dt)

  expect_equal(nrow(result), 1000)
  expect_true("intervention_combo" %in% colnames(result))

  # Check pattern repeats correctly
  expect_equal(result$intervention_combo[1], "ITN")
  expect_equal(result$intervention_combo[2], "IRS")
  expect_equal(result$intervention_combo[999], "ITN")
  expect_equal(result$intervention_combo[1000], "IRS")
})

test_that("add_intervention_combo_column preserves other columns", {
  dt <- data.table(
    id = 1:3,
    deployed_int_ITN = c(TRUE, FALSE, TRUE),
    deployed_int_IRS = c(FALSE, TRUE, TRUE),
    value = c(10.5, 20.5, 30.5),
    name = c("A", "B", "C")
  )

  result <- add_intervention_combo_column(dt)

  # All original columns should still be there
  expect_true("id" %in% colnames(result))
  expect_true("value" %in% colnames(result))
  expect_true("name" %in% colnames(result))

  # Values should be unchanged
  expect_equal(result$id, c(1, 2, 3))
  expect_equal(result$value, c(10.5, 20.5, 30.5))
  expect_equal(result$name, c("A", "B", "C"))
})

test_that("add_intervention_combo_column handles intervention names with underscores", {
  dt <- data.table(
    deployed_int_ITN_PBO = c(TRUE, FALSE),
    deployed_int_IRS_NEW = c(FALSE, TRUE)
  )

  result <- add_intervention_combo_column(dt)

  # Should extract the part after "deployed_int_"
  expect_equal(result$intervention_combo[1], "ITN_PBO")
  expect_equal(result$intervention_combo[2], "IRS_NEW")
})

test_that("add_intervention_combo_column returns data.table", {
  dt <- data.table(
    deployed_int_ITN = c(TRUE, FALSE),
    deployed_int_IRS = c(FALSE, TRUE)
  )

  result <- add_intervention_combo_column(dt)

  expect_s3_class(result, "data.table")
})

test_that("add_intervention_combo_column preserves row order", {
  dt <- data.table(
    id = c(5, 3, 1, 4, 2),
    deployed_int_ITN = c(TRUE, FALSE, TRUE, FALSE, TRUE),
    deployed_int_IRS = c(FALSE, TRUE, FALSE, TRUE, FALSE)
  )

  result <- add_intervention_combo_column(dt)

  # Row order should be preserved
  expect_equal(result$id, c(5, 3, 1, 4, 2))
  expect_equal(result$intervention_combo[1], "ITN")
  expect_equal(result$intervention_combo[2], "IRS")
})
