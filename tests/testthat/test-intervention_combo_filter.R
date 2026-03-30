box::use(
  testthat[...],
  app/logic/interventions/intervention_combo_filter[
    generate_counterfactual_combos,
    build_combo_where_clause
  ],
  pool[dbPool, poolClose],
  RSQLite[SQLite],
)

test_that("generate_counterfactual_combos returns original + counterfactuals", {
  target <- c(
    deployed_int_CM = TRUE,
    deployed_int_ICCM = FALSE,
    deployed_int_IRS = TRUE,
    deployed_int_STD_Nets = TRUE,
    deployed_int_Vaccine = FALSE
  )

  result <- generate_counterfactual_combos(target)

  # Should have original + 2 counterfactuals (IRS and STD_Nets)
  expect_equal(length(result), 3)
  expect_true("original" %in% names(result))
  expect_true("deployed_int_IRS" %in% names(result))
  expect_true("deployed_int_STD_Nets" %in% names(result))

  # Original should match input
  expect_equal(result$original, target)

  # Counterfactuals should have one intervention switched off
  expect_equal(result$deployed_int_IRS["deployed_int_IRS"], c(deployed_int_IRS = FALSE))
  expect_equal(result$deployed_int_STD_Nets["deployed_int_STD_Nets"], c(deployed_int_STD_Nets = FALSE))
})

test_that("generate_counterfactual_combos excludes CM and iCCM", {
  target <- c(
    deployed_int_CM = TRUE,
    deployed_int_ICCM = TRUE,
    deployed_int_IRS = TRUE
  )

  result <- generate_counterfactual_combos(target)

  # Should have original + only IRS counterfactual (CM and iCCM excluded)
  expect_equal(length(result), 2)
  expect_true("original" %in% names(result))
  expect_true("deployed_int_IRS" %in% names(result))
  expect_false("deployed_int_CM" %in% names(result))
  expect_false("deployed_int_ICCM" %in% names(result))
})

test_that("generate_counterfactual_combos handles only base interventions", {
  target <- c(
    deployed_int_CM = TRUE,
    deployed_int_ICCM = TRUE
  )

  result <- generate_counterfactual_combos(target)

  # Should only have original (no counterfactuals possible)
  expect_equal(length(result), 1)
  expect_equal(names(result), "original")
})

test_that("generate_counterfactual_combos handles custom base interventions", {
  target <- c(
    deployed_int_CM = TRUE,
    deployed_int_IRS = TRUE,
    deployed_int_Vaccine = TRUE
  )

  result <- generate_counterfactual_combos(
    target,
    base_interventions = c("deployed_int_CM", "deployed_int_IRS")
  )

  # Should have original + only Vaccine counterfactual
  expect_equal(length(result), 2)
  expect_true("deployed_int_Vaccine" %in% names(result))
  expect_false("deployed_int_IRS" %in% names(result))
})

test_that("build_combo_where_clause generates valid SQL", {
  # Create a mock database connection
  con <- dbPool(SQLite(), dbname = ":memory:")
  on.exit(poolClose(con))

  combos <- list(
    original = c(deployed_int_CM = TRUE, deployed_int_IRS = TRUE),
    deployed_int_IRS = c(deployed_int_CM = TRUE, deployed_int_IRS = FALSE)
  )

  clause <- build_combo_where_clause(combos, con)

  # Check that clause contains expected structure
  expect_true(grepl("deployed_int_CM", clause))
  expect_true(grepl("deployed_int_IRS", clause))
  expect_true(grepl("AND", clause))
  expect_true(grepl("OR", clause))
  expect_true(grepl("=1", clause))
  expect_true(grepl("=0", clause))

  # Check parentheses are balanced
  expect_equal(
    sum(gregexpr("\\(", clause)[[1]] > 0),
    sum(gregexpr("\\)", clause)[[1]] > 0)
  )
})

test_that("build_combo_where_clause handles empty combos", {
  con <- dbPool(SQLite(), dbname = ":memory:")
  on.exit(poolClose(con))

  clause <- build_combo_where_clause(list(), con)

  expect_equal(clause, "")
})

test_that("build_combo_where_clause handles single combination", {
  con <- dbPool(SQLite(), dbname = ":memory:")
  on.exit(poolClose(con))

  combos <- list(
    original = c(deployed_int_CM = TRUE, deployed_int_IRS = FALSE)
  )

  clause <- build_combo_where_clause(combos, con)

  # Should have outer parens and one combination
  expect_true(startsWith(clause, "("))
  expect_true(endsWith(clause, ")"))
  expect_false(grepl("OR", clause)) # No OR if only one combo
})
