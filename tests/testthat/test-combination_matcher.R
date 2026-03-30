box::use(
  app/logic/interventions/combination_matcher,
  data.table[data.table, setDT],
  testthat[...],
)

test_that("calculate_hamming_distance works correctly", {
  target <- c(TRUE, FALSE, TRUE, FALSE)
  candidate <- c(TRUE, TRUE, TRUE, FALSE)

  distance <- combination_matcher$calculate_hamming_distance(target, candidate)

  expect_equal(distance, 1) # Second position differs
})

test_that("calculate_hamming_distance returns 0 for exact match", {
  target <- c(TRUE, FALSE, TRUE)
  candidate <- c(TRUE, FALSE, TRUE)

  distance <- combination_matcher$calculate_hamming_distance(target, candidate)

  expect_equal(distance, 0)
})

test_that("calculate_hamming_distance counts all differences", {
  target <- c(TRUE, TRUE, TRUE, TRUE)
  candidate <- c(FALSE, FALSE, FALSE, FALSE)

  distance <- combination_matcher$calculate_hamming_distance(target, candidate)

  expect_equal(distance, 4)
})

test_that("create_intervention_list creates correct boolean vector", {
  all_cols <- c(
    "deployed_int_CM",
    "deployed_int_IRS",
    "deployed_int_STD_Nets"
  )
  selected <- c("deployed_int_CM", "deployed_int_STD_Nets")

  result <- combination_matcher$create_intervention_list(selected, all_cols)

  expect_type(result, "list")
  expect_equal(names(result), all_cols)
  expect_true(result$deployed_int_CM)
  expect_false(result$deployed_int_IRS)
  expect_true(result$deployed_int_STD_Nets)
})

test_that("create_intervention_list handles empty selection", {
  all_cols <- c("deployed_int_CM", "deployed_int_IRS")
  selected <- character(0)

  result <- combination_matcher$create_intervention_list(selected, all_cols)

  expect_type(result, "list")
  expect_equal(names(result), all_cols)
  expect_false(result$deployed_int_CM)
  expect_false(result$deployed_int_IRS)
})

test_that("create_intervention_list handles all selected", {
  all_cols <- c("deployed_int_CM", "deployed_int_IRS")
  selected <- all_cols

  result <- combination_matcher$create_intervention_list(selected, all_cols)

  expect_true(result$deployed_int_CM)
  expect_true(result$deployed_int_IRS)
})

test_that("find_best_match returns exact match when available", {
  # Create mock data pool
  int_data_pool <- data.table(
    admin_2 = c("District_A", "District_A", "District_A"),
    scenario_name = c("scen_1", "scen_2", "scen_3"),
    deployed_int_CM = c(TRUE, TRUE, FALSE),
    deployed_int_IRS = c(FALSE, TRUE, FALSE),
    deployed_int_STD_Nets = c(TRUE, FALSE, TRUE)
  )

  target <- list(
    deployed_int_CM = TRUE,
    deployed_int_IRS = FALSE,
    deployed_int_STD_Nets = TRUE
  )

  result <- combination_matcher$find_best_match(
    district_name = "District_A",
    target_interventions = target,
    int_data_pool_dt = int_data_pool
  )

  expect_equal(result$distance, 0)
  expect_true(result$exact_match)
  expect_equal(result$matched_data$scenario_name, "scen_1")
})

test_that("find_best_match returns closest match when no exact match", {
  # Create mock data pool without exact match
  int_data_pool <- data.table(
    admin_2 = c("District_A", "District_A", "District_A"),
    scenario_name = c("scen_1", "scen_2", "scen_3"),
    deployed_int_CM = c(TRUE, TRUE, FALSE),
    deployed_int_IRS = c(TRUE, TRUE, FALSE),
    deployed_int_STD_Nets = c(TRUE, FALSE, TRUE)
  )

  target <- list(
    deployed_int_CM = TRUE,
    deployed_int_IRS = FALSE,
    deployed_int_STD_Nets = TRUE
  )

  result <- combination_matcher$find_best_match(
    district_name = "District_A",
    target_interventions = target,
    int_data_pool_dt = int_data_pool
  )

  expect_gt(result$distance, 0)
  expect_false(result$exact_match)
  expect_equal(result$distance, 1) # Closest is scen_1 with 1 difference
})

test_that("find_best_match uses priority tiebreaker", {
  # Create mock data pool with two equidistant matches
  int_data_pool <- data.table(
    admin_2 = c("District_A", "District_A"),
    scenario_name = c("scen_1", "scen_2"),
    deployed_int_CM = c(TRUE, FALSE),
    deployed_int_IRS = c(FALSE, TRUE),
    deployed_int_STD_Nets = c(FALSE, FALSE)
  )

  target <- list(
    deployed_int_CM = FALSE,
    deployed_int_IRS = FALSE,
    deployed_int_STD_Nets = FALSE
  )

  priorities <- c(
    deployed_int_CM = 10,
    deployed_int_IRS = 5,
    deployed_int_STD_Nets = 5
  )

  result <- combination_matcher$find_best_match(
    district_name = "District_A",
    target_interventions = target,
    int_data_pool_dt = int_data_pool,
    priority_weights = priorities
  )

  # Both have distance 1, but scen_1 has CM (priority 10) vs scen_2 IRS (priority 5)
  # Should select scen_1
  expect_equal(result$matched_data$scenario_name, "scen_1")
})

test_that("find_best_match handles empty data pool", {
  int_data_pool <- data.table(
    admin_2 = character(0),
    scenario_name = character(0),
    deployed_int_CM = logical(0)
  )

  target <- list(deployed_int_CM = TRUE)

  result <- combination_matcher$find_best_match(
    district_name = "District_A",
    target_interventions = target,
    int_data_pool_dt = int_data_pool
  )

  expect_equal(result$distance, Inf)
  expect_false(result$exact_match)
  expect_equal(nrow(result$matched_data), 0)
})

test_that("find_best_match filters to correct district", {
  # Create data pool with multiple districts
  int_data_pool <- data.table(
    admin_2 = c("District_A", "District_B", "District_A"),
    scenario_name = c("scen_1", "scen_2", "scen_3"),
    deployed_int_CM = c(TRUE, TRUE, FALSE),
    deployed_int_IRS = c(FALSE, FALSE, TRUE)
  )

  target <- list(
    deployed_int_CM = TRUE,
    deployed_int_IRS = FALSE
  )

  result <- combination_matcher$find_best_match(
    district_name = "District_A",
    target_interventions = target,
    int_data_pool_dt = int_data_pool
  )

  # Should match District_A scen_1 (exact match)
  expect_equal(result$distance, 0)
  expect_equal(result$matched_data$admin_2, "District_A")
  expect_equal(result$matched_data$scenario_name, "scen_1")
})

test_that("find_best_match returns match details", {
  int_data_pool <- data.table(
    admin_2 = c("District_A"),
    scenario_name = c("scen_1"),
    deployed_int_CM = c(TRUE),
    deployed_int_IRS = c(FALSE)
  )

  target <- list(
    deployed_int_CM = TRUE,
    deployed_int_IRS = FALSE
  )

  result <- combination_matcher$find_best_match(
    district_name = "District_A",
    target_interventions = target,
    int_data_pool_dt = int_data_pool
  )

  expect_true("matched_data" %in% names(result))
  expect_true("distance" %in% names(result))
  expect_true("exact_match" %in% names(result))
  expect_true("original_target" %in% names(result))
  expect_true("matched_interventions" %in% names(result))

  expect_equal(result$original_target, target)
  expect_type(result$matched_interventions, "list")
})
