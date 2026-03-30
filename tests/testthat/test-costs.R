box::use(
  data.table[data.table],
  testthat[
    expect_equal,
    expect_error,
    expect_false,
    expect_true,
    test_that
  ],
)

# fmt: skip
box::use(
  app/logic/analytics/costs[calculate_cost_effectiveness, calculate_intervention_costs],
)

test_that("calculate_cost_effectiveness calculates cost per case averted correctly", {
  # Create mock costs data
  costs_dt <- data.table(
    scenario_name = c("reference", "intervention1"),
    total_cost_lci = c(0, 9000),
    total_cost_mean = c(0, 10000),
    total_cost_uci = c(0, 11000)
  )

  # Create mock simulation data
  # Proper structure: 2 scenarios x 3 EIR_CI x 2 years x 2 seeds x 1 admin = 24 rows
  # Reference has more cases than intervention
  sim_dt <- data.table(
    scenario_name = rep(c("reference", "intervention1"), each = 12),
    admin_1 = rep("region1", 24),
    admin_2 = rep("district1", 24),
    EIR_CI = rep(rep(c("EIR_lci", "EIR_mean", "EIR_uci"), each = 4), 2),
    year = rep(rep(c(2025, 2026), each = 2), 6),
    seed = rep(1:2, 12),
    cum_nUncomp = c(
      # Reference - EIR_lci: 2025 (seed 1,2), 2026 (seed 1,2)
      1000,
      1000,
      2000,
      2000,
      # Reference - EIR_mean
      1000,
      1000,
      2000,
      2000,
      # Reference - EIR_uci
      1000,
      1000,
      2000,
      2000,
      # Intervention - EIR_lci
      800,
      800,
      1500,
      1500,
      # Intervention - EIR_mean
      800,
      800,
      1500,
      1500,
      # Intervention - EIR_uci
      800,
      800,
      1500,
      1500
    ),
    cum_nSevere = rep(100, 24),
    cum_tUncomp = rep(50, 24),
    cum_tSevere = rep(10, 24),
    cum_expectedDirectDeaths = rep(5, 24)
  )

  result <- calculate_cost_effectiveness(
    costs_dt = costs_dt,
    sim_dt = sim_dt,
    reference = "reference"
  )

  # Should have one row (intervention only, reference removed)
  expect_equal(nrow(result), 1)
  expect_equal(result$scenario_name, "intervention1")

  # Check that cost effectiveness columns exist
  expect_true("cost_per_averted_lci" %in% names(result))
  expect_true("cost_per_averted_mean" %in% names(result))
  expect_true("cost_per_averted_uci" %in% names(result))

  # Cases averted (year 2026): 2000 - 1500 = 500
  # Cost per case averted (mean): 10000 / 500 = 20
  expect_equal(result$cost_per_averted_mean, 20)
})

test_that("calculate_cost_effectiveness handles multiple interventions", {
  costs_dt <- data.table(
    scenario_name = c("reference", "intervention1", "intervention2"),
    total_cost_lci = c(0, 9000, 18000),
    total_cost_mean = c(0, 10000, 20000),
    total_cost_uci = c(0, 11000, 22000)
  )

  sim_dt <- data.table(
    scenario_name = rep(
      c("reference", "intervention1", "intervention2"),
      each = 12
    ),
    admin_1 = rep("region1", 36),
    admin_2 = rep("district1", 36),
    EIR_CI = rep(rep(c("EIR_lci", "EIR_mean", "EIR_uci"), each = 4), 3),
    year = rep(rep(c(2025, 2026), each = 2), 18),
    seed = rep(1:2, 18),
    cum_nUncomp = c(
      # Reference - EIR_lci: 2025 (seed 1,2), 2026 (seed 1,2)
      2000,
      2000,
      4000,
      4000,
      # Reference - EIR_mean
      2000,
      2000,
      4000,
      4000,
      # Reference - EIR_uci
      2000,
      2000,
      4000,
      4000,
      # Intervention 1 - EIR_lci
      1800,
      1800,
      3500,
      3500,
      # Intervention 1 - EIR_mean
      1800,
      1800,
      3500,
      3500,
      # Intervention 1 - EIR_uci
      1800,
      1800,
      3500,
      3500,
      # Intervention 2 - EIR_lci
      1600,
      1600,
      3000,
      3000,
      # Intervention 2 - EIR_mean
      1600,
      1600,
      3000,
      3000,
      # Intervention 2 - EIR_uci
      1600,
      1600,
      3000,
      3000
    ),
    cum_nSevere = rep(100, 36),
    cum_tUncomp = rep(50, 36),
    cum_tSevere = rep(10, 36),
    cum_expectedDirectDeaths = rep(5, 36)
  )

  result <- calculate_cost_effectiveness(
    costs_dt = costs_dt,
    sim_dt = sim_dt,
    reference = "reference"
  )

  # Should have two rows (interventions only)
  expect_equal(nrow(result), 2)
  expect_true("intervention1" %in% result$scenario_name)
  expect_true("intervention2" %in% result$scenario_name)

  # Intervention 1: 500 averted, cost 10000, ratio = 20
  # Intervention 2: 1000 averted, cost 20000, ratio = 20
  expect_equal(
    result[scenario_name == "intervention1"]$cost_per_averted_mean,
    20
  )
  expect_equal(
    result[scenario_name == "intervention2"]$cost_per_averted_mean,
    20
  )
})

test_that("calculate_cost_effectiveness filters year 2026", {
  costs_dt <- data.table(
    scenario_name = c("reference", "intervention1"),
    total_cost_lci = c(0, 9000),
    total_cost_mean = c(0, 10000),
    total_cost_uci = c(0, 11000)
  )

  # Proper structure: 2 scenarios x 3 EIR_CI x 3 years x 2 seeds = 36 rows
  sim_dt <- data.table(
    scenario_name = rep(c("reference", "intervention1"), each = 18),
    admin_1 = rep("region1", 36),
    admin_2 = rep("district1", 36),
    EIR_CI = rep(rep(c("EIR_lci", "EIR_mean", "EIR_uci"), each = 6), 2),
    year = rep(rep(c(2025, 2026, 2027), each = 2), 6),
    seed = rep(1:2, 18),
    cum_nUncomp = c(
      # Reference - EIR_lci: 2025 (s1,s2), 2026 (s1,s2), 2027 (s1,s2)
      1000,
      1000,
      2000,
      2000,
      3000,
      3000,
      # Reference - EIR_mean
      1000,
      1000,
      2000,
      2000,
      3000,
      3000,
      # Reference - EIR_uci
      1000,
      1000,
      2000,
      2000,
      3000,
      3000,
      # Intervention - EIR_lci
      900,
      900,
      1500,
      1500,
      2000,
      2000,
      # Intervention - EIR_mean
      900,
      900,
      1500,
      1500,
      2000,
      2000,
      # Intervention - EIR_uci
      900,
      900,
      1500,
      1500,
      2000,
      2000
    ),
    cum_nSevere = rep(100, 36),
    cum_tUncomp = rep(50, 36),
    cum_tSevere = rep(10, 36),
    cum_expectedDirectDeaths = rep(5, 36)
  )

  result <- calculate_cost_effectiveness(
    costs_dt = costs_dt,
    sim_dt = sim_dt,
    reference = "reference"
  )

  # Should use year 2026 data only
  # Cases averted at 2026: 2000 - 1500 = 500
  # Cost per case: 10000 / 500 = 20
  expect_equal(result$cost_per_averted_mean, 20)
})

test_that("calculate_cost_effectiveness aggregates across admin regions", {
  costs_dt <- data.table(
    scenario_name = c("reference", "intervention1"),
    total_cost_lci = c(0, 18000),
    total_cost_mean = c(0, 20000),
    total_cost_uci = c(0, 22000)
  )

  # Two different admin regions
  # Proper structure: 2 scenarios x 2 admin x 3 EIR_CI x 2 years x 2 seeds = 48 rows
  sim_dt <- data.table(
    scenario_name = rep(c("reference", "intervention1"), each = 24),
    admin_1 = rep(rep(c("region1", "region2"), each = 12), 2),
    admin_2 = rep(rep(c("district1", "district2"), each = 12), 2),
    EIR_CI = rep(rep(c("EIR_lci", "EIR_mean", "EIR_uci"), each = 4), 4),
    year = rep(rep(c(2025, 2026), each = 2), 12),
    seed = rep(1:2, 24),
    cum_nUncomp = c(
      # Reference - region1 - EIR_lci: 2025 (s1,s2), 2026 (s1,s2)
      1000,
      1000,
      2000,
      2000,
      # Reference - region1 - EIR_mean
      1000,
      1000,
      2000,
      2000,
      # Reference - region1 - EIR_uci
      1000,
      1000,
      2000,
      2000,
      # Reference - region2 - EIR_lci
      500,
      500,
      1000,
      1000,
      # Reference - region2 - EIR_mean
      500,
      500,
      1000,
      1000,
      # Reference - region2 - EIR_uci
      500,
      500,
      1000,
      1000,
      # Intervention - region1 - EIR_lci
      900,
      900,
      1700,
      1700,
      # Intervention - region1 - EIR_mean
      900,
      900,
      1700,
      1700,
      # Intervention - region1 - EIR_uci
      900,
      900,
      1700,
      1700,
      # Intervention - region2 - EIR_lci
      450,
      450,
      850,
      850,
      # Intervention - region2 - EIR_mean
      450,
      450,
      850,
      850,
      # Intervention - region2 - EIR_uci
      450,
      450,
      850,
      850
    ),
    cum_nSevere = rep(100, 48),
    cum_tUncomp = rep(50, 48),
    cum_tSevere = rep(10, 48),
    cum_expectedDirectDeaths = rep(5, 48)
  )

  result <- calculate_cost_effectiveness(
    costs_dt = costs_dt,
    sim_dt = sim_dt,
    reference = "reference"
  )

  # Total cases in reference (2026): 2000 + 1000 = 3000
  # Total cases in intervention (2026): 1700 + 850 = 2550
  # Cases averted: 3000 - 2550 = 450
  # Cost per case: 20000 / 450 = 44.44...
  expect_equal(round(result$cost_per_averted_mean, 2), 44.44)
})

test_that("calculate_cost_effectiveness averages across seeds", {
  costs_dt <- data.table(
    scenario_name = c("reference", "intervention1"),
    total_cost_lci = c(0, 9000),
    total_cost_mean = c(0, 10000),
    total_cost_uci = c(0, 11000)
  )

  # Different values for different seeds
  # Proper structure: 2 scenarios x 3 EIR_CI x 2 years x 2 seeds = 24 rows
  sim_dt <- data.table(
    scenario_name = rep(c("reference", "intervention1"), each = 12),
    admin_1 = rep("region1", 24),
    admin_2 = rep("district1", 24),
    EIR_CI = rep(rep(c("EIR_lci", "EIR_mean", "EIR_uci"), each = 4), 2),
    year = rep(rep(c(2025, 2026), each = 2), 6),
    seed = rep(1:2, 12),
    cum_nUncomp = c(
      # Reference - EIR_lci: 2025 (seed1, seed2), 2026 (seed1, seed2)
      1000,
      1100,
      2000,
      2200,
      # Reference - EIR_mean
      1000,
      1100,
      2000,
      2200,
      # Reference - EIR_uci
      1000,
      1100,
      2000,
      2200,
      # Intervention - EIR_lci
      900,
      990,
      1800,
      1980,
      # Intervention - EIR_mean
      900,
      990,
      1800,
      1980,
      # Intervention - EIR_uci
      900,
      990,
      1800,
      1980
    ),
    cum_nSevere = rep(100, 24),
    cum_tUncomp = rep(50, 24),
    cum_tSevere = rep(10, 24),
    cum_expectedDirectDeaths = rep(5, 24)
  )

  result <- calculate_cost_effectiveness(
    costs_dt = costs_dt,
    sim_dt = sim_dt,
    reference = "reference"
  )

  # Mean cases reference (2026): (2000 + 2200) / 2 = 2100
  # Mean cases intervention (2026): (1800 + 1980) / 2 = 1890
  # Cases averted: 2100 - 1890 = 210
  # Cost per case: 10000 / 210 = 47.619...
  expect_equal(round(result$cost_per_averted_mean, 2), 47.62)
})

test_that("calculate_cost_effectiveness handles different EIR confidence intervals", {
  costs_dt <- data.table(
    scenario_name = c("reference", "intervention1"),
    total_cost_lci = c(0, 9000),
    total_cost_mean = c(0, 10000),
    total_cost_uci = c(0, 11000)
  )

  # Proper structure: 2 scenarios x 3 EIR_CI x 2 years x 2 seeds = 24 rows
  sim_dt <- data.table(
    scenario_name = rep(c("reference", "intervention1"), each = 12),
    admin_1 = rep("region1", 24),
    admin_2 = rep("district1", 24),
    EIR_CI = rep(rep(c("EIR_lci", "EIR_mean", "EIR_uci"), each = 4), 2),
    year = rep(rep(c(2025, 2026), each = 2), 6),
    seed = rep(1:2, 12),
    cum_nUncomp = c(
      # Reference - EIR_lci: 2025 (s1,s2), 2026 (s1,s2)
      900,
      900,
      1800,
      1800,
      # Reference - EIR_mean
      1000,
      1000,
      2000,
      2000,
      # Reference - EIR_uci
      1100,
      1100,
      2200,
      2200,
      # Intervention - EIR_lci
      810,
      810,
      1620,
      1620,
      # Intervention - EIR_mean
      850,
      850,
      1700,
      1700,
      # Intervention - EIR_uci
      935,
      935,
      1870,
      1870
    ),
    cum_nSevere = rep(100, 24),
    cum_tUncomp = rep(50, 24),
    cum_tSevere = rep(10, 24),
    cum_expectedDirectDeaths = rep(5, 24)
  )

  result <- calculate_cost_effectiveness(
    costs_dt = costs_dt,
    sim_dt = sim_dt,
    reference = "reference"
  )

  # Check all three CI columns exist and have different values
  expect_true("cost_per_averted_lci" %in% names(result))
  expect_true("cost_per_averted_mean" %in% names(result))
  expect_true("cost_per_averted_uci" %in% names(result))

  # lci: (1800 - 1620) = 180 averted, cost 9000, ratio = 50
  # mean: (2000 - 1700) = 300 averted, cost 10000, ratio = 33.33
  # uci: (2200 - 1870) = 330 averted, cost 11000, ratio = 33.33
  expect_equal(result$cost_per_averted_lci, 50)
  expect_equal(round(result$cost_per_averted_mean, 2), 33.33)
  expect_equal(round(result$cost_per_averted_uci, 2), 33.33)
})

test_that("calculate_cost_effectiveness removes reference from results", {
  costs_dt <- data.table(
    scenario_name = c("reference", "intervention1"),
    total_cost_lci = c(0, 10000),
    total_cost_mean = c(0, 10000),
    total_cost_uci = c(0, 10000)
  )

  # Proper structure: 2 scenarios x 3 EIR_CI x 2 years x 2 seeds = 24 rows
  sim_dt <- data.table(
    scenario_name = rep(c("reference", "intervention1"), each = 12),
    admin_1 = rep("region1", 24),
    admin_2 = rep("district1", 24),
    EIR_CI = rep(rep(c("EIR_lci", "EIR_mean", "EIR_uci"), each = 4), 2),
    year = rep(rep(c(2025, 2026), each = 2), 6),
    seed = rep(1:2, 12),
    cum_nUncomp = c(
      # Reference - EIR_lci: 2025 (s1,s2), 2026 (s1,s2)
      1000,
      1000,
      2000,
      2000,
      # Reference - EIR_mean
      1000,
      1000,
      2000,
      2000,
      # Reference - EIR_uci
      1000,
      1000,
      2000,
      2000,
      # Intervention - EIR_lci
      900,
      900,
      1500,
      1500,
      # Intervention - EIR_mean
      900,
      900,
      1500,
      1500,
      # Intervention - EIR_uci
      900,
      900,
      1500,
      1500
    ),
    cum_nSevere = rep(100, 24),
    cum_tUncomp = rep(50, 24),
    cum_tSevere = rep(10, 24),
    cum_expectedDirectDeaths = rep(5, 24)
  )

  result <- calculate_cost_effectiveness(
    costs_dt = costs_dt,
    sim_dt = sim_dt,
    reference = "reference"
  )

  # Reference should not be in results
  expect_false("reference" %in% result$scenario_name)
  expect_equal(nrow(result), 1)
  expect_equal(result$scenario_name, "intervention1")
})

test_that("calculate_cost_effectiveness errors on missing required columns in costs_dt", {
  # Missing total_cost_uci
  costs_dt <- data.table(
    scenario_name = c("reference", "intervention1"),
    total_cost_lci = c(0, 10000),
    total_cost_mean = c(0, 10000)
  )

  sim_dt <- data.table(
    scenario_name = rep(c("reference", "intervention1"), each = 12),
    admin_1 = rep("region1", 24),
    admin_2 = rep("district1", 24),
    EIR_CI = rep(rep(c("EIR_lci", "EIR_mean", "EIR_uci"), each = 4), 2),
    year = rep(c(2025, 2026), 12),
    seed = rep(1:2, 12),
    cum_nUncomp = rep(1000, 24),
    cum_nSevere = rep(100, 24),
    cum_tUncomp = rep(50, 24),
    cum_tSevere = rep(10, 24),
    cum_expectedDirectDeaths = rep(5, 24)
  )

  expect_error(
    calculate_cost_effectiveness(
      costs_dt = costs_dt,
      sim_dt = sim_dt,
      reference = "reference"
    )
  )
})

test_that("calculate_cost_effectiveness errors on missing required columns in sim_dt", {
  costs_dt <- data.table(
    scenario_name = c("reference", "intervention1"),
    total_cost_lci = c(0, 10000),
    total_cost_mean = c(0, 10000),
    total_cost_uci = c(0, 10000)
  )

  # Missing cum_nSevere and other columns
  sim_dt <- data.table(
    scenario_name = rep(c("reference", "intervention1"), each = 12),
    admin_1 = rep("region1", 24),
    admin_2 = rep("district1", 24),
    EIR_CI = rep(rep(c("EIR_lci", "EIR_mean", "EIR_uci"), each = 4), 2),
    year = rep(c(2025, 2026), 12),
    cum_nUncomp = rep(1000, 24)
  )

  expect_error(
    calculate_cost_effectiveness(
      costs_dt = costs_dt,
      sim_dt = sim_dt,
      reference = "reference"
    )
  )
})

test_that("calculate_cost_effectiveness errors on invalid reference parameter", {
  costs_dt <- data.table(
    scenario_name = c("reference", "intervention1"),
    total_cost_lci = c(0, 10000),
    total_cost_mean = c(0, 10000),
    total_cost_uci = c(0, 10000)
  )

  sim_dt <- data.table(
    scenario_name = rep(c("reference", "intervention1"), each = 12),
    admin_1 = rep("region1", 24),
    admin_2 = rep("district1", 24),
    EIR_CI = rep(rep(c("EIR_lci", "EIR_mean", "EIR_uci"), each = 4), 2),
    year = rep(c(2025, 2026), 12),
    seed = rep(1:2, 12),
    cum_nUncomp = rep(1000, 24),
    cum_nSevere = rep(100, 24),
    cum_tUncomp = rep(50, 24),
    cum_tSevere = rep(10, 24),
    cum_expectedDirectDeaths = rep(5, 24)
  )

  expect_error(
    calculate_cost_effectiveness(
      costs_dt = costs_dt,
      sim_dt = sim_dt,
      reference = 123 # Should be string
    ),
    "reference must be a string"
  )
})

test_that("calculate_intervention_costs calculates costs correctly for single intervention", {
  # Proper structure: 2 seeds x 3 EIR_CI x 2 years = 12 rows
  dt <- data.table(
    scenario_name = rep("scenario1", 12),
    EIR_CI = rep(c("EIR_lci", "EIR_mean", "EIR_uci"), each = 4),
    year = rep(rep(c(2020, 2021), each = 2), 3),
    seed = rep(1:2, 6),
    deployed_int_ITN = rep(1, 12),
    coverage_ITN = rep(0.8, 12),
    nHost = rep(1000, 12)
  )

  unit_costs <- list(ITN = 5)

  result <- calculate_intervention_costs(
    dt = dt,
    interventions = c("ITN"),
    intervention_cols = c("deployed_int_ITN"),
    intervention_cov = c("coverage_ITN"),
    unit_costs = unit_costs,
    year_start = 2020,
    year_end = 2021,
    pop_col = "nHost"
  )

  # Should have one row per scenario
  expect_equal(nrow(result), 1)
  expect_equal(result$scenario_name, "scenario1")

  # Check that cost columns exist
  expect_true("itn_pop_lci" %in% names(result))
  expect_true("itn_cost_lci" %in% names(result))
  expect_true("total_cost_lci" %in% names(result))

  # Population covered per seed: 1000 * 1 * 0.8 * 2 years = 1600
  # Average across 2 seeds: 1600
  # Cost: 1600 * 5 = 8000
  expected_pop <- 1600
  expected_cost <- 8000

  expect_equal(result$itn_pop_lci, expected_pop)
  expect_equal(result$itn_cost_lci, expected_cost)
  expect_equal(result$total_cost_lci, expected_cost)
})

test_that("calculate_intervention_costs handles multiple interventions", {
  # Proper structure: 2 seeds x 3 EIR_CI x 2 years = 12 rows
  dt <- data.table(
    scenario_name = rep("scenario1", 12),
    EIR_CI = rep(c("EIR_lci", "EIR_mean", "EIR_uci"), each = 4),
    year = rep(rep(c(2020, 2021), each = 2), 3),
    seed = rep(1:2, 6),
    deployed_int_ITN = rep(1, 12),
    deployed_int_IRS = rep(1, 12),
    coverage_ITN = rep(0.8, 12),
    coverage_IRS = rep(0.6, 12),
    nHost = rep(1000, 12)
  )

  unit_costs <- list(ITN = 5, IRS = 10)

  result <- calculate_intervention_costs(
    dt = dt,
    interventions = c("ITN", "IRS"),
    intervention_cols = c("deployed_int_ITN", "deployed_int_IRS"),
    intervention_cov = c("coverage_ITN", "coverage_IRS"),
    unit_costs = unit_costs,
    year_start = 2020,
    year_end = 2021,
    pop_col = "nHost"
  )

  # Check that both intervention columns exist
  expect_true("itn_cost_mean" %in% names(result))
  expect_true("irs_cost_mean" %in% names(result))

  # ITN: 1000 * 1 * 0.8 * 2 years = 1600, cost = 8000
  # IRS: 1000 * 1 * 0.6 * 2 years = 1200, cost = 12000
  # Total: 20000
  expect_equal(result$total_cost_mean, 20000)
})

test_that("calculate_intervention_costs handles multiple scenarios", {
  # Proper structure: 2 scenarios x 2 seeds x 3 EIR_CI x 2 years = 24 rows
  dt <- data.table(
    scenario_name = rep(c("scenario1", "scenario2"), each = 12),
    EIR_CI = rep(rep(c("EIR_lci", "EIR_mean", "EIR_uci"), each = 4), 2),
    year = rep(rep(rep(c(2020, 2021), each = 2), 3), 2),
    seed = rep(rep(1:2, 6), 2),
    deployed_int_ITN = c(rep(1, 12), rep(0, 12)),
    coverage_ITN = rep(0.8, 24),
    nHost = rep(1000, 24)
  )

  unit_costs <- list(ITN = 5)

  result <- calculate_intervention_costs(
    dt = dt,
    interventions = c("ITN"),
    intervention_cols = c("deployed_int_ITN"),
    intervention_cov = c("coverage_ITN"),
    unit_costs = unit_costs,
    year_start = 2020,
    year_end = 2021,
    pop_col = "nHost"
  )

  # Should have two rows (one per scenario)
  expect_equal(nrow(result), 2)
  expect_true("scenario1" %in% result$scenario_name)
  expect_true("scenario2" %in% result$scenario_name)

  # scenario1 has intervention, scenario2 doesn't
  expect_equal(result[scenario_name == "scenario1"]$total_cost_mean, 8000)
  expect_equal(result[scenario_name == "scenario2"]$total_cost_mean, 0)
})

test_that("calculate_intervention_costs filters by year range correctly", {
  dt <- data.table(
    scenario_name = rep("scenario1", 9),
    EIR_CI = rep(c("EIR_lci", "EIR_mean", "EIR_uci"), each = 3),
    year = rep(c(2020, 2021, 2022), 3),
    seed = rep(1, 9),
    deployed_int_ITN = rep(1, 9),
    coverage_ITN = rep(0.8, 9),
    nHost = rep(1000, 9)
  )

  unit_costs <- list(ITN = 5)

  # Only include 2020 and 2021
  result <- calculate_intervention_costs(
    dt = dt,
    interventions = c("ITN"),
    intervention_cols = c("deployed_int_ITN"),
    intervention_cov = c("coverage_ITN"),
    unit_costs = unit_costs,
    year_start = 2020,
    year_end = 2021,
    pop_col = "nHost"
  )

  # Population: 1000 * 1 * 0.8 * 2 years = 1600
  # Cost: 1600 * 5 = 8000
  expect_equal(result$total_cost_mean, 8000)

  # Now include all three years
  result_all <- calculate_intervention_costs(
    dt = dt,
    interventions = c("ITN"),
    intervention_cols = c("deployed_int_ITN"),
    intervention_cov = c("coverage_ITN"),
    unit_costs = unit_costs,
    year_start = 2020,
    year_end = 2022,
    pop_col = "nHost"
  )

  # Population: 1000 * 1 * 0.8 * 3 years = 2400
  # Cost: 2400 * 5 = 12000
  expect_equal(result_all$total_cost_mean, 12000)
})

test_that("calculate_intervention_costs averages across seeds", {
  dt <- data.table(
    scenario_name = rep("scenario1", 6),
    EIR_CI = rep(c("EIR_lci", "EIR_mean", "EIR_uci"), each = 2),
    year = rep(2020, 6),
    seed = rep(1:2, 3),
    deployed_int_ITN = rep(1, 6),
    coverage_ITN = rep(c(0.8, 0.6), 3), # Different coverage by seed
    nHost = rep(1000, 6)
  )

  unit_costs <- list(ITN = 5)

  result <- calculate_intervention_costs(
    dt = dt,
    interventions = c("ITN"),
    intervention_cols = c("deployed_int_ITN"),
    intervention_cov = c("coverage_ITN"),
    unit_costs = unit_costs,
    year_start = 2020,
    year_end = 2020,
    pop_col = "nHost"
  )

  # Seed 1: 1000 * 1 * 0.8 = 800
  # Seed 2: 1000 * 1 * 0.6 = 600
  # Average: (800 + 600) / 2 = 700
  # Cost: 700 * 5 = 3500
  expect_equal(result$total_cost_mean, 3500)
})

test_that("calculate_intervention_costs handles zero coverage", {
  dt <- data.table(
    scenario_name = rep("scenario1", 3),
    EIR_CI = c("EIR_lci", "EIR_mean", "EIR_uci"),
    year = rep(2020, 3),
    seed = rep(1, 3),
    deployed_int_ITN = rep(1, 3),
    coverage_ITN = rep(0, 3),
    nHost = rep(1000, 3)
  )

  unit_costs <- list(ITN = 5)

  result <- calculate_intervention_costs(
    dt = dt,
    interventions = c("ITN"),
    intervention_cols = c("deployed_int_ITN"),
    intervention_cov = c("coverage_ITN"),
    unit_costs = unit_costs,
    year_start = 2020,
    year_end = 2020,
    pop_col = "nHost"
  )

  # Zero coverage means zero cost
  expect_equal(result$total_cost_mean, 0)
})

test_that("calculate_intervention_costs handles intervention not deployed", {
  dt <- data.table(
    scenario_name = rep("scenario1", 3),
    EIR_CI = c("EIR_lci", "EIR_mean", "EIR_uci"),
    year = rep(2020, 3),
    seed = rep(1, 3),
    deployed_int_ITN = rep(0, 3),
    coverage_ITN = rep(0.8, 3),
    nHost = rep(1000, 3)
  )

  unit_costs <- list(ITN = 5)

  result <- calculate_intervention_costs(
    dt = dt,
    interventions = c("ITN"),
    intervention_cols = c("deployed_int_ITN"),
    intervention_cov = c("coverage_ITN"),
    unit_costs = unit_costs,
    year_start = 2020,
    year_end = 2020,
    pop_col = "nHost"
  )

  # Intervention not deployed means zero cost
  expect_equal(result$total_cost_mean, 0)
})

test_that("calculate_intervention_costs creates separate columns for EIR_CI", {
  dt <- data.table(
    scenario_name = rep("scenario1", 3),
    EIR_CI = c("EIR_lci", "EIR_mean", "EIR_uci"),
    year = rep(2020, 3),
    seed = rep(1, 3),
    deployed_int_ITN = rep(1, 3),
    coverage_ITN = rep(0.8, 3),
    nHost = c(900, 1000, 1100) # Different populations for different EIR_CI
  )

  unit_costs <- list(ITN = 5)

  result <- calculate_intervention_costs(
    dt = dt,
    interventions = c("ITN"),
    intervention_cols = c("deployed_int_ITN"),
    intervention_cov = c("coverage_ITN"),
    unit_costs = unit_costs,
    year_start = 2020,
    year_end = 2020,
    pop_col = "nHost"
  )

  # Should have separate columns for lci, mean, uci
  expect_true("total_cost_lci" %in% names(result))
  expect_true("total_cost_mean" %in% names(result))
  expect_true("total_cost_uci" %in% names(result))

  # lci: 900 * 1 * 0.8 * 5 = 3600
  # mean: 1000 * 1 * 0.8 * 5 = 4000
  # uci: 1100 * 1 * 0.8 * 5 = 4400
  expect_equal(result$total_cost_lci, 3600)
  expect_equal(result$total_cost_mean, 4000)
  expect_equal(result$total_cost_uci, 4400)
})

test_that("calculate_intervention_costs errors on empty data frame", {
  dt <- data.table()
  unit_costs <- list(ITN = 5)

  expect_error(
    calculate_intervention_costs(
      dt = dt,
      interventions = c("ITN"),
      intervention_cols = c("deployed_int_ITN"),
      intervention_cov = c("coverage_ITN"),
      unit_costs = unit_costs,
      year_start = 2020,
      year_end = 2020,
      pop_col = "nHost"
    ),
    "dt must be a non-empty data frame"
  )
})

test_that("calculate_intervention_costs errors on invalid interventions parameter", {
  dt <- data.table(
    scenario_name = "scenario1",
    EIR_CI = "EIR_mean",
    year = 2020,
    seed = 1,
    deployed_int_ITN = 1,
    coverage_ITN = 0.8,
    nHost = 1000
  )
  unit_costs <- list(ITN = 5)

  expect_error(
    calculate_intervention_costs(
      dt = dt,
      interventions = 123, # Should be character vector
      intervention_cols = c("deployed_int_ITN"),
      intervention_cov = c("coverage_ITN"),
      unit_costs = unit_costs,
      year_start = 2020,
      year_end = 2020,
      pop_col = "nHost"
    ),
    "interventions must be a character vector"
  )
})

test_that("calculate_intervention_costs errors on invalid unit_costs parameter", {
  dt <- data.table(
    scenario_name = "scenario1",
    EIR_CI = "EIR_mean",
    year = 2020,
    seed = 1,
    deployed_int_ITN = 1,
    coverage_ITN = 0.8,
    nHost = 1000
  )

  expect_error(
    calculate_intervention_costs(
      dt = dt,
      interventions = c("ITN"),
      intervention_cols = c("deployed_int_ITN"),
      intervention_cov = c("coverage_ITN"),
      unit_costs = list(), # Empty list
      year_start = 2020,
      year_end = 2020,
      pop_col = "nHost"
    ),
    "unit_costs must be a non-empty list"
  )
})

test_that("calculate_intervention_costs errors on invalid year_start/end parameter", {
  dt <- data.table(
    scenario_name = "scenario1",
    EIR_CI = "EIR_mean",
    year = 2020,
    seed = 1,
    deployed_int_ITN = 1,
    coverage_ITN = 0.8,
    nHost = 1000
  )
  unit_costs <- list(ITN = 5)

  expect_error(
    calculate_intervention_costs(
      dt = dt,
      interventions = c("ITN"),
      intervention_cols = c("deployed_int_ITN"),
      intervention_cov = c("coverage_ITN"),
      unit_costs = unit_costs,
      year_start = "not_an_integer", # Should be an integer
      year_end = "not_an_integer", # Should be an integer
      pop_col = "nHost"
    ),
    "year_start must be an integer"
  )
})

test_that("calculate_intervention_costs errors on missing required columns", {
  dt <- data.table(
    scenario_name = "scenario1",
    EIR_CI = "EIR_mean",
    year = 2020
    # Missing: seed, deployed_int_ITN, coverage_ITN, nHost
  )
  unit_costs <- list(ITN = 5)

  expect_error(
    calculate_intervention_costs(
      dt = dt,
      interventions = c("ITN"),
      intervention_cols = c("deployed_int_ITN"),
      intervention_cov = c("coverage_ITN"),
      unit_costs = unit_costs,
      year_start = 2020,
      year_end = 2020,
      pop_col = "nHost"
    )
  )
})

test_that("calculate_intervention_costs uses custom pop_col parameter", {
  dt <- data.table(
    scenario_name = rep("scenario1", 3),
    EIR_CI = c("EIR_lci", "EIR_mean", "EIR_uci"),
    year = rep(2020, 3),
    seed = rep(1, 3),
    deployed_int_ITN = rep(1, 3),
    coverage_ITN = rep(0.8, 3),
    custom_pop = rep(2000, 3) # Custom population column
  )

  unit_costs <- list(ITN = 5)

  result <- calculate_intervention_costs(
    dt = dt,
    interventions = c("ITN"),
    intervention_cols = c("deployed_int_ITN"),
    intervention_cov = c("coverage_ITN"),
    unit_costs = unit_costs,
    year_start = 2020,
    year_end = 2020,
    pop_col = "custom_pop"
  )

  # Population: 2000 * 1 * 0.8 = 1600
  # Cost: 1600 * 5 = 8000
  expect_equal(result$total_cost_mean, 8000)
})
