box::use(
  app/logic/interventions/intervention_conflicts,
  testthat[...],
)

test_that("get_mutual_exclusivity_rules returns expected structure", {
  rules <- intervention_conflicts$get_mutual_exclusivity_rules()

  expect_type(rules, "list")
  expect_true("case_management" %in% names(rules))
  expect_true("net_types" %in% names(rules))
  expect_true("preventive_chemo" %in% names(rules))

  # Check case management group
  expect_true("deployed_int_CM" %in% rules$case_management)
  expect_true("deployed_int_ICCM" %in% rules$case_management)

  # Check net types group
  expect_true("deployed_int_STD_Nets" %in% rules$net_types)
  expect_true("deployed_int_PBO_Nets" %in% rules$net_types)
  expect_true("deployed_int_IG2_Nets" %in% rules$net_types)
})

test_that("detect_conflicts identifies CM/ICCM conflict", {
  interventions <- c("deployed_int_CM", "deployed_int_ICCM")
  result <- intervention_conflicts$detect_conflicts(interventions, "Test District")

  expect_true(result$has_conflict)
  expect_true("case_management" %in% names(result$conflict_groups))
  expect_equal(
    sort(result$conflict_groups$case_management),
    sort(c("deployed_int_CM", "deployed_int_ICCM"))
  )
  expect_gt(length(result$details), 0)
})

test_that("detect_conflicts identifies net type conflicts", {
  interventions <- c("deployed_int_STD_Nets", "deployed_int_PBO_Nets")
  result <- intervention_conflicts$detect_conflicts(interventions, "Test District")

  expect_true(result$has_conflict)
  expect_true("net_types" %in% names(result$conflict_groups))
  expect_equal(
    sort(result$conflict_groups$net_types),
    sort(c("deployed_int_STD_Nets", "deployed_int_PBO_Nets"))
  )
})

test_that("detect_conflicts finds no conflict for valid combinations", {
  interventions <- c(
    "deployed_int_CM",
    "deployed_int_STD_Nets",
    "deployed_int_IRS"
  )
  result <- intervention_conflicts$detect_conflicts(interventions, "Test District")

  expect_false(result$has_conflict)
  expect_equal(length(result$conflict_groups), 0)
  expect_equal(length(result$details), 0)
})

test_that("detect_conflicts handles empty intervention list", {
  interventions <- character(0)
  result <- intervention_conflicts$detect_conflicts(interventions, "Test District")

  expect_false(result$has_conflict)
  expect_equal(length(result$conflict_groups), 0)
})

test_that("detect_conflicts identifies multiple conflicts", {
  interventions <- c(
    "deployed_int_CM",
    "deployed_int_ICCM",
    "deployed_int_STD_Nets",
    "deployed_int_PBO_Nets"
  )
  result <- intervention_conflicts$detect_conflicts(interventions, "Test District")

  expect_true(result$has_conflict)
  expect_equal(length(result$conflict_groups), 2)
  expect_true("case_management" %in% names(result$conflict_groups))
  expect_true("net_types" %in% names(result$conflict_groups))
})

test_that("resolve_conflicts keeps highest priority", {
  interventions <- c("deployed_int_CM", "deployed_int_ICCM")
  conflicts <- intervention_conflicts$detect_conflicts(interventions, "Test")
  priorities <- c(deployed_int_CM = 10, deployed_int_ICCM = 9)

  resolved <- intervention_conflicts$resolve_conflicts(
    interventions,
    conflicts,
    priorities
  )

  expect_equal(resolved, "deployed_int_CM")
})

test_that("resolve_conflicts returns original if no conflicts", {
  interventions <- c("deployed_int_CM", "deployed_int_STD_Nets")
  conflicts <- intervention_conflicts$detect_conflicts(interventions, "Test")
  priorities <- intervention_conflicts$get_intervention_priorities()

  resolved <- intervention_conflicts$resolve_conflicts(
    interventions,
    conflicts,
    priorities
  )

  expect_equal(sort(resolved), sort(interventions))
})

test_that("resolve_conflicts handles multiple conflict groups", {
  interventions <- c(
    "deployed_int_CM",
    "deployed_int_ICCM",
    "deployed_int_STD_Nets",
    "deployed_int_PBO_Nets"
  )
  conflicts <- intervention_conflicts$detect_conflicts(interventions, "Test")
  priorities <- intervention_conflicts$get_intervention_priorities()

  resolved <- intervention_conflicts$resolve_conflicts(
    interventions,
    conflicts,
    priorities
  )

  # Should keep CM (higher than ICCM) and one net type
  expect_true("deployed_int_CM" %in% resolved)
  expect_false("deployed_int_ICCM" %in% resolved)
  # Should keep only one net type
  net_count <- sum(c(
    "deployed_int_STD_Nets",
    "deployed_int_PBO_Nets"
  ) %in% resolved)
  expect_equal(net_count, 1)
})

test_that("get_intervention_priorities returns all interventions", {
  priorities <- intervention_conflicts$get_intervention_priorities()

  expect_true(!is.null(names(priorities)))

  # Check some key priorities
  expect_true("deployed_int_CM" %in% names(priorities))
  expect_true("deployed_int_ICCM" %in% names(priorities))
  expect_true("deployed_int_STD_Nets" %in% names(priorities))
  expect_true("deployed_int_Vaccine" %in% names(priorities))

  # Check priority ordering (CM should be highest)
  expect_equal(priorities[["deployed_int_CM"]], 10)
  expect_equal(priorities[["deployed_int_ICCM"]], 9)
  expect_equal(priorities[["deployed_int_Vaccine"]], 5)
})

test_that("resolve_conflicts handles missing priorities gracefully", {
  interventions <- c("deployed_int_CM", "deployed_int_ICCM")
  conflicts <- intervention_conflicts$detect_conflicts(interventions, "Test")
  priorities <- c(deployed_int_CM = 10) # Missing ICCM priority

  expect_warning(
    resolved <- intervention_conflicts$resolve_conflicts(
      interventions,
      conflicts,
      priorities
    ),
    "Missing priorities"
  )

  # Should still return a result (keeps first in list)
  expect_type(resolved, "character")
  expect_equal(length(resolved), 1)
})
