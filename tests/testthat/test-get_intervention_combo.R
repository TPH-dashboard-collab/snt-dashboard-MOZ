box::use(
  testthat[
    expect_equal,
    expect_error,
    test_that
  ],
)

# fmt: skip
box::use(
  app/logic/interventions/get_intervention_combo[get_intervention_combo],
)

# Tests for get_intervention_combo ------------------------------------------

test_that("get_intervention_combo combines active interventions", {
  result <- get_intervention_combo(
    TRUE,
    FALSE,
    TRUE,
    int_names = c("ITN", "IRS", "SMC")
  )

  expect_equal(result, "ITN + SMC")
})

test_that("get_intervention_combo handles all interventions active", {
  result <- get_intervention_combo(
    TRUE,
    TRUE,
    TRUE,
    int_names = c("ITN", "IRS", "SMC")
  )

  expect_equal(result, "ITN + IRS + SMC")
})

test_that("get_intervention_combo handles no active interventions", {
  result <- get_intervention_combo(
    FALSE,
    FALSE,
    FALSE,
    int_names = c("ITN", "IRS", "SMC")
  )

  expect_equal(result, "No intervention")
})

test_that("get_intervention_combo handles single active intervention", {
  result <- get_intervention_combo(
    FALSE,
    TRUE,
    FALSE,
    int_names = c("ITN", "IRS", "SMC")
  )

  expect_equal(result, "IRS")
})

test_that("get_intervention_combo handles two active interventions", {
  result <- get_intervention_combo(
    TRUE,
    TRUE,
    FALSE,
    int_names = c("ITN", "IRS", "SMC")
  )

  expect_equal(result, "ITN + IRS")
})

test_that("get_intervention_combo works with single intervention", {
  result <- get_intervention_combo(
    TRUE,
    int_names = c("ITN")
  )

  expect_equal(result, "ITN")
})

test_that("get_intervention_combo returns no intervention for single FALSE", {
  result <- get_intervention_combo(
    FALSE,
    int_names = c("ITN")
  )

  expect_equal(result, "No intervention")
})

test_that("get_intervention_combo works with many interventions", {
  result <- get_intervention_combo(
    TRUE,
    FALSE,
    TRUE,
    TRUE,
    FALSE,
    TRUE,
    int_names = c("ITN", "IRS", "SMC", "PBO", "Vaccine", "MDA")
  )

  expect_equal(result, "ITN + SMC + PBO + MDA")
})

test_that("get_intervention_combo preserves order", {
  result <- get_intervention_combo(
    FALSE,
    FALSE,
    TRUE,
    TRUE,
    FALSE,
    int_names = c("ITN", "IRS", "SMC", "PBO", "Vaccine")
  )

  expect_equal(result, "SMC + PBO")
})

test_that("get_intervention_combo handles custom no_int_name", {
  result <- get_intervention_combo(
    FALSE,
    FALSE,
    int_names = c("ITN", "IRS"),
    no_int_name = "None"
  )

  expect_equal(result, "None")
})

test_that("get_intervention_combo handles long intervention names", {
  result <- get_intervention_combo(
    TRUE,
    TRUE,
    int_names = c("Insecticide_Treated_Nets", "Indoor_Residual_Spraying")
  )

  expect_equal(result, "Insecticide_Treated_Nets + Indoor_Residual_Spraying")
})

test_that("get_intervention_combo handles intervention names with spaces", {
  result <- get_intervention_combo(
    TRUE,
    FALSE,
    TRUE,
    int_names = c("Bed Nets", "Indoor Spray", "Mass Drug")
  )

  expect_equal(result, "Bed Nets + Mass Drug")
})

test_that("get_intervention_combo handles intervention names with special chars", {
  result <- get_intervention_combo(
    TRUE,
    TRUE,
    int_names = c("ITN-PBO", "IRS_2024")
  )

  expect_equal(result, "ITN-PBO + IRS_2024")
})

test_that("get_intervention_combo handles all FALSE with custom no_int_name", {
  result <- get_intervention_combo(
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    int_names = c("ITN", "IRS", "SMC", "PBO"),
    no_int_name = "Baseline"
  )

  expect_equal(result, "Baseline")
})

# Parameter validation tests -------------------------------------------------

test_that("get_intervention_combo errors on non-character int_names", {
  expect_error(
    get_intervention_combo(
      TRUE,
      FALSE,
      int_names = c(1, 2)
    ),
    "int_names must be a non-empty character vector"
  )
})

test_that("get_intervention_combo errors on empty int_names", {
  expect_error(
    get_intervention_combo(
      TRUE,
      int_names = character(0)
    ),
    "int_names must be a non-empty character vector"
  )
})

test_that("get_intervention_combo errors on NULL int_names", {
  expect_error(
    get_intervention_combo(
      TRUE,
      FALSE,
      int_names = NULL
    ),
    "int_names must be a non-empty character vector"
  )
})

test_that("get_intervention_combo errors on non-string no_int_name", {
  expect_error(
    get_intervention_combo(
      TRUE,
      FALSE,
      int_names = c("ITN", "IRS"),
      no_int_name = 123
    ),
    "no_int_name must be a non-empty string"
  )
})

test_that("get_intervention_combo errors on empty no_int_name", {
  expect_error(
    get_intervention_combo(
      TRUE,
      FALSE,
      int_names = c("ITN", "IRS"),
      no_int_name = ""
    ),
    "no_int_name must be a non-empty string"
  )
})

test_that("get_intervention_combo errors on NULL no_int_name", {
  expect_error(
    get_intervention_combo(
      TRUE,
      FALSE,
      int_names = c("ITN", "IRS"),
      no_int_name = NULL
    ),
    "no_int_name must be a non-empty string"
  )
})

test_that("get_intervention_combo errors on non-boolean inputs", {
  expect_error(
    get_intervention_combo(
      1,
      0,
      int_names = c("ITN", "IRS")
    ),
    "... must be booleans"
  )
})

test_that("get_intervention_combo errors on character inputs", {
  expect_error(
    get_intervention_combo(
      "TRUE",
      "FALSE",
      int_names = c("ITN", "IRS")
    ),
    "... must be booleans"
  )
})

test_that("get_intervention_combo errors on NA inputs", {
  expect_error(
    get_intervention_combo(
      TRUE,
      NA,
      int_names = c("ITN", "IRS")
    ),
    "... must be booleans"
  )
})

test_that("get_intervention_combo errors when no boolean values provided", {
  expect_error(
    get_intervention_combo(
      int_names = c("ITN", "IRS")
    ),
    "... must be booleans"
  )
})

test_that("get_intervention_combo errors on mismatched lengths", {
  # When there are more boolean values than int_names, should error
  expect_error(
    get_intervention_combo(
      TRUE,
      TRUE,
      TRUE,
      int_names = c("ITN", "IRS") # Only 2 names for 3 boolean values
    ),
    "subscript out of bounds"
  )
})

test_that("get_intervention_combo handles more names than boolean values", {
  # When there are more int_names than boolean values, extras are ignored
  result <- get_intervention_combo(
    TRUE,
    FALSE,
    int_names = c("ITN", "IRS", "SMC", "PBO") # 4 names for 2 boolean values
  )

  # Should only use first 2 names
  expect_equal(result, "ITN")
})

# Edge cases -----------------------------------------------------------------

test_that("get_intervention_combo handles single TRUE value", {
  result <- get_intervention_combo(
    TRUE,
    int_names = c("SingleIntervention")
  )

  expect_equal(result, "SingleIntervention")
})

test_that("get_intervention_combo handles alternating pattern", {
  result <- get_intervention_combo(
    TRUE,
    FALSE,
    TRUE,
    FALSE,
    TRUE,
    int_names = c("A", "B", "C", "D", "E")
  )

  expect_equal(result, "A + C + E")
})

test_that("get_intervention_combo handles first and last only", {
  result <- get_intervention_combo(
    TRUE,
    FALSE,
    FALSE,
    FALSE,
    TRUE,
    int_names = c("First", "Second", "Third", "Fourth", "Last")
  )

  expect_equal(result, "First + Last")
})

test_that("get_intervention_combo works with numeric-looking names", {
  result <- get_intervention_combo(
    TRUE,
    TRUE,
    int_names = c("2020", "2021")
  )

  expect_equal(result, "2020 + 2021")
})

test_that("get_intervention_combo handles intervention names with dots", {
  result <- get_intervention_combo(
    TRUE,
    FALSE,
    TRUE,
    int_names = c("ITN.v1", "IRS.v2", "SMC.v3")
  )

  expect_equal(result, "ITN.v1 + SMC.v3")
})

test_that("get_intervention_combo handles very long combinations", {
  # Test with 10 interventions all active
  result <- get_intervention_combo(
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    int_names = c(
      "ITN",
      "IRS",
      "SMC",
      "PBO",
      "Vaccine",
      "MDA",
      "LLIN",
      "Nets",
      "Spray",
      "Treatment"
    )
  )

  expected <- "ITN + IRS + SMC + PBO + Vaccine + MDA + LLIN + Nets + Spray + Treatment"
  expect_equal(result, expected)
})

test_that("get_intervention_combo handles single character names", {
  result <- get_intervention_combo(
    TRUE,
    FALSE,
    TRUE,
    int_names = c("A", "B", "C")
  )

  expect_equal(result, "A + C")
})

test_that("get_intervention_combo handles unicode in names", {
  result <- get_intervention_combo(
    TRUE,
    TRUE,
    int_names = c("ITN\u00E9", "IRS\u00F6")
  )

  expect_equal(result, "ITN\u00E9 + IRS\u00F6")
})

test_that("get_intervention_combo is case sensitive", {
  result <- get_intervention_combo(
    TRUE,
    TRUE,
    TRUE,
    int_names = c("itn", "ITN", "Itn")
  )

  expect_equal(result, "itn + ITN + Itn")
})

test_that("get_intervention_combo handles empty string custom no_int_name behavior", {
  # This should error since no_int_name must be non-empty
  expect_error(
    get_intervention_combo(
      FALSE,
      int_names = c("ITN"),
      no_int_name = ""
    ),
    "no_int_name must be a non-empty string"
  )
})
