box::use(
  data.table[data.table],
  testthat[
    expect_equal,
    expect_error,
    expect_length,
    expect_warning,
    test_that
  ],
)

# fmt: skip
box::use(
  app/logic/interventions/colnames_to_interventions[colnames_to_interventions],
)

test_that("colnames_to_interventions extracts intervention names correctly", {
  # Basic case with two interventions
  dt <- data.table(
    deployed_int_ITN = c(TRUE, FALSE),
    deployed_int_IRS = c(FALSE, TRUE),
    other_column = 1:2
  )

  result <- colnames_to_interventions(dt)

  expect_equal(result, c("ITN", "IRS"))
  expect_length(result, 2)
})

test_that("colnames_to_interventions works with single intervention", {
  dt <- data.table(
    deployed_int_SMC = c(TRUE, FALSE, TRUE),
    year = 2020:2022
  )

  result <- colnames_to_interventions(dt)

  expect_equal(result, "SMC")
  expect_length(result, 1)
})

test_that("colnames_to_interventions works with many interventions", {
  dt <- data.table(
    deployed_int_ITN = c(TRUE),
    deployed_int_IRS = c(FALSE),
    deployed_int_SMC = c(TRUE),
    deployed_int_PBO = c(FALSE),
    deployed_int_LLIN = c(TRUE)
  )

  result <- colnames_to_interventions(dt)

  expect_equal(result, c("ITN", "IRS", "SMC", "PBO", "LLIN"))
  expect_length(result, 5)
})

test_that("colnames_to_interventions errors on empty data frame", {
  dt <- data.table()

  expect_error(
    colnames_to_interventions(dt),
    "dt must be a non-empty data frame"
  )
})

test_that("colnames_to_interventions errors when no intervention columns found", {
  dt <- data.table(
    some_column = 1:3,
    another_column = letters[1:3]
  )

  expect_error(
    colnames_to_interventions(dt),
    "No intervention columns found matching pattern"
  )
})

test_that("colnames_to_interventions handles data frames (not just data.table)", {
  dt <- data.frame(
    deployed_int_ITN = c(TRUE, FALSE),
    deployed_int_IRS = c(FALSE, TRUE)
  )

  result <- colnames_to_interventions(dt)

  expect_equal(result, c("ITN", "IRS"))
})

test_that("colnames_to_interventions warns on empty intervention names", {
  dt <- data.table(
    deployed_int_ = c(TRUE, FALSE),
    deployed_int_ITN = c(FALSE, TRUE)
  )

  expect_warning(
    result <- colnames_to_interventions(dt),
    "Found 1 column\\(s\\) with empty intervention names"
  )

  # Should still return the valid intervention
  expect_equal(result, "ITN")
})

test_that("colnames_to_interventions warns on duplicate intervention names", {
  dt <- data.table(
    deployed_int_ITN = c(TRUE, FALSE),
    deployed_int_ITN_duplicate = c(FALSE, TRUE)
  )

  # Rename to create actual duplicate pattern
  colnames(dt)[2] <- "deployed_int_ITN"

  expect_warning(
    result <- colnames_to_interventions(dt),
    "Duplicate intervention names found: ITN"
  )

  # Should return unique values
  expect_equal(result, "ITN")
  expect_length(result, 1)
})

test_that("colnames_to_interventions returns unique values when duplicates exist", {
  dt <- data.table(
    deployed_int_ITN = c(TRUE),
    deployed_int_IRS = c(FALSE),
    deployed_int_ITN_2 = c(TRUE)
  )

  # Rename to create duplicate
  colnames(dt)[3] <- "deployed_int_ITN"

  suppressWarnings(result <- colnames_to_interventions(dt))

  # Should only return unique interventions
  expect_equal(sort(result), c("IRS", "ITN"))
  expect_length(result, 2)
})

test_that("colnames_to_interventions preserves order of first occurrence", {
  dt <- data.table(
    deployed_int_SMC = c(TRUE),
    deployed_int_ITN = c(FALSE),
    deployed_int_IRS = c(TRUE),
    deployed_int_PBO = c(FALSE)
  )

  result <- colnames_to_interventions(dt)

  expect_equal(result, c("SMC", "ITN", "IRS", "PBO"))
})

test_that("colnames_to_interventions handles intervention names with underscores", {
  dt <- data.table(
    deployed_int_ITN_PBO = c(TRUE, FALSE),
    deployed_int_LLIN_NEW = c(FALSE, TRUE)
  )

  result <- colnames_to_interventions(dt)

  expect_equal(result, c("ITN_PBO", "LLIN_NEW"))
})

test_that("colnames_to_interventions handles intervention names with numbers", {
  dt <- data.table(
    deployed_int_ITN2020 = c(TRUE, FALSE),
    deployed_int_IRS2021 = c(FALSE, TRUE)
  )

  result <- colnames_to_interventions(dt)

  expect_equal(result, c("ITN2020", "IRS2021"))
})

test_that("colnames_to_interventions errors when only empty intervention names exist", {
  dt <- data.table(
    deployed_int_ = c(TRUE, FALSE)
  )

  expect_error(
    suppressWarnings(colnames_to_interventions(dt)),
    "No valid intervention names could be extracted"
  )
})
