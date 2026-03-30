box::use(
  testthat[
    expect_error,
    expect_match,
    test_that
  ],
)

# fmt: skip
box::use(
  app/logic/visualization/tables[format_with_ci],
)

# --- format_with_ci tests ---

test_that("format_with_ci produces correct format", {
  result <- format_with_ci(100, 80, 120, digits = 0)
  expect_match(result, "100")
  expect_match(result, "80")
  expect_match(result, "120")
  expect_match(result, "\\[.*:.*\\]")
})

test_that("format_with_ci respects digits parameter", {
  result <- format_with_ci(1.234, 1.1, 1.5, digits = 2)
  expect_match(result, "1.23")
})

test_that("format_with_ci handles large numbers with big.mark", {
  result <- format_with_ci(1100000, 900000, 1100000)
  # Should contain space as thousands separator for numbers that don't
  # trigger scientific notation
  expect_match(result, "1 100 000")
})

test_that("format_with_ci rejects non-numeric inputs", {
  expect_error(format_with_ci("a", 1, 2))
  expect_error(format_with_ci(1, "b", 2))
  expect_error(format_with_ci(1, 2, "c"))
})

test_that("format_with_ci rejects invalid digits", {
  expect_error(format_with_ci(1, 2, 3, digits = "a"))
})
