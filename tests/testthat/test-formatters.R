box::use(
  testthat[expect_equal, test_that],
)

box::use(
  app/logic/visualization/formatters[format_number],
)

test_that("format_number formats billions correctly", {
  expect_equal(format_number(1100000000), "1.10 B")
  expect_equal(format_number(1280000000), "1.28 B")
  expect_equal(format_number(1500000000), "1.50 B")
  expect_equal(format_number(1000000000), "1.00 B")
})

test_that("format_number formats millions correctly", {
  expect_equal(format_number(1100000), "1.10 M")
  expect_equal(format_number(1280000), "1.28 M")
  expect_equal(format_number(1500000), "1.50 M")
  expect_equal(format_number(1000000), "1.00 M")
  expect_equal(format_number(999999999), "1,000.00 M")
})

test_that("format_number formats thousands correctly", {
  expect_equal(format_number(1100), "1.10 k")
  expect_equal(format_number(1280), "1.28 k")
  expect_equal(format_number(1500), "1.50 k")
  expect_equal(format_number(1000), "1.00 k")
  expect_equal(format_number(999999), "1,000.00 k")
})

test_that("format_number formats small numbers with commas", {
  expect_equal(format_number(999), "999")
  expect_equal(format_number(100), "100")
  expect_equal(format_number(0), "0")
  expect_equal(format_number(1), "1")
})

test_that("format_number handles vector input", {
  result <- format_number(c(1500, 1280000, 1100000000))
  expect_equal(result[1], "1.50 k")
  expect_equal(result[2], "1.28 M")
  expect_equal(result[3], "1.10 B")
})

test_that("format_number preserves trailing zeros", {
  expect_equal(format_number(1000000), "1.00 M")
  expect_equal(format_number(1000000000), "1.00 B")
  expect_equal(format_number(1000), "1.00 k")
})
