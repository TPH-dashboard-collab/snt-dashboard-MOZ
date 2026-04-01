box::use(
  testthat[expect_equal, test_that],
)

# fmt: skip
box::use(
  app/logic/visualization/formatters[format_large_number],
)

test_that("format_large_number formats billions correctly", {
  expect_equal(format_large_number(1100000000), "$1.1B")
  expect_equal(format_large_number(1500000000), "$1.5B")
  expect_equal(format_large_number(1000000000), "$1.0B")
})

test_that("format_large_number formats millions correctly", {
  expect_equal(format_large_number(1100000), "$1.1M")
  expect_equal(format_large_number(1500000), "$1.5M")
  expect_equal(format_large_number(1000000), "$1.0M")
  expect_equal(format_large_number(999000000), "$999.0M")
})

test_that("format_large_number formats thousands with K when threshold allows", {
  expect_equal(format_large_number(1500, threshold = 1e3), "$2K")
  expect_equal(format_large_number(5000, threshold = 1e3), "$5K")
  expect_equal(format_large_number(999999, threshold = 1e3), "$1000K")
})

test_that("format_large_number formats small numbers with commas", {
  expect_equal(format_large_number(999), "$999")
  expect_equal(format_large_number(100), "$100")
  expect_equal(format_large_number(0), "$0")
  expect_equal(format_large_number(1500), "$1,500")
})

test_that("format_large_number handles negative values", {
  expect_equal(format_large_number(-1500000), "-$1.5M")
  expect_equal(format_large_number(-500), "-$500")
})

test_that("format_large_number handles NA and non-finite values", {
  expect_equal(format_large_number(NA), "N/A")
  expect_equal(format_large_number(Inf), "N/A")
  expect_equal(format_large_number(NaN), "N/A")
})

test_that("format_large_number handles vector input", {
  result <- format_large_number(c(1500, 1500000, 1500000000))
  expect_equal(result, c("$1,500", "$1.5M", "$1.5B"))
})

test_that("format_large_number respects custom prefix", {
  expect_equal(format_large_number(1500000, prefix = ""), "1.5M")
  expect_equal(format_large_number(500, prefix = "EUR "), "EUR 500")
})
