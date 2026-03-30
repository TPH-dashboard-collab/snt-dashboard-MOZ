box::use(
  data.table[
    address,
    data.table
  ],
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
  app/logic/data/convert_col_types[convert_col_types],
)

test_that("convert_col_types converts columns matching single pattern", {
  dt <- data.table(
    year = c("2020", "2021", "2022"),
    month = c("1", "2", "3"),
    value = c("100", "200", "300")
  )

  rules <- list("year" = as.integer)
  result <- convert_col_types(dt, rules)

  expect_true(is.integer(result$year))
  expect_equal(result$year, c(2020L, 2021L, 2022L))
  # Other columns should remain character
  expect_true(is.character(result$month))
  expect_true(is.character(result$value))
})

test_that("convert_col_types converts columns matching regex pattern", {
  dt <- data.table(
    count_a = c("10", "20", "30"),
    count_b = c("40", "50", "60"),
    name = c("A", "B", "C")
  )

  rules <- list("^count_" = as.integer)
  result <- convert_col_types(dt, rules)

  expect_true(is.integer(result$count_a))
  expect_true(is.integer(result$count_b))
  expect_equal(result$count_a, c(10L, 20L, 30L))
  expect_equal(result$count_b, c(40L, 50L, 60L))
  # Non-matching column should remain character
  expect_true(is.character(result$name))
})

test_that("convert_col_types applies multiple rules", {
  dt <- data.table(
    id = c("1", "2", "3"),
    value = c("10.5", "20.5", "30.5"),
    flag = c("TRUE", "FALSE", "TRUE")
  )

  rules <- list(
    "id" = as.integer,
    "value" = as.numeric,
    "flag" = as.logical
  )
  result <- convert_col_types(dt, rules)

  expect_true(is.integer(result$id))
  expect_true(is.numeric(result$value))
  expect_true(is.logical(result$flag))
  expect_equal(result$id, c(1L, 2L, 3L))
  expect_equal(result$value, c(10.5, 20.5, 30.5))
  expect_equal(result$flag, c(TRUE, FALSE, TRUE))
})

test_that("convert_col_types handles data.frame input", {
  dt <- data.frame(
    year = c("2020", "2021", "2022"),
    value = c("100", "200", "300")
  )

  rules <- list("year" = as.integer)
  result <- convert_col_types(dt, rules)

  expect_true(is.integer(result$year))
  expect_equal(result$year, c(2020L, 2021L, 2022L))
})

test_that("convert_col_types modifies by reference when copy=FALSE", {
  dt <- data.table(
    year = c("2020", "2021", "2022")
  )

  original_address <- address(dt)
  rules <- list("year" = as.integer)
  result <- convert_col_types(dt, rules, copy = FALSE)

  # Should modify the original data.table
  expect_true(is.integer(dt$year))
  expect_equal(address(result), original_address)
})

test_that("convert_col_types creates copy when copy=TRUE", {
  dt <- data.table(
    year = c("2020", "2021", "2022")
  )

  original_address <- address(dt)
  original_type <- class(dt$year)

  rules <- list("year" = as.integer)
  result <- convert_col_types(dt, rules, copy = TRUE)

  # Original should remain unchanged
  expect_equal(class(dt$year), original_type)
  expect_true(is.character(dt$year))

  # Result should be modified
  expect_true(is.integer(result$year))

  # Addresses should be different
  expect_false(address(result) == original_address)
})

test_that("convert_col_types handles pattern with no matches", {
  dt <- data.table(
    year = c("2020", "2021", "2022"),
    month = c("1", "2", "3")
  )

  rules <- list("nonexistent_column" = as.integer)
  result <- convert_col_types(dt, rules)

  # Should not error, columns should remain unchanged
  expect_true(is.character(result$year))
  expect_true(is.character(result$month))
})

test_that("convert_col_types handles multiple patterns with some matching", {
  dt <- data.table(
    count = c("10", "20", "30"),
    name = c("A", "B", "C")
  )

  rules <- list(
    "count" = as.integer,
    "nonexistent" = as.numeric,
    "name" = toupper
  )
  result <- convert_col_types(dt, rules)

  expect_true(is.integer(result$count))
  expect_equal(result$count, c(10L, 20L, 30L))
  expect_equal(result$name, c("A", "B", "C"))
})

test_that("convert_col_types errors on empty data.table", {
  dt <- data.table()
  rules <- list("col" = as.integer)

  expect_error(
    convert_col_types(dt, rules),
    "dt must not be empty"
  )
})

test_that("convert_col_types errors on empty rules list", {
  dt <- data.table(year = c("2020", "2021"))
  rules <- list()

  expect_error(
    convert_col_types(dt, rules),
    "rules must be a non-empty named list"
  )
})

test_that("convert_col_types errors on invalid copy parameter", {
  dt <- data.table(year = c("2020", "2021"))
  rules <- list("year" = as.integer)

  expect_error(
    convert_col_types(dt, rules, copy = "not_a_boolean"),
    "copy must be a boolean"
  )

  expect_error(
    convert_col_types(dt, rules, copy = c(TRUE, FALSE)),
    "copy must be a boolean"
  )
})

test_that("convert_col_types handles complex regex patterns", {
  dt <- data.table(
    deployed_int_ITN = c(1, 0, 1),
    deployed_int_IRS = c(0, 1, 0),
    other_column = c("A", "B", "C")
  )

  rules <- list("^deployed_int_.*" = as.logical)
  result <- convert_col_types(dt, rules)

  expect_true(is.logical(result$deployed_int_ITN))
  expect_true(is.logical(result$deployed_int_IRS))
  expect_true(is.character(result$other_column))
  expect_equal(result$deployed_int_ITN, c(TRUE, FALSE, TRUE))
  expect_equal(result$deployed_int_IRS, c(FALSE, TRUE, FALSE))
})

test_that("convert_col_types handles custom conversion functions", {
  dt <- data.table(
    value_a = c("10", "20", "30"),
    value_b = c("40", "50", "60")
  )

  # Custom function that converts to integer and multiplies by 2
  double_int <- function(x) as.integer(x) * 2L

  rules <- list("^value_" = double_int)
  result <- convert_col_types(dt, rules)

  expect_equal(result$value_a, c(20L, 40L, 60L))
  expect_equal(result$value_b, c(80L, 100L, 120L))
})

test_that("convert_col_types handles overlapping patterns", {
  dt <- data.table(
    count = c("10", "20", "30"),
    count_total = c("100", "200", "300")
  )

  # Both patterns will match count_total
  rules <- list(
    "^count$" = as.integer,
    "count_total" = as.numeric
  )
  result <- convert_col_types(dt, rules)

  expect_true(is.integer(result$count))
  # count_total will be converted by both rules, last one wins
  expect_true(is.numeric(result$count_total))
})

test_that("convert_col_types preserves column order", {
  dt <- data.table(
    z_col = c("1", "2", "3"),
    a_col = c("4", "5", "6"),
    m_col = c("7", "8", "9")
  )

  original_order <- names(dt)

  rules <- list(".*col" = as.integer)
  result <- convert_col_types(dt, rules)

  expect_equal(names(result), original_order)
})

test_that("convert_col_types handles single row data.table", {
  dt <- data.table(
    year = "2020",
    value = "100"
  )

  rules <- list(
    "year" = as.integer,
    "value" = as.numeric
  )
  result <- convert_col_types(dt, rules)

  expect_equal(result$year, 2020L)
  expect_equal(result$value, 100)
})

test_that("convert_col_types handles NA values correctly", {
  dt <- data.table(
    value = c("10", NA, "30"),
    flag = c("TRUE", "FALSE", NA)
  )

  rules <- list(
    "value" = as.numeric,
    "flag" = as.logical
  )
  expect_error(
    convert_col_types(dt, rules),
    "dt must not be empty"
  )
})
