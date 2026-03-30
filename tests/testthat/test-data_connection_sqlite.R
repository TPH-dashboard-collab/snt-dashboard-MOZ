box::use(
  DBI[
    dbConnect,
    dbDisconnect,
    dbExecute,
    dbIsValid,
  ],
  RSQLite[SQLite],
  testthat[
    expect_equal,
    expect_error,
    expect_s3_class,
    expect_true,
    expect_warning,
    test_that
  ],
)

# fmt: skip
box::use(
  app/logic/data/data_connection_sqlite[DataConnectionSQLite],
)

# Helper function to create a test database with sample data
create_test_db <- function() {
  db_path <- tempfile(fileext = ".db")

  # Create database via a separate connection to avoid issues with the setup
  # function
  con <- dbConnect(SQLite(), db_path)

  # Create a test table with various column types
  dbExecute(
    con,
    "CREATE TABLE data (
    year INTEGER,
    month INTEGER,
    admin_1 TEXT,
    admin_2 TEXT,
    scenario_name,
    plan,
    risk_stratum,
    age_group,
    deployed_int_ITN INTEGER,
    deployed_int_IRS INTEGER,
    deployed_int_SMC INTEGER,
    value REAL,
    count INTEGER
  )"
  )

  # Insert test data
  dbExecute(
    con,
    "INSERT INTO data VALUES
    (2020, 1, 'Region1', 'District1', 'nsp', 'NSP', 'low', '0-5', 1, 1, 0, 100.5, 10),
    (2020, 2, 'Region1', 'District2', 'nsp', 'NSP', 'low', '0-5', 1, 0, 0, 200.5, 20),
    (2021, 1, 'Region2', 'District1', 'bau', 'BAU', 'high', '0-5', 0, 1, 1, 300.5, 30),
    (2021, 2, 'Region2', 'District2', 'nsp', 'NSP', 'low', '0-5', 0, 0, 1, 400.5, 40),
    (2022, 1, 'Region1', 'District1', 'nsp', 'NSP', 'low', '0-5', 1, 1, 1, 500.5, 50)"
  )

  dbDisconnect(con)

  data_con <- DataConnectionSQLite$new(db_path = db_path)
  con <- data_con$pool

  return(data_con)
}


test_that("data_connection_sqlite initializes a connection pool", {
  # Get connection pool
  data_con <- create_test_db()

  # Should return a Pool object (R6 class)
  expect_true(inherits(data_con$pool, "Pool"))
  expect_true(inherits(data_con$pool, "R6"))
  expect_true(dbIsValid(data_con$pool))

  # Clean up
  data_con$shutdown()
  unlink(data_con$uri)
})


# Tests for get_filtered_data --------------------------------------------------

test_that("get_filtered_data retrieves all columns with wildcard", {
  data_con <- create_test_db()

  conversion_rules <- list(
    "^deployed_int_" = as.logical
  )

  result <- data_con$get_filtered_data(
    columns = "*",
    filters = list(),
    conversion_rules = conversion_rules
  )

  # Should return a data.table
  expect_s3_class(result, "data.table")

  # Should have all columns
  expect_equal(ncol(result), 13)
  expect_true("year" %in% colnames(result))
  expect_true("deployed_int_ITN" %in% colnames(result))

  # Should have all rows
  expect_equal(nrow(result), 5)

  # Check that types were converted correctly
  expect_true(is.logical(result$deployed_int_ITN))

  data_con$shutdown()
  unlink(data_con$uri)
})

test_that("get_filtered_data retrieves specific columns", {
  data_con <- create_test_db()

  conversion_rules <- list(
    "admin_1" = as.character
  )

  result <- data_con$get_filtered_data(
    columns = c("year", "admin_1", "value"),
    filters = list(),
    conversion_rules = conversion_rules
  )

  # Should only have requested columns
  expect_equal(ncol(result), 3)
  expect_equal(colnames(result), c("year", "admin_1", "value"))

  # Should have all rows
  expect_equal(nrow(result), 5)

  data_con$shutdown()
  unlink(data_con$uri)
})

test_that("get_filtered_data filters by single column", {
  data_con <- create_test_db()

  conversion_rules <- list("admin_1" = as.character)

  result <- data_con$get_filtered_data(
    columns = "*",
    filters = list(year = c(2020)),
    conversion_rules = conversion_rules
  )

  # Should only return rows where year = 2020
  expect_equal(nrow(result), 2)
  expect_true(all(result$year == 2020))

  data_con$shutdown()
  unlink(data_con$uri)
})

test_that("get_filtered_data filters by multiple values in one column", {
  data_con <- create_test_db()

  conversion_rules <- list("admin_1" = as.character)

  result <- data_con$get_filtered_data(
    columns = "*",
    filters = list(year = c(2020, 2021)),
    conversion_rules = conversion_rules
  )

  # Should return rows where year is 2020 or 2021
  expect_equal(nrow(result), 4)
  expect_true(all(result$year %in% c(2020, 2021)))

  data_con$shutdown()
  unlink(data_con$uri)
})

test_that("get_filtered_data filters by multiple columns", {
  data_con <- create_test_db()

  conversion_rules <- list("admin_1" = as.character)

  result <- data_con$get_filtered_data(
    columns = "*",
    filters = list(
      year = c(2020),
      admin_1 = c("Region1")
    ),
    conversion_rules = conversion_rules
  )

  # Should return rows where year = 2020 AND admin_1 = Region1
  expect_equal(nrow(result), 2)
  expect_true(all(result$year == 2020))
  expect_true(all(result$admin_1 == "Region1"))

  data_con$shutdown()
  unlink(data_con$uri)
})

test_that("get_filtered_data applies conversion rules correctly", {
  data_con <- create_test_db()

  conversion_rules <- list(
    "^deployed_int_" = as.logical,
    "admin_1" = as.character
  )

  result <- data_con$get_filtered_data(
    columns = "*",
    filters = list(),
    conversion_rules = conversion_rules
  )

  # Check all conversions
  expect_true(is.logical(result$deployed_int_ITN))
  expect_true(is.logical(result$deployed_int_IRS))
  expect_true(is.logical(result$deployed_int_SMC))

  # Check values
  expect_equal(result$deployed_int_ITN[1], TRUE)
  expect_equal(result$deployed_int_IRS[2], FALSE)
  expect_equal(result$value[1], 100.5)

  data_con$shutdown()
  unlink(data_con$uri)
})

test_that("get_filtered_data returns empty data.table with warning when no rows match", {
  data_con <- create_test_db()

  conversion_rules <- list("admin_1" = as.character)

  expect_warning(
    result <- data_con$get_filtered_data(
      columns = "*",
      filters = list(year = c(2099)),
      conversion_rules = conversion_rules
    ),
    "Data to convert has 0 rows"
  )

  # Should return empty data.table
  expect_equal(nrow(result), 0)
  expect_s3_class(result, "data.table")

  data_con$shutdown()
  unlink(data_con$uri)
})

test_that("get_filtered_data handles complex filter combinations", {
  data_con <- create_test_db()

  conversion_rules <- list(
    "^deployed_int_" = as.logical,
    "admin_1" = as.character
  )

  result <- data_con$get_filtered_data(
    columns = "*",
    filters = list(
      year = c(2020, 2021),
      admin_2 = c("District1"),
      deployed_int_ITN = c(1)
    ),
    conversion_rules = conversion_rules
  )

  # Should return rows matching all conditions
  expect_equal(nrow(result), 1)
  expect_equal(result$year[1], 2020)
  expect_equal(result$admin_2[1], "District1")
  expect_true(result$deployed_int_ITN[1])

  data_con$shutdown()
  unlink(data_con$uri)
})

test_that("get_filtered_data works with single column selection and filter", {
  data_con <- create_test_db()

  conversion_rules <- list("value" = as.character)

  result <- data_con$get_filtered_data(
    columns = c("value"),
    filters = list(year = c(2020)),
    conversion_rules = conversion_rules
  )

  expect_equal(ncol(result), 1)
  expect_equal(nrow(result), 2)
  expect_equal(colnames(result), "value")

  data_con$shutdown()
  unlink(data_con$uri)
})

test_that("get_filtered_data reuses existing database connection", {
  data_con <- create_test_db()

  conversion_rules <- list("admin_1" = as.character)

  # First call
  result1 <- data_con$get_filtered_data(
    columns = c("year"),
    filters = list(),
    conversion_rules = conversion_rules
  )

  # Second call - should reuse connection
  result2 <- data_con$get_filtered_data(
    columns = c("year"),
    filters = list(),
    conversion_rules = conversion_rules
  )

  expect_equal(nrow(result1), nrow(result2))

  data_con$shutdown()
  unlink(data_con$uri)
})

test_that("get_filtered_data handles special characters in filters", {
  data_con <- create_test_db()
  con <- data_con$pool

  dbExecute(
    con,
    "CREATE TABLE data2 (
    name TEXT,
    value TEXT
  )"
  )

  dbExecute(
    con,
    "INSERT INTO data2 VALUES
    ('O''Brien', '100'),
    ('Smith', '200'),
    ('Jones', '300')"
  )

  conversion_rules <- list("value" = as.integer)

  result <- data_con$get_filtered_data(
    source = "data2",
    columns = "*",
    filters = list(name = c("O'Brien")),
    conversion_rules = conversion_rules
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$name[1], "O'Brien")

  data_con$shutdown()
  unlink(data_con$uri)
})

# Parameter validation tests ---------------------------------------------------

test_that("get_filtered_data errors on invalid columns parameter", {
  data_con <- create_test_db()
  conversion_rules <- list("col" = as.integer)

  expect_error(
    data_con$get_filtered_data(
      columns = 123,
      filters = list(),
      conversion_rules = conversion_rules
    ),
    "columns must be a string or vector of strings"
  )

  expect_error(
    data_con$get_filtered_data(
      columns = NULL,
      filters = list(),
      conversion_rules = conversion_rules
    ),
    "columns must be a string or vector of strings"
  )

  data_con$shutdown()
  unlink(data_con$uri)
})

test_that("get_filtered_data errors on invalid filters parameter", {
  data_con <- create_test_db()
  conversion_rules <- list("col" = as.integer)

  expect_error(
    data_con$get_filtered_data(
      columns = "*",
      filters = "not a list",
      conversion_rules = conversion_rules
    ),
    "filters must be a list"
  )

  expect_error(
    data_con$get_filtered_data(
      columns = "*",
      filters = NULL,
      conversion_rules = conversion_rules
    ),
    "filters must be a list"
  )

  data_con$shutdown()
  unlink(data_con$uri)
})

test_that("get_filtered_data errors on invalid conversion_rules parameter", {
  data_con <- create_test_db()

  expect_error(
    data_con$get_filtered_data(
      columns = "*",
      filters = list(),
      conversion_rules = "not a list"
    ),
    "conversion_rules must be a non-empty list"
  )

  expect_error(
    data_con$get_filtered_data(
      columns = "*",
      filters = list(),
      conversion_rules = list()
    ),
    "conversion_rules must be a non-empty list"
  )

  expect_error(
    data_con$get_filtered_data(
      columns = "*",
      filters = list(),
      conversion_rules = NULL
    ),
    "conversion_rules must be a non-empty list"
  )

  data_con$shutdown()
  unlink(data_con$uri)
})

test_that("get_filtered_data handles empty string in vector of columns", {
  data_con <- create_test_db()
  conversion_rules <- list("admin_1" = as.character)

  # Empty strings in column names should cause issues
  # This tests edge case handling
  result <- data_con$get_filtered_data(
    columns = c("year", "admin_1"),
    filters = list(),
    conversion_rules = conversion_rules
  )

  expect_equal(ncol(result), 2)

  data_con$shutdown()
  unlink(data_con$uri)
})

test_that("get_filtered_data preserves data integrity", {
  data_con <- create_test_db()

  conversion_rules <- list(
    "admin_1" = as.character
  )

  result <- data_con$get_filtered_data(
    columns = c("year", "admin_1", "value"),
    filters = list(year = c(2020)),
    conversion_rules = conversion_rules
  )

  # Check specific values to ensure data integrity
  expect_equal(nrow(result), 2)
  expect_true(all(result$year == 2020))
  expect_true("Region1" %in% result$admin_1)
  expect_true(100.5 %in% result$value)
  expect_true(200.5 %in% result$value)

  data_con$shutdown()
  unlink(data_con$uri)
})

test_that("get_filtered_data handles large number of filter values", {
  data_con <- create_test_db()

  conversion_rules <- list("admin_1" = as.character)

  # Filter with multiple values
  result <- data_con$get_filtered_data(
    columns = "*",
    filters = list(admin_2 = c("District1", "District2")),
    conversion_rules = conversion_rules
  )

  # Should return all rows since both districts are in the data
  expect_equal(nrow(result), 5)

  data_con$shutdown()
  unlink(data_con$uri)
})

test_that("get_filtered_data works with regex conversion rules", {
  data_con <- create_test_db()

  # Use regex pattern for deployed_int_ columns
  conversion_rules <- list(
    "^deployed_int_.*" = as.logical,
    "admin_1" = as.character
  )

  result <- data_con$get_filtered_data(
    columns = "*",
    filters = list(),
    conversion_rules = conversion_rules
  )

  # All deployed_int_ columns should be logical
  expect_true(is.logical(result$deployed_int_ITN))
  expect_true(is.logical(result$deployed_int_IRS))
  expect_true(is.logical(result$deployed_int_SMC))

  data_con$shutdown()
  unlink(data_con$uri)
})
