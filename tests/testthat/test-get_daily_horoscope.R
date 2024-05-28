test_that("getdailyhoroscope(aries) returns a string starting with today's date", {
  # Call the function
  result <- getdailyhoroscope("aries")

  date_pattern <- "\\b[A-Z][a-z]+ \\d{1,2}, \\d{4}\\b"

  # Extract the date string
  date_string <- stringr::str_extract(result, date_pattern)

  # Convert to Date object
  date <- lubridate::mdy(date_string)

  # Convert to Date object
  date_result <- lubridate::ymd(date)

  # Check if the date part is today
  expect_equal(date_result, lubridate::today())
})

test_that("getdailyhoroscope(taurus) returns a string starting with today's date", {
  # Call the function
  result <- getdailyhoroscope("Taurus")

  date_pattern <- "\\b[A-Z][a-z]+ \\d{1,2}, \\d{4}\\b"

  # Extract the date string
  date_string <- stringr::str_extract(result, date_pattern)

  # Convert to Date object
  date <- lubridate::mdy(date_string)

  # Convert to Date object
  date_result <- lubridate::ymd(date)

  # Check if the date part is today
  expect_equal(date_result, lubridate::today())
})

test_that("getdailyhoroscope() returns invalid string when misspell", {

  correct <- "Invalid zodiac sign. Please ensure correct spelling and try again."

  expected <- getdailyhoroscope("Tarus")

  # Check if the date part is today
  expect_equal(expected, correct)
})
