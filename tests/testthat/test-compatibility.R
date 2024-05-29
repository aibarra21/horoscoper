
#test for valid sign inputs


test_that("compatibility function returns error message for invalid sign inputs", {
  my_result <- compatibility("TestSign", "Leo")
  correct_result <- "Invalid Zodiac Signs entered. Please check for correct spelling and capitalization.\n"
  expect_equal(result, correct_result)

  my_result1 <- compatibility("Aries", "TestSign")
  expect_equal(my_result1, correct_result)

  my_result2 <- compatibility("TestSign", "TestSign")
  expect_equal(my_result2,correct_result)
})

test_that("compatibility function is case-sensitive", {
  result <- compatibility("virgo", "cancer")
  expect_equal(result, "Invalid Zodiac Signs entered. Please check for correct spelling and capitalization.\n")
})

