
#test for valid sign inputs


test_that("compatibility function returns error message for invalid sign inputs", {

  correct_result <- "Invalid Zodiac Signs entered. Please check for correct spelling and punctuation"

  my_result <- compatibility("TestSign", "Leo")
  expect_equal(my_result, correct_result)

  my_result1 <- compatibility("Test", "Test")
  expect_equal(my_result1, correct_result)

  my_result2 <- compatibility("Leo", "Test")
  expect_equal(my_result2, correct_result)


})



