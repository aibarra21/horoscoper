test_that("get_zodiac_sign works", {

  correct_result <- "Your zodiac sign is Aries"

  my_result <- get_zodiac_sign("April 10 2002")

  expect_equal(my_result, correct_result)

})

test_that("get_zodiac_sign works", {

  correct_result <- "Your zodiac sign is Taurus"

  my_result <- get_zodiac_sign("April 24 2002")

  expect_equal(my_result, correct_result)

})
