mock_horoscope_data <- data.frame(
  sign = c("Aries", "Taurus"),
  horoscope = c("Today is a good day.", "Today is a lucky day.")
)

# Mock the getdailyhoroscope function
test_that("getdailyhoroscope prints correct horoscope", {
  # Mock the horoscope_data
  assignInNamespace("horoscope_data", mock_horoscope_data, ns = asNamespace("horoscoper"))

  # Test valid zodiac sign
  expect_output(getdailyhoroscope("Aries"), "Sign: Aries \nDaily Horoscope: Today is a good day.") # Adjusted expected output

  # Test invalid zodiac sign
  expect_output(getdailyhoroscope("invalid"), "Invalid zodiac sign. Please ensure correct spelling and try again.")
})
