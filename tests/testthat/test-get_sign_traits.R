test_that("get_sign_traits works", {

  correct_result <- "Sign: Aries\nSymbol: The Ram\nLucky Gem: Diamond\nFlower: Thistle & honeysuckle\nYou are described as Ambitious, Independent, Impatient.\nYou are most compatible with Sagittarius.\nMotto: \"When you know yourself, you're empowered. When you accept yourself, you're invincible.\""

  my_result <- get_sign_traits("aries")

  print(my_result)

  expect_equal(my_result, correct_result)

})

