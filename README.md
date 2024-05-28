
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Horoscoper

<!-- badges: start -->
<!-- badges: end -->

The goal of **horoscoper** is to provide detailed information about
zodiac signs and horoscopes.  
This package uses webscrapping to get the horoscope information from
[horoscope.com](https://www.horoscope.com/us/index.aspx).

## Installation

You can install the the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("aibarra21/horoscoper")
#> Using GitHub PAT from the git credential store.
#> Skipping install of 'horoscoper' from a github remote, the SHA1 (d287ed5c) has not changed since last install.
#>   Use `force = TRUE` to force installation
```

``` r
library(horoscoper)
```

## Zodiac Sign

You can use the function get_zodiac_sign() to determine what zodiac sign
you are by entering your birthday. Enter the month, day, then year in
any format.

``` r
get_zodiac_sign("april 24 2002")
#> [1] "Your zodiac sign is Taurus"
```

You can use the function get_sign_traits() to determine traits and
characteristics of your sign.

``` r
get_sign_traits("taurus")
#> Sign: Taurus
#> Symbol: Bull
#> Lucky Gem: Emerald
#> Flower: Rose, Poppy, & Foxglove
#> You are described as Dependable, Musical, Practical
#> You are most compatible with Cancer.
#> Motto: "Nothing worth having comes easy."
```

## Daily Horoscope

You can use the function getdailyhoroscope() to get your daily horoscope
for your zodiac. This function updates daily.

``` r
getdailyhoroscope("taurus")
#> Sign: Taurus 
#> Daily Horoscope: May 28, 2024 - Make sure there's an equal amount of give and take in your day, Taurus. Sometimes when you love someone, you just want to keep giving and giving to demonstrate your incredible love. Be sure that you aren't draining yourself of valuable energy that you need for yourself, because maintaining your health is absolutely critical now. Also be sure that the people on the receiving end are also giving their fair share in return.
```

## Compatability

You can use the function compatibility() to determine how compatibile
any two zodian signs are.

``` r
# compatibility()
```
