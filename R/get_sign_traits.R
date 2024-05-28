#' Tells sign traits
#'
#' @param zodiac the person's zodiac sign
#' @importFrom rvest read_html html_node html_text
#' @importFrom stringr str_split str_remove str_to_title
#' @return A string with all sign information
#' @export
#'
#' @examples
#' get_sign_traits("aries)


# function that prints out the zodiac sign trait information
get_sign_traits <- function(zodiac) {
  zodiac <- tolower(zodiac) # making the zodiac lowercase

  signdf <- sign_traits(zodiac) # running helperfunc to webscrape

  #printing the zodiac information
  cat("Sign:", stringr::str_to_title(zodiac),
      "\nSymbol:", signdf$symbol,
      "\nLucky Gem:",signdf$luckygem,
      "\nFlower:", signdf$flower,
      "\nYou are described as", signdf$traits,
      "\nYou are most compatible with", signdf$top_match,
      "\nMotto:", signdf$motto)
}



# helper function that webscrapes horoscope.com and stores the zodiac info
sign_traits <- function(zodiac) {

  # reading in the url to webscrape from
  url <- paste0("https://www.horoscope.com/zodiac-signs/", zodiac)
  sign_link <- rvest::read_html(url)   # reading the url

  # getting the symbol info
  symbol <- sign_link %>%
    rvest::html_node(".title h4") %>%
    rvest::html_text(trim = TRUE)
  symbol <- stringr::str_split(symbol, "\\|", simplify = TRUE)

  # getting the gem info
  luckygem <- sign_link %>%
    rvest::html_node(".facts div+ div h3:nth-child(2) , .facts div+ div h3:nth-child(2) strong") %>%
    rvest::html_text(trim = TRUE)
  luckygem <- stringr::str_remove(luckygem, "Lucky Gem: ")

  # getting the flower info
  flower <- sign_link %>%
    rvest::html_node(".facts div+ div h3:nth-child(3)") %>%
    rvest::html_text(trim = TRUE)
  flower <- stringr::str_remove(flower, "Flower: ")

  # getting the 3 trait info
  traits <- sign_link %>%
    rvest::html_node(".traits") %>%
    rvest::html_text(trim = TRUE)
  traits <- stringr::str_to_title(traits)

  # getting compatible sign info
  top_match <- sign_link %>%
    rvest::html_node(".facts a") %>%
    rvest::html_text(trim = T)

  # getting motto
  motto <- sign_link %>%
    rvest::html_node("blockquote") %>%
    rvest::html_text(trim = T)

  # storing info into a dataframe
  sign_info <- data.frame(sign, symbol[1], luckygem, flower,
                          traits, top_match, motto)
  return(sign_info)
}

