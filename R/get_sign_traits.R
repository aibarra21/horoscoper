#' Tells a daily horoscope
#'
#' @param
#' @importFrom rvest read_html html_node html_text
#' @return
#' @export



get_sign_traits <- function(zodiacsign) {

  signdf <- sign_traits(zodiacsign)

  cat("Sign:", stringr::str_to_title(zodiacsign),
      "\nSymbol:", signdf$symbol,
      "\nLucky Gem:",signdf$luckygem,
      "\nFlower:", signdf$flower,
      "\nYou are described as", signdf$traits,
      "\nYou are most compatible with", signdf$top_match,
      "\nMotto:", signdf$motto)

}


sign_traits <- function(zodiacsign) {

  url <- paste0("https://www.horoscope.com/zodiac-signs/", zodiacsign)
  sign_link <- rvest::read_html(url)   # reading the url

  symbol <- sign_link %>%
    rvest::html_node(".title h4") %>%
    rvest::html_text(trim = TRUE)
  symbol <- stringr::str_split(symbol, "\\|", simplify = TRUE)

  luckygem <- sign_link %>%
    rvest::html_node(".facts div+ div h3:nth-child(2) , .facts div+ div h3:nth-child(2) strong") %>%
    rvest::html_text(trim = TRUE)
  luckygem <- stringr::str_remove(luckygem, "Lucky Gem: ")

  flower <- sign_link %>%
    rvest::html_node(".facts div+ div h3:nth-child(3)") %>%
    rvest::html_text(trim = TRUE)
  flower <- stringr::str_remove(flower, "Flower: ")

  traits <- sign_link %>%
    rvest::html_node(".traits") %>%
    rvest::html_text(trim = TRUE)
  traits <- stringr::str_to_title(traits)

  top_match <- sign_link %>%
    rvest::html_node(".facts a") %>%
    rvest::html_text(trim = T)

  motto <- sign_link %>%
    rvest::html_node("blockquote") %>%
    rvest::html_text(trim = T)

  sign_info <- data.frame(sign, symbol[1], luckygem, flower,
                          traits, top_match, motto)
  return(sign_info)
}

