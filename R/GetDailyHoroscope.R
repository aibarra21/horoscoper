#' Tells a daily horoscope
#'
#'
#' @param zodiac the person's zodiac sign
#' @importFrom rvest read_html html_node html_text
#' @importFrom stringr str_replace_all str_trim
#' @return A horoscope string
#'



# helper function to get only one horoscope by webscrapping horoscope.com

get_onedaily_horoscope <- function(page_number){

  url <- paste0("https://www.horoscope.com/us/horoscopes/general/horoscope-general-daily-today.aspx?sign=", page_number)
  sign_link <- rvest::read_html(url)   # reading the url

  sign <- sign_link %>%  # getting the sign from the webpage
    rvest::html_node("h1") %>%
    rvest::html_text(trim = TRUE)
  sign <- stringr::str_replace_all(sign, "Horoscope", "") #trimming excess words

  daily_horoscope <- sign_link %>%        # reading in the daily horoscope
    rvest::html_node(".switcher+ p") %>%
    rvest::html_text(trim = TRUE)

  horoscope_info <- data.frame(sign, daily_horoscope[1])   #putting into dataframe
  names(horoscope_info) <- c("sign","horoscope")
  return(horoscope_info) # return that sign
}


