#' Tells a daily horoscope
#'
#' @param zodiac the person's zodiac sign
#' @importFrom rvest read_html html_node html_text
#' @importFrom stringr str_replace_all str_trim
#' @return A horoscope string
#' @export
#'
#' @examples
#' getdailyhoroscope("Aries")

getdailyhoroscope <- function(zodiacsign) {

  if (zodiacsign %in% horoscope_data$sign) {
    daily <- horoscope_data[horoscope_data$sign == zodiacsign, "horoscope"]
    cat("Sign:", zodiacsign, "\nDaily Horoscope:", daily) #prints wanted sign and horoscope
  }

  else {
    cat("Invalid zodiac sign. Please capatilize and try again.")  #if spelled wrong or capitalized then print error
  }
}



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


# Building data frame with all 12 zodiac signs and their respective daily horoscopes

horoscope_data <- data.frame(sign = character(),
                             horoscope = character(),
                             stringsAsFactors = FALSE) #creating empty dataframe
for (sign in 1:12){
  horoscope_infor <- get_onedaily_horoscope(sign) #looping thru and getting each of 12 signs

  horoscope_infor <- as.data.frame(lapply(horoscope_infor, stringr::str_trim),
                                   stringsAsFactors = FALSE) # trimming whitespace

  horoscope_data <- rbind(horoscope_data, horoscope_infor) #adding sign to dataframe

  Sys.sleep(1) #adding a one second pause to not overload server
}
