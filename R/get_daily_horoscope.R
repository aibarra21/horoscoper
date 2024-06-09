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
   zodiacsign <- tolower(zodiacsign)

  if (zodiacsign %in% horoscope_data$sign) {
    daily <- horoscope_data[horoscope_data$sign == zodiacsign, "horoscope"]
    output<- glue::glue("Sign: {stringr::str_to_title(zodiacsign)}
               Daily Horoscope: {daily}" ) #prints wanted sign and horoscope
    return(output)
  }

  else {
    return("Invalid zodiac sign. Please ensure correct spelling and try again.")  #if spelled wrong or capitalized then print error
  }
}


# helper function to get only one horoscope by webscrapping horoscope.com

get_onedaily_horoscope <- function(page_number){
  Sys.sleep(1)

  url <- paste0("https://www.horoscope.com/us/horoscopes/general/horoscope-general-daily-today.aspx?sign=", page_number)
  sign_link <- rvest::read_html(url)   # reading the url

  sign <- sign_link %>%  # getting the sign from the webpage
    rvest::html_node("h1") %>%
    rvest::html_text(trim = TRUE) %>%
    stringr::str_replace_all("Horoscope", "") %>% #trimming excess words
    tolower() %>%
    stringr::str_trim()


  daily_horoscope <- sign_link %>%        # reading in the daily horoscope
    rvest::html_node(".switcher+ p") %>%
    rvest::html_text(trim = TRUE)

  horoscope_info <- data.frame(sign, daily_horoscope)   #putting into dataframe
  names(horoscope_info) <- c("sign","horoscope")
  return(horoscope_info) # return that sign
}


# Making data frame with the 12 zodiac signs and their respective daily horoscopes
sign_numbers <- 1:12
horoscope_list <- lapply(sign_numbers, get_onedaily_horoscope)  #using lapply for vectorization
horoscope_data <- bind_rows(horoscope_list)  #combining all the data frames into one

print(horoscope_data) #viewing final dataframe
