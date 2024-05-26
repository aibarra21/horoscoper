#Gives someone their compatibility with another sign

#@param your_sign The person's sign
#@param their_sign The other person's sign

#@ Gives user prompt for inputs with instructions
#@ Gives response message if user error with input
#@ Regardless of capitalization, but spelling important

# @return A compatibility message string from webscraped website
# Url https://www.horoscope.com/us/index.aspx
#https://www.horoscope.com/us/horoscopes/general/horoscope-general-daily-today.aspx?sign=12

# @export

library(tidyverse)
library(rvest)


#implement a compatibility scraping function as helper function
#sign = 1:12 at the end of url
#.list_item gives all sign compatibilities
#make one df with all, keep unique

sign_number <- 12

scrape_comp_page <- function(sign_number) {

  #consider pages for all 12 signs

  url <- paste0("https://www.horoscope.com/us/horoscopes/general/horoscope-general-daily-today.aspx?sign=", sign_number) #consider all sign pages
  webpage <- read_html(url)

  print(url)


  #define sign items

  sign_items <- webpage %>%
    html_node(".list_item a") %>%
    html_text(trim = TRUE)

  #initialize vectors

  sign_v_sign_names <- vector("character", length(sign_items))
  compatibility_desc <- vector("character", length(sign_items))
  compatibility_score <- vector("character", length(sign_items))

  # for each sign item:

  for (x in seq_along(sign_items)) {
    sign_v_sign_names[x] <- sign_items[x] %>%
      html_node(".list_item") %>%
        html_text(trim = true)

    # scrape compatibility attributes

    compatibility_desc[x] <- sign_items[x] %>%
      html_node("div p") %>% # or div p
        html_text(trim = TRUE)

     compatibility_score[x] <- sign_items[x] %>%
       html_node("div b") %>%
        html_text(trim = TRUE)

  }


  #create a data frame for this sign page

  data_frame(
    SignvSign = sign_v_sign_names,
    Url = urls
  )

  #return data_frame?

}


# create one data frame with all compatibilities

all_comp <- map_df(1:12, scrape_comp_page)

get_compatibility <- function(your_sign, their_sign) {

  #check how sign names are formatted
  #split sign v sign into two signs: 1 and 2
  #then check for the presence of two in the get_compatibility function
  #use helper function to check





}
