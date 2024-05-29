#' This function, given user input of two zodiac signs, gives someone their romantic compatibility score and a description of their compatibility with another sign

#' @param your_sign The person's sign
#' @param their_sign The other person's sign

# Gives user prompt for inputs with instructions
#Gives response message if user error with input
#capitalization and spelling important: first letter of sign is capitalized

#' @return A compatibility message string from webscraped website
#' @import rvest
#' @import stringr
#' @import tidyverse
#' @export


#Main Compatibility Function

compatibility <- function(your_sign, their_sign) {

  #Test for Signs in Matched Rows and return message

   matched_row <- comp_data %>%
      filter(sign1 == as.factor(your_sign) || sign2 == as.factor(their_sign))

    if (nrow(matched_row) > 0) {

      result <- paste0("Romantic Compatibility between", your_sign, "and", their_sign, ":\n",
                      "Compatibility Score out of 10:", matched_row$compatibility_score, "\n",
                      "Description:", matched_row$selected_description, "\n")

      return(result)
    }

   else {
    return("Invalid Zodiac Signs entered. Please check for correct spelling and capitalization.\n")
    }
  }

compatibility("Taurus","Aries")


#Helper Function for Websraping horoscope website

scrape_comp_page <- function(sign_number, sign_number2) {


  url <- paste0("https://www.horoscope.com/us/games/compatibility/game-love-compatibility.aspx?ZodiacSignSelector_alphastring=", sign_number, "&PartnerZodiacSignSelector_alphastring=", sign_number2) #consider all sign pages
  webpage <- rvest::read_html(url)


  #Define compatibility items from website

  sign_items1 <- webpage %>%
    html_nodes(".flex-center-inline div:nth-child(1) h3") %>%
    html_text(trim = FALSE)

  sign_items2 <- webpage %>%
    html_nodes(".icon-heart + div h3") %>%
    html_text(trim = FALSE)

  compatibility_score <- webpage %>%
    html_node("b") %>%
    html_text(trim = TRUE)

  compatibility_desc <- webpage %>%
    html_node(".text-center p") %>%
    html_text(trim = TRUE)

  # Extract first eight rows of the description using another helper function

  extract_description(compatibility_desc, n = 8)

  # Return compatibility info

  compatibility_info <- data.frame(sign_items1, sign_items2, compatibility_score, compatibility_desc)
  names(compatibility_info) <- c("sign1", "sign2", "compatibility_score", "selected_description")
  return(compatibility_info)

}

#Helper function for extracting compatibility description

extract_description <- function(text, n){
  sentences <- stringr::str_extract_all(text, "(.*?[.!])", simplify = TRUE)[1:n]
  return(paste(sentences, collapse = " "))
}


scrape_comp_page("0","1")

#Create final data frame

comp_data <- data.frame(signs = character(),
                        compatibility_score = character(),
                        selected_description = character(),
                        stringsAsFactors = FALSE)

#iterate through first signs and second signs
#return final data frame

for(z in 0:11){
  for (x in 0:11){
    compinfor <- scrape_comp_page(z,x)
    comp_data <- rbind(comp_data, compinfor)

  }
}





