#' This function, given user input of two zodiac signs, gives someone their romantic compatibility score and a description of their compatibility with another sign
#' @param your_sign The person's sign
#' @param their_sign The other person's sign
#' @importFrom rvest read_html html_node html_text
#' @importFrom stringr str_replace_all str_trim str_to_title
#' @import tidyverse
#' @return A compatibility message string from webscraped website
#'
#' @export



#Main Compatibility Function

compatibility <- function(your_signs, their_signs) {

  # Convert signs to title case

  your_signs <- stringr::str_to_title(your_signs)
  their_signs <- stringr::str_to_title(their_signs)

  # Create a data frame of inputted signs
  input_data <- data.frame(your_sign = your_signs, their_sign = their_signs, stringsAsFactors = FALSE)

  # Merge input data with comp_data and find matches
  merged_data <- input_data %>%
    left_join(comp_data, by = c("your_sign" = "sign1", "their_sign" = "sign2"))

  # result message

  result <- merged_data %>%
    mutate(result_message = ifelse(!is.na(compatibility_score),
                                   glue::glue("Romantic Compatibility between {your_sign} and {their_sign}:
                                         Compatibility Score: {compatibility_score}
                                         Description: {selected_description}"),
                                   "Invalid Zodiac Signs entered. Please check for correct spelling and punctuation")) %>%
    pull(result_message)

  return(result)

}


#Helper Function for Websraping horoscope website

scrape_comp_page <- function(sign_number, sign_number2) {


  url <- paste0("https://www.horoscope.com/us/games/compatibility/game-love-compatibility.aspx?ZodiacSignSelector_alphastring=", sign_number, "&PartnerZodiacSignSelector_alphastring=", sign_number2) #consider all sign pages
  webpage <- rvest::read_html(url)


  #Define compatibility items from website

  sign_items1 <- webpage %>%
    rvest::html_nodes(".flex-center-inline div:nth-child(1) h3") %>%
    rvest::html_text(trim = FALSE)

  sign_items2 <- webpage %>%
    rvest::html_nodes(".icon-heart + div h3") %>%
    rvest::html_text(trim = FALSE)

  compatibility_score <- webpage %>%
    rvest::html_node("b") %>%
    rvest::html_text(trim = TRUE)

  compatibility_desc <- webpage %>%
    rvest::html_node(".text-center p") %>%
    rvest::html_text(trim = TRUE)

  # Extract first five rows of the description using another helper function

  compatibility_desc <- extract_description(compatibility_desc, n = 8)

  # Return compatibility info

  compatibility_info <- data.frame(sign_items1, sign_items2, compatibility_score, compatibility_desc, stringsAsFactors = FALSE)
  names(compatibility_info) <- c("sign1", "sign2", "compatibility_score", "selected_description")
  return(compatibility_info)

}

#Helper function for extracting compatibility description

extract_description <- function(text, n){
  sentences <- stringr::str_extract_all(text, "(.*?[.!])", simplify = TRUE)[1:n]
  return(paste(sentences, collapse = " "))
}




#Create final data frame

comp_data <- data.frame(sign1 = character(),
                        sign2 = character(),
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





