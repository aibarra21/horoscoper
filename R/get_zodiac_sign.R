#' Gives zodiac sign
#'
#' @param user_date the person's birthday
#' @importFrom rvest read_html
#' @importFrom lubridate mdy month day
#' @return the zodiac sign
#' @export
#'


get_zodiac_sign <- function(user_date){

  #This converts the user inputted date using the mdy() function from lubridate
  #and saves it to a "date" variable
  date <- lubridate::mdy(user_date)

  #This extracts the month and day from the date variable
  month <- lubridate::month(date)
  day <- lubridate::day(date)

  #Determines the zodiac sign based off of the month and day variables
  #and returns the user's zodiac sign
  if((month == 1 && day >= 20 || month == 2 && day <= 18))
    return("Your zodiac sign is Aquarius")

  else {
    if((month == 2 && day >= 19 || month == 3 && day <= 20))
      return("Your zodiac sign is Pisces")

    else {
      if((month == 3 && day >= 21 || month == 4 && day <= 19))
        return("Your zodiac sign is Aries")

      else {
        if((month == 4 && day >= 20 || month == 5 && day <= 20))
          return("Your zodiac sign is Taurus")

        else {
          if((month == 5 && day >= 21 || month == 6 && day <= 20))
            return("Your zodiac sign is Gemini")

          else {
            if((month == 6 && day >= 21 || month == 7 && day <= 22))
              return("Your zodiac sign is Cancer")

            else {
              if((month == 7 && day >= 23 || month == 8 && day <= 22))
                return("Your zodiac sign is Leo")

              else {
                if((month == 8 && day >= 23 || month == 9 && day <= 22))
                  return("Your zodiac sign is Virgo")

                else {
                  if((month == 9 && day >= 23 || month == 10 && day <= 22))
                    return("Your zodiac sign is Libra")

                  else {
                    if((month == 10 && day >= 23 || month == 11 && day <= 21))
                      return("Your zodiac sign is Scorpio")

                    else {
                      if((month == 11 && day >= 22 || month == 12 && day <= 21))
                        return("Your zodiac sign is Sagittarius")

                      else {
                        if((month == 12 && day >= 22 || month == 1 && day <= 19))
                          return("Your zodiac sign is Capricorn")

                        else {
                          return(NULL)
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }

}


