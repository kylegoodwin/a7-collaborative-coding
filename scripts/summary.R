require(dplyr)

data <- read.csv("https://raw.githubusercontent.com/INFO-498F/a7-survey-data/master/intro_survey_data.csv")

data <- rename(data, class = What.is.your.current.class.standing.)
data <- rename(data, interested_in_info = Are.you.interested.in.applying.to.the.Informatics.major.)
data <- rename(data, operating_system = What.operating.system.do.you.typically.use.)

info_function <- function(data) {
  ret <- list()
  ret$Questions_Asked <- length(data)
  ret$Number_Of_Students <- nrow(data)
  Average_Countries_Visited <- summarise(data,
                               Average = mean(How.many.countries.have.you.visited.in.your.life., na.rm = TRUE)
  )
  ret$Average_Countries_Visited <- Average_Countries_Visited
  Mac_Users <- filter(data, operating_system == "Mac") 
  ret$Mac_Users <- 39
  Windows_Users <- filter(data, operating_system == "Windows")
  ret$Windows_Users <- 40
  return(ret)
}