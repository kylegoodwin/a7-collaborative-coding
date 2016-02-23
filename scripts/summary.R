require(dplyr)

data <- read.csv("https://raw.githubusercontent.com/INFO-498F/a7-survey-data/master/intro_survey_data.csv")

data <- rename(data, class = What.is.your.current.class.standing.)
data <- rename(data, interested_in_info = Are.you.interested.in.applying.to.the.Informatics.major.)
data <- rename(data, operating_system = What.operating.system.do.you.typically.use.)

info_function <- function(data) {
  ret <- list()
  ret$length <- length(data)
  # 
  return(ret)
}