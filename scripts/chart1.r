#Hello World
require(plotly)
require(dplyr)
require(magrittr)

chart1 <- function(data){
  data_byclass <- group_by(data, class) %>% summarize(NumberOfStudents = length(class))
  return(hist(data_byclass));
}