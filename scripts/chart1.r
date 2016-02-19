#Hello World
require(plotly)
require(dplyr)
require(magrittr)

makeVis <- function(data){
  group_by(data, class) %>% summarize(Number = length(class))
}