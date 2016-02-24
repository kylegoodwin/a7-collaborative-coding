
require(plotly)
require(dplyr)
require(magrittr)

chart1 <- function(data){
  names(data)[1] <- 'class'
  freshman_data <- filter(data, class == 'Freshman')
  sophomore_data <- filter(data, class == 'Sophomore')
  junior_data <- filter(data,class == 'Junior')
  senior_data <- filter(data, class == 'Senior')
  
  nums <- c(length(freshman_data$class),length(sophomore_data$class),
         length(junior_data$class),length(senior_data$class))
  
  sum <- sum(nums)
  
  lbs <- c( paste0("Freshman: ", round(nums[1]/sum*100,1),'%'),paste0("Sophomore: ",round(nums[2]/sum*100,1), '%'), paste0("Junior: ",round(nums[3]/sum*100,1), 
           '%'),paste("Senior: ", round(nums[1]/sum*100,1),'%'))
  

return(pie(nums,labels = lbs,main="Distribution of Class Rankings"))
  
}