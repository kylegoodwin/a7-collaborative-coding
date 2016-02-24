
#Add Needed Libraries
require(dplyr)
require(magrittr)

#Charting function: Makes a pie chart that shows the distribution of different class rankings in INFO 498f
chart1 <- function(data){
  names(data)[1] <- 'class'
  
  #Get data for each class
  freshman_data <- filter(data, class == 'Freshman')
  sophomore_data <- filter(data, class == 'Sophomore')
  junior_data <- filter(data,class == 'Junior')
  senior_data <- filter(data, class == 'Senior')
  
  #Get length of data from each class ranking
  nums <- c(length(freshman_data$class),length(sophomore_data$class),
         length(junior_data$class),length(senior_data$class))
  
  #Sum this data for percentages
  sum <- sum(nums)
  
  #Put in array for ploting
  lbs <- c( paste0("Freshman: ", round(nums[1]/sum*100,1),'%'),paste0("Sophomore: ",round(nums[2]/sum*100,1), '%'), paste0("Junior: ",round(nums[3]/sum*100,1), 
           '%'),paste("Senior: ", round(nums[1]/sum*100,1),'%'))
  
#Return a piechart of the rankings
return(pie(nums,labels = lbs,main="Distribution of Class Rankings"))
  
}