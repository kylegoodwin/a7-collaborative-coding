# This function creates a line graph that shows average experiences based on operating systems.
# Using this graph we can compare which OS users tend to have more experience in a particular tool.
# According to the data, the Mac users tend to have slightly more experience in
# Programming, Command Line, and Markdown; however, the Windows users tend to have more experience in R.
# This graph also shows that overall, students tend to have more experience in general programing,
# command line, version control, R, and Markdown in order.

chart2 <- function(d) {
  
  # requires package
  library(dplyr)
  library(plotly)
  library(knitr)
  
  # manipulates data
  d <- data[3:8]
  names(d)[1] <- "OS"
  names(d)[2] <- "Command_Line"
  names(d)[3] <- "Version_Control"
  names(d)[4] <- "Markdown"
  names(d)[5] <- "R"
  names(d)[6] <- "Programming_Experience"
  levels(d$Command_Line) <- c(levels(d$Command_Line), c(0,1,2))
  d$Command_Line[d$Command_Line == "Never used it"] <- 0
  d$Command_Line[d$Command_Line == "Have used it a few times"] <- 1
  d$Command_Line[d$Command_Line == "Intermediate user"] <- 2
  levels(d$Version_Control) <- c(levels(d$Version_Control), c(0,1,2))
  d$Version_Control[d$Version_Control == "Never used it"] <- 0
  d$Version_Control[d$Version_Control == "Have used it a few times"] <- 1
  d$Version_Control[d$Version_Control == "Intermediate user"] <- 2
  levels(d$Markdown) <- c(levels(d$Markdown), c(0,1,2))
  d$Markdown[d$Markdown == "Never used it"] <- 0
  d$Markdown[d$Markdown == "Have used it a few times"] <- 1
  d$Markdown[d$Markdown == "Intermediate user"] <- 2
  levels(d$R) <- c(levels(d$R), c(0,1,2))
  d$R[d$R == "Never used it"] <- 0
  d$R[d$R == "Have used it a few times"] <- 1
  d$R[d$R == "Intermediate user"] <- 2
  levels(d$Programming_Experience) <- c(levels(d$Programming_Experience), c(0,1,2,3))
  d$Programming_Experience[d$Programming_Experience == "Never written code"] <- 0
  d$Programming_Experience[d$Programming_Experience == "Experimented a bit with simple programming"] <- 1
  d$Programming_Experience[d$Programming_Experience == "Moderate experience with a scripting language (Python, R, JavaScript, Java, etc.)"] <- 2
  d$Programming_Experience[d$Programming_Experience == "Lots of experience with a scripting language (Python, R, JavaScript, Java, etc.)"] <- 3
  levels(d$OS) <- c(levels(d$OS), c("Other"))
  d$OS[d$OS == "Other: Windows at home, Linux (Ubuntu) at work"] <- "Other"
  
  # groups the data and summarize
  d2 <- group_by(d, OS) %>%
        summarise(Command_Line = mean(as.numeric(as.character(Command_Line))),
              Version_Control = mean(as.numeric(as.character(Version_Control))),
              Markdown = mean(as.numeric(as.character(Markdown))),
              R = mean(as.numeric(as.character(R))),
              Programming_Experience = mean(as.numeric(as.character(Programming_Experience)))
    )
  
  # draws the graph
  graph <-plot_ly(d2, x = OS, y = Programming_Experience, type = "line", name = "Programming Experience") %>%
    add_trace(x = OS, y = Command_Line, type = 'line', name = "Command Line") %>%
    add_trace(x = OS, y = Version_Control, type = 'line', name = "Version Control") %>%
    add_trace(x = OS, y = Markdown, type = 'line', name = "Markdown") %>%
    add_trace(x = OS, y = R, type = 'line', name = "R") %>%
    layout(title = "Average Experiences<br>(Based on Operating Systems)",
           yaxis = list(title = "Experience Level<br>0(never) ~ 2(intermidiate)"),
           xaxis = list(title = "Operating Systems")
    )
  return(graph)
}