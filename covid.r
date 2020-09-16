#covid.r
#-------
# Set things up
covid19.dir <- "~/Desktop/covid19/"

# Required packages
library(tidyverse)
library(lubridate)

get.data <- function(download=FALSE){
  # Download the current data
  if(download){
    nytimes.state.cv <- download.file("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv",paste0(covid19.dir,"covid19.csv"),method="curl")
    nytimes.county.cv <- download.file("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv",paste0(covid19.dir,"covid19-counties.csv"),method="curl")
    #rt.live <- download.file("https://d14wlfuexuxgcm.cloudfront.net/covid/rt.csv","rt.csv",method="curl")
  }
  
  # Set up the data
  covid19.s <- read.csv(paste0(covid19.dir,"covid19.csv"))
  covid19.s$date <- ymd(covid19.s$date)
  covid19.state <<- covid19.s
  covid19.c <- read.csv(paste0(covid19.dir,"covid19-counties.csv"))
  covid19.c$date <- ymd(covid19.c$date)
  covid19.county <<- covid19.c
}

capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

calc.data <- function(cdata){
  n.rows <- dim(cdata)[1]
  cases.new <- deaths.new <- rep(0,n.rows)
  cases.7day.mean <- deaths.7day.mean <- rep(NA,n.rows)
  cases.3day.mean <- deaths.3day.mean <- rep(NA,n.rows)
  calcdata <- cbind(cdata,cases.new,deaths.new,
                    cases.7day.mean,deaths.7day.mean,
                    cases.3day.mean,deaths.3day.mean)
  for(i in 2:dim(calcdata)[1]){
    calcdata$cases.new[i] <- calcdata$cases[i] - calcdata$cases[i-1]
    calcdata$deaths.new[i] <- calcdata$deaths[i] - calcdata$deaths[i-1]
    if (i > 2){
      calcdata$cases.3day.mean[i] <- mean(c(calcdata$cases.new[i-2],
                                            calcdata$cases.new[i-1],
                                            calcdata$cases.new[i]),na.rm=TRUE)
      calcdata$deaths.3day.mean[i] <- mean(c(calcdata$deaths.new[i-2],
                                             calcdata$deaths.new[i-1],
                                             calcdata$deaths.new[i]),na.rm=TRUE)
      if (i >6){
        calcdata$cases.7day.mean[i] <- mean(c(calcdata$cases.new[i-6],
                                              calcdata$cases.new[i-5],
                                              calcdata$cases.new[i-4],
                                              calcdata$cases.new[i-3],
                                              calcdata$cases.new[i-2],
                                              calcdata$cases.new[i-1],
                                              calcdata$cases.new[i]),na.rm=TRUE)
        calcdata$deaths.7day.mean[i] <- mean(c(calcdata$deaths.new[i-6],
                                               calcdata$deaths.new[i-5],
                                               calcdata$deaths.new[i-4],
                                               calcdata$deaths.new[i-3],
                                               calcdata$deaths.new[i-2],
                                               calcdata$deaths.new[i-1],
                                               calcdata$deaths.new[i]),na.rm=TRUE)
      }
    }
  }
  calcdata
}

plot.data <- function(plotdata,title="Plot"){
  
  plotdata.calc <- calc.data(plotdata)
  
  # Make the plot
  ggplot(plotdata.calc,aes(date,cases.new,group=1)) +
    geom_bar(stat="identity",fill="steelblue2") +
    ylab("New Daily Cases") +
    xlab("Date") +
    ggtitle(title) +
    geom_smooth(aes(date,cases.7day.mean,color="7-day average"),stat="identity",na.rm=TRUE) +
    geom_smooth(aes(date,cases.3day.mean,color="3-day average"),stat="identity",na.rm=TRUE) +
    scale_x_date(date_labels = "%b %d") +
    scale_color_manual(name="",values=c("mediumpurple3","red")) +
    theme(legend.pos="bottom",
          legend.key = element_rect(fill = "white"))
}

# Check if new data needs to be downloaded (is it older than today?)
data.check <- function(){
  cur.date <- Sys.Date()
  file.date <- substr(as.character(file.mtime(paste0(covid19.dir,"covid19.csv"))),1,10)
  if (file.date < cur.date){
    print("Getting new data")
    get.data(download=TRUE)
  } else get.data()
}

plot.state <- function(state.name){
  State.name <- capwords(state.name)
  data.check()
  plot.data(covid19.state[covid19.state$state==State.name,],State.name)
}

plot.county <- function(county.name,state.name){
  County.name <- capwords(county.name)
  State.name <- capwords(state.name)
  data.check()
  plot.data(covid19.county[covid19.county$state==State.name & covid19.county$county==County.name,],paste0(County.name," County, ",State.name))
}

### USAGE Examples
#plot.state("Wisconsin")
#plot.county("Dane","Wisconsin")