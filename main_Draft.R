library(data.table)
library(dplyr)
library(assertthat)
library(ggplot2)


#'
#'
#'
upload_meeting_usage_report <- function(path = 'data/participants_88515606254.csv') {
  # test path is avaliabe
  assert_that(file.exists(path), msg = "File not found")
  # Read the path 
  meeting_usage_header <- data.table::fread(path,nrows = 1)
  setnames(meeting_usage_header,old = "Duration (minutes)", new = "host_Duration_minutes")
  meeting_usage_data <- data.table::fread(path,skip = 1)
  setnames(meeting_usage_data,old = "Duration (minutes)", new = "participant_Duration_minutes")
  meeting.report <- cbind(meeting_usage_data,meeting_usage_header)
  return(meeting.report)
}

#'
#'
f <- list.files(path = "data", pattern = "participants", full.names = TRUE,recursive = T)

dipush.db <- 
  lapply(f, upload_meeting_usage_report) %>%
    rbindlist()

# write POSIXct to 11/05/2024 08:54:30 PM
dipush.db$`Start time` <- 
  as.POSIXct(dipush.db$`Start time`, format = "%m/%d/%Y %I:%M:%S %p")

dipush.db$`End time` <- 
  as.POSIXct(dipush.db$`End time`, format = "%m/%d/%Y %I:%M:%S %p")

dipush.db <- 
  dipush.db[order(`Start time`)]


participants.freq.view <- 
  unique(
    dipush.db[,list(`Name (original name)`, `Start time`,`End time`)]
    )[,.N,by = `Name (original name)`][order(N)] 

#figure.of.participants.freq.view
ggplot2::ggplot(participants.freq.view, aes(x = N)) +
  ggplot2::geom_histogram(binwidth = 1, fill = "navy",
                          color = "black", alpha = .4) +
  ggplot2::labs(title = "Participants Frequency",
                x = "Frequency of Meetings per Participant",
                y = "Count") +
  ggplot2::theme_minimal()

# plot trend line of participants with fill points and filled area under the line
p <- 
ggplot2::ggplot(dipush.db[`In waiting room` == 'No'], aes(x = `Start time`, y = Participants)) +
  ggplot2::geom_line(color = "navy") +
  ggplot2::geom_point(color = "navy", fill = "navy") +
  ggplot2::geom_area(fill = "navy", alpha = .4) +
  ggplot2::labs(title = "Participants Trend",
                x = "Meeting Start Time",
                y = "Participants") +
  ggplot2::theme_minimal() 

plotly::ggplotly(p)


# cumsum of the participants
part.dt <- 
  unique(dipush.db[,list(`Start time`,Name = `Name (original name)`,tmp = 1)])
part.dt[,freq_till_now := cumsum(tmp),by = Name]
part.dt[,tmp:= NULL]

freq.over.time <- 
  part.dt[,.N,by = c("freq_till_now","Start time")][order(`Start time`)]
# plot the cumsum of the participants
freq.over.time$freq_till_now <- as.factor(freq.over.time$freq_till_now) 
ggplot2::ggplot(freq.over.time[freq_till_now %in% c(2,3)],
                aes(x = `Start time`,
                    y = N,
                    group =1,
                    color = freq_till_now,
                    fill =  freq_till_now)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::geom_area(alpha = .4) +
  ggplot2::labs(title = "Participants Cumulative Sum",
                x = "Meeting Start Time",
                y = "Cumulative Sum of Participants") +
  ggplot2::theme_minimal() + facet_wrap(~freq_till_now)




