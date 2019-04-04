# Loads Libraries.
packages <- c("RCurl", "XML")

# Checks for packages
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

# Creates archive dataframe if it does not already exist
df_list <- names(which(sapply(.GlobalEnv, is.data.frame))) 
if ("cancellations_archive" %in% df_list == FALSE) {
  cancellations_archive <- as.data.frame(matrix(ncol = 12, nrow = 0))
  colnames(cancellations_archive) <- c("date_text", "alert", "date", "line", "from", "orig_train_num", "orig_time", "new_train_num", "new_time", "reason", "late_min", "event")
  print("ARCHIVE CREATED")
}
df_list <- names(which(sapply(.GlobalEnv, is.data.frame)))

# Gets Train Alerts Feed
feed <- "https://www.njtransit.com/rss/RailAdvisories_feed.xml"
script <- getURL(feed)
doc <- xmlParse(script)
date_text <- xpathSApply(doc,'//item/title',xmlValue)
alert <- xpathSApply(doc,'//item/description',xmlValue)
alerts <- as.data.frame(cbind(date_text, alert))

# Filters row by cancellations and updates
for (row in 1:nrow(alerts)) {
  alert <- as.character(alerts$alert[row])
  alert <- gsub("[[:punct:] ]+"," ",alert) 
  words <- strsplit(alert, " ")[[1]]
  if ("Update" %in% words) {
    alerts$update[row] <- 1
  } else {alerts$update[row] <- 0}
  
  if ("cancelled" %in% words) {
    alerts$cancelled[row] <- 1
  } else {
    alerts$cancelled[row] <- 0
  }
}
cancellations <- alerts[which(alerts$cancelled == 1 | alerts$update == 1),]

# Checks whether there are new cancellations to add to the archive
if (nrow(cancellations) != 0) {
  # Converts dates
  cancellations$date <- 0
  year <- as.numeric(format(Sys.Date(), "%Y"))
  for (row in 1:nrow(cancellations)) {
    date_text <- as.character(cancellations$date_text[row])
    date_m_d <- strsplit(date_text, as.character(year))[[1]][1]
    date <- paste(date_m_d, as.character(year), sep = "")
    date <- as.Date(date, format = "%b %d, %Y")
    date <- as.character(date)
    cancellations$date[row] <- date
  }
  
  # Extracts metadata
  for (row in 1:nrow(cancellations)) {
    alert_text <- as.character(cancellations$alert[row])
    # Train Line
    line <- gsub("Update:","",alert_text)
    line <- trimws(line)
    line <- strsplit(line, " ")[[1]][1]
    cancellations$line[row] <- line
    # From
    from <- strsplit(alert_text, "from")[[1]][2]
    from <- trimws(from)
    from <- strsplit(from, " ")[[1]][1]
    from <- gsub("[[:punct:] ]+","",from)
    cancellations$from[row] <- from
    # Original Train Number
    orig_train_num <- strsplit(alert_text, "train")[[1]][2]
    orig_train_num <- trimws(orig_train_num)
    orig_train_num <- strsplit(orig_train_num, " ")[[1]][1]
    orig_train_num <- trimws(orig_train_num)
    orig_train_num <- gsub("[[:punct:] ]+","",orig_train_num)
    cancellations$orig_train_num[row] <- orig_train_num
    # New Train Number
    new_train_num <- strsplit(alert_text, "train")[[1]][3]
    new_train_num <- strsplit(new_train_num, ",")[[1]][1]
    new_train_num <- trimws(new_train_num)
    new_train_num <- strsplit(new_train_num, " ")[[1]][1]
    new_train_num <- gsub("[[:punct:] ]+","",new_train_num)
    cancellations$new_train_num[row] <- new_train_num
    # Original Time
    orig_time <- strsplit(alert_text, "the")[[1]][2]
    orig_time <- trimws(orig_time)
    orig_time <- strsplit(orig_time, " ")[[1]][1]
    orig_time <- format(strptime(orig_time, "%I:%M%p"), "%H:%M")
    orig_time_text <- as.character(orig_time)
    cancellations$orig_time[row] <- orig_time_text
    orig_time <- strptime(orig_time, format = "%H:%M")
    # New Train Time
    new_time <- strsplit(alert_text, "the")[[1]][3]
    new_time <- trimws(new_time)
    new_time <- strsplit(new_time, " ")[[1]][1]
    new_time <- format(strptime(new_time, "%I:%M%p"), "%H:%M")
    new_time_text <- as.character(new_time)
    cancellations$new_time[row] <- new_time_text
    new_time <- strptime(new_time, format = "%H:%M")
    # Reason
    reason <- strsplit(alert_text, "due to")[[1]][2]
    reason <- strsplit(reason, "\\.")[[1]][1]
    reason <- trimws(reason)
    cancellations$reason[row] <- reason
    # Late Departure
    late <- difftime(new_time, orig_time)
    late <- as.integer(late)
    cancellations$late_min[row] <- late
    # Updates and Cancellations
    if (cancellations$cancelled[row] == 1) {
      cancellations$event[row] <- "cancelled"
    }
    else {cancellations$event[row] <- "update"}
  }
  
  useful_metadata <- c("date_text", "alert", "date", "line", "from", "orig_train_num", "new_train_num", "orig_time",  "new_time", "reason", "late_min", "event")
  cancellations <- cancellations[useful_metadata]
  
  # Adds cancellations to archive
  cancellations_archive <- rbind(cancellations, cancellations_archive)
  
  # Fixes row numbers
  row.names(cancellations_archive) <- NULL
  
  # Removed duplicates
  cancellations_archive <- cancellations_archive[!duplicated(cancellations_archive$alert), ]
}

# Saves archive version by day
today <- as.character(Sys.Date())
archive_name <- paste("cancellations_archive_", today, ".csv", sep = "")
write.csv(cancellations_archive, archive_name, row.names = FALSE)

# Saves current archive to stats folder
write.csv(cancellations_archive, "rail_data/NJT_cancellations.csv", row.names = FALSE)

#quit(save = "yes")
