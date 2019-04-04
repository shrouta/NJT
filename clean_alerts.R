cancellations_analysis <- cancellations_archive[which(cancellations_archive$event != "update"),]
cancellations_analysis <- cancellations_analysis[which(cancellations_analysis$line != "Update"),]

# Adds dates when there are no observations
add_row <- as.data.frame(matrix(ncol = 12, nrow = 1))
colnames(add_row) <- c("date_text", "alert", "date", "line", "from", "orig_train_num", "orig_time", "new_train_num", "new_time", "reason", "late_min", "event")
add_row$date <- as.Date(add_row$date)

if (today %in% all_dates == FALSE) {
  all_dates[length(all_dates)+1] <- as.character(today)
}

all_dates <- as.Date(all_dates)
cancellations_analysis$date <- as.Date(cancellations_analysis$date)

for (day in 1:length(all_dates)) {
  if (all_dates[day] %in% cancellations_analysis$date == FALSE) {
    date_char <- all_dates[day]
    date_char <- as.character(date_char)
    add_row$date[1] <- date_char
    add_row$date <- as.Date(add_row$date)
    cancellations_analysis <- rbind(cancellations_analysis, add_row)
  }
}

cancellations_analysis <- cancellations_analysis[order(cancellations_analysis$date),]

row.names(cancellations_analysis) <- NULL

cancellations_analysis$date <- as.character(cancellations_analysis$date)

# Adds cancellation categories
cancellations_analysis$reason_cleaned <- 0
for (row in 1:nrow(cancellations_analysis)) {
  reason <- as.character(cancellations_analysis$reason[row])
  words_reason <- strsplit(reason, " ")[[1]]
  if ("equipment" %in% words_reason) {
    cancellations_analysis$reason_cleaned[row] <- "equipment"
  }
  else if ("manpower" %in% words_reason) {
    cancellations_analysis$reason_cleaned[row] <- "personnel"
  }
  else if ("mechanical" %in% words_reason) {
    cancellations_analysis$reason_cleaned[row] <- "mechanical"
  }
  else if ("earlier" %in% words_reason) {
    cancellations_analysis$reason_cleaned[row] <- "earlier"
  }
  else {
    cancellations_analysis$reason_cleaned[row] <- "other"
  }
}

# Converts dates to date format
for (row in 1:nrow(cancellations_analysis)) {
  date_time <- as.character(cancellations_analysis$date_text[row])
  date_time <- as.POSIXct(date_time, format = "%m/%d/%y %H:%M")
}


# Updates Terminals
cancellations_analysis$from <- as.character(cancellations_analysis$from)
for (row in 1:nrow(cancellations_analysis)) {
  from <- as.character(cancellations_analysis$from[row])
  from <- strsplit(from, " ")[[1]][1]
  if (is.na(from) == TRUE) {
    cancellations_analysis$from[row] <- "NA"
  }
  else if ("Jersey" == from) {
    cancellations_analysis$from[row] <- "Jersey Avenue"
  }
  else if ("Mount" == from) {
    cancellations_analysis$from[row] <- "Mount Olive"
  }
  else if ("Port" == from) {
    cancellations_analysis$from[row] <- "Port Jervis"
  }
  else if ("South" == from) {
    cancellations_analysis$from[row] <- "South Amboy"
  }
  else if ("Spring" == from) {
    cancellations_analysis$from[row] <- "Spring Valley"
  }
}


# Splits dataframes
by_day <- split(cancellations_analysis, cancellations_analysis$date, drop = FALSE)
by_line <- split(cancellations_analysis, cancellations_analysis$line)
by_terminus <- split(cancellations_analysis, cancellations_analysis$from)
by_reason <- split(cancellations_analysis, cancellations_analysis$reason_cleaned)
by_train <- split(cancellations_analysis, cancellations_analysis$orig_train_num)
