library(ggplot2)

# Sets size
reason_by_line_rows <- ceiling(length(by_day)/7)
reason_by_line_height <- reason_by_line_rows*3
  
bar_name <- paste("reason_by_line", today, ".jpg", sep = "")
jpeg(bar_name, width = 9, height = reason_by_line_height, units = "in", res = 300)
ggplot(data=cancellations_analysis, aes(reason_cleaned, fill= line)) + geom_bar(position = "stack") + facet_wrap(~date, ncol = 7) + scale_fill_manual(values = c("#16AB57", "#F2D418", "#C97267", "#E83D3F", "#089FD7", "#490447", "#FA9E42"))  + labs(title = "Reasons by Line", x = "Reason for Cancellation", y = "Cancellations by Line") + theme(axis.text.x = element_text(angle=90)) + scale_y_continuous(breaks=seq(1,20,1))
dev.off()