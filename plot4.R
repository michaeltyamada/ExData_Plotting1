#
# plot4.R
# Reads file and outputs PNG per spec
#

plot4 <- function() {
     
     # Setup
     library(data.table)
     filename <- "household_power_consumption.txt"
     outputname <- "plot4.png"

     # Check for filename and open
     if ( file.exists(filename)) {
          data <- fread(filename, na.strings = "?")
     } else {
          stop(paste("error, file:",filename,"does not exist in working directory"))
     }

     # Add a date that is formatted correctly
     data[, FormattedDate:=as.Date(Date, "%d/%m/%Y")]

     # select only the data we care about then remove large data table from memory (not really needed, but...)
     my_data <- data[(data$FormattedDate == "2007-02-01" | data$FormattedDate == "2007-02-02"), ]
     remove(data)
     
     # construct a datetime for graphing. 
     my_data[, DateTime:=as.POSIXct(paste(FormattedDate,Time),format = "%Y-%m-%d %H:%M:%S", tz = "")]
     
     # Specify output format, filename, and size
     png(filename = outputname, width=480, height=480, bg = "transparent")
     
     # Plot in a 2x2
     par(mfcol = c(2,2), mar = c(5,4,4,1))
     
     # Plot the line graph
     with(my_data, plot(DateTime , Global_active_power, ylab = "Global Active Power",
          xlab = "", main = "", type = "l"))
     
     with(my_data, plot(DateTime , Sub_metering_1, ylab = "Energy sub metering", xlab = "", 
          main = "", type = "l"))
     points(my_data$DateTime, my_data$Sub_metering_2, type = "l", col = "red")
     points(my_data$DateTime, my_data$Sub_metering_3, type = "l", col = "blue")
     legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
          col = c("black", "red", "blue"), lty = c(1,1), bty = "n")
     
     with(my_data, plot(DateTime, Voltage, main = "", xlab = "datetime", type = "l"))
     with(my_data, plot(DateTime, Global_reactive_power, main = "", xlab = "datetime", type = "l"))
     
     dev.off()

}