#
# plot2.R
# Reads file and outputs PNG per spec
#

plot2 <- function() {
     
     # Setup
     library(data.table)
     filename <- "household_power_consumption.txt"
     outputname <- "plot2.png"

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
     
     # Plot the line graph
     plot(my_data$DateTime , my_data$Global_active_power, 
          ylab = "Global Active Power (kilowatts)", xlab = "Date", main = "", type = "l")
     
     dev.off()

}