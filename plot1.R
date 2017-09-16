#
# Plot1.R
# Reads file and outputs PNG per spec
#

plot1 <- function() {
     filename <- "household_power_consumption.txt"
     outputname <- "plot1.png"

     if ( file.exists(filename)) {
          data <- fread(filename, na.strings = "?")
     } else {
          stop(paste("error, file:",filename,"does not exist in working directory"))
     }

     # Add a date that is formatted correctly
     data[, FormattedDate:=as.Date(Date, "%d/%m/%Y")]

     my_data <- data[data$FormattedDate == "2007-02-01" | data$FormattedDate == "2007-02-02", ]

     my_data[, DateTime:=as.POSIXct(paste(FormattedDate,Time),format = "%Y-%m-%d %H:%M:%S", tz = "")]
     
     png(filename = outputname, width=480, height=480, bg = "transparent")
     hist(my_data$Global_active_power, col = "red", xlab = "Global Active Power (kilowatts)", ylab = "Frequency", 
          main = "Global Active Power")
     dev.off()

}