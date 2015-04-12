plot3 <- function(workingdir) {

  library(dplyr)
  
  setwd(workingdir)
  
  data = read.table("household_power_consumption.txt", header = TRUE, sep = ";" )
  data$Date <- as.Date(data$Date, "%d/%m/%Y")
  
  fdata <- filter(data, Date == "2007-02-01" | Date == "2007-02-02")
  
  fdata$DateTime <- paste(fdata$Date, fdata$Time)
  
  fdata$DateTime <- strptime(fdata$DateTime, "%Y-%m-%d %H:%M:%S")
  
  fdata$Sub_metering_1 <- as.numeric(fdata$Sub_metering_1)
  fdata$Sub_metering_2 <- as.numeric(fdata$Sub_metering_2)
  fdata$Sub_metering_3 <- as.numeric(fdata$Sub_metering_3)
  
  plot(fdata$DateTime, fdata$Sub_metering_1, 
       type = "l",
       ylab = "Energy sub metering",
       xlab = "")
  
  lines(fdata$DateTime,fdata$Sub_metering_2,col="red")
  lines(fdata$DateTime,fdata$Sub_metering_3,col="blue")
  
  legend("topright", 
         col=c("black","red","blue"),
         c("Sub_metering_1 ","Sub_metering_2", "Sub_metering_3"),
         lty=c(1,1), 
         lwd=c(1,1))
  
  dev.copy(png, file="plot3.png", width=480, height=480)
  dev.off()

}
