plot4 <- function(workingdir) {

  library(dplyr)
  
  setwd(workingdir)
  
  data = read.table("household_power_consumption.txt", header = TRUE, sep = ";" )
  data$Date <- as.Date(data$Date, "%d/%m/%Y")
  
  fdata <- filter(data, Date == "2007-02-01" | Date == "2007-02-02")
  
  fdata$DateTime <- paste(fdata$Date, fdata$Time)
  
  fdata$DateTime <- strptime(fdata$DateTime, "%Y-%m-%d %H:%M:%S")
  
  fdata$Global_active_power <- as.numeric(fdata$Global_active_power)
  
  fdata$Sub_metering_1 <- as.numeric(fdata$Sub_metering_1)
  fdata$Sub_metering_2 <- as.numeric(fdata$Sub_metering_2)
  fdata$Sub_metering_3 <- as.numeric(fdata$Sub_metering_3)
  
  fdata$Voltage <- as.numeric(fdata$Voltage)
  
  par(mfrow=c(2,2))
  
  ##plot 1
  
  plot(fdata$DateTime, fdata$Global_active_power, 
       type = "l",
       ylab = "Global Active Power",
       xlab = "")
  ##plot 2
  plot(fdata$DateTime,fdata$Voltage, type="l", xlab="datetime", ylab="Voltage")
  
  ##plot3
  
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
         bty="n", 
         cex=.5)
  
  ##plot 4
  plot(fdata$DateTime,
       fdata$Global_reactive_power, 
       type="l", 
       xlab="datetime", 
       ylab="Global_reactive_power")
  
  dev.copy(png, file="plot4.png", width=480, height=480)
  dev.off()

}
