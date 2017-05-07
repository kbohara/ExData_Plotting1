#This function will read in the household_power_consumption.txt file and
#create multiple plots based on the requirements for the project. 
plot4 <- function () {
    #Create date class to read in date
    setClass('myDate')
    setAs("character","myDate", function(from) as.Date(from, format="%d/%m/%Y") )
    
    #Read the txt file into a table
    allhpc <- read.table("../household_power_consumption.txt", sep=";", 
                         header = TRUE, as.is = TRUE, 
                         colClasses = c("myDate", rep("character", 8)))
    
    #Extract data only from Feb-01 to Feb-02 of 2007
    hpc <- allhpc[allhpc$Date >= "2007-02-01" & allhpc$Date <= "2007-02-02", ]
    
    #Find any rows that have the missing value '?' and drop those rows
    drops <- apply(hpc, 1, function(r) any( (r %in% c("?"))))
    drops <- !drops
    hpc <- hpc[drops, ]
    
    #Find the names of columns in data frame and change non Date/Time to numeric
    cols <- names(hpc)
    cols.char <- c("Date","Time")
    cols.num <- cols[!cols %in% cols.char]
    
    hpc.char <- hpc[cols.char]
    hpc.num <- as.data.frame(lapply(hpc[cols.num],as.numeric))
    hpc <- cbind(hpc.char, hpc.num)
    
    #Create a datetime field that merges the Date and Time together
    dow <- function(x) format(as.Date(x), "%A")
    hpc$day <- dow(hpc$Date)
    hpc$datetime <- as.POSIXct(paste(hpc$Date, hpc$Time), format="%Y-%m-%d %H:%M:%S")
    
    #Plot 4
    png(filename = "plot4.png",width = 480, height = 480, units = "px")
    par(mfrow = c(2, 2))
    plot(hpc$datetime, hpc$Global_active_power, type = "l", xlab = "", 
         ylab = "Global Active Power")
    with (hpc, plot(datetime, Voltage, type = "l"))
    plot(hpc$datetime, hpc$Sub_metering_1, type = "n", xlab = "", ylab = "Energy sub meeting")
    lines(hpc$datetime, hpc$Sub_metering_1, type = "l", col = "black")
    lines(hpc$datetime, hpc$Sub_metering_2, type = "l", col = "red")
    lines(hpc$datetime, hpc$Sub_metering_3, type = "l", col = "blue")
    legend("topright", legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
           col=c("black", "red", "blue"), lty=1, bty = "n")
    
    with (hpc, plot(datetime, Global_reactive_power, type = "l"))
    dev.off()
}