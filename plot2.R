#This function will read in the household_power_consumption.txt file and
#create a plot for Global Active Power. 
plot2 <- function () {
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
    
    #Plot 2
    png(filename = "plot2.png",width = 480, height = 480, units = "px")
    plot(hpc$datetime, hpc$Global_active_power, type = "l", xlab = "", 
         ylab = "Global Active Power (kilowatts)")
    dev.off()
}