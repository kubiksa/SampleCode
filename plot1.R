household_power_consumption <- read.csv("household_power_consumption.txt", sep=";")
data<-household_power_consumption
data.df<-data.frame(data)
data.df$Date<-as.Date(data[,1], format='%d/%m/%Y')
data1<-data.df[data.df$Date == as.Date("2007-02-02"), ]
data2<-data.df[data.df$Date == as.Date("2007-02-01"), ]
dataset<-rbind(data1,data2)

hist(as.numeric(dataset$Global_active_power), col="red", xlab="Global Active Power (kilowatts)", main="Global Active Power")
dev.copy(png,file="plot1.png", width=480, height=480)
dev.off()