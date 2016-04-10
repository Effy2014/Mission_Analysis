library(dplyr)
library(ggplot2)
loyalty.mission <- read.csv("loyaltymission.csv")
loyalty.mission$Start.Date <- as.Date(loyalty.mission$Start.Date, format='%m/%d/%y')
loyalty.mission$End.Date <- as.Date(loyalty.mission$End.Date, format="%m/%d/%y")
loyalty.mission$year <- as.numeric(format(loyalty.mission$Start.Date, "%Y"))
#convert dollar value (factors) to numeric
for (i in c(13:18)){
    loyalty.mission[,i] <- sapply(loyalty.mission[,i], function(x){as.numeric(gsub('\\$|,', '', as.character(x)))}) 
}

other.mission <- read.csv("othermission.csv")
other.mission$Start.Date <- as.Date(other.mission$Start.Date, format='%m/%d/%y')
other.mission$End.Date <- as.Date(other.mission$End.Date, format="%m/%d/%y")
other.mission$year <- as.numeric(format(other.mission$Start.Date, "%Y"))
other.mission <- convert(other.mission)
for (i in c(13:18)){
        other.mission[,i] <- sapply(other.mission[,i], function(x){as.numeric(gsub('\\$|,', '', as.character(x)))}) 
    }

DF <- rbind(data.frame(type = "loyalty", year = loyalty.mission$year), data.frame(type = "other", year=other.mission$year))
ggplot(DF, aes(x=year, fill=type))+geom_bar(position="dodge")+ggtitle("Fundraising Missions Created")

#amount raised 
grouped <- group_by(loyalty.mission, year)
summary <- summarize(grouped, sum(Amount.Raised.to.Date))
names(summary) <- c("Year", "Amount")
summary <- as.data.frame(summary)

grouped.1 <- group_by(other.mission, year)
summary.1 <- summarize(grouped.1, sum(Amount.Raised.to.Date))
names(summary.1) <- c("Year", "Amount")
summary.1 <- as.data.frame(summary.1)

DF.1 <- rbind(data.frame(type = "loyalty", Year = summary$Year, Amount = summary$Amount), data.frame(type = "other", Year = summary.1$Year, Amount = summary.1$Amount))
ggplot(DF.1, aes(x=Year, y=Amount, fill=type))+geom_bar(stat = "identity", position = "dodge")+ggtitle("Fundraising Amount")
qplot(x=year, y=Amount.Raised.to.Date, data=loyalty.mission)
q <- ggplot(summary[-6,], aes(year,Amount))
q+geom_bar(stat="identity")
