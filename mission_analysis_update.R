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
ggplot(DF, aes(x=year, fill=type))+geom_bar(position="dodge")+ggtitle("# Fundraising Missions Created")

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
ggplot(DF.1, aes(x=Year, y=Amount, fill=type))+geom_bar(stat = "identity", position = "dodge")+ggtitle("$ Fundraising Amount")
qplot(x=year, y=Amount.Raised.to.Date, data=loyalty.mission)
q <- ggplot(summary[-6,], aes(year,Amount))
q+geom_bar(stat="identity")

install.packages("xlsx")
library(xlsx)
tech <- read.xlsx("Mission Category up date thru Nov2015 (2).xls", "TECH")
tu <- read.xlsx("Mission Category up date thru Nov2015 (2).xls", "TU")
team <- read.xlsx("Mission Category up date thru Nov2015 (2).xls", "TEAM")
ca <- read.xlsx("Mission Category up date thru Nov2015 (2).xls", "CA")
ot <- read.xlsx("Mission Category up date thru Nov2015 (2).xls", "OT")
ph <- read.xlsx("Mission Category up date thru Nov2015 (2).xls", "PH")
se <- read.xlsx("Mission Category up date thru Nov2015 (2).xls", "SE")
sp <- read.xlsx("Mission Category up date thru Nov2015 (2).xls", "SP")
sa <- read.xlsx("Mission Category up date thru Nov2015 (2).xls", "SA")
tp <- read.xlsx("Mission Category up date thru Nov2015 (2).xls", "TP")
tb <- read.xlsx("Mission Category up date thru Nov2015 (2).xls", "TB")
loyalty.number <- data.frame(rbind(c(type="TECH", total=sum(loyalty.mission$Mission.ID %in% tech$Mission.ID)),
                                   c(type="TU", total=sum(loyalty.mission$Mission.ID %in% tu$Mission.ID)),
                                   c(type="Team", total=sum(loyalty.mission$Mission.ID %in% team$Mission.ID)),
                                   c(type="CA", total=sum(loyalty.mission$Mission.ID %in% ca$Mission.ID)),
                                   c(type="OT", total=sum(loyalty.mission$Mission.ID %in% ot$Mission.ID)),
                                   c(type="PH", total=sum(loyalty.mission$Mission.ID %in% ph$Mission.ID)),
                                   c(type="SE", total=sum(loyalty.mission$Mission.ID %in% se$Mission.ID)),
                                   c(type="SP", total=sum(loyalty.mission$Mission.ID %in% sp$Mission.ID)),
                                   c(type="SA", total=sum(loyalty.mission$Mission.ID %in% sa$Mission.ID)),
                                   c(type="TP", total=sum(loyalty.mission$Mission.ID %in% tp$Mission.ID)),
                                   c(type="TB", total=sum(loyalty.mission$Mission.ID %in% tb$Mission.ID))))
loyalty.number$total<- as.numeric(as.character(loyalty.number$total))
loyalty.number <-loyalty.number %>% arrange(desc(total)) %>% mutate(perc=total/sum(total)*100)
pie <- ggplot(loyalty.number, aes(x = "",y=perc ,fill = type))+geom_bar(width = 1,stat="identity")
pie + coord_polar(theta = "y")+labs(title="# of loyalty missions by category")

other.number <- data.frame(rbind(c(type="TECH", total=sum(other.mission$Mission.ID %in% tech$Mission.ID)),
                                   c(type="TU", total=sum(other.mission$Mission.ID %in% tu$Mission.ID)),
                                   c(type="Team", total=sum(other.mission$Mission.ID %in% team$Mission.ID)),
                                   c(type="CA", total=sum(other.mission$Mission.ID %in% ca$Mission.ID)),
                                   c(type="OT", total=sum(other.mission$Mission.ID %in% ot$Mission.ID)),
                                   c(type="PH", total=sum(other.mission$Mission.ID %in% ph$Mission.ID)),
                                   c(type="SE", total=sum(other.mission$Mission.ID %in% se$Mission.ID)),
                                   c(type="SP", total=sum(other.mission$Mission.ID %in% sp$Mission.ID)),
                                   c(type="SA", total=sum(other.mission$Mission.ID %in% sa$Mission.ID)),
                                   c(type="TP", total=sum(other.mission$Mission.ID %in% tp$Mission.ID)),
                                   c(type="TB", total=sum(other.mission$Mission.ID %in% tb$Mission.ID))))

other.number<-other.number%>% arrange(desc(as.numeric(as.character(total))))%>%mutate(perc=total/sum(total))
pie <- ggplot(other.number, aes(x = "",y=perc ,fill = type))+geom_bar(width = 1,stat = "identity")
pie + coord_polar(theta = "y")+labs(title="# of other missions by category")

all <- merge(loyalty.number,other.number,by="type")
all <- all %>% mutate(total=as.numeric(as.character(total.x))+as.numeric(as.character(total.y)), perc=total/sum(total)) %>% arrange(desc(total))

pie <- ggplot(all, aes(x = "",y=perc ,fill = type))+geom_bar(width = 1,stat="identity")
pie + coord_polar(theta = "y")+labs(title="# of missions by category")

loyalty.donated <- data.frame(rbind(c(type="TECH", total=sum(loyalty.mission$Amount.Raised.to.Date[loyalty.mission$Mission.ID %in% tech$Mission.ID], na.rm = T)),
                                    c(type="TU", total=sum(loyalty.mission$Amount.Raised.to.Date[loyalty.mission$Mission.ID %in% tu$Mission.ID], na.rm = T)),
                                    c(type="Team", total=sum(loyalty.mission$Amount.Raised.to.Date[loyalty.mission$Mission.ID %in% team$Mission.ID], na.rm = T)),
                                    c(type="CA", total=sum(loyalty.mission$Amount.Raised.to.Date[loyalty.mission$Mission.ID %in% ca$Mission.ID], na.rm = T)),
                                    c(type="OT", total=sum(loyalty.mission$Amount.Raised.to.Date[loyalty.mission$Mission.ID %in% ot$Mission.ID], na.rm = T)),
                                    c(type="PH", total=sum(loyalty.mission$Amount.Raised.to.Date[loyalty.mission$Mission.ID %in% ph$Mission.ID], na.rm = T)),
                                    c(type="SE", total=sum(loyalty.mission$Amount.Raised.to.Date[loyalty.mission$Mission.ID %in% se$Mission.ID], na.rm = T)),
                                    c(type="SP", total=sum(loyalty.mission$Amount.Raised.to.Date[loyalty.mission$Mission.ID %in% sp$Mission.ID], na.rm = T)),
                                    c(type="SA", total=sum(loyalty.mission$Amount.Raised.to.Date[loyalty.mission$Mission.ID %in% sa$Mission.ID], na.rm = T)),
                                    c(type="TP", total=sum(loyalty.mission$Amount.Raised.to.Date[loyalty.mission$Mission.ID %in% tp$Mission.ID], na.rm = T)),
                                    c(type="TB", total=sum(loyalty.mission$Amount.Raised.to.Date[loyalty.mission$Mission.ID %in% tb$Mission.ID], na.rm = T))))
loyalty.donated$total<-as.numeric(as.character(loyalty.donated$total))
loyalty.donated<-loyalty.donated %>%  mutate(perc=total/sum(total)*100) %>% arrange(desc(total))
pie <- ggplot(loyalty.donated, aes(x = "",y=perc ,fill = type))+geom_bar(width = 1,stat = "identity")
pie + coord_polar(theta = "y")+labs(title="$ donated of loyalty missions by category")

other.donated <- data.frame(rbind(c(type="TECH", total=sum(other.mission$Amount.Raised.to.Date[other.mission$Mission.ID %in% tech$Mission.ID], na.rm = T)),
                                    c(type="TU", total=sum(other.mission$Amount.Raised.to.Date[other.mission$Mission.ID %in% tu$Mission.ID], na.rm = T)),
                                    c(type="Team", total=sum(other.mission$Amount.Raised.to.Date[other.mission$Mission.ID %in% team$Mission.ID], na.rm = T)),
                                    c(type="CA", total=sum(other.mission$Amount.Raised.to.Date[other.mission$Mission.ID %in% ca$Mission.ID], na.rm = T)),
                                    c(type="OT", total=sum(other.mission$Amount.Raised.to.Date[other.mission$Mission.ID %in% ot$Mission.ID], na.rm = T)),
                                    c(type="PH", total=sum(other.mission$Amount.Raised.to.Date[other.mission$Mission.ID %in% ph$Mission.ID], na.rm = T)),
                                    c(type="SE", total=sum(other.mission$Amount.Raised.to.Date[other.mission$Mission.ID %in% se$Mission.ID], na.rm = T)),
                                    c(type="SP", total=sum(other.mission$Amount.Raised.to.Date[other.mission$Mission.ID %in% sp$Mission.ID], na.rm = T)),
                                    c(type="SA", total=sum(other.mission$Amount.Raised.to.Date[other.mission$Mission.ID %in% sa$Mission.ID], na.rm = T)),
                                    c(type="TP", total=sum(other.mission$Amount.Raised.to.Date[other.mission$Mission.ID %in% tp$Mission.ID], na.rm = T)),
                                    c(type="TB", total=sum(other.mission$Amount.Raised.to.Date[other.mission$Mission.ID %in% tb$Mission.ID], na.rm = T))))
other.donated$total<-as.numeric(as.character(other.donated$total))
other.donated<-other.donated %>%  mutate(perc=total/sum(total)*100) %>% arrange(desc(total))
pie <- ggplot(other.donated, aes(x = "",y=perc ,fill = type))+geom_bar(width = 1,stat = "identity")
pie + coord_polar(theta = "y")+labs(title="$ donated of other missions by category")
all.donate <- merge(loyalty.donated, other.donated, by="type")
all.donate<-all.donate %>% mutate(total=total.x+total.y, perc=total/sum(total)*100)%>%arrange(desc(total))
pie <- ggplot(all.donate, aes(x = "",y=perc ,fill = type))+geom_bar(width = 1,stat = "identity")
pie + coord_polar(theta = "y")+labs(title="$ donated by category")
