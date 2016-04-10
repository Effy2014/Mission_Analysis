setwd("~/Desktop")
library(dplyr)
library(ggplot2)
mission <- read.csv("GoEnnounce_Mission_Export_03-30-2016_01-03-16-PM.csv")
mission$Start.Date <- as.Date(mission$Start.Date, format='%m/%d/%y')
mission$End.Date <- as.Date(mission$End.Date, format="%m/%d/%y")
mission$year <- as.numeric(format(mission$Start.Date, "%Y"))
#convert dollar value (factors) to numeric
for (i in c(13:18)){
    mission[,i] <- sapply(mission[,i], function(x){as.numeric(gsub('\\$|,', '', as.character(x)))}) 
}
#get users have created more than one missions
summary <- group_by(mission, User.ID)
count <- summarize(summary, n = n())
index <- filter(count, n >= 2)
multiple <- filter(mission, mission$User.ID %in% index$User.ID)
single <- filter(mission, !mission$User.ID %in% index$User.ID)
multiple$year <- as.numeric(format(multiple$Start.Date, "%Y"))
single$year <- as.numeric(format(single$Start.Date, "%Y"))
#multiple means an user creats more than 2 missions, and single means an user only created one mission 
DF <- rbind(data.frame(type = "multiple", year = multiple$year), data.frame(type = "single", year=single$year))
ggplot(DF, aes(x=year, fill=type))+geom_bar(position="dodge")+ggtitle("Missions Created by Year")

summary.single <- group_by(single, year)
Amount.single <- summarize(summary.single, Amount = sum(Amount.Raised.to.Date))

Amount.multiple <- multiple %>% group_by(year) %>% summarize(Amount = sum(Amount.Raised.to.Date))
DF.amount <- rbind(data.frame(type="multiple", year=Amount.multiple$year,amount=Amount.multiple$Amount),
                   data.frame(type="single", year=Amount.single$year,amount=Amount.single$Amount))
ggplot(DF.amount, aes(x=year,y=amount,fill=type))+geom_bar(position="dodge", stat="identity")+ggtitle("Money Raised by Year")

mission$label <- ifelse(mission$Mission.ID %in% multiple$Mission.ID, 1, 0)
mission.sub <- mission[, -which(names(mission) %in% c("Start.Date", "End.Date"))]
mission.sub <- mission.sub[,c(13:22,28)]

library(caret)
index <- createDataPartition(mission.sub$label, p=0.8, list = F)
train <- mission.sub[index, ]
test <- mission.sub[-index, ]
library(gbm)
gbm.model <- gbm(label~. , data = train, distribution = "bernoulli", n.trees = 3000, interaction.depth = 5 ,shrinkage = 0.001, cv.folds = 5)
nTrees <- gbm.perf(gbm.model)
summary(gbm.model)
pred <- predict(gbm.model, train, type="response", n.trees = nTrees)
pred.ad <- ifelse(pred>=0.5, 1, 0)
confusionMatrix(pred.ad, train$label)
pred.test <- predict(gbm.model, test, type = "response", n.trees = nTrees)
pred.test.ad <- ifelse(pred.test>=0.5, 1, 0)
confusionMatrix(pred.test.ad, test$label)
rocplot(pred.test.ad, test$label)

