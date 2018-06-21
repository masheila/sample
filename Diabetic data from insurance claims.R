library("dplyr")
library("magrittr")
library("ggplot2")

library("caTools")
library("ROCR")
quality <- read.csv("quality.csv")

str(quality)
# mycolors <- c("blue2", "red2")
mycolors <- c("forestgreen", "firebrick2")
ggplot(data = quality, aes(x = OfficeVisits, y = Narcotics)) + theme_bw() + 
  scale_colour_manual(values = mycolors) +
  xlab("Number of Office Visits") + 
  ylab("Number of Narcotics Prescribed") + 
  geom_point(aes(col = factor(PoorCare), alpha = 0.5), pch = 16, size = 4.0)

tmp <- table(quality$PoorCare)
tmp
# for Reproducibility
set.seed(88)
split <- sample.split(quality$PoorCare, SplitRatio = 0.75)
qualityTrain <- subset(quality, split == TRUE)

qualityTest <- subset(quality, split == FALSE)
QualityLog <- glm(PoorCare ~ OfficeVisits + Narcotics, data = qualityTrain, family = binomial)
summary(QualityLog)
predictTrain <- predict(QualityLog, type="response")
summary(predictTrain)
tapply(predictTrain, qualityTrain$PoorCare, mean)
tmp <- table(qualityTrain$PoorCare, predictTrain > 0.5)
tmp
tmp <- table(qualityTrain$PoorCare, predictTrain > 0.7)
tmp
tmp <- table(qualityTrain$PoorCare, predictTrain > 0.2)
tmp

# Prediction function
ROCRpred <- prediction(predictTrain, qualityTrain$PoorCare)

# Performance function
ROCRperf <- performance(ROCRpred, "tpr", "fpr")

# Plot ROC curve
# plot(ROCRperf, main = "Receiver Operator Characteristic Curve", lwd = 3)

# Add colors
# plot(ROCRperf, main = "Receiver Operator Characteristic Curve", lwd = 3, colorize = TRUE)

# Add threshold labels 
plot(ROCRperf, main = "Receiver Operator Characteristic Curve", lwd = 3, colorize = TRUE, 
     print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

predictTest <- predict(QualityLog, type = "response", newdata = qualityTest)
ROCRpredTest <- prediction(predictTest, qualityTest$PoorCare)
auc <- as.numeric(performance(ROCRpredTest, "auc")@y.values)

tmp <- table(qualityTest$PoorCare, predictTest > 0.3)
tmp 


















