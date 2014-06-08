
########################################

# A program for practicing R, graphs, models, and analysis
# Author: Paul James
# Year: 2014

########################################

# Load in any packages to be used
library(ggplot2)

####################

mls <- list()
# Read in the desired csv files
mls$Old <- read.table("../data/csv/mlsOld.csv", header = TRUE, sep = "|")
mls$Now <- read.table("../data/csv/mlsNow.csv", header = TRUE, sep = "|")

#####################
#
## Set the year as a datetime
#for(i in 1:length(mls)){
#    mls[[i]]$Year <- strptime(mls[[i]]$Year, "%Y")
#}
#
#####################

# Let's get a quick look at our tables
for(i in 1:length(mls)){
    print(str(mls[[i]]))
    print(summary(mls[[i]]))
}

####################

rslNow <- mls$Now[which(mls$Now$Club == "RSL"), ]
rslOld <- mls$Old[which(mls$Old$Club == "RSL"), ]

porOld <- mls$Old[which(mls$Old$Club == "POR"), ]
twentyTenOld <- mls$Old[which(mls$Old$Year == "2010"), ]

rslNow
summary(rslOld)

####################

# Make some quick sorts
mls$Old[order(-mls$Old$PPG, -mls$Old$Wpct, -mls$Old$GFPG), ,drop = FALSE]
mls$Old[order(-mls$Old$PPG), , drop = FALSE]
mls$Old[order(-mls$Old$GFPG), , drop = FALSE]
mls$Old[order(mls$Old$GAPG), , drop = FALSE]

mls$Now[order(-mls$Now$PPG, -mls$Now$Wpct, -mls$Now$GFPG), ,drop = FALSE]

# Create a DF for each conference for the  mls$Old DF
eastOld <- subset(mls$Old, mls$Old$Conf == "East")
westOld <- subset(mls$Old, mls$Old$Conf == "West")
eastOld[order(-eastOld$PPG), ,drop = FALSE]
westOld[order(-westOld$PPG), ,drop = FALSE]

####################

# Make an RSL subset of data from the historical dataset
rslOld <- subset(mls$Old, mls$Old$Club == "RSL")
rslOld[order(-rslOld$PPG), , drop = FALSE]

##################################################
##################################################

# Practice Plots Lab follows

####################

# Quick dot plot of PPG by team
oldPPG.qplot <- qplot(mls$Old$Club, mls$Old$PPG)
oldPPG.qplot

# Boxplot if the distribution of PPG
pdf("../data/figs/oldPPG_dist01.pdf")

oldPPG.boxplot <- boxplot(mls$Old$PPG,
                          main = "Historical PPG Distribution in the MLS",
                          ylab = "PPG for One Season for One Team")
dev.off()

# Set up a histogram for the distribution of PPG
pdf("../data/figs/oldPPG_dist03.pdf")

hist(mls$Old$PPG, freq = FALSE, xlab = "Points Per Game", ylab = "Relative Frequency", main = "Distribution Analysis of Historical Points Per Game")
# Add a red kernel density plot
lines(density(mls$Old$PPG), new = FALSE, col = "red", lwd = 2)
# Add a blue normal plot
curve(dnorm(x, mean = mean(mls$Old$PPG), sd = sd(mls$Old$PPG)), col = "darkblue", lty = 2, lwd = 2, add = TRUE)
# Add a legend
legend(2, 1.2, legend = c("Kernel", "Normal"), lty = c(1, 2), lwd = c(2,
    2), col = c("red", "blue"))

dev.off()

# Another way by calculating bin width and using ggplot2
bin.width <- 2 * IQR(mls$Old$PPG) * length(mls$Old$PPG)^-(1/3)

pdf("../data/figs/oldPPG_dist02.pdf")

oldPPG.hist <- ggplot(mls$Old, aes(x = PPG))
oldPPG.hist + geom_histogram(aes(y = ..density..), binwidth = bin.width, colour = "white") + geom_density(size = 1, colour = "orange")

dev.off()

# Check out the scatterplot matrices
library(car)

pdf("../data/figs/oldPPG_spm01.pdf")

scatterplotMatrix(~ PPG + Wpct + Lpct + Dpct + GDPG, data = mls$Old)

dev.off()

# Regress and plot PPG by GDPG
pdf("../data/figs/ppg~gdpg01.pdf")

ggplot(mls$Old, aes(x = GDPG, y = PPG)) +
  geom_point(shape = 1) +    # Use hollow circles
  geom_smooth(method = lm)   # Add linear regression line

dev.off()

####################

library(leaps) # to fit all possible models
# allModels <- regsubsets(lm, data, and such)

library(bestglm)


####################

old.model1 <- lm(mlsOld$PPG ~ mlsOld$GDPG)
Anova(old.model1)
summary(old.model1)

####################

# Plot and predict PPG by W
pdf("../data/figs/ppg~w01.pdf")
ggplot(mlsOld, aes(x = W, y = PPG)) +
  geom_point(shape = 1) +    # Use hollow circles
  geom_smooth(method = lm)   # Add linear regression line
dev.off()

####################

# USE THE . TO INCLUDE ALL PARAMETERS IN THE MODEL AT ONCE!!!
old.model2 <- lm(PPG ~ ., data = mlsOld)
Anova(old.model2)
summary(old.model2) #best model here based on adj-R-sq

####################

# AIC model for multiple regression
# Use paired down DF
mlsOld2 <- mlsOld[ , c(1, 3, 6:9, 13:15)]
old.model3 <- lm(PPG ~ ., data = mlsOld2)
Anova(old.model3)
summary(old.model3)

####################

# HOW TO DO A STEP-WISE MODEL
library(MASS)
fit <- lm(PPG ~ .,data = mlsOld2)
step <- stepAIC(fit, direction = "both")

# display results
summary(step)
str(step)
step$anova
step$model
step$fitted.values

# Predict 2013
mlsNow2 <- mlsNow[ , c(1, 3, 6:12)]
mlsNow2
predictNow.ppg <- predict(step, mlsNow2)
str(predictNow.ppg)
predictNow.ppg

# Add the predicted column to mlsNow
predict <- data.frame(round(predictNow.ppg, digits = 2))
colnames(predict) <- "RM"
mlsNow3 <- cbind(predict, mlsNow2)
mlsNow3

# Calculate the avg error per prediction
rm.error <- mlsNow3$PPG - mlsNow3$RM
rm.error.avg <- round(sum(abs(rm.error)) / length(rm.error),
                      digits = 4)
rm.error.avg # AVG ERROR is 0.0358

percent.error.range <- rm.error.avg / (max(mlsNow3$PPG) -
                                         min(mlsNow3$PPG))
percent.error.range # 0.0358 is 2.9% of the range for PPG

