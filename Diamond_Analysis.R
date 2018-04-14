require(ggplot2)
require(corrplot)
require(tibble)
require(dplyr)

# Some basic analysis on diamond prices based on https://github.com/amarder/diamonds/blob/master/diamonds.Rmd

# Get cur dir from source of R Script
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

# Import Data
FinalDF <- read.csv("./Clean_Data/Diamond_Data.csv", sep = rawToChar(as.raw(127)))

# Reorder Factor Levels
FinalDF$Cut <- factor(FinalDF$Cut, levels(FinalDF$Cut)[c(4,1,3,5,2)])
FinalDF$Clarity <- factor(FinalDF$Clarity, levels(FinalDF$Clarity)[c(1,6,7,4,5,2,3)])


## Plot of Caret vs Price and Amarder Analysis --------------------------------------------------------

# Plots
# Cut
ggplot(FinalDF,
       aes(x=Carat,
           y=Price)) +
  geom_point(aes(shape=Cut, color=Cut)) +
  ggtitle("Caret vs Price With Cut As Legend") +
  theme_bw()

# Color
ggplot(FinalDF,
       aes(x=Carat,
           y=Price)) +
  geom_point(aes(shape=Color, color=Color)) +
  ggtitle("Caret vs Price With Color As Legend") +
  theme_bw()

# Clarity
ggplot(FinalDF,
       aes(x=Carat,
           y=Price)) +
  geom_point(aes(shape=Clarity, color=Clarity)) +
  ggtitle("Caret vs Price With Clarity As Legend") +
  theme_bw()

## Blue Nile’s buying guide describes how the four C’s (cut, color, clarity, and carat weight) are the most
## important characteristics when buying a diamond. It seems reasonable to model price as a function of those
## four characteristics. Having played around with the data bit, a multiplicative model seems like a good choice.
## I model price as a product of carat weight raised to the power β times multipliers for the cut, color, and
## clarity of the diamond.
##
## Pricei∝caratβi ⋅ cuti ⋅ colori ⋅ clarityi
##
## Taking log’s of both sides allows this model to be estimated using a linear regression
##
## log(pricei)=α+βlog(carati)+δcuti+δcolori+δclarityi+ϵi

# Create dummy var for Cut, Color, Clarity and disregard the CutGood, ColorG, and ClarityVS2 as they are dependent on the other respective variables
tempDF <- as.tibble(cbind(model.matrix( ~ Cut - 1, data=FinalDF), model.matrix( ~ Color - 1, data=FinalDF), model.matrix( ~ Clarity - 1, data=FinalDF)))
FinalDF <- as.tibble(cbind(FinalDF, tempDF))
colnames(FinalDF) <- make.names(colnames(FinalDF))

fString <- paste('log(Price) ~ log(Carat)+', paste(colnames(FinalDF)[-c(1:6, 12, 20, 27)], collapse = '+'), sep = '')
fit <- lm(fString, data=FinalDF)

# Summary of fit
summary(fit)

# Correlation
linDependTerm <- alias(fit)

# Find the coeff of fit and use it to plot fitted line
# https://www.statmethods.net/stats/regression.html for more documentation on Fitting lm
coeff=coefficients(fit)


# PLotting regression line
ggplot(FinalDF, aes(x=Carat,
                    y=Price,
                    color=Cut)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red")

# Adding in the regression forecasts back into df
FinalDF <- cbind(FinalDF, Forecast=exp(predict(fit)))
FinalDF <- cbind(FinalDF, Residual=resid(fit))

# Top 1% with least residuals
focus <- FinalDF[FinalDF$Residual <= quantile(FinalDF$Residual, 0.1), ]

# Only ideal + cuts
focus <-focus[(focus$CutTrue.Hearts == 1 | focus$CutExcellent == 1 | focus$CutIdeal == 1 | focus$CutVery.Good == 1), ]

#Only color i and above

# Plot of Top 1%
ggplot(focus,
       aes(x=Carat,
           y=Price,
           color=Cut)) +
  geom_point(aes(shape=Cut, color=Cut)) +
  ggtitle("Top 1% Cut") +
  theme_bw()

ggplot(focus,
       aes(x=Carat,
           y=Price,
           color=Color)) +
  geom_point(aes(shape=Color, color=Color)) +
  ggtitle("Top 1% Color") +
  theme_bw()

ggplot(focus,
       aes(x=Carat,
           y=Price,
           color=Clarity)) +
  geom_point(aes(shape=Clarity, color=Clarity)) +
  ggtitle("Top 1% Clarity") +
  theme_bw()

# Write to table to explore
write.table(focus[,c(1:6,27:28)], "./Output/focus.csv", sep = ",", row.names = FALSE)
