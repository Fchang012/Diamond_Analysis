require(ggplot2)

# Some basic analysis on diamond prices based on https://amarder.github.io/how-to-buy-a-diamond.html

# Get cur dir from source of R Script
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

# Import Data
FinalDF <- read.csv("./Clean_Data/Diamond_Data.csv", sep = rawToChar(as.raw(127)))

# Reorder Factor Levels
FinalDF$Cut <- factor(FinalDF$Cut, levels(FinalDF$Cut)[c(1,2,4,5,3)])
FinalDF$Clarity <- factor(FinalDF$Clarity, levels(FinalDF$Clarity)[c(1,4,5,2,3)])


## Plot of Caret vs Price and Amarder Analysis --------------------------------------------------------

# Plots
# Cut
ggplot(FinalDF,
       aes(x=Carat,
           y=Price,
           color=Cut)) +
  geom_point(aes(shape=Cut, color=Cut)) +
  ggtitle("Caret vs Price With Cut As Legend")

# Color
ggplot(FinalDF,
       aes(x=Carat,
           y=Price,
           color=Color)) +
  geom_point(aes(shape=Color, color=Color)) +
  ggtitle("Caret vs Price With Color As Legend")

# Clarity
ggplot(FinalDF,
       aes(x=Carat,
           y=Price,
           color=Clarity)) +
  geom_point(aes(shape=Clarity, color=Clarity)) +
  ggtitle("Caret vs Price With Clarity As Legend")

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