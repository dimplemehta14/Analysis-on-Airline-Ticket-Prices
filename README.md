# Analysis-on-Airline-Ticket-Prices
This project was done to study the factors affecting the difference of price in Premium Economy and Economy seats in Airline.

# Read data into R
SixAirline <- read.csv(paste("SixAirlinesDataV2.csv", sep="")) 
View(SixAirline)

# Summarize the data to understand the mean, median, standard deviation of  each variable
summary(SixAirline)

# Draw Box Plots to visualize the distribution of Price of Economy tickets in different Airlines
boxplot(SixAirline$PriceEconomy ~ SixAirline$Airline,
        data=SixAirline, 
        horizontal=TRUE, 
        yaxt="n", 
        ylab="Airline",
        xlab="PriceEconomy", 
        main="Comparison of Price in different Airlines")
axis(side=2, at=c(1,2,3,4,5,6), 
     labels=c("AirFrance", "British "," Delta","Jet","Singapore","Virgin"))
     
# Draw Box Plots to visualize the distribution of Price of Premium tickets in different Airlines
boxplot(SixAirline$PricePremium ~ SixAirline$Airline,
        data=SixAirline, 
        horizontal=TRUE, 
        yaxt="n", 
        ylab="Airline",
        xlab="PricePremium", 
        main="Comparison of Price in different Airlines")
axis(side=2, at=c(1,2,3,4,5,6), 
     labels=c("AirFrance", "British "," Delta","Jet","Singapore","Virgin"))   

# Draw Bar Plots to visualize the distribution of Pitch size in different Airlines

# Pitch size in Economy class
seate.mean <- aggregate(PitchEconomy ~ Airline, data=SixAirline, mean)
# Pitch size in Premium class
seatp.mean <- aggregate(PitchPremium ~ Airline, data=SixAirline, mean)
# Pitch size in Economy class
seate.mean <- aggregate(PitchEconomy ~ Airline, data=SixAirline, mean)
# Pitch size in Premium class
seatp.mean <- aggregate(PitchPremium ~ Airline, data=SixAirline, mean)
Hide

# Visualizing them
library(lattice)
barchart(PitchEconomy ~ Airline, data=seate.mean, col="grey")
barchart(PitchPremium ~ Airline, data=seatp.mean, col="grey")

# To visualize the distribution of Width size in different Airlines

# Width size in Economy class
widthe.mean <- aggregate(WidthEconomy ~ Airline, data=SixAirline, mean)

# Width size in Premium class
widthp.mean <- aggregate(WidthPremium ~ Airline, data=SixAirline, mean)
Hide

# visualizing them
barchart(WidthEconomy ~ Airline, data=widthe.mean, col="grey")
barchart(WidthPremium ~ Airline, data=widthp.mean, col="grey")

# Making scatter plot matrix to see the coorelation between different variables
scatterplotMatrix( SixAirline[ ,c("PitchPremium","PitchEconomy")], 
                   spread=FALSE, 
                   smoother.args=list(lty=2), 
                   main="Scatter Plot Matrix")
scatterplotMatrix( SixAirline[ ,c("WidthPremium","WidthEconomy")], 
                   spread=FALSE, 
                   smoother.args=list(lty=2), 
                   main="Scatter Plot Matrix")    
scatterplotMatrix( SixAirline[ ,c("PriceEconomy","PricePremium")], 
                   spread=FALSE, 
                   smoother.args=list(lty=2), 
                   main="Scatter Plot Matrix")
scatterplotMatrix( SixAirline[ ,c("SeatsPremium","SeatsEconomy")], 
                   spread=FALSE, 
                   smoother.args=list(lty=2), 
                   main="Scatter Plot Matrix")
 scatterplotMatrix( SixAirline[ ,c("PitchDifference","WidthDifference")], 
                   spread=FALSE, 
                   smoother.args=list(lty=2), 
                   main="Scatter Plot Matrix")

# To make Scatter Plots to understand how are the variables correlated pair-wise

# Correlation between Price of Premium Ticket and Pitch Difference
library(car)
scatterplot(PricePremium ~ PitchDifference, data=Airline,
            spread=FALSE, smoother.args=list(lty=2), pch=19,
            main="Scatterplot of Pitch Diff",
            xlab="PitchDifference",
            ylab="PricePremium")

# Correlation between Price of Economy Ticket and Pitch Difference
scatterplot(PriceEconomy ~ PitchDifference, data=Airline,
            spread=FALSE, smoother.args=list(lty=2), pch=19,
            main="Scatterplot of Pitch Diff",
            xlab="PitchDifference",
            ylab="PriceEconomy")

# Correlation between Price of Economy Ticket and Width Difference
scatterplot(PriceEconomy ~ WidthDifference, data=Airline,
            spread=FALSE, smoother.args=list(lty=2), pch=19,
            main="Scatterplot of Width Diff",
            xlab="WidthDifference",
            ylab="PriceEconomy")

# Correlation between Price of Premium Ticket and Width Difference
scatterplot(PricePremium ~ WidthDifference, data=Airline,
            spread=FALSE, smoother.args=list(lty=2), pch=19,
            main="Scatterplot of Width Diff",
            xlab="WidthDifference",
            ylab="PricePremium")          

# Draw a Corrgram
library(corrgram)
corrgram(SixAirline, order=FALSE, 
         lower.panel=panel.shade,
         upper.panel=panel.pie, 
         diag.panel=panel.minmax,
         text.panel=panel.txt,
         main="Corrgram of Six Airline intercorrelations")        

# Create a Variance-Covariance Matrix
options(digits=2)
cor(SixAirline$PitchPremium, SixAirline$PricePremium)
cor(SixAirline$WidthPremium, SixAirline$PricePremium)
cor(SixAirline$PitchEconomy, SixAirline$PriceEconomy)
cor(SixAirline$WidthEconomy, SixAirline$PriceEconomy)

# Perform a Pearson Test for coorelation
resP <- cor.test(SixAirline$WidthPremium, SixAirline$PricePremium, 
                    method = "pearson")
resP

# Perform a Pearson Test for coorelation
resE <- cor.test(SixAirline$WidthEconomy, SixAirline$PriceEconomy, 
                    method = "pearson")
resE

# Articulate a Hypothesis (or two) that you could test using a Regression Model
# 1.NULL HYPOTHESIS: Increase in PitchPremium does not contribute in increase in PricePremium
# 2.NULL HYPOTHESIS: Increase in WidthPremium does not contribute in increase in Price Premium

# Run T-Tests to test the first hypothesis
t.test(SixAirline$PitchPremium, SixAirline$PricePremium)

# Run T-Tests to test the second hypothesis
t.test(SixAirline$WidthPremium, SixAirline$PricePremium)

# Formulate a Regression Model
m2 <- lm(PricePremium ~ 
           PitchPremium + 
           WidthPremium,
         data=SixAirline)
summary(m2)

# According to the result, this is made clear that increase in Pitch size and Width size are the major factors contributing to increase in price of Premium ticket.
