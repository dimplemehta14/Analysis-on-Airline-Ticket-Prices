# Analysis-on-Airline-Ticket-Prices
This project was done to study the factors affecting the difference of price in Premium Economy and Economy seats in Airline.
#Read data into R
SixAirline <- read.csv(paste("SixAirlinesDataV2.csv", sep="")) 
View(SixAirline)

#Summarize the data to understand the mean, median, standard deviation of  each variable
summary(SixAirline)

#Draw Box Plots to visualize the distribution of Price of Economy tickets in different Airlines
boxplot(SixAirline$PriceEconomy ~ SixAirline$Airline,
        data=SixAirline, 
        horizontal=TRUE, 
        yaxt="n", 
        ylab="Airline",
        xlab="PriceEconomy", 
        main="Comparison of Price in different Airlines")
axis(side=2, at=c(1,2,3,4,5,6), 
     labels=c("AirFrance", "British "," Delta","Jet","Singapore","Virgin"))
     
     
