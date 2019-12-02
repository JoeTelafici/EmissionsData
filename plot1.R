## Joseph Telafici
## Johns Hopkins Data Science - Exlporatory Analysis
## Week 4 Assignment - Plot 1
## Oct 13, 2018


## Read PM25 data
if (!exists("poll")) {
    poll <- readRDS("./summarySCC_PM25.rds") 
}

## Read source classification data
if (!exists("src")) {
    src <- readRDS("./Source_Classification_Code.rds")
}

## merge pollutant data with source codes
if (!exists("allPMdata")) {
    allPMdata <- merge(poll, src, by.x="SCC", by.y="SCC")
}

##Calculate sum by year
yearlyEmissions <- data.frame(tapply(allPMdata$Emissions, allPMdata$year, sum))
colnames(yearlyEmissions) <- c("TotalEmissions")
##plot
plot(rownames(yearlyEmissions), yearlyEmissions$TotalEmissions/1000000, xlim=c(1998,2009), xlab="Year", ylab="Total Emissions (tons)", pch=16, col="red", fg="black", xaxp=c(1999,2008,3), axes=FALSE)
axis(1)
axis(2, at=pretty(yearlyEmissions$TotalEmissions/1000000), labels=paste(pretty(yearlyEmissions$TotalEmissions/1000000), "MM", sep=""))
abline(lm(yearlyEmissions$TotalEmissions/1000000 ~ as.numeric(rownames(yearlyEmissions)), data=yearlyEmissions),col="gray60",lty=2)
title("Total Annual PM25 Emissions, United States, 1999-2008")
box()

dev.copy(png, file = "plot1.png") 
dev.off()





