## Joseph Telafici
## Johns Hopkins Data Science - Exlporatory Analysis
## Week 4 Assignment - Plot 2
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

##Sum all Emissions data by year where fips = 24510
BaltEmissions <- data.frame(tapply(allPMdata[allPMdata$fips=="24510",c("Emissions")],allPMdata[allPMdata$fips=="24510",c("year")],sum))
colnames(BaltEmissions) <- c("Emissions")

plot(rownames(BaltEmissions), BaltEmissions$Emissions, xlim=c(1998,2009), xlab="Year", ylab="Total Emissions (tons)", pch=16, col="red", fg="black", xaxp=c(1999,2008,3), axes=FALSE)
axis(1)
title("Total Annual PM25 Emissions, Baltimore, 1999-2008")
axis(2)
abline(lm(BaltEmissions[[1]] ~ as.numeric(rownames(BaltEmissions)), data=BaltEmissions),col="gray60",lty=2)
box()

dev.copy(png, file = "plot2.png") 
dev.off()


