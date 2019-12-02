## Joseph Telafici
## Johns Hopkins Data Science - Exlporatory Analysis
## Week 4 Assignment - Plot 3
## Oct 13, 2018

## used ggrepel to make labels more legible, especially for bottom series in graphs
## included both mean and total emissions as assigned question was non-specific and it is unclear whether data represents all 
## sources or statistical sampling of potential sources
## both graphs sometimes fail to render completely in RStudio on a random basis (timeout), but file output should be correct.

library(ggplot2)
library(reshape2)
library(gridExtra)
library (ggrepel)

## Read PM25 data
if (!exists("poll")) {
    poll <- readRDS("./summarySCC_PM25.rds") 
}

## Read source classification data
##if (!exists("src")) {
    src <- readRDS("./Source_Classification_Code.rds")
##}

## merge pollutant data with source codes
if (!exists("allPMdata")) {
    allPMdata <- merge(poll, src, by.x="SCC", by.y="SCC")
}

##Sum all Emissions data by year where fips = 24510
BaltEmissions <- data.frame(allPMdata[which(allPMdata$fips=="24510" & allPMdata$Data.Category %in% c("Point","Nonpoint","Nonroad","Onroad")),c("Emissions","year","Data.Category")])
BaltEmissions$Data.Category <-factor(BaltEmissions$Data.Category)
BMDTypeSum <- by(BaltEmissions$Emissions, INDICES=list(BaltEmissions$Data.Category,BaltEmissions$year),FUN=sum)
BMDTypeMean <- by(BaltEmissions$Emissions, INDICES=list(BaltEmissions$Data.Category,BaltEmissions$year),FUN=mean)
BMDSourceYearSum <- melt(array(BMDTypeSum, dim(BMDTypeSum),dimnames(BMDTypeSum)))
BMDSourceYearMean <- melt(array(BMDTypeMean, dim(BMDTypeMean),dimnames(BMDTypeMean)))
colnames(BMDSourceYearSum) <- c("source","year","totalpm25")
colnames(BMDSourceYearMean) <- c("source","year","meanpm25")

library(ggplot2)

plot1 <- ggplot(BMDSourceYearSum, aes(year, totalpm25, colour=source, label = round(totalpm25, digits=1), position_dodge())) 
plot1 <- plot1 + geom_line(size=1)  + geom_label_repel(size = 4) + labs(title="Total Baltimore PM25 Emissions by Data Source")
plot2 <- ggplot(BMDSourceYearMean, aes(year, meanpm25, colour=source, label = round(meanpm25, digits=2), position_dodge()))
plot2 <- plot2 + geom_line(size=1)  + geom_label_repel(size = 4) + labs(title="Mean Baltimore PM25 Emissions by Data Source")
grid.arrange(plot1, plot2, nrow=2)

dev.copy(png, file = "plot3.png") 
dev.off()