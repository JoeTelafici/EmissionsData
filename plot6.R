## Joseph Telafici
## Johns Hopkins Data Science - Exlporatory Analysis
## Week 4 Assignment - Plot 6
## Oct 13, 2018

## 
## both graphs sometimes fail to render completely in RStudio on a random basis (timeout), but file output should be correct.
## ggrepel yields weird artifacts with last data point in bottom graph regardless of nudge, direction, vjust, hjust

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

##Sum all Emissions data by year where EI>Sector contains "Coal"
LA_Balt_VehData <- data.frame(allPMdata[allPMdata$fips=="24510" | allPMdata$fips=="06037",])

##drop unnecessary data
LA_Balt_VehData <- LA_Balt_VehData[,2:7]

##summarize data
LA_Balt_VehSummary <- ddply(LA_Balt_VehData, c("year","fips"), summarize, N=length(Emissions), sum=sum(Emissions), mean= mean(Emissions), sd=sd(Emissions), se=sd/sqrt(N))

##treating years as factors makes the bars prettier
LA_Balt_VehSummary$year = factor(LA_Balt_VehSummary$year)

##replace fips code with City name
LA_Balt_VehSummary[LA_Balt_VehSummary$fips=="06037",]$fips = "Los Angeles"
LA_Balt_VehSummary[LA_Balt_VehSummary$fips=="24510",]$fips = "Baltimore"
LA_Balt_VehSummary$fips = factor(LA_Balt_VehSummary$fips, levels=c("Los Angeles","Baltimore"))

##first graph - total emissions
plot1 <- ggplot(LA_Balt_VehSummary, aes(year, sum, group=year, fill=fips)) + 
    geom_bar(stat="identity", position="dodge2", width=0.8) + 
    #coord_cartesian (ylim=c(0,650000)) +
    labs(title="Total Emissions from Baltimore & LA Vehicles 1999-2008", tag="A") + 
    ylab("Total Emissions (tons)") + #scale_x_continuous(breaks = c(1999, 2002, 2005, 2008)) + 
    geom_text_repel(aes(label=format(sum, digits=4), color=fips), position=position_nudge(y=5), vjust=1, hjust = 1, size = 3.5)

##second graph - mean emissions with error bars
plot2 <- ggplot(LA_Balt_VehSummary, aes(x=year, y=mean, fill =fips)) + 
    geom_bar(position=position_dodge2(), stat="identity", width=0.8) + 
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.4, position=position_dodge(0.9)) +
    labs(title="Average Emissions from Baltimore & LA Vehicles 1999-2008", tag="B") + 
    ylab("Average Emissions (tons)") + #scale_x_continuous(breaks = c(1999, 2002, 2005, 2008)) + 
    geom_text_repel(aes(label=format(mean, digits=3), color=fips), direction = "y", position=position_nudge(y=5,x=0.35), vjust=1, hjust = 1, size = 3.5)

grid.arrange(plot1, plot2, nrow=2)



dev.copy(png, file = "plot6.png") 
dev.off()