## Joseph Telafici
## Johns Hopkins Data Science - Exlporatory Analysis
## Week 4 Assignment - Plot 5
## April 8, 2019

## both graphs sometimes fail to render completely in RStudio on a random basis (timeout), but file output should be correct.

library(plyr)
library(ggplot2)
library(ggrepel)

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

##Sum all Emissions data by year where fips = 24510 & where SCC.Level.Two contains "Vehicles"
BaltMotorEmissions <- subset (allPMdata, fips=="24510" & grepl("Vehicles", allPMdata$SCC.Level.Two))

##drop unnecessary data
BaltMotorEmissions <- BaltMotorEmissions[,2:7]

##summarize data
BaltVehSummary <- ddply(BaltMotorEmissions, "year", summarize, N=length(Emissions), sum=sum(Emissions), mean= mean(Emissions), sd=sd(Emissions), se=sd/sqrt(N))

##treating years as factors makes the bars prettier
BaltVehSummary$year = factor(BaltVehSummary$year)

##first graph - total emissions
plot1 <- ggplot(BaltVehSummary, aes(year, sum, group=year)) + 
    geom_bar(stat="identity", position="dodge2", width=0.8, fill="red") + 
    labs(title="Baltimore Total Emissions from Vehicles 1999-2008", tag="A") + 
    coord_cartesian (ylim=c(0,375)) +
    ylab("Total Emissions (tons)") + 
    geom_text_repel(aes(label=format(sum, digits=4), color= "red"), vjust=1, hjust = 0.5, size = 4) +
    theme(legend.position="none")

##second graph - mean emissions with error bars
plot2 <- ggplot(BaltVehSummary, aes(x=year, y=mean)) + 
    geom_bar(position=position_dodge2(), stat="identity", fill="blue") + 
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.4, position=position_dodge(0.9)) +
    labs(title="Baltimore Average Emissions from Vehicles 1999-2008", tag="B") + 
    ylab("Average Emissions (tons)") + 
    geom_text_repel(aes(label=format(mean, digits=2)), position=position_nudge(x=-0.2, y=0.1), vjust=1, hjust = 0, size = 3.5, color="blue") +
    theme(legend.position="none")

grid.arrange(plot1, plot2, nrow=2)

##write graphcs to file
dev.copy(png, file = "plot5.png") 
dev.off()
