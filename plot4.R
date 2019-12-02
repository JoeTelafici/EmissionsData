## Joseph Telafici
## Johns Hopkins Data Science - Exlporatory Analysis
## Week 4 Assignment - Plot 4
## Oct 13, 2018

## 
## both graphs sometimes fail to render completely in RStudio on a random basis (timeout), but file output should be correct.

library (ggplot2)
library (gridExtra)
library (ggrepel)
library (plyr)

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
CoalEmissions <- data.frame(allPMdata[grep("(?i).*Coal.*", allPMdata$EI.Sector),])

##drop unnecessary data
CoalEmissions <- CoalEmissions[,2:7]

##summarize data
CoalSummary <- ddply(CoalEmissions, c("year","Data.Category"), summarize, N=length(Emissions), sum=sum(Emissions), mean= mean(Emissions), sd=sd(Emissions), se=sd/sqrt(N))

##treating years as factors makes the bars prettier
CoalSummary$year = factor(CoalSummary$year)

##first graph - total emissions
plot1 <- ggplot(CoalSummary, aes(year, sum, group=year, fill=Data.Category)) + 
    geom_bar(stat="identity", position="dodge2", width=0.8) + 
    coord_cartesian (ylim=c(0,650000)) +
    labs(title="US Total Emissions from Coal Combustion 1999-2008", tag="A") + 
    ylab("Total Emissions (tons)") + #scale_x_continuous(breaks = c(1999, 2002, 2005, 2008)) + 
    geom_text_repel(aes(label=format(sum, digits=4), color= Data.Category), position=position_nudge(x=-0.25), vjust=1, hjust = 0, size = 3.5)

##second graph - mean emissions with error bars
plot2 <- ggplot(CoalSummary, aes(x=year, y=mean, fill = Data.Category)) + 
    
    geom_bar(position=position_dodge2(), stat="identity") + 
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.4, position=position_dodge(0.9)) +
    labs(title="US Average Emissions from Coal Combustion 1999-2008", tag="B") + 
    ylab("Average Emissions (tons)") + #scale_x_continuous(breaks = c(1999, 2002, 2005, 2008)) + 
    geom_text_repel(aes(label=format(mean, digits=2), color= Data.Category), position=position_nudge(x=-0.05), vjust=1, hjust = 0, size = 3.5)

grid.arrange(plot1, plot2, nrow=2)



dev.copy(png, file = "plot4.png") 
dev.off()