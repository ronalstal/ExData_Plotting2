# plot2.R: Exploratory Data Analysis - Project 2 - Question 2
# ===========================================================

library(data.table, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(ggplot2, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(gridExtra, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)

NEI <- as.data.table(readRDS("data/summarySCC_PM25.rds"))
SCC <- as.data.table(readRDS("data/Source_Classification_Code.rds"))

# function to determine whether tendency increasing or decreasing
getTrend <- function(years, emiss, eps=0.005) {
    linreg <- lsfit(1:length(years)-1, emiss)
    ang <- linreg$coefficients[2]/(max(emiss)-min(emiss))
    return(ifelse(ang < -1*eps, "decreasing", ifelse(ang > eps, "increasing", "stable")))
}

# Have total emissions from PM2.5 decreased in the **Baltimore City, Maryland** 
# (`fips == "24510"`) from 1999 to 2008?

# total emissions for Baltimore by year
SUM <- NEI[fips == "24510", ][, sum(Emissions), by=year][ ,`:=`(totEm=V1,V1=NULL)]
trend <- getTrend(SUM$year, SUM$totEm)

par(bg = "white", mar = c(5,5,5,2), oma = c(0,0,0,1))
barplot(SUM$totEm, col = "red", names.arg = SUM$year, ylim = c(0, 4000),
     main = "Trend of Total PM2.5 Emissions\nBaltimore City / MD, 1999 to 2008",
     xlab = "Year", ylab = "Total PM2.5 in tons")
abline(lsfit(1:length(SUM$year)-1, SUM$totEm), lwd = 2, col = "blue")
legend("topright",paste("tendency:", trend), 
       lty = "solid", lwd = 2, col = "blue", bty = "n", inset = c(0.01, -0.05), cex = 1.0)

# copy the screen plot to png file
noprint <- dev.copy(png, file = "plot2.png",
                         width = 720, height = 480, units = "px")
noprint <- dev.off() ## Don't forget to close the PNG device!

