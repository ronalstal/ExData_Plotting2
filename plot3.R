# plot3.R: Exploratory Data Analysis - Project 2 - Question 3
# ===========================================================

library(data.table, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(ggplot2, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)

NEI <- as.data.table(readRDS("data/summarySCC_PM25.rds"))
SCC <- as.data.table(readRDS("data/Source_Classification_Code.rds"))

# function to determine whether tendency increasing or decreasing
getTrend <- function(years, emiss, eps=0.005) {
    linreg <- lsfit(1:length(years)-1, emiss)
    ang <- linreg$coefficients[2]/(max(emiss)-min(emiss))
    return(ifelse(ang < -1*eps, "decreasing", ifelse(ang > eps, "increasing", "stable")))
}
# table of tendencies for each facet
TrendTable <- function(dt, byCol, margin=FALSE, eps=0.005) {
    evalX <- function(txt) {eval(parse(text=txt))}
    st <- evalX(paste0("dt[ ,sum(Emissions), by=list(year,",byCol,")]"))
    tt <- evalX(paste0("data.table(grp=levels(as.factor(st$",byCol,")))"))
    tt <- evalX(paste0("tt[,trend:=sapply(grp,function(x){t<-st[",
                       byCol,"==x];getTrend(t$year, t$V1, eps=eps)})]"))
    if (margin) {
        rbind(tt, list("(all)",getTrend(st$year, st$V1, eps=eps)))
    }
    return(tt)
}

# Of the 4 **type** of sources which increase/decrease
# for **Baltimore City, Maryland** (`fips == "24510"`) from 1999 to 2008?

# Emissions Baltimore by year and type
# set type as factor in desired order (default is alphabetical)
BALT <- NEI[fips == "24510", ][ ,sum(Emissions), by=list(year,type)
          ][ ,`:=`(type=factor(type, levels=c("POINT","NONPOINT","ON-ROAD","NON-ROAD"))
                   ,Emissions=V1,V1=NULL)]

# get tendency for each type
tt <- TrendTable(BALT,"type")
setkey(BALT,type); setkey(tt,grp)
BALT <- BALT[tt][, `:=`(x=2003.4, y=2200)]

gp3 <- ggplot(BALT, aes(year,Emissions)) +
       facet_grid(. ~ type) +
       geom_bar(stat="identity", fill="red") +
       scale_x_continuous(breaks=BALT$year, labels=BALT$year) +
       geom_smooth(method = "lm", se=FALSE, size=2, colour="navy") +
       geom_text(aes(x, y, label=trend), data=BALT, colour="navy") +
       ggtitle("Trends of PM2.5 Emissions per Type\nBaltimore City / MD, 1999 to 2008") +
       theme(strip.text.x = element_text(size=14, face="bold", colour="#5050AA"),
             plot.title = element_text(lineheight=.9, face="bold", size=16))
print(gp3)

# copy the screen plot to png file
noprint <- dev.copy(png, file = "plot3.png",
                         width = 720, height = 480, units = "px")
noprint <- dev.off() ## Don't forget to close the PNG device!

