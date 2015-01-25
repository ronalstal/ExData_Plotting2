# plot5.R: Exploratory Data Analysis - Project 2 - Question 5
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



# A function to select SCCs for "motor vehicle emissions" used in questions 5 and 6

# MOTOR VEHICLE SOURCES:
# ==> I chose to include all Onroad Categories plus Recreational Vehicles,
#     but not airplanes, locomotives, machinery etc from  Mobile EI.Sectors
# ==> from SCC select sources with "Data.Category" equal to "Onroad"
#     and add SCCs with "recreat" in Short.Name (they all have "mobile" in EI.Sector)
# ==> make fuel groups "Diesel", "Gasoline" by grepping SCC.Level.Two

selMotorVehic <- function(SCC) {
    
  s <- SCC[Data.Category == "Onroad", ]                             # all Onroad catgories
  s <- rbind(s, SCC[grep("recreat",Short.Name,ignore.case=TRUE), ]) # add recreational vehicles
  s <- s[, list(SCC, SCC.Level.Two, Data.Category)                  # only these columns needed
       ][, fuel:=as.factor("Gasoline")                              # group "Gasoline"
       ][grep("Diesel", SCC.Level.Two), fuel:="Diesel"]             # .. or "Diesel"

}

# How have emissions from motor vehicle sources changed
# for **Baltimore City, Maryland** (`fips == "24510"`) from 1999 to 2008?

# call function to select SCCs for "motor vehicle emissions" used in questions 5 and 6
SCCsel <- selMotorVehic(SCC)

# ==> select these SCCs from NEI for Baltimore, merging in column Fuel
#     (in SQL terms: INNER JOIN)

setkey(NEI,SCC); setkey(SCCsel,SCC)                     # "ON clause" for join
BALT <- NEI[fips == "24510", list(SCC,year,Emissions)   # Baltimore City, MD
          ][SCCsel[, list(SCC, fuel)], nomatch=0        # inner join on selected SCC
          ][ ,sum(Emissions), by=list(year,fuel)        # sum per year and fuel group
          ][ ,`:=`(Emissions=V1,V1=NULL)]
# can't use margins=TRUE on facet_grid (trend labels get confused), so add the total per year
BALT <- rbind(BALT, BALT[ ,sum(Emissions), by=year,
           ][ ,`:=`(Emissions=V1,V1=NULL,fuel="TOTAL")])

# get tendency for each fuel group
setkeyv(BALT,c("year","fuel"))
tt <- TrendTable(BALT,"fuel")
maxEm <- BALT[,ceiling(max(Emissions))]
setkey(BALT,fuel); setkey(tt,grp)                # "ON" clause for join
BALT <- BALT[tt][, `:=`(x=2003.5, y=maxEm+20)]   # join trends, coords for labels

gp5 <- ggplot(BALT, aes(year,Emissions)) +
       facet_grid(. ~ fuel, margins=FALSE) +
       geom_bar(stat="identity", fill="red") +
       scale_x_continuous(breaks=BALT$year, labels=BALT$year) +
       geom_smooth(method = "lm", se=FALSE, size=2, colour="navy") +
       geom_text(aes(x, y, label=trend), colour="navy") +
       xlab(paste0("Year\n\n",
                   "Emission from sources with Data.Category == 'Onroad' ",
                   "or Short.Name contains 'recreat'")) +
       ylab("Emissions in tons") +
       ggtitle(paste("Trends for PM2.5 Emissions for Motor Vehicles",
                     "Baltimore City / MD, 1999 to 2008",
                     sep = "\n")) +
       theme(strip.text.x = element_text(size=14, face="bold", colour="#5050AA"),
             plot.title = element_text(lineheight=.9, face="bold", size=16))
print(gp5)

# copy the screen plot to png file
noprint <- dev.copy(png, file = "plot5.png",
                         width = 720, height = 480, units = "px")
noprint <- dev.off() ## Don't forget to close the PNG device!

