# plot6.R: Exploratory Data Analysis - Project 2 - Question 6
# ===========================================================

library(data.table, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(ggplot2, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(gridExtra, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)

NEI <- as.data.table(readRDS("data/summarySCC_PM25.rds"))
SCC <- as.data.table(readRDS("data/Source_Classification_Code.rds"))

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

# Which city has seen greater changes in motor vehicle emissions from 1999 to 2008?
## **Baltimore City, Maryland** (`fips == "24510"`)
## **Los Angeles County, California** (`fips == "06037"`)

# POPULATION:
# ==> Baltimore City, MD from http://research.stlouisfed.org/fred2/series/MDBALT5POP#
#     Los Angeles County from http://research.stlouisfed.org/fred2/series/CALOSA7POP#
# ==> downloaded .xls (sic!) files; result hard coded here:

POP <- data.table(
        year = c(rep(1999,2),rep(2002,2),rep(2005,2),rep(2008,2)),
        fips = rep(c("24510", "06037"), 4),    # Baltimore City, L.A. County
        pop = c( 657441, 9437290, 642514, 9717836, 640064, 9802296, 637901, 9771522)
    )

# AREA:
# ==> Baltimore City, MD from http://en.wikipedia.org/wiki/List_of_counties_in_Maryland
#     Los Angeles County from http://en.wikipedia.org/wiki/List_of_counties_in_California
# ==> result hard coded here, area in square miles

AREA <- data.table(
        year = c(rep(1999,2),rep(2002,2),rep(2005,2),rep(2008,2)),
        fips = rep(c("24510", "06037"), 4),    # Baltimore City, L.A. County
        area = rep(c(92, 4060), 4)             # square miles Balt, L.A.
    )


# call function to select SCCs for "motor vehicle emissions" used in questions 5 and 6
SCCsel <- selMotorVehic(SCC)

# ==> select these SCCs from NEI for Baltimore and L.A., merging in column Fuel and Population
#     (in SQL terms: INNER JOIN)

setkey(NEI,SCC); setkey(SCCsel,SCC)                     # "ON clause" for join
BALA <- NEI[fips %in% c("24510","06037"),               # Baltimore City, L.A. County
                list(SCC, year, fips, Emissions)        # .. cols to keep
          ][SCCsel[, list(SCC, fuel)], nomatch=0        # inner join on selected SCC
          ][ ,sum(Emissions), by=list(year,fips,fuel)   # sum per year, county and fuel group
          ][ ,`:=`(totEm=V1,V1=NULL,                    # rename total Emissions (in tons)
                   fips=as.factor(fips))]               # fips as factor for plot
# add the total of all fuel types per year
BALA <- rbind(BALA, BALA[ ,sum(totEm), by=list(year,fips)
           ][ ,`:=`(totEm=V1,V1=NULL,fuel="ALL FUELS")])
# join in population and area per fips and year
setkeyv(BALA,c("year","fips"))                                 # "ON clause" for join
setkeyv(POP,c("year","fips")); setkeyv(AREA,c("year","fips"))
BALA <- BALA[POP]                                              # join population for each year
BALA <- BALA[AREA]                                             # join area in sq mi
# compute Emissions in pounds per capita (1 ton = 2000 lbs) and in tons per square mile
BALA <- BALA[, `:=`(popEm=2000*totEm/pop, areEm=totEm/area)]
# index the Emissions so that 1999 = 100%
B99 <- BALA[year == 1999, ][ ,list(fips, fuel, totEm, popEm, areEm)  # values for 1999
          ][ ,`:=`(tot99=totEm, pop99=popEm, are99=areEm, 
                   totEm=NULL, popEm=NULL, areEm=NULL)]
setkeyv(BALA,c("fips","fuel")); setkeyv(B99,c("fips","fuel"))  # "ON clause" for join
BALA <- BALA[B99                                               # join in Emissions of 1999
            ][,`:=`(iTotEm=round(100*totEm/tot99),             # calculate indexed by 1999 in %
                    iPopEm=round(100*popEm/pop99),
                    iAreEm=round(100*areEm/are99))]

facet_labelling <- function(variable, value) {
    ifelse(value=="06037","Los Angeles",
           ifelse(value=="24510","Baltimore City",
                  label_value(variable,value)))
}

# prepare plots for total Emissions, relative, by area and by population

g6a <- ggplot(BALA, aes(year,totEm)) +
       facet_grid(fips ~ fuel, margins=FALSE, labeller="facet_labelling") +
       geom_bar(stat="identity", fill="red") +
       scale_x_continuous(breaks=BALA$year, labels=BALA$year) +
       geom_smooth(method = "lm", se=FALSE, size=2, colour="navy") +
       ylab("Emissions in tons") +
       ggtitle(paste("comparing total emissions",
                     sep = "\n")) +
       theme(
             strip.text.x = element_text(size=14, face="bold", colour="#5050AA"),
             strip.text.y = element_text(size=11, face="bold", colour="Navy"),
             plot.title = element_text(lineheight=.9, face="bold", size=13))

g6b <- ggplot(BALA, aes(year,iTotEm)) +
       facet_grid(fips ~ fuel, margins=FALSE, labeller="facet_labelling") +
       geom_bar(stat="identity", fill="red") +
       scale_x_continuous(breaks=BALA$year, labels=BALA$year) +
       geom_smooth(method = "lm", se=FALSE, size=2, colour="navy") +
       ylab("indexed: 1999 = 100%") +
       ggtitle(paste("comparing total emissions in % of 1999",
                     sep = "\n")) +
       theme(
             strip.text.x = element_text(size=14, face="bold", colour="#5050AA"),
             strip.text.y = element_text(size=11, face="bold", colour="Navy"),
             plot.title = element_text(lineheight=.9, face="bold", size=13))

g6c <- ggplot(BALA, aes(year,areEm)) +
       facet_grid(fips ~ fuel, margins=FALSE, labeller="facet_labelling") +
       geom_bar(stat="identity", fill="red") +
       scale_x_continuous(breaks=BALA$year, labels=BALA$year) +
       geom_smooth(method = "lm", se=FALSE, size=2, colour="navy") +
       ylab("Emissions in tons per square mile") +
       ggtitle(paste("comparing emissions in tons per square mile of county area",
                     sep = "\n")) +
       theme(
             strip.text.x = element_text(size=14, face="bold", colour="#5050AA"),
             strip.text.y = element_text(size=11, face="bold", colour="Navy"),
             plot.title = element_text(lineheight=.9, face="bold", size=13))

g6d <- ggplot(BALA, aes(year,popEm)) +
       facet_grid(fips ~ fuel, margins=FALSE, labeller="facet_labelling") +
       geom_bar(stat="identity", fill="red") +
       scale_x_continuous(breaks=BALA$year, labels=BALA$year) +
       geom_smooth(method = "lm", se=FALSE, size=2, colour="navy") +
       ylab("Emissions in lbs per capita") +
       ggtitle(paste("comparing emissions in pounds (lbs) per capita",
                     sep = "\n")) +
       theme(
             strip.text.x = element_text(size=14, face="bold", colour="#5050AA"),
             strip.text.y = element_text(size=11, face="bold", colour="Navy"),
             plot.title = element_text(lineheight=.9, face="bold", size=13))

# put the four plots on one page using gridExtra
# http://www.r-bloggers.com/extra-extra-get-your-gridextra/

grid.arrange(g6a, g6b, g6c, g6d, ncol=2,
             main = paste("Emissions from Motor Vehicle sources",
                          "Los Angeles County vs Baltimore City",
                          sep="\n"),
             sub  = paste("Emission from sources with Data.Category == 'Onroad'",
                          "or Short.Name contains 'recreat'",
                          sep=" ")
             )

# copy the screen plot to png file
noprint <- dev.copy(png, file = "plot6.png",
                         width = 800, height = 720, units = "px")
noprint <- dev.off() ## Don't forget to close the PNG device!


