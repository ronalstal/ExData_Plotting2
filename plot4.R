# plot4.R: Exploratory Data Analysis - Project 2 - Question 4
# ===========================================================

library(data.table, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(ggplot2, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(grid, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)

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

# How have emissions from coal combustion-related sources changed 
# in the USA from 1999 to 2008?

# COAL COMBUSTION-RELATED SOURCES:
# ==> from SCC select sources with "Short.Name" containing "comb" and ("coal" or "lignite")
#     (ignoring lower-/uppercase) (some "lignite" short names do not contain "coal")

SCCsel <- SCC[grep("comb.+(coal|lignite)", Short.Name, ignore.case=TRUE, value=FALSE), 
                list(SCC,SCC.Level.Two)]
# resume SCC.Level.Two to "Electric Generation" and "Others"
SCCsel <- SCCsel[, `:=`(L2res=ifelse(SCC.Level.Two=="Electric Generation",
                                     "Electric Generation", "Others"))]

# ==> select these SCCs from NEI, merging in columns SCC.Level.Two, L2res (for grouping)
#     (in SQL terms: INNER JOIN)

setkey(NEI,SCC); setkey(SCCsel,SCC)
COAL <- NEI[, list(SCC,year,Emissions)][SCCsel, nomatch=0]

# PLOT a): facetting by resumed level L2res
#          - can't use margins=TRUE on facet_grid; trend labels get confused,
#            so add the total per year
COALa <- COAL[ ,sum(Emissions), by=list(year,L2res)
           ][ ,`:=`(Emissions=V1/1000,V1=NULL)]     # kilo-tons
COALa <- rbind(COALa, COALa[ ,sum(Emissions), by=year,
           ][ ,`:=`(Emissions=V1,V1=NULL,L2res="TOTAL")])     # already kilo-tons

# get tendency for each L2res and (all)
tt <- TrendTable(COALa,"L2res", margin=FALSE, eps=0.1)
maxEm <- COALa[,ceiling(max(Emissions))]
setkey(COALa,L2res); setkey(tt,grp)                 # "ON" clause for join
COALa <- COALa[tt][, `:=`(x=2003.5, y=maxEm+20)]    # join trends, coords for labels

gp4a <- ggplot(COALa, aes(year,Emissions)) +
        facet_grid(. ~ L2res, margins=FALSE) +
        geom_bar(stat="identity", fill="red") +
        scale_x_continuous(breaks=COALa$year, labels=COALa$year) +
        scale_y_continuous(name="Emissions in 1000 tons") +
        geom_smooth(method = "lm", se=FALSE, size=2, colour="navy") +
        geom_text(aes(x, y, label=trend), data=COALa, colour="navy") +
        xlab("Year") +
        ggtitle(paste("Trends for PM2.5 Emissions from Coal Combustion-Related Sources",
                      "USA, 1999 to 2008",
                      sep = "\n")) +
        theme(strip.text.x = element_text(size=10, face="bold", colour="#5050AA"),
              plot.title = element_text(lineheight=.9, face="bold", size=12))

# PLOT b): facetting by SCC.Level.Two where L2res == "Others"
#          except "Space Heaters" and "Total Area...."
COALb <- COAL[L2res=="Others" & 
              !SCC.Level.Two %in% c("Space Heaters","Total Area Source Fuel Combustion",
                                    "Electric Utility"), 
            ][, sum(Emissions), by=list(year,SCC.Level.Two)
            ][ ,`:=`(Emissions=V1/1000,V1=NULL,L2=as.character(SCC.Level.Two))]     # kilo-tons

# get tendency for each SCC.Level.Two
tt <- TrendTable(COALb,"L2", margin=FALSE, eps=0.005)
maxEm <- COALb[,ceiling(max(Emissions))]
setkey(COALb,L2); setkey(tt,grp)                    # "ON" clause for join
COALb <- COALb[tt][, `:=`(x=2003.5, y=maxEm+10)]    # join trends, coords for labels

gp4b <- ggplot(COALb, aes(year,Emissions)) +
        facet_grid(. ~ SCC.Level.Two, margins=FALSE) +
        geom_bar(stat="identity", fill="red") +
        scale_x_continuous(breaks=COALb$year, labels=COALb$year) +
        scale_y_continuous(name="Emissions in 1000 tons") +
        geom_smooth(method = "lm", se=FALSE, size=2, colour="navy") +
        geom_text(aes(x, y, label=trend), data=COALb, colour="navy") +
        xlab(paste0("Year\n\n",
                    "Emissions from sources with Short.Name ",
                    "containing 'comb' and ('coal' or 'lignite')",
                    " grouped by SCC.Level.Two")) +
        ggtitle(" ... details of some of the 'Others' from above ...") +
        theme(strip.text.x = element_text(size=10, face="bold", colour="#5050AA"),
              plot.title = element_text(face="bold", size=12))

# put the two plots on one page
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 1)))
print(gp4a, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(gp4b, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))

# copy the screen plot to png file
noprint <- dev.copy(png, file = "plot4.png",
                         width = 720, height = 960, units = "px")
noprint <- dev.off() ## Don't forget to close the PNG device!

