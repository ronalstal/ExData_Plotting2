

# BALT <- NEI[fips == "24510", ][ ,sum(Emissions), by=list(year,type)
#                                ][ ,`:=`(type=factor(type, levels=c("POINT","NONPOINT","ON-ROAD","NON-ROAD"))
#                                         ,Emissions=V1,V1=NULL)]
dt <- data.table(
        year = c(2001,2002,2003),
        a = paste0("a",1:4),
        b = paste0("b",1:5),
        Emissions = rnorm(60)
)

# function to determine whether tendency increasing or decreasing
getTrend <- function(years, emiss) {
    e <- 0.005  # tolerance for "stable"
    linreg <- lsfit(1:length(years)-1, emiss)
    ang <- linreg$coefficients[2]/(max(emiss)-min(emiss))
    return(ifelse(ang < -1*e, "decreasing", ifelse(ang > e, "increasing", "stable")))
}

# get tendency for each type
getTrendFor <- function (forType) {
    b <- BALT[type==forType]
    getTrend(b$year, b$Emissions)
}

TrendTable <- function(dt, byCol) {
    evalX <- function(txt) {eval(parse(text=txt))}
    st <- evalX(paste0("dt[ ,sum(Emissions), by=list(year,",byCol,")]"))
    tt <- evalX(paste0("data.table(grp=levels(as.factor(st$",byCol,")))"))
    tt <- evalX(paste0("tt[,trend:=sapply(grp,function(x){t<-st[",
                                 byCol,"==x];getTrend(t$year,t$V1)})]"))
    return(tt)
}


tt <- data.table(type=levels(BALT$type))
tt <- tt[ ,trend:=sapply(tt$type,getTrendFor)]
tt

setkey(BALT,type); setkey(tt,type)
BALT <- BALT[tt][, `:=`(x=2003.4, y=2200)]

