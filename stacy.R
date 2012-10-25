
# Load the XLConnect package
require(XLConnect)

# Load spreadsheet
wb <- loadWorkbook("FBTB.xlsx")
SmearPos <- readWorksheet(wb, sheet = "SmearPos", startRow=2, startCol=1, header = TRUE) 
CulturePos <- readWorksheet(wb, sheet = "CulturePos", startRow=2, startCol=1, header = TRUE)
B1Pul <- readWorksheet(wb, sheet = "B1Pul", startRow=2, startCol=1, header = TRUE)
PanelTreat1991 <- readWorksheet(wb, sheet = "PanelTreat1991", startRow=2, startCol=1, header = TRUE)
PanelTreat2007 <- readWorksheet(wb, sheet = "PanelTreat2007", startRow=2, startCol=1, header = TRUE)
FBTB <- readWorksheet(wb, sheet = "FBTB", startRow=1, startCol=1, header = TRUE)

# New varaibles calculated in DNA
PanelDiagnosed <- readWorksheet(wb, sheet = "PanelDiagnosed", startRow=2, startCol=1, header = TRUE)
SmearPosCultureNeg <- readWorksheet(wb, sheet = "SmearPosCultureNeg", startRow=2, startCol=1, header = TRUE)
SmearNegCulturePos <- readWorksheet(wb, sheet = "SmearNegCulturePos", startRow=2, startCol=1, header = TRUE)
TotalPanelRx <- readWorksheet(wb, sheet = "TotalPanelRx", startRow=2, startCol=1, header = TRUE)


# Re-label years...
SmearPos$Year = sapply(strsplit(SmearPos$Year,' '),"[[",3)  
CulturePos$Year = sapply(strsplit(CulturePos$Year,' '),"[[",3)
B1Pul$Year = sapply(strsplit(B1Pul$Year,' '),"[[",3)
PanelTreat1991$Year = sapply(strsplit(PanelTreat1991$Year,' '),"[[",3)
PanelTreat2007$Year = sapply(strsplit(PanelTreat2007$Year,' '),"[[",3)
PanelDiagnosed$Year = sapply(strsplit(PanelDiagnosed$Year,' '),"[[",3)
SmearPosCultureNeg$Year = sapply(strsplit(SmearPosCultureNeg$Year,' '),"[[",3)
SmearNegCulturePos$Year = sapply(strsplit(SmearNegCulturePos$Year,' '),"[[",3)
TotalPanelRx$Year = sapply(strsplit(TotalPanelRx$Year,' '),"[[",3)

#combine Panel Treat data from 1991 and 2007 TB TI
# PanelTreat = merge(PanelTreat1991, PanelTreat2007)
# PanelTreat$PanelTreat = PanelTreat$PanelTreat1991 + PanelTreat$PanelTreat2007 

# IF FBTB data not available, predict from trend over previous 5 years
trend = glm(Foreign.born.TB ~ Year, data = FBTB[FBTB$Year>2005,]) # ?limit to [FBTB$Year>2006,] 
predict = c(2012, predict(trend, 2012, newdata=data.frame(Year=2012), type = "response")$fit )
FBTB = rbind(FBTB, predict)

# combine data
rm(data)
data = merge(SmearPos, CulturePos) 
data = merge(data, B1Pul, all.x = TRUE) 
data = merge(data, PanelTreat1991, all.x = TRUE) 
data = merge(data, PanelTreat2007, all.x = TRUE)
data = merge(data, FBTB, all.x = TRUE) 
data = merge(data, PanelDiagnosed, all.x = TRUE) 
data = merge(data, SmearPosCultureNeg, all.x = TRUE) 
data = merge(data, SmearNegCulturePos, all.x = TRUE) 
data = merge(data, TotalPanelRx, all.x = TRUE)

# convert year to numeric
data$Year = as.numeric(data$Year)

# define decline in RVCT cases
baselineFBTB = mean(FBTB$Foreign.born.TB[FBTB$Year %in% c(2002,2003,2004,2005,2006)]) #  Baseline FB cases, 2002-2006
data$dirt=baselineFBTB-data$Foreign.born.TB

# define number treated by 2007 TB TI
# data$treated =  data$SmearNegCulturePos + data$SmearPosCultureNeg  + data$PanelDiagnosed
data$treated =  data$PanelTreat2007

# impute value for current year by weighting up to 12 months
currentmonth = as.numeric(format(Sys.Date(), "%m"))
data[data$Year==2012, ]$treated = (12/currentmonth)*data[data$Year==2012, ]$treated

# convert year to numeric
data$Year = as.numeric(data$Year)

# plots
library(ggplot2)

chart_title = "FB TB Cases" 
ggplot(data=data, aes(y=Foreign.born.TB, x=as.numeric(Year) ))  + 
     geom_point(size=4) + 
     geom_line( size=1) +
     ylab("TB Cases") +
     theme_bw() +
     scale_x_continuous(limits=c(2002,2013), breaks=seq(1,2020,1)) +
#      scale_y_continuous(breaks=seq(-500,1500,250)) +
     theme(
          legend.justification=c(0,0), legend.position=c(0.25,0.66)
     )
# ggsave("FBcases.png", width=6, height=6, dpi=300)


chart_title = "Annual Number of TB Cases Prevented through Overseas TB Screening and \nDecline in Number of Foreign-Born TB Cases "

# Shift year of panel treat to previous year -- the year they would have arrived
i = 2:(length(data$Year))
data$RxOverseas = rep(0, length(data$Year))
data[i-1,]$RxOverseas = data[i,]$PanelTreat2007

# reshape data
require(reshape)
data.melt=  melt(data, id.vars=c("Year") , measure.vars=c("RxOverseas", "dirt"), na.rm=TRUE, variable_name="TB")

# label TB groups
levels(data.melt$TB) = c("Overseas TB Treatment", "Decline in Foreign-born TB")

# change value to match production server, annualized from 3 quarters of year
data.melt[data.melt$Year==2011 & data.melt$TB=="Overseas TB Treatment","value"] = 1012
data.melt= rbind(data.melt, data.frame(Year = 2007, TB="Overseas TB Treatment", value = 0))

ggplot(data=data.melt, aes(y=value, x=Year, group=TB)) + 
     geom_point(size=4) + 
     geom_line(aes(color=TB), size=1) +
     scale_color_manual( 
          breaks=c("Overseas TB Treatment", 
                   "Decline in Foreign-born TB"), 
          values=c("red", "black"), 
          name="TB Cases"
          ) +
     ylab("TB Cases") +
     theme_bw() +
     scale_x_continuous(limits=c(2002,2011), breaks=seq(1,2020,1)) +
     scale_y_continuous(breaks=seq(-500,1500,250)) +
     theme(
          legend.justification=c(0,0), legend.position=c(0.1,0.65)) +
     theme(legend.text = element_text(face="bold", colour="black", size=15)) 
       

# breaks=c("TB Diagnosed and Treated \n among Immigrant Applicants", 
#          "Decline in Foreign-born TB \n from Previous Year"),
     
 ggsave("Chart FBdecline vs TBTI.png", width=6, height=6, dpi=300)