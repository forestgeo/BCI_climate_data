#### Produce a figure with rainfall, temperature, CO2, etc from 1981 to 2015
```{r Fig.S2 , eval=T, echo=F,message=F,warning=F,fig.width=12,fig.height=8,fig.pos='H',fig.cap='Mean time interval (year) between two censuses',tidy.opts=list(width.cutoff=50),tidy=TRUE, fig.show='asis',cache=TRUE}
library(xlsx)
library(ggplot2)
library(tidyquant)

# Import data
# download.file("http://biogeodb.stri.si.edu/physical_monitoring/pdf/Monthly%20summaries_BCI_vertical.xlsx","C:/Users/Ervan/Dropbox/AGB BCI/clim_data_ori.xlsx")

# Rainfall
rain <- read.xlsx("C:/Users/Ervan/Dropbox/AGB BCI/clim_data.xlsx","Rain")
RR <- melt(rain,id.vars="Year",variable.name="Month")
RR <- RR[order(RR$Year),]
RR$yearfrac <- RR$Year + rep(seq(0,.98,1/12),nrow(rain))
RR$var="rain"

# MAx/min T°
temp <- read.xlsx("C:/Users/Ervan/Dropbox/AGB BCI/clim_data.xlsx","Temp")
RR2 <- melt(temp,id.vars=c("Year","yearfrac","month"),variable.name="var")
RR2 <- RR2[,c(1,3,5,2,4)]
names(RR2) <- names(RR)
RR <- rbind(RR,RR2)

# CO2
temp <- read.xlsx("C:/Users/Ervan/Dropbox/AGB BCI/clim_data.xlsx","CO2")
RR2 <- temp[,c("Year","yearfrac","interpolated")]
RR2$var <- "CO2"
RR2$month <- seq(1,12,1)
RR2 <- RR2[,c(1,5,3,2,4)]
names(RR2) <- names(RR)
RR <- rbind(RR,RR2)

# ENSO
# enso <- read.xlsx("C:/Users/Ervan/Dropbox/AGB BCI/clim_data.xlsx","ENSO")
# RR3 <- melt(enso,id.vars="Year",variable.name="midmonth")
# RR3 <- RR3[order(RR3$Year),]
# RR3 <- RR3[RR3$Year>=1980,]
# RR3$yearfrac <- RR3$Year + rep(seq(-.08,.9,1/12))
# RR3$var="enso"
# names(RR3) <- names(RR)
# RR <- rbind(RR,RR3)

# Soil
temp <- read.xlsx("C:/Users/Ervan/Dropbox/AGB BCI/clim_data.xlsx","Soil")
RR2 <- temp[,c("Year","month","X0_10","yearfrac")]
names(RR2) <- names(RR)[1:4]
RR2$var<- "soil"
RR <- rbind(RR,RR2)

# Solar radiation
# temp <- read.xlsx("C:/Users/Ervan/Dropbox/AGB BCI/clim_data.xlsx","Solar_rad")
# RR2 <- melt(temp,id.vars="Year",variable.name="Month")
# names(RR2) <- names(RR)[1:3]
# RR2 <- RR2[order(RR2$Year),]
# RR2$yearfrac <- RR2$Year + rep(seq(0,.98,1/12),nrow(temp))
# RR2$var="rad"
# RR <- rbind(RR,RR2)
tab <- read.csv("C:/Users/Ervan/Dropbox/AGB BCI/BCIMeteo1983-2016.csv",head=T)
library(lubridate)
A <- head(as.character(tab$date))
tab$Month <- month(as.POSIXlt(as.character(tab$date), format="%m/%d/%Y"))
tab$Year <- year(as.POSIXlt(as.character(tab$date), format="%m/%d/%Y"))
TT <- setDT(tab)[,.(mean(gap_fill.2,na.rm=T)),by=.(Year,Month)]
names(TT)[3] <- "value"
TT$yearfrac <- TT$Year + (TT$Month-1)/12
TT$var <- "rad"
RR <- rbind(RR,TT)

DF <- LOAD(paste0(path_folder,"/output/",site,"_85-15_20perc_byquadrat_apr19.Rdata"))
MIDCENSUS <- DF[,.(x1=decimal_date(as.Date(mean(date1),origin=as.Date("1960-01-01"))),x2=decimal_date(as.Date(mean(date2),origin=as.Date("1960-01-01")))),by=year]

RR2 <- setDT(RR[RR$year>=1980,])
RR2$group <- NA
RR2$group[RR2$yearfrac>=MIDCENSUS[[2,2]] & RR2$yearfrac<MIDCENSUS[[2,3]]]<- 1
RR2$group[RR2$yearfrac>=MIDCENSUS[[3,2]] & RR2$yearfrac<MIDCENSUS[[3,3]]]<- 2
RR2$group[RR2$yearfrac>=MIDCENSUS[[4,2]] & RR2$yearfrac<MIDCENSUS[[4,3]]]<- 3
RR2$group[RR2$yearfrac>=MIDCENSUS[[5,2]] & RR2$yearfrac<MIDCENSUS[[5,3]]]<- 4
RR2$group[RR2$yearfrac>=MIDCENSUS[[6,2]] & RR2$yearfrac<MIDCENSUS[[6,3]]]<- 5
RR2$group[RR2$yearfrac>=MIDCENSUS[[7,2]] & RR2$yearfrac<MIDCENSUS[[7,3]]]<- 6

# Test slope & P value by variable
RES <- matrix(NA,5,3)
RR3 <- RR2[Year>=1985 & Year<2016,.(value=mean(value)),by=.(Year,var)]
VAR <- c("max" , "min" , "rad"  ,"rain" ,"soil")
for (i in 1:5) { # rainfall, soil moisture, min temp, max temp, solar radiation
	LM <- lm(value~Year,data=RR3[var==VAR[i]])
	RES[i,1:2] <- coef(summary(LM))[2,c(1,4)]
	RES[i,3] <- sd(RR3[var==VAR[i],value])*100/mean(RR3[var==VAR[i],value])  # CV in %
}
RES <- data.frame(RES)
RES$var <- VAR

LAB = c("rain"=expression(paste("Rainfall (mm/year)")),"soil"=expression(paste("Soil moisture (%) 0-10 cm")),"min"=expression(paste("Minimum temperature (°C)")),"max"=expression(paste("Maximum temperature (°C)")),"rad"=expression(paste("Solar radiation (W ",m^2,")")),"CO2"=expression(paste(CO[2]," (ppm)")))
#"enso"=expression(paste("Multivariate ENSO Index ")),

RR2[var=="rain",runmean:=rollapplyr(c(rep(NA,6),value,rep(NA,5)),12,sum)]
RR2[var!="rain", runmean := rollapplyr(c(rep(NA,6),value,rep(NA,5)),12,mean), by=var]

RR2$var2 <- factor(RR2$var,levels=c("rain","soil","min","max","rad","CO2"),labels=LAB)

RR2 <- subset(RR2, !is.na(var))
MEANS <- setDT(RR2)[!is.na(group),mean(value,na.rm=T),by=.(group,var2)]
MEANS$x1 <- MIDCENSUS[2:7,2]
MEANS$x2 <- MIDCENSUS[2:7,3]
MID <- c(MEANS$x1[1:6],MEANS$x2[6])

RR2[,inter:=as.numeric(NA)]
RR2 <- within(RR2,inter[var=="enso"] <- 0)

# Add extreme El Niño/Ninã (https://ggweather.com/enso/oni.htm)
EE <- enso <- read.xlsx("C:/Users/Ervan/Dropbox/AGB BCI/clim_data.xlsx","Extreme_ENSO")
RR4 <- melt(EE,id.vars="Year",variable.name="3month_av")
RR4 <- RR4[order(RR4$Year),]
RR4 <- RR4[RR4$Year>=1980,]
RR4$yearfrac <- RR4$Year + rep(seq(-0.5,.45,1/12))

# Extreme El Nino (>=6 months with +0.5 3month average anomaly)
test <- rle(RR4$value>0.5)
COND <- which(test$values==T & test$lengths>9)
test$values <- rep(0,length(test$values))
test$values[COND] <- seq_along(1:length(COND))
RR4$group <- inverse.rle(test)
RR4$ext <- ifelse(RR4$group==0,NA,"EL")

# Extreme Nina (>=6 months with -0.5 3month average anomaly)
test <- rle(RR4$value<(-0.5))
COND <- which(test$values==T & test$lengths>9)
test$values <- rep(0,length(test$values))
test$values[COND] <- seq_along(1:length(COND))
RR4 <- within(RR4,group[group==0] <- inverse.rle(test)[RR4$group==0])
RR4 <- within(RR4,ext[group!=0 & is.na(ext)] <- "LN")

B <- setDT(RR4)[group!=0,.("ini"=min(yearfrac),"end"=max(yearfrac)),by=.(group,ext)]

LAB2 <- RR2[,.SD[1],by=var2]

G <- ggplot(RR2,aes(x=yearfrac,y=value)) + facet_wrap(~var2,ncol=1,scale="free",labeller=label_parsed,strip.position = "left")  + labs(x="Time (year)",y="") + scale_color_manual(values=COL)  + geom_rect(data=B,aes(x=NULL,y=NULL,xmin=ini,xmax=end,ymin=-Inf,ymax=Inf,fill=ext),alpha=0.4,show.legend = F)+ geom_line(aes(x=yearfrac,y=runmean),linetype="solid",col="grey40",alpha=1) + geom_line(aes(x=yearfrac,y=runsum),linetype="solid",col="grey40",alpha=1) + geom_vline(xintercept=MID,linetype="dotted",size=1) + guides(colour=FALSE)  + theme(plot.margin=unit(c(0.5,1,0.5,0.5),"cm"),strip.background = element_blank(),strip.placement = "outside")  + scale_x_continuous(breaks=seq(1985,2015,3)) + coord_cartesian(xlim=c(1984.9,2015.8)) + geom_text(data= LAB2,mapping = aes(x = -Inf, y = Inf,label = c("(a)","(b)","(c)","(d)","(e)","(f)")),hjust=-0.5,vjust =+1,fontface="bold")  + scale_fill_manual(values=c("darkgoldenrod1","palegreen")) + scale_alpha(guide = 'none')

# + geom_ma(data=RR2,ma_fun = SMA, n = 12,linetype="solid",col="grey40",alpha=0.7)
#  + geom_segment(data=MEANS,aes(x=x1,y=V1,xend=x2,yend=V1),size=1,col="blue")  # average climate data by 5 years
DATE <- "2019-03-28"
ggsave(G,file=paste0("C:/Users/Ervan/Documents/Work/R/CTFS/Code_compil/biomass_fluxes_BCI/output/graph/apr19/SI/climate",DATE,".png"),width = 30, height = 40, units = "cm")
```