#paper2
rm(list = ls())
terastub3=read.csv("C:\\Users\\AJIT\\Documents\\Data\\TURB\\teras-turb-9_20171129-123051.csv",
                   header=TRUE,stringsAsFactors =FALSE, sep=",") 
terastub3=subset(terastub3,select=-c(X,
                  IDF.B.IN.MOT.CTRL.DMP.CTRL.CMD,IDF.A.HYD.CPL.CTRL.CMD))
data=as.data.frame(terastub3)
sum(is.na(data))
data=na.omit(data)
#data=subset(data,select = c(Date.Time,IDF.A.IN.MOT.CTRL.DMP.CTRL.CMD))
##########################################################################################
data$Date.Time <- format(as.Date(data$Date.Time),"%d%b%Y %H:%M:%S")
data=data[order(as.Date(data$Date.Time,format="%d%b%Y %H:%M:%S")),]
data$Date.Time=as.Date(data$Date.Time,"%d%b%Y %H:%M:%S")
data=aggregate(.~Date.Time,data=data, mean)
str(data)
##########################################################################################
library(ggplot2)
ggplot(data,aes(x=Date.Time, y=TUR.DC.EMER.OIL.PMP.OUTLET.PR,
        color=TUR.DC.EMER.OIL.PMP.OUTLET.PR))+ geom_line()+theme(legend.position="none")
library(plotly)
plot_ly(data,x=~Date.Time,y=~TUR.DC.EMER.OIL.PMP.OUTLET.PR,mode="lines")
#plot_ly(data,x=~Date.Time,y=~IDF.A.IN.MOT.CTRL.DMP.CTRL.CMD,mode="lines")

##################################EDA#####################################################
ggplot(data, aes(y=TUR.DC.EMER.OIL.PMP.OUTLET.PR,
                 fill="green")) + geom_boxplot(outlier.colour = "red")

library(plotly)
plot_ly(data,y=~TUR.DC.EMER.OIL.PMP.OUTLET.PR,type="box")
summary(data$TUR.DC.EMER.OIL.PMP.OUTLET.PR)

###############################################################################
library(tidyverse)
library(anomalize)
library(reshape)
library(magrittr)
library(ggplot2)
#ndata=melt(data[,2:8])
#ndata=cbind(Date.Time=data[,1],ndata)
data %>%
  ggplot(aes(Date.Time,TUR.DC.EMER.OIL.PMP.OUTLET.PR)) +
  geom_point(color = "blue", alpha = 0.25) +
  facet_wrap(~TUR.DC.EMER.OIL.PMP.OUTLET.PR, scale = "free_y", ncol = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  labs(title = "Sensor Dataset",
       subtitle = "Data from Turbine")

################################# STL+IQR ###############################################
library(tibbletime)
ndata <- as_tbl_time(data, index = Date.Time)

ndata %>% 
  as_period(period="daily")
ndata %>%
  time_decompose(TUR.DC.EMER.OIL.PMP.OUTLET.PR, method = "stl") %>%
  anomalize(remainder, method = "iqr") %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.25) +
  labs(title = "Turbine Anomalies", subtitle = "STL + IQR Methods") 
#########################################################################################
ndata %>%
  # Twitter + GESD
  time_decompose(TUR.DC.EMER.OIL.PMP.OUTLET.PR, method = "twitter", trend = "2 months") %>%
  anomalize(remainder, method = "gesd") %>%
  time_recompose() %>%
  # Anomaly Visualziation
  plot_anomalies(time_recomposed = TRUE) +
  labs(title = "TUR.DC.EMER.OIL.PMP.OUTLET.PR Anomalies", subtitle =
         "Twitter + GESD Methods")

#########################################################################################
ndata %>%
  time_decompose(TUR.DC.EMER.OIL.PMP.OUTLET.PR) %>%
  anomalize(remainder) %>%
  plot_anomaly_decomposition() +
  labs(title = "Decomposition of Anomalized TUR.DC.EMER.OIL.PMP.OUTLET.PR")
#################################### Anom data points###################################
ndata %>% 
  time_decompose(TUR.DC.EMER.OIL.PMP.OUTLET.PR) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  filter(anomaly == 'Yes') 
##################################################################################
qplot(1:length(data[1:50,3]), 
      data[1:50,3], 
      main = "Simulated Anomalies",
      xlab = "Index")
iqr_outliers <- iqr(data[1:50,3], alpha = 0.05, 
                    max_anoms = 0.2, verbose = TRUE)$outlier_report
ggsetup <- function(data) {
  data %>%
    ggplot(aes(rank, value, color = outlier)) +
    geom_point()+
    geom_line(aes(y = limit_upper), color = "red", linetype = 2) +
    geom_line(aes(y = limit_lower), color = "red", linetype = 2) +
    geom_text(aes(label = index), vjust = -1.25) +
    theme_bw() +
    scale_color_manual(values = c("No" = "#2c3e50", "Yes" = "#e31a1c")) +
    expand_limits(y = 13) +
    theme(legend.position = "bottom")
}


# Visualize
iqr_outliers %>% 
  ggsetup() +
  ggtitle("IQR: Top outlers sorted by rank") 
