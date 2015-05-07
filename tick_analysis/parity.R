library("lubridate")
library("sandwich") 
library("lmtest") 
library("car") 
library("zoo") 
library("xts") 
library("dplyr") 
library("broom") 
library("ggplot2") 
library("stats")
require(graphics)

CSVtoTimeSeries <- function(data, name=c("value")) {
  data_temp <- data.frame(timestamp=strptime(data$V1, "%Y%m%d %H%M%S"),value=data$V2)
  data_ts <- xts(data_temp$value, order.by=data_temp$timestamp)
  colnames(data_ts) <- name
  return(data_ts)
}

Sys.setlocale("LC_TIME","C") 

# load tick data for one month
# Ask
EU_a <- read.csv(file="DAT_NT_EURUSD_T_ASK_201504.csv", header = FALSE, sep = ";", stringsAsFactors = FALSE)
EG_a <- read.csv(file="DAT_NT_EURGBP_T_ASK_201504.csv", header = FALSE, sep = ";", stringsAsFactors = FALSE)
GU_a <- read.csv(file="DAT_NT_GBPUSD_T_ASK_201504.csv", header = FALSE, sep = ";", stringsAsFactors = FALSE)
# Bid
EU_b <- read.csv(file="DAT_NT_EURUSD_T_BID_201504.csv", header = FALSE, sep = ";", stringsAsFactors = FALSE)
EG_b <- read.csv(file="DAT_NT_EURGBP_T_BID_201504.csv", header = FALSE, sep = ";", stringsAsFactors = FALSE)
GU_b <- read.csv(file="DAT_NT_GBPUSD_T_BID_201504.csv", header = FALSE, sep = ";", stringsAsFactors = FALSE)

# data to time series
EU_a_ts <- CSVtoTimeSeries(EU_a,c("EU_a"))
EG_a_ts <- CSVtoTimeSeries(EG_a,c("EG_a"))
GU_a_ts <- CSVtoTimeSeries(GU_a,c("GU_a"))
EU_b_ts <- CSVtoTimeSeries(EU_b,c("EU_b"))
EG_b_ts <- CSVtoTimeSeries(EG_b,c("EG_b"))
GU_b_ts <- CSVtoTimeSeries(GU_b,c("GU_b"))

# lets consider just one day
EU_b_day <- subset(EU_b_ts, time(EU_b_ts) < (strptime("20150402 000001", "%Y%m%d %H%M%S")))
EG_b_day <- subset(EG_b_ts, time(EG_b_ts) < (strptime("20150402 000001", "%Y%m%d %H%M%S")))
GU_b_day <- subset(GU_b_ts, time(GU_b_ts) < (strptime("20150402 000001", "%Y%m%d %H%M%S")))
EU_a_day <- subset(EU_a_ts, time(EU_a_ts) < (strptime("20150402 000001", "%Y%m%d %H%M%S")))
EG_a_day <- subset(EG_a_ts, time(EG_a_ts) < (strptime("20150402 000001", "%Y%m%d %H%M%S")))
GU_a_day <- subset(GU_a_ts, time(GU_a_ts) < (strptime("20150402 000001", "%Y%m%d %H%M%S")))

df_in <- na.approx(cbind(EU_b_day, EG_a_day, GU_a_day))
df_out <- na.approx(cbind(EU_a_day, EG_b_day, GU_b_day))

# Parity equation:
# EURGBP*GBPUSD - EURUSD = 0 by definition
# construct portfolio data for entering and exiting
d_parity_in <- (df_in$EG_a * df_in$GU_a - df_in$EU_b)
d_parity_out <- (df_out$EG_b * df_out$GU_b - df_out$EU_a)

# there is overlap
plot(d_parity_in, ylim=c(-0.0002,0.0002))
lines(d_parity_out, col=rgb(1,0,0,0.5))

