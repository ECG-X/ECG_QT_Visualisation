library(pracma)
library(readr)
library(tibble)
library(signal)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggcyto)

#keep one column for the time 
TF <- read_excel("C:/Users/alahmada/Desktop/CHI after review/Camera ready/R/QT_417_Baseline.xlsx")
colnames(TF) <- c("time", "II")

#read the file and change the column names 
ECG <- read_excel("C:/Users/alahmada/Desktop/CHI after review/Camera ready/R/QT_455.xlsx")
colnames(ECG) <- c("time", "II")

#format the time
ECG$time <- TF$time

#plot the signal
plot(ECG$II, type="l", col="navy") + grid()

#find R peaks
# find peaks more than 0.6 
peaks1 <- findpeaks(ECG$II, minpeakheight=0.6, minpeakdistance=100)

#include peaks that have two sustained repeated values  
peaks2 <- findpeaks(ECG$II, minpeakheight=0.6, minpeakdistance=100, peakpat = "[+]{1,}[0]{1,}[-]{1,}")

#comibe all R peaks in one dataframe
peaks <- rbind(peaks1,peaks2)

#plot R peaks using red circels 
points(peaks[, 2], peaks[, 1], pch=20, col="maroon")

#name the columns in R peaks matrix 
colnames(peaks) <- c("R_mv","time","no_need1","no_need2")

#remove unneeded columns 
Rpeaks<-peaks[,-3:-4]

#sort the peaks by time, we need them sorted when calucating RR-intervals
new_Rpeaks_sorted <- Rpeaks[order(Rpeaks[, 2]), ] 
new_Rpeaks_sorted

#convert R_peaks matrix to dataframe
df_Rpeaks_RR <- as.data.frame(new_Rpeaks_sorted)

#delete the vector of mv , and keep only time vector
df_Rpeaks_RR <- df_Rpeaks_RR[,-1]


# values of RR-intervals will be used to caluclate average RR-interval and HR


# assign the index time of R-peaks in a vector 
R_peaks_time <- Rpeaks[,2]
R_peaks_time

Time_RR <- ECG$time 
mv_RR <- ECG$II 

#Calculate the heart rate from the RR-intervals
# first calulcate the RR-intervals
RR_interval <- NA

for (i in 1:length(df_Rpeaks_RR)-1)
{
  RR_interval[i] = df_Rpeaks_RR[i+1] - df_Rpeaks_RR[i]
}
RR_interval

#take the average RR-interval 
RR <- mean(RR_interval)
RR

#caluclate the heart rate 
HR <- 60000/RR
HR


#create a new ECG file
ECG_ <- cbind(Time_RR,mv_RR)
write.csv(ECG_,"C:/Users/alahmada/Desktop/CHI after review/Camera ready/R/file2.csv")

file2 <- read_csv("C:/Users/alahmada/Desktop/CHI after review/Camera ready/R/file2.csv")



#caluclate the incresed value fo half RR0interval to draw the dashed lines
half_RR_value <- (df_Rpeaks_RR[2] - df_Rpeaks_RR[1] ) / 2
#covert to millisconds 
half_RR_value <- half_RR_value/1000
half_RR_value

# plot the ECG with the colour scale
p <- ggplot(data=file2, aes(x=Time_RR, y=mv_RR))
p  +  
  #draw the vertical lines of R-peaks and dahsed lines for half RR-intervals
  geom_vline(xintercept = df_Rpeaks_RR[1]/1000, size=0.7) +
  geom_vline(xintercept = df_Rpeaks_RR[2]/1000, size=0.7) +
  geom_vline(xintercept = df_Rpeaks_RR[3]/1000, size=0.7) +
  geom_vline(xintercept = df_Rpeaks_RR[4]/1000, size=0.7) +
  geom_vline(xintercept = df_Rpeaks_RR[5]/1000, size=0.7) +
  geom_vline(xintercept = df_Rpeaks_RR[6]/1000, size=0.7) +
  geom_vline(xintercept = df_Rpeaks_RR[7]/1000, size=0.7) +
  geom_vline(xintercept = df_Rpeaks_RR[8]/1000, size=0.7) +
  geom_vline(xintercept = df_Rpeaks_RR[9]/1000, size=0.7) +
  geom_vline(xintercept = df_Rpeaks_RR[10]/1000, size=0.7) +
  
  geom_vline(xintercept = (df_Rpeaks_RR[1]/1000) + half_RR_value, linetype="dashed",  size=0.7) +
  geom_vline(xintercept = (df_Rpeaks_RR[2]/1000) + half_RR_value, linetype="dashed", size=0.7) +
  geom_vline(xintercept = (df_Rpeaks_RR[3]/1000) + half_RR_value,linetype="dashed", size=0.7) +
  geom_vline(xintercept = (df_Rpeaks_RR[4]/1000) + half_RR_value, linetype="dashed", size=0.7) +
  geom_vline(xintercept = (df_Rpeaks_RR[5]/1000) + half_RR_value, linetype="dashed", size=0.7) +
  geom_vline(xintercept = (df_Rpeaks_RR[6]/1000) + half_RR_value, linetype="dashed", size=0.7) +
  geom_vline(xintercept = (df_Rpeaks_RR[7]/1000) + half_RR_value, linetype="dashed", size=0.7) +
  geom_vline(xintercept = (df_Rpeaks_RR[8]/1000) + half_RR_value, linetype="dashed", size=0.7) +
  geom_vline(xintercept = (df_Rpeaks_RR[9]/1000) + half_RR_value, linetype="dashed", size=0.7) +
  geom_vline(xintercept = (df_Rpeaks_RR[10]/1000) + half_RR_value, linetype="dashed", size=0.7) +
  
  scale_y_continuous(minor_breaks = seq(-0.5, +1, 0.1),breaks = seq(-0.5, +1, 0.5), lim = c(-0.5, +1)) +
  scale_x_continuous(minor_breaks = seq(0 , 9.7, 0.04),breaks = seq(0, 9.6, 0.2), lim = c(0,9.7)) + 
  
  geom_ribbon(aes(ymin=-1, ymax=-0.5),fill="grey70",alpha =0,size=1) +
  geom_ribbon(aes(ymin=-0.5, ymax=0),fill="grey70",alpha =0.3,size=1) + 
  geom_ribbon(aes(ymin=0, ymax=0.5),fill="grey70",alpha =0,size=1) +
  geom_ribbon(aes(ymin=0.5, ymax=1),fill="grey70",alpha =0.3,size=1) + 
  
  theme(panel.grid.minor = element_line(colour="white"), panel.grid.major = element_line(colour = "white", size=1),legend.key.height=grid::unit(1.5,"cm")) + 
  
  geom_line(size=0.73) +
  theme(axis.text.x= element_text(size=20, color = "black",face="bold")) +
  theme(axis.text.y= element_text(size=20, color = "black",face="bold")) +
  
  geom_line(data=file2,aes(x=Time_RR, y=0), colour="#444444", lwd=0.5) + 
  labs(x ="Time (seconds)", y="mV") + theme(legend.text=element_text(size=11), axis.title=element_text(size=25,face="bold"))
ggsave("C:/Users/alahmada/Desktop/CHI after review/Camera ready/R/QT_455_C.png", width=32.31, height=6.14)

