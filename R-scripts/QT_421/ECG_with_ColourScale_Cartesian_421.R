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
TF <- read_excel("C:/Users/alahmada/Desktop/CHI_study/vis/R/QT_417_Baseline.xlsx")
colnames(TF) <- c("time", "II")

#read the file and change the column names 
ECG <- read_excel("C:/Users/alahmada/Desktop/CHI_study/vis/R/421/QT_421.xlsx")
colnames(ECG) <- c("time", "II")

#format the time
ECG$time <- TF$time

#plot the signal
plot(ECG$II, type="l", col="navy") + grid()

#find R peaks
# find peaks more than 0.6 mv
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

#delete the vector of mv, and keep only time vector
df_Rpeaks_RR <- df_Rpeaks_RR[,-1]

df_Rpeaks_RR
# values of RR-intervals will be used to caluclate average RR-interval and HR
df_Rpeaks_RR

# assign the index time of R-peaks in a vector 
R_peaks_time <- Rpeaks[,2]


Time_RR <- ECG$time 
mv_RR <- ECG$II 

#Calculate the heart rate from the RR-intervals
# first calulcate the RR-intervals
RR_interval <- NA

for (i in 1:length(df_Rpeaks_RR)-1)
{
  RR_interval[i] = df_Rpeaks_RR[i+1] - df_Rpeaks_RR[i]
}


#take the average RR-interval 
RR <- mean(RR_interval)
RR

#caluclate the heart rate 
HR <- 60000/RR
HR


#next code for creating the colour scale vector
maximumColourVector <- 490 #half RR-interval when HR around 60 bpm
colourVector <- NA
length(colourVector) <- 10000 #this equals to the length of ECG signal  

for (R in 1:length(R_peaks_time))
{ if (R_peaks_time[R] >= 100)
  index <- R_peaks_time[R] - 28
else index <- R_peaks_time[R]

for (i in 1:maximumColourVector)
{ 
  colourVector[index +i]=i
}

}


#any time out of the 570 range (i.e. NA) make it grey 

colourVector[is.na(colourVector)] <- 800

LowerColourlimit <- 250

UpperColourLimit <- 570

for (n in 1:length(colourVector))
{ 
  if(colourVector[n] < LowerColourlimit)
  { colourVector[n] <- LowerColourlimit}
  
  if(colourVector[n] > UpperColourLimit && colourVector[n]< 800)
  {colourVector[n] <- UpperColourLimit}
  
}

#create a new ECG file
ECG_with_colourScale <- cbind(Time_RR,mv_RR,colourVector,UpperColourLimit,LowerColourlimit)
write.csv(ECG_with_colourScale,"C:/Users/alahmada/Desktop/CHI_study/vis/R/file2.csv")

file2 <- read_csv("C:/Users/alahmada/Desktop/CHI_study/vis/R/file2.csv")


#next script is for visulising the ECG with the colour scale
colorcodesValues <- NA
length(colorcodesValues) <- 9
colorcodesValues[1] <- 255 # purple 
colorcodesValues[2] <- UpperColourLimit - (40*7) #blue
colorcodesValues[3] <- UpperColourLimit - (40*6) #lime
colorcodesValues[4] <- UpperColourLimit - (40*5) #green
colorcodesValues[5] <- UpperColourLimit - (40*4) #yellow
colorcodesValues[6] <- UpperColourLimit - (40*3) #orange
colorcodesValues[7] <- UpperColourLimit - (40*2) #dark orange
colorcodesValues[8] <- UpperColourLimit - (40*1) #red
colorcodesValues[9] <- 565 #dark red

#create the pesudo colour vector using spectral codes
myColor <- rev(RColorBrewer::brewer.pal(11, "Spectral"))
myColor_scale_fill <- scale_fill_gradientn(colours = myColor,breaks=c(colorcodesValues[1],colorcodesValues[2],colorcodesValues[3],colorcodesValues[4],colorcodesValues[5],colorcodesValues[6],colorcodesValues[7],colorcodesValues[8],colorcodesValues[9]),labels=c("> 250",290,330,370,410,450,490,530,"< 570"),limits=c(250,570))

#caluclate the incresed value fo half RR0interval to draw the dashed lines
half_RR_value <- (df_Rpeaks_RR[2] - df_Rpeaks_RR[1] ) / 2
#covert to millisconds 
half_RR_value <- half_RR_value/1000
half_RR_value

# plot the ECG with the colour scale
p <- ggplot(data=file2, aes(x=Time_RR, y=mv_RR, fill=colourVector))
p  +  
  #draw the vertical lines of R-peaks and dahsed lines for half RR-intervals
  geom_vline(xintercept = 0, size=0.7) +
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
  
  
  geom_vline(xintercept = 0 + half_RR_value, linetype="dashed",  size=0.7) +
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
  scale_x_continuous(minor_breaks = seq(0 , 9.7, 0.04),breaks = seq(0, 9.7, 0.2), lim = c(0,9.7)) + 
  
  geom_ribbon(aes(ymin=-1, ymax=-0.5),fill="grey70",alpha =0,size=1) +
  geom_ribbon(aes(ymin=-0.5, ymax=0),fill="grey70",alpha =0.3,size=1) + 
  geom_ribbon(aes(ymin=0, ymax=0.5),fill="grey70",alpha =0,size=1) +
  geom_ribbon(aes(ymin=0.5, ymax=1),fill="grey70",alpha =0.3,size=1) + 
  
  theme(panel.grid.minor = element_line(colour="white"), panel.grid.major = element_line(colour = "white", size=1),legend.key.height=grid::unit(2.5,"cm"),legend.key.width = unit(1,"cm")) + 
  
  geom_bar(stat="identity", position ="dodge") + geom_line(size=0.73) + myColor_scale_fill +
  
  geom_line(data=file2,aes(x=Time_RR, y=0), colour="#444444", lwd=0.5) + 
  theme(axis.text.x= element_text(size=20, color = "black",face="bold")) +
  theme(axis.text.y= element_text(size=20, color = "black",face="bold")) +

  geom_line(data=file2,aes(x=Time_RR, y=0), colour="#444444", lwd=0.5) + 
  labs(x ="Time (seconds)", y="mV", fill = "1/2 RR-Interval (ms)") + theme(legend.text=element_text(size=15),axis.title=element_text(size=25,face="bold"),legend.title=element_text(size=20))
  ggsave("C:/Users/alahmada/Desktop/CHI_study/vis/R/QT_421_CC.png", width=32.31, height=6.14)

