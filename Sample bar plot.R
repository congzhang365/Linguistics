#---------Loading libraries: some are necessary some are not--------
library(Matrix)
library(plyr)
library(lme4)
library(lmerTest)
library(multcomp)
library(tidyr)
library(RColorBrewer)
library(xlsxjars)
library(xlsx)
library(plotrix) # for std error function
library(dplyr) # for group_by and summarise_each function
library(ggplot2) # for creating ggplot
Sys.setlocale("LC_ALL","English")

#--------Loading files------------
raw.data = read.csv("C:/Users/abc.csv") #Read from csv files. Use read.table to read txt.
raw.data$A<-as.factor(raw.data$A) # Making A into a 'factor'
raw.data$B<- as.factor(raw.data$B) # Making B into a factor too
str(raw.data) # have a look at your data
#-----------loading data----------------------
data <- raw.data %>% filter(type %in% c("S","Q")) %>% group_by(type,tone) 
%>% summarize(MeanF0=mean(mean.pitch..ERB.,na.rm=TRUE), #calculate mean value
              sd=sd(mean.pitch..ERB.,na.rm=TRUE), #calculate standard deviation
              se=std.error(mean.pitch..ERB.,na.rm=TRUE), #calculate standard error
              MAX=max(mean.pitch..ERB.,na.rm=TRUE),  #get the maximum value
              MIN=min(mean.pitch..ERB.,na.rm=TRUE)) #get the minimum value

##rewrite tones
data$tone <- revalue(data$tone, c("T1"="T1(L)", "T2"="T2(H)","T3"="T3(LH)","T4"="T4(HL)"))

##save data into excel
##!!!!check check check check before saving!!!!
##-----Are you sure you want to save?------##
write.xlsx(data,"descriptive data.xlsx",sheetName = "ABC",append = TRUE)


#---------------make bar plot----------------
#if you want to add the outline, use colour="black" or other colours.
plot <-ggplot(data,aes(x=tone,y=MeanF0,fill=type))+
  geom_bar(stat="identity",position="dodge") + xlab("Tone") + ylab("Mean Pitch (ERB)")

#---------------add values--------------------
#round: how many demicals you want to keep
#vjust: control the height of the texts
text=geom_text(aes(label=round(MeanF0,2)),colour="black", position = position_dodge(0.9), vjust = -4.5)
plot = plot+text
plot
#use this when you want to draw the error bar as SE
#errorbar = geom_errorbar(aes(ymin=MeanF0-se,ymax= MeanF0 + se),position = position_dodge(width = 0.9),width= .4)
#errorbar = sd
errorbar = geom_errorbar(aes(ymin=MeanF0-sd,ymax= MeanF0 + sd),position = position_dodge(width = 0.9),width= .4)
plot = plot + errorbar
plot
plot.colour <- plot + expand_limits(y=c(0,9)) +  scale_y_continuous(breaks=seq(0,9,1))#+ coord_fixed(ratio = 0.6)
plot.colour<-plot.colour +scale_fill_brewer(palette="Set1")
plot.colour
#save the plot
ggsave("zi-onset-mean.pitch..ERB.-cl.png")
#make a black and white version
plot.bw = plot.colour + scale_fill_grey(start=0.8, end=0.4) 
plot.bw
ggsave("zi-onset-mean.pitch..ERB.-bw.png")
#####
