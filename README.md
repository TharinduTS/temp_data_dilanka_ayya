# temp_data_dilanka_ayya

```r
#set current path as wd
library("rstudioapi") 
setwd(dirname(getActiveDocumentContext()$path))

current_working_directory<-dirname(getActiveDocumentContext()$path)


require("gdata")
require("ggplot2")
require("dplyr")
require("tidyverse")
require("lubridate")
require("readr")
require("lubridate")
require(rlang)

library(dplyr) 
library(ggplot2) 
library(lubridate) 
library(scales)
library(xlsx)
library(readxl)
library(tidyverse)
library(readr)
library(lubridate)
library(scales)
library(ggplot2)
library(rlang)




# list files from the relavant folder
files <- list.files(path = current_working_directory, pattern = "\\.ddf")

# read the first file only/ I will be adding the rest of the temperature columns from other files to this dataframe later
# ( here skipped 26 lines to remove column names as well, added column names later)
data<-read.table(files[[1]],skip = 26,sep = "\t", dec = ".",
                 stringsAsFactors = FALSE)

#subset temp data only
data_temp_only<-subset.data.frame(data,select = V1:V2)

#create column names adding time and file name

#get line for time from file
line_for_time_from_file<-read_lines(files[[1]],skip = 10,n_max = 1)
#separate time 
time_seperated<-str_split(line_for_time_from_file,"\t",simplify = TRUE)
#get time only
time_only<-time_seperated[1,2]
time_only_symbols_fixed<-sub(":","_",time_only)
time_only_symbols_full_fixed<-sub(":","_",time_only_symbols_fixed)

#Seperate date and time for each file
#Sperate date
line_for_date_from_file<-read_lines(files[[1]],skip = 9,n_max = 1)
date_seperated<-str_split(line_for_date_from_file,"\t",simplify = TRUE)
#get date only
date_only<-date_seperated[1,2]
#combine date and time
temp_col_name_datetime1 <-paste(date_only,time_only,sep = " ")
temp_col_name_datetime1 <- as.POSIXct(temp_col_name_datetime1,format='%Y/%m/%d %H:%M:%S')
temp_col_name_datetime1 <- strftime(temp_col_name_datetime1)



#get file name
filename<-gsub(".ddf", "",files[[1]])
filename_spaces_removed<-gsub(" ","_",filename)

temp_col_name<-paste(filename_spaces_removed,time_only_symbols_full_fixed,sep = "_")

#change colnames
#colnames(data_temp_only)<-c("Length",temp_col_name)
colnames(data_temp_only) <- c("Length",temp_col_name_datetime1)


# add rest of the temp columns from other files
for (file in 2:length(files)) {
  
  
  # read data from files 2:length
  data_temperory<-read.table(files[[file]],skip = 26,sep = "\t", dec = ".",
                             stringsAsFactors = FALSE)
  
  #subset temp data only
  data_temp_only_temporary<-subset.data.frame(data_temperory,select = V2)
  
  #create column names adding time and file name
  
  #get line fortime from file
  line_for_time_from_file_temporary1<-read_lines(files[[file]],skip = 10,n_max = 1)
  #seperate time 
  time_seperated_temporary1<-str_split(line_for_time_from_file_temporary1,"\t",simplify = TRUE)
  #get time only
  time_only_temporary1<-time_seperated_temporary1[1,2]
  #time_only_temporary_symbols_fixed<-sub(":","_",time_only_temporary)
  #time_only_temporary_symbols_full_fixed<-sub(":","_",time_only_temporary_symbols_fixed)
  
  #Seperate date
  line_for_date_from_file1<-read_lines(files[[file]],skip = 9,n_max = 1)
  date_seperated1<-str_split(line_for_date_from_file1,"\t",simplify = TRUE)
  #get date only
  date_only1<-date_seperated1[1,2]
  #combine date and time
  
  
  #get file name
  #filename_temporary<-gsub(".ddf", "",files[[file]])
  #filename_temporary_spaces_removed<-gsub(" ","_",filename_temporary)
  
  
  temp_col_name_datetime1 <-paste(date_only1,time_only_temporary1,sep = " ")
  temp_col_name_datetime1 <- as.POSIXct(temp_col_name_datetime1,format='%Y/%m/%d %H:%M:%S')
  #temp_col_name_temporary<-paste(filename_temporary_spaces_removed,time_only_temporary_symbols_full_fixed,sep = "_")
  
  #change colnames
  colnames(data_temp_only_temporary)<-c(temp_col_name_datetime1)
  
  
  # add this column to dataframe with all data
  
  data_temp_only<-bind_cols(data_temp_only,data_temp_only_temporary)
 
}


colnames(data_temp_only)
# save all data in a tsv and csv

write.table(data_temp_only, file = "all_data.tsv",sep = "\t")
write.csv(data_temp_only, file = "all_data.csv")

 

# fix filenames to get colnames
columns<-colnames(data_temp_only)

Length<-"length"

#exchange columns to rows and delete first row
require(data.table)
library(data.table)
library(data.table)
data_temp_onlyrename <- data_temp_only
df2 <- data.frame(t(data_temp_onlyrename[]))

df3 <- df2

#names(df3) <- as.matrix(df3[1, ])
#df3 <- df3[-1, ]
#df3[1]

# solve the colname issue
require(data.table)
library(data.table)
df3 <- setDT(df2, keep.rownames = TRUE)[]
df4 <- df3[-1, ] 
colnames(df4) <- as.character(df3[1,])

#change first column to date time
require("anytime")
library(anytime)
df4$Length <- anytime::anydate(df4$Length)


#calculate daily average

#get daily mean value
Daily_Avg <-df4 %>%
  group_by(Length) %>% 
  summarise_all(mean)

Daily_Avg1 <- Daily_Avg

final_matrix<-t(Daily_Avg1)
final_df1<-as.data.frame(final_matrix)

final_df2 <- final_df1[-1, ]
final_df3<-setDT(final_df2, keep.rownames = TRUE)[]

date_list<-as.character(final_matrix[1,])
colnames_list<-append("length",date_list)
colnames(final_df3) <- colnames_list

final_df3$length<-as.numeric(final_df3$length)
final_df3$`2021-03-08`<-as.numeric(as.character(final_df3$`2021-03-08`))

final_df3[, 2:ncol(final_df3)] <- lapply(2:ncol(final_df3), function(x) as.numeric(as.character(final_df3[[x]])))
#final_df_lower_limit_rmvd<-subset.data.frame(final_df3,40<length)
#final_df_both_limits_applied<-subset.data.frame(final_df_lower_limit_rmvd,length<1960)

column_list<-colnames(final_df3)
selected_col_nmaes<-column_list[-1]
final_col_list<-as.character(selected_col_nmaes)


plot<-ggplot(data = final_df3,aes(x = length,y= `2021-03-08`,color=final_col_list[1]))+
  geom_line()+
  xlim(40,1960)+
  ylim(-10,10)
 
 for (col_number in 3:length(colnames_list)) {
   #column_for_y<-paste("final_df3$`",colnames_list[col_number],"`",sep = "")
  
   
    plot<-plot+geom_line(data = final_df3,aes_string(x = "final_df3$length",y= paste("final_df3$`",colnames_list[col_number],"`",sep = ""),color=paste("final_col_list[",col_number-1,"]")))
    
    
  }

#plot

ggsave(filename = "testing.pdf",plot = plot,height = 10,width =15 )




















# #plot data
# 
# #change rows and columns to make plotting easier
# df4plt     <- as.data.frame(t(Daily_Avg1[]))
# df4plt <- setDT(df4plt, keep.rownames = TRUE)[]
# colnames(df4plt) <- as.character(df4plt[1,])
# df4plt <- df4plt[-1, ] 
# 
# #df4plt$date <- rownames(df4plt)
# #df4plt    <- melt(df4plt, id.vars=c("date"))
# 
# #plot data after getting the mean values
# colnames(df4plt)
# 
# require(reshape2)
# library(reshape2)
# df5 <- melt(df4plt, id.vars='Length')
# df5nw <- as.data.frame(df5)
# 
# #plot
# plotdf5<-ggplot(data=df5nw, aes(x=Length, y=value, colour = variable, group = variable)) +
#   geom_line()+ scale_x_discrete(limits = c(40,1960))+scale_y_discrete(limits = c(-10,10))
#   
# plotdf5  
# 
# plotdf<-ggplot(data=df4plt, aes(x=Length, y=, colour = variable, group = variable)) +
#   geom_line()+ scale_x_discrete(limits = c(40,1960))+scale_y_discrete(limits = c(-10,10))
# 
# plotdf5 
# 
# firstplt <- ggplot(data = df4plt, aes(x=Length, Y= 2021-03-08))+geom_line()
# # plot all data
# 
# for (plot_number in 2:length(files)) {
#   file_name_part<-substring(columns[plot_number],1,27)
#   file_name_for_title<-gsub("_"," ",file_name_part)
#   
#   time_part<-substring(columns[plot_number],30,37)
#   time_for_title<-gsub("_",":",time_part)
#   full_title<-paste(file_name_for_title,time_for_title)
#   
# ylabplot = expression('Temperature ('*~degree*C*')')
# plot_x<-ggplot(data = data_temp_only,aes_string(x=columns[1],y=columns[plot_number]))+
#   geom_line (color = "blue", size = .6)+  theme_bw()+ labs(x="Length after fibre (m)",y= ylabplot)+
#   scale_x_continuous(expand = c(0, 40),limits = c(40, 1960),breaks = seq( 0,1900, by = 100))+scale_y_continuous(limits = c(-10,10), breaks = seq(from = -10, to = 10, by = 2))+
#   theme(text=element_text(size=20))+ ggtitle(paste(file_name_for_title,time_for_title))
# 
# ggsave(filename = paste("./plots/plot",plot_number-1,".png",sep = ""),plot = plot_x,height = 10,width =15 )
# 
# }
# 
# 
# 

```
