#Planning:
#find out all 30 lm lines and layer all onto one plot to show the trend of steps per day through out the year
#for fitness tracker dataset. use bar chart and fill by ratings or type of band or brand to find out which device users prefer using ratings as a metric
#use weightLog to check if device was used for weight loss but have to mention the small sample size and come up with possible reason
#for sleep, draw two lines to compare total sleep and total time in bed and look at the trend or if two lines intersect

#Assumptions/Hypothesis:
#the dip in Steps per day at the end of the month and mid May can be caused by the fact....

steps_day <- read_csv("DailySteps_merged_v2.csv")

# transform ActivityDay date type from character to numeric values.
as.Date(steps_day$ActivityDay,format="%m%d%y")

#clean the original steps_day dataset by removing the zeros and no NA since there is no null value. 
# I then ordered the data by ActivityDay so the dates can be in chronological order.
steps_cleaned <- steps_day %>% filter(StepTotal !=0) %>% 
  arrange(ActivityDay)

#My goal is to group total steps by Dates and find the average, because I noticed there are duplicated Dates in the long table. 
# I didn't group them by the duplicated Ids because we want to find the average step count among the 30 participants, 
# so we are not going to individualize them, but I did make sure the outliers are cleaned. 
new_steps_day <- steps_cleaned %>% group_by(ActivityDay) %>% summarize(mean_step = mean(StepTotal))
print(new_steps_day) %>%  arrange(ActivityDay)

#Attemps:
#arrange the date in order 
# slice(new_steps_day, -c(21,22,23))
# # 21: 5/10/2016, 9397.136
# # 22: 5/11/2016, 8203.091
# # 23: 5/12/2016, 4301.706
# new_steps_day[29,] <- c(as.character(5/10/2016)), c(9397.136)


ggplot(new_steps_day, aes(ActivityDay, mean_step, group=1))+
  geom_line(color="hot pink")+
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(title="Average Step Count in a Month",subtitle=
         "average of 30 participants from Month April to May",
       x="Date", y= "Average Step Count")


# annotate("text", x = "5/2/2016", y = 6000, label = "Big dip explained below", angle = 85) +
# end of steps per day analysis
_______________________________________________________________________________________________- 
# start of calories per day analysis
  
calories <- read_csv("DailyCalories_v2.csv")
as.Date(steps_day$ActivityDay,format="%m%d%y")

calories_cleaned <- calories %>% filter(Calories !=0) %>% 
  arrange(ActivityDay)

new_calories<- calories_cleaned %>% group_by(ActivityDay) %>% 
  summarize(mean_calorie=mean(Calories))

ggplot(new_calories, aes(ActivityDay, mean_calorie))+
  geom_line(color="dark green")+
  labs(title="Average Calories Consumed per Day", 
       subtitle="Generated among 30 participants from April to May",
       x="Date", y="Average Calories Consumed")
#need more investigation into the dip at the end of the plot
------------------------------------------------------------------------------------
 #start of Intensity Analysis
Intensity <- read_csv("DailyActIntensity_v2.csv")

#planning:
#make two graphs: layered line chart and pie chart
#use activity min instead of distance because minbute has higer temperal resolution than distance, so to avoid abiguity.

    
intensity <- select(Intensity, ActivityDay, SedentaryMinutes, LightlyActiveMinutes, FairlyActiveMinutes, VeryActiveMinutes)

#to find the average minutes of different intensity 
new_intensity <- intensity %>% group_by(ActivityDay) %>% summarize(mean_sit_min=mean(SedentaryMinutes), 
  mean_lit_min=mean(LightlyActiveMinutes), mean_fair_min=mean(FairlyActiveMinutes),
  mean_act_min=mean(VeryActiveMinutes))

#one layered graph one facet_wrap

#reshape the dataset into a new dataset
data_ggp <- data.frame(x = new_intensity$ActivityDay,
                       y = c(new_intensity$mean_sit_min, 
                             new_intensity$mean_lit_min, 
                             new_intensity$mean_fair_min, 
                             new_intensity$mean_act_min), 
group = c(rep("mean_sit_min", nrow(new_intensity)),
rep("mean_lit_min", nrow(new_intensity)),
rep("mean_fair_min", nrow(new_intensity)),
rep("mean_act_min", nrow(new_intensity))))

#original graph (can be too packed)
ggplot(data_ggp, aes(x,y,col=group, group=1)) + geom_line() 

#closed up graph
range<- c(as.Date("2016-04-18"), as.Date("2016-05-02"))
ggplot(data_ggp, aes(x,y,col=group, group=1)) + geom_line() + 
  scale_x_continuous(limits = range) 
range<- c(as.Date("2016-04-18"), as.Date("2016-05-02"))
ggplot(data_ggp, aes(x,y,col=group, group=1)) + geom_line() + geom_smooth()+
  scale_x_continuous(limits = range) 
#pie chart
pie_intensity <- new_intensity %>% summarize(pie_sed=mean(mean_sit_min), 
                           pie_lit=mean(mean_lit_min),
                           pie_fair=mean(mean_fair_min),
                           pie_act=mean(mean_act_min))
print(pie_intensity)

act_lvl <- c(986, 192, 13.5, 21)
labels <- c("Sedentary","Lightly Active", "Fairly Active", "Very Active")
pie_v1 <- pie(act_lvl, labels, cex=0.7, main="Activity Level Average Minute Distribution", col=rainbow(length(act_lvl)) )

piepercent <- round (100* (act_lvl/sum(act_lvl)),1 )
pie_v2 <- pie(act_lvl, labels=piepercent, cex=0.7, main="Activity Level Average Minute Percent Distribution", col=rainbow(length(act_lvl)) )
legend("topright", c("Sedentary","Lightly Active", "Fairly Active", "Very Active"),
       cex = 0.7, fill = rainbow(length(act_lvl)))
------------------------------------------------------------------------------------------------------------------------------------------------
#Start WeightLog analysis
#cleaning data using BigQuert to turn Date to datetime format and Weights to numeric values
weight <- read_csv("WeightLog.csv") 
new_weight <- weight %>% group_by(DATE) %>% summarize(mean_kg=mean(WeightKg),
mean_pounds=mean(WeightPds), mean_BMI=mean(BMI)) 

3Q <- weight %>% summarize(mean_bmi=mean(BMI))*1.5
weight<-filter(weight, BMI< 30)
#removed one outlier because there was only out of 67 measurements and is gretaer than the third quartile:37.2

#line graph of weightPounds vs BMI
#In pounds
data_ggplot <- data.frame(x = new_weight$DATE,
                       y = c(new_weight$mean_pounds, 
                             new_weight$mean_BMI), 
                       group = c(rep("mean_pounds", nrow(new_weight)),
                                 rep("mean_BMI", nrow(new_weight))))
ggplot(data_ggplot, aes(x,y,col=group)) + geom_line() 

#In kilograms
data_ggplot1 <- data.frame(x = new_weight$DATE,
                          y = c(new_weight$mean_kg, 
                                new_weight$mean_BMI), 
                          group = c(rep("mean_kg", nrow(new_weight)),
                                    rep("mean_BMI", nrow(new_weight))))
ggplot(data_ggplot1, aes(x,y,col=group)) + geom_line() 

#bar graphs
bar_weight <- weight %>% group_by(DATE) %>% summarize(mean_kg=mean(WeightKg),
mean_pounds=mean(WeightPds), mean_BMI=mean(BMI), IsManualReport) 

bar_weight %>% ggplot()+geom_col( aes(x=DATE, y=mean_pounds, fill=IsManualReport))+
  labs(title="Avergae Weight Change during a Month", subtitle="differentiate by Report Method", y="Weight(Pds)")
----------------------------------------------------------------------------------------------------------------------------------------------------
#start DailySleep analysis
#Data cleaning process
#Used SQL code:_______ to transforme date, time into numeric values
sleep_day <- read_csv("DailySleep_8.24.22.csv")

#cleaning data
new_sleep <- sleep_day %>% 
  group_by(DATE) %>%
  summarize(mean_asleep=mean(TotalMinutesSleep),
            mean_inbed=mean(TotalTimeInBed))

#line graph down below
data_ggp <- data.frame(x = new_sleep$DATE,
                       y = c(new_sleep$mean_asleep, 
                             new_sleep$mean_inbed), 
                       group = c(rep("mean_asleep", nrow(new_sleep)),
                                 rep("mean_inbed", nrow(new_sleep))))
ggplot(data_ggp, aes(x,y,col=group)) + geom_line() +
  labs(title="Comparison between Time in Bed vs. Time Asleep", x="Date", y="Time(min)")

#bar graoh with fill as diff
data <- data.frame(col1=c(new_sleep$DATE),
                   Sleep=c(new_sleep$mean_asleep),
                   Inbed=c(new_sleep$mean_inbed))
data_mod <- cbind(data[1], stack(data[2:3]))
data_mod %>% ggplot()+ geom_col(aes(x=col1, y=values, fill=ind))+
  labs(title="Time in Bed vs. Time Asleep", x="Date", y="Time(min)")

#stacked bar graph in percent
percent_sleep<- data.frame(new_sleep$DATE, asleep=c(new_sleep$mean_asleep/new_sleep$mean_inbed),
                           sleep=c(new_sleep$mean_inbed-new_sleep$mean_asleep)/new_sleep$mean_inbed)
data <- data.frame(col1=c(percent_sleep$new_sleep.DATE),
                   Inbed=c(percent_sleep$sleep),
                   asleep=c(percent_sleep$asleep))
data_mod1 <- cbind(data[1], stack(data[2:3]))
data_mod1 %>% ggplot()+ geom_col(aes(x=col1, y=values, fill=ind))+
  labs(title="Time in Bed vs. Time Asleep Ratio in Percentage", x="Date", y="Time(min)")
--------------------------------------------------------------------------------------------------------------
#Start Fitness Tracker dataset

fit_tracker <- read_csv("fitness_tracker.csv")
fit_tracker %>% filter(!is.na(Rating__Out_of_5_))
#use bar graph or pie chart to show the diff between each character variable seperated by each level of rating
lvl1<- filter(fit_tracker, Rating__Out_of_5_ %in% (0:1))
lvl2<- filter(fit_tracker, Rating__Out_of_5_ %in% (1:2))
lvl3<- filter(fit_tracker, Rating__Out_of_5_ %in% (2:3))
lvl4<- filter(fit_tracker, Rating__Out_of_5_ %in% (3:4))
lvl5<- filter(fit_tracker, Rating__Out_of_5_ %in% (4:5))

lvl4 %>% ggplot()+geom_bar(aes(x=Brand_Name), fill="Violet")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(title="Count of Brands with Rating Above Three", x="Brands")

lvl5 %>% ggplot()+geom_bar(aes(x=Brand_Name), fill="blue")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(title="Count of Brands with Rating Above Four", x="Brands")

lvl5 %>% ggplot()+geom_bar(aes(x=Color), fill="blue")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(title="Count of Colors with Rating Above Four", x="Color")

fit_tracker %>% ggplot()+geom_col(aes(x=Brand_Name, y=Rating__Out_of_5_))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

fit_tracker %>% ggplot()+geom_col(aes(x=Strap_Material, y=Rating__Out_of_5_))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#pic charts for device type 
pie_device <- lvl5 %>% group_by() %>% summarize(pie_watch=sum(lvl5$Device_type=='Smartwatch'), pie_band=sum(lvl5$Device_type=='FitnessBand'))
print(pie_device)

rating_lvl <- c( 43, 6)
labels <- c("Smartwatch", "FitnessBand")
pie_v1 <- pie(rating_lvl, labels, cex=0.7, main="Device Type Distribution for Rating over Four", col=rainbow(length(rating_lvl)) )

piepercent <- round (100* (rating_lvl/sum(rating_lvl)),1 )
pie_v2 <- pie(rating_lvl, labels=piepercent, cex=0.7, main="Device Type with Rating over Four Percent Distribution", col=rainbow(length(act_lvl)) )
legend("topright", c("Smartwatch", "FitnessBand"),
       cex = 0.7, fill = rainbow(length(rating_lvl)))


# color <- select(fit_tracker, Color, Rating__Out_of_5_ )
# print(color)
# 
# color$data<- str_split_fixed(color$Color, ", ", 6)
# data_mod1 <- cbind(color[2], stack(color[3:4]))

#use geom_line to find trend between "Price(s)" vs "Rating"

new_fit_tracker<-fit_tracker %>% group_by(Rating__Out_of_5_) %>% 
  summarize(Selling_Price=mean(Selling_Price), 
            Original_Price=mean(Original_Price))

data_ggpp <- data.frame(x = new_fit_tracker$Rating__Out_of_5_,
                       y = c(new_fit_tracker$Selling_Price, 
                             new_fit_tracker$Original_Price), 
                       group = c(rep("Selling_Price", nrow(new_fit_tracker)),
                                 rep("Original_Price", nrow(new_fit_tracker))))
ggplot(data_ggpp, aes(x,y,col=group)) + geom_line()+
  labs(title="Rating vs Price of Device", subtitle="Take average of the price based on Ratings", x="Rating (out of 5)", y="Price")






