# Biostat-620-HW1
library(pacman)
p_load(dplyr,ggplot2,lubridate,readxl,GGally,circular,lubridate)

Biostat620_Trackdata <- read_excel("~/Desktop/Umich/Winter 2024/Biostat 620/Biostat620_Trackdata.xlsx")
#View(Biostat620_Trackdata)                                                                                               
df1 <- Biostat620_Trackdata 

# problem 1
# d:create and add two new variables
df1 <- df1 %>%
  mutate(daily_prop_Social = Social.ST.min/Total.ST.min) %>% 
  # daily proportion of social screen time
  mutate(daily_duration_per_use = Total.ST.min/Pickups)
  # daily duration per use
#View(df1)
# problem 2
# a: time series plots
#head(df1)
# total screen time
ggplot(df1, aes(x = Date, y = Total.ST.min)) +
  geom_line(color = "blue") + geom_point() +
  xlab("Date") + ylab("Total Screen Time (minutes)") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 60, hjust = 1))
# social media time
ggplot(df1, aes(x = Date, y = Social.ST.min)) +
  geom_line(color = "green") + geom_point() +
  xlab("Date") + ylab("Social Screen Time (minutes)") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 60, hjust = 1))
# pickups
ggplot(df1, aes(x = Date, y = Pickups)) +
  geom_line(color = "red") + geom_point() +
  xlab("Date") + ylab("Pickups") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 60, hjust = 1))
#daily_prop_Social
ggplot(df1, aes(x = Date, y = daily_prop_Social)) +
  geom_line(color = "purple") + geom_point() +
  xlab("Date") + ylab("Daily Proportion of Social Use") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 60, hjust = 1))
#daily_duration_per_use
ggplot(df1, aes(x = Date, y = daily_duration_per_use)) +
  geom_line(color = "orange") + geom_point() +
  xlab("Date") + ylab("Daily duration per use") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 60, hjust = 1))

# b: pairwise scatterplots
df1 %>% 
  ggpairs(columns = c("Total.ST.min","Social.ST.min","Pickups","daily_prop_Social","daily_duration_per_use"),
          columnLabels = c("Total Screentime","Social Screentime","Total Pickups","Daily Proportion of Social Use",
                           "Daily duration per use"),
          progress = F) +
  theme_bw()

# c: occupation time:
# total screentime
total_st_min_ecdf <- ecdf(df1$Total.ST.min)
ggplot(data.frame(x = unique(df1$Total.ST.min)), aes(x = x)) +
  stat_function(fun = function(x) total_st_min_ecdf(x), geom = "step") +
  xlab("Total Screen Time (minutes)") + ylab("Proportion of Time") +
  ggtitle("Occupation Time Curve for Total Screen Time") +
  theme_minimal()

# social screen time
social_st_min_ecdf <- ecdf(df1$Social.ST.min)
ggplot(data.frame(x = unique(df1$Social.ST.min)), aes(x = x)) +
  stat_function(fun = function(x) social_st_min_ecdf(x), geom = "step") +
  xlab("Social Screen Time (minutes)") + ylab("Proportion of Time") +
  ggtitle("Occupation Time Curve for Social Screen Time") +
  theme_minimal()

# pickups
pickup_st_min_ecdf <- ecdf(df1$Pickups)
ggplot(data.frame(x = unique(df1$Pickups)), aes(x = x)) +
  stat_function(fun = function(x) pickup_st_min_ecdf(x), geom = "step") +
  xlab("Pickups") + ylab("Proportion of Time") +
  ggtitle("Occupation Time Curve for Pickups") +
  theme_minimal()

#daily_prop_Social
daily_prop_Social_ecdf <- ecdf(df1$daily_prop_Social)
ggplot(data.frame(x = unique(df1$daily_prop_Social)), aes(x = x)) +
  stat_function(fun = function(x) daily_prop_Social_ecdf(x), geom = "step") +
  xlab("Daily Proportion of Social Use") + ylab("Proportion of Time") +
  ggtitle("Occupation Time Curve for Daily Proportion of Social Use") +
  theme_minimal()

#daily_duration_per_use
daily_duration_per_use_ecdf <- ecdf(df1$daily_duration_per_use)
ggplot(data.frame(x = unique(df1$daily_duration_per_use)), aes(x = x)) +
  stat_function(fun = function(x) daily_duration_per_use_ecdf(x), geom = "step") +
  xlab("Daily Duration per Use (minutes)") + ylab("Proportion of Time") +
  ggtitle("Occupation Time Curve for Daily Duration per Use") +
  theme_minimal()

# d:
# total screentime
acf(df1$Total.ST.min)
acf(df1$Total.ST.min,plot=FALSE)
# social screen time
acf(df1$Social.ST.min)
acf(df1$Social.ST.min,plot=FALSE)
# pickups
acf(df1$Pickups)
acf(df1$Pickups,plot=F)
# daily_prop_Social
acf(df1$daily_prop_Social)

acf(df1$daily_prop_Social,plot=FALSE)
#daily_duration_per_use
acf(df1$daily_duration_per_use)
acf(df1$daily_duration_per_use,plot=FALSE)

# problem 3
# a: Transform (or covert) the time of first pickup to an angle ranged from 0 to 360 degree
df1 <- df1 %>%
  mutate(Pickup_1st.angular = (hour(Pickup.1st)*60+minute(Pickup.1st))/(24*60)*360)
View(df1)
first.pickup.cir <- circular(df1$Pickup_1st.angular,units = "degrees",template = "clock24")

# b: scatter plot
plot(first.pickup.cir,col="blue")

# c:histogram
plot(first.pickup.cir, stack = TRUE, bins = 360, col = "green")

# problem 4
# b
model1 <- glm(Pickups ~ offset(log(Total.ST.min)), data = df1, family
                     = "poisson")
summary(model1)

# c: add dummy variable
df1$weekday <- weekdays(df1$Date,abbreviate = T)
df1 <- df1 %>%
  mutate(weekend = weekday %in% c("Sat", "Sun")) %>%
  mutate(z = if_else(Date >= as.Date("2024-01-10"), 1, 0))
model2 <- glm(Pickups ~ offset(log(Total.ST.min)) + weekend + z, data = df1, family
              = "poisson")
summary(model2)

# problem 5
# a
first_pickup_rad <- circular((first.pickup.cir)*pi/180-pi,units = "radians")
var4 <- mle.vonmises(first_pickup_rad)
var5 <- var4$mu
var6 <- var4$kappa

# b:prob of later 8:30
time <- ((8*60)+30)/(24*60) *360
time2 <- (-time * (pi/180))-pi
1-(pvonmises(circular(time2),mu=var5,kappa = var6))
