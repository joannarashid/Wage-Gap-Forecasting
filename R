#Gender Wage Gap Forecasting Project
#ECON 138 Spring 2021


library(devtools)
library(MTS)
library(fpp)
library(fpp2)
library(fpp3)
library(seasonal)
library(GGally)
library(tidyverse)
library(tsibble)
library(magrittr)
library(dplyr)
library(emulator)
library(ggplot2)
library(tidyverse)
library(ggfortify)
library(forecast)
library(broom)

######DATA IMPORT#########
wage_gap <- read.csv("~/Desktop/R_ECON_138/wage_gap.csv")

birth_rate <- read.csv("~/Desktop/R_ECON_138/avg_child_per_woman_US.csv")

marital_status <- read.csv("~/Desktop/R_ECON_138/marital_status_US.csv")

percent_bach_achieved <- read.csv("~/Desktop/R_ECON_138/edu_attainment_women.csv")

labor_force <- read.csv("~/Desktop/R_ECON_138/labor_force_particip.csv")

######DATA CLEANING########
#(some data cleaning and filtering done in excel)
#making new dataframe called df for joined data

#selecting Year, female:male wage ratio, female median earnings
df <- wage_gap %>% select(Year, fm_ratio, median_earnings)

#joining birth_rate with wage_gap by year 
df <- left_join(
  wage_gap,
  birth_rate,
  by = 'Year',
  copy = FALSE,
  keep = FALSE,
  na_matches = c("na", "never")
)

#creating new variable for marriage rate
marital_status$mar_rate <- marital_status$Married/marital_status$Total

#joining marital data to df
df <- left_join(
  df,
  marital_status,
  by = 'Year',
  copy = TRUE,
  keep = FALSE,
  na_matches = c("na", "never")
)

#joining percent >=bachelors data to df
df <- left_join(
  df,
  percent_bach_achieved,
  by = 'Year',
  copy = TRUE,
  keep = FALSE,
  na_matches = c("na", "never")
)

#joining labor force participation data to df
df <- left_join(
  df,
  labor_force,
  by = 'Year',
  copy = TRUE,
  keep = FALSE,
  na_matches = c("na", "never")
)

#creating df with all original data
df_original <- df

#eliminating years with missing values
df <- df %>% filter(df$Year > 1992)

#transforming earning to smaller figures
df$median_earnings = (df$median_earnings/10000)

#eliminating uneeded columns
df <- subset (df, select = -c(workers, Total.x, Married, 
                              Total.y, bachelors))
#reversing order to be decending
df<- df[seq(dim(df)[1],1),]

#converting our df to mts
df_mts = ts(df,
            frequency = 1,
            start = c(1993), 
            class = "mts")

##########EDA########
df_mts_summary <- summary(df_ts)

df_mts_summary

#exporting summary table to txt file
write.table(df_mts_summary, "~/Desktop/R_ECON_138/df_summary.txt", sep="\t")

######DATA VIZ########

#scatter plot of wage gap over time
ggplot(data = df_original, aes(x = Year, y = fm_ratio)) + 
  geom_point() +
  theme_grey() +
  geom_smooth(method = "lm", se = TRUE, color = "Gold") +
  ylim(0, 1)+
  labs(
    title = "Ratio of Female:Male Wages (Wage Gap)",
    x = "Year",
    y = "Ratio")

#scatter plot of average number of children/woman born over time
ggplot(data = df_original, aes(x = Year, y = child)) + 
  geom_point() +
  theme_light() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    title = "Average Number of Children Born per Woman",
    x = "Year",
    y = "Average Number of Children")


#scatter plot of marriage rates
ggplot(data = df_original, aes(x = Year, y = mar_rate)) + 
  geom_point() +
  theme_light() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Percent Adult Women Married",
    x = "Year",
    y = "% Married Woman")

#scatter plot of percent bachelor attainment
ggplot(data = df, aes(x = Year, y = percent_bachelors)) + 
  geom_point() +
  theme_light() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Percent of Bachelor Attainment",
    x = "Year",
    y = "Percent Bachelors Attaiment")


#scatter plot labor force particpation
ggplot(data = df, aes(x = Year, y = lfp_rate)) + 
  geom_point() +
  theme_light() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Labor Force Participation Rate",
    x = "Year",
    y = "LFP Rate")

#Correlation matrix
ggpairs(data=df, columns=2:7,
        title="F:M Wage Ratio - Variable Correlation Matrix")

df_ts <- as.ts(df)

#plots all variables seperately
plot(df_ts, main ="Variables Affecting the Ratio of F to M Wages") 


######LINEAR REGRESSION###########

l_model <- tslm(fm_ratio ~ 
              child + 
              mar_rate +
              median_earnings+
              percent_bachelors,
              #lfp_rate,
              data=df_ts)
summary(l_model)

#exporting model data
tidy_lmf <- tidy(l_model)
write.table(tidy_lmf, "~/Desktop/R_ECON_138/model_summary.txt", sep="\t")

#####Checking Residual of Model

checkresiduals(l_model)

#no heteroskedacisity or autocorrelation found

######Test for Stationary#######

adf.test(df$fm_ratio, k=1)
adf.test(df$child, k=1)
adf.test(df$mar_rate, k=1)
adf.test(df$median_earnings, k=1)
adf.test(df$percent_bachelors, k=1)
adf.test(df$lfp_rate, k=1)

#PROF ADVISES USING A DIFFERENCE MODEL AS A RESULT OF ALL THESE ADF TESTS

#######Difference Model setup:
N = length(df$fm_ratio)

##lagged variables
lagged_fm_ratio = c(NA, df$fm_ratio[1:(N-1)]) #lagged fm_ratio
lagged_birth_rate = c(NA, df$child[1:(N-1)])  #lagged birth rate
lagged_median_earning =c(NA, df$median_earning[1:(N-1)]) #lagged median earnings
lagged_mar_rate = c(NA, df$mar_rate[1:(N-1)]) #lagged marriage rate
lagged_percent =c(NA, df$percent_bachelors[1:(N-1)]) #lagged percent bachelors attainment
lagged_lfp_rate = c(NA, df$lfp_rate[1:(N-1)]) #lagged labor force participation rate

#Deltas:
delta_fm_ratio = df$fm_ratio - lagged_fm_ratio #delta: fm_ratio
delta_median_earning = df$median_earnings- lagged_median_earning #delta: median earnings
delta_mar_rate = df$mar_rate - lagged_mar_rate #delta:marriage rate
delta_birth_rate = df$child - lagged_birth_rate #delta: birth rate
delta_percent = df$percent_bachelors - lagged_percent #delta:percent of bachelor degrees attained
delta_lfp_rate = df$lfp_rate - lagged_lfp_rate #delta: labor force participation rate

#difference model
delta_model = tslm(fm_ratio ~ 
                   delta_birth_rate + 
                   delta_median_earning +
                   delta_mar_rate + 
                   delta_percent,
                   data= df_ts)
                   #delta_lfp_rate)
summary(delta_model)

#exporting model data
tidy_delta_lmf <- tidy(delta_model)
write.table(tidy_delta_lmf, "~/Desktop/R_ECON_138/delta_model_summary.txt", sep="\t")

#joining fitted Values to df_ts
df$fitted <- delta_model$fitted.values

#exporting completed df to make actual vs. fitted table in excel
write.csv(df,"~/Desktop/R_ECON_138/df.csv", row.names = FALSE)

#plot fitted vs actual
ggplot(data = df, aes(x = Year, y = fm_ratio, color = "actual"))+
  geom_point()+
  geom_line(aes(y = fitted, color="fitted"))+
  theme_grey() +
  labs(title = "Gender Wage Ratio Fitted vs. Actual",
    x = "Year",
    y = "F:M Wage Ratio")


#########Forecasting#########

#simple forecast
lm_fcast <- forecast(df$fm_ratio, h= 50)
autoplot(lm_fcast)

#forecast using fitted values
y <- delta_model$fitted.values

fcast <- forecast(
  y,
  h = 30,
  level = c(80,95),
  fan = TRUE,
  robust = TRUE,
  lambda = "auto",
  biasadj = TRUE,
  find.frequency = FALSE,
  allow.multiplicative.trend = FALSE
)

#visualize the forecast plot
autoplot(fcast, 
         main = "Gender Wage Ratio Forecast, 95% Confidence Interval, Hoizon = 30 years",
         xlab = "F:M Wage Ratio",
         ylab = "Year")
