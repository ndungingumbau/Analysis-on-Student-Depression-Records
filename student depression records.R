library(readr)
library(dplyr)
library(ggplot2)

df <- read.csv("C:/Users/pc/Desktop/R/studentdepressiondataset.csv")
df

summary(df)
str(df)

sapply(df, class)

head(df)
tail(df)

colSums(is.na(df))

df <- na.omit(df)

mean(df$Depression)

df%>%
  group_by(Gender)%>%
  summarise(mean_score = mean(Depression, na.rm = TRUE))

ggplot(df, aes(x = Depression))+
  geom_histogram(bins = 30, fill = "skyblue", color = "black")+
  theme_minimal()

ggplot(df, aes(x= Academic.Pressure, y= Depression))+
  geom_boxplot(fill = "lightgreen")+
  theme_minimal()

ggplot(df, aes(x = Gender, y = Depression, fill = Gender))+
  geom_violin(trim = FALSE)+
  theme_minimal()

ggplot(df, aes(x=Sleep.Duration, y=Depression))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  theme_minimal()

t.test(Depression ~ Gender, data = df)

anova_result = aov(Depression ~ Academic.Pressure, data = df)
summary(anova_result)


      
