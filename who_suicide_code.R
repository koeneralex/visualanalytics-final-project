## Libraries & Data -------

library(ggplot2)
library(tidyverse)
library(reshape2)

suicideData <- read.csv("who_suicide_statistics.csv")

levels <- c("5-14 years","15-24 years", "25-34 years", "35-54 years", "55-74 years", "75+ years")
suicideData$age <- factor(suicideData$age, levels = levels)

suicideData <- suicideData[!is.na(suicideData$suicides_no),]


ggplot(suicideData, aes(x=age, y=suicides_no, color=sex)) +
  geom_col()


## US Suicides ----------

usSuicides <- suicideData %>%
  filter(country == "United States of America")

# Col Suicide by age.
ggplot(usSuicides, aes(x=age, y=suicides_no, fill=sex)) +
  geom_col()
  #theme(legend.position="none")

# Col Suicide by year, age.
ggplot(usSuicides, aes(x=year, y=suicides_no, fill = age)) +
  geom_col()
#theme(legend.position="none")


# Scatter of all variables; m and f
ggplot(usSuicides, aes(x=year, y=suicides_no, shape = age, color = sex)) +
  geom_point() +
  geom_path()




## Linear models and Calcualtions --------

m1 <- lm(suicides_no ~ sex, usSuicides)
summary(m1)

m2 <- lm(suicides_no ~ age, usSuicides)
summary(m2)

m3 <- lm(suicides_no ~ population, usSuicides)
summary(m3)


usSuicidesFemale <- usSuicides %>%
  filter(sex == "female")
sum(usSuicidesFemale$suicides_no)


usSuicidesMale <- usSuicides %>%
  filter(sex == "male")
sum(usSuicidesMale$suicides_no)



# 5-14 year olds
usSuicidesKids <- usSuicides %>%
  filter(age == "5-14 years")
child <- sum(usSuicidesKids$suicides_no)

# 15-24 year olds
usSuicidesTeens <- usSuicides %>%
  filter(age == "15-24 years")
teen <- sum(usSuicidesTeens$suicides_no)

# 25-34 years old
usSuicidesYoungAdult <- usSuicides %>%
  filter(age == "25-34 years")
youngAdult <- sum(usSuicidesYoungAdult$suicides_no)

# 35-54 years old
usSuicidesAdult <- usSuicides %>%
  filter(age == "35-54 years")
adult <- sum(usSuicidesAdult$suicides_no)

# 55-74 years old
usSuicidesElder <- usSuicides %>%
  filter(age == "55-74 years")
elder <- sum(usSuicidesElder$suicides_no)

# 75+ years old
usSuicidesOld <- usSuicides %>%
  filter(age == "75+ years")
old <- sum(usSuicidesOld$suicides_no)


# Final Plots ----------------
  

# Scatter/line of all variables; ages, sex.
ggplot(usSuicides, aes(x=year, y=suicides_no, color = age)) +
  geom_line() +
  theme_linedraw() +
  labs(title = "US Suicides 1979-2016 by Age and Sex",
       y = "Number of Suicides",
       x = "Year",
       color = "Age Group") +
  facet_grid(rows=vars(sex))


# Col Suicide by year; ages colored
ggplot(usSuicides, aes(x=year, y=suicides_no, fill = age)) +
  geom_col() +
  theme_linedraw()+
  facet_grid(rows=vars(sex)) +
  labs(title = "US Suicides 1979-2016",
       y = "Number of Suicides",
       x = "Year",
       fill = "Age Group")

# Col Suicide M vs F
ggplot(usSuicides, aes(x=age, y=suicides_no, fill=sex)) +
  geom_col() +
  theme_linedraw() +
  labs(title = "US Suicides 1979-2016",
       y = "Number of Suicides",
       x = "Age Group",
       fill = "Gender")


