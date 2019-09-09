###### Section 2: Linear Models ######

#For baseball data
install.packages("Lahman")
library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()

#For Galton father-son data
library(tidyverse)
library(dslabs)
library(HistData)
data("GaltonFamilies")
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

#For Galton mother-daughter data
set.seed(1989, sample.kind="Rounding")

library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits
female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight) 


### Section 2.2: Least Squares Estimates


  #Q3: Filter the Teams data from the Lahman library to the years 1961-2001.
      #Run a linear model predicting the number of runs per game based on both
      #the number of base on balls and the number of home runs

data("Teams")
fit<-Teams %>% 
  filter(yearID %in% 1961:2001) %>%
  mutate(R_game = R/G, BB_game = BB/G, HR_game = HR/G) %>%
  lm(R_game ~ BB_game + HR_game, data = .)
summary(fit)

  #Q7: Using the Galton mother-daughter data, fit alinear regression model 
      #predicting the mothers' heights using daughters' heights.
      #What are the slope and intercept of the model? (slope = 0.31; intercept = 44.18)

female_heights %>%
  lm(mother ~ daughter, data = .)

  #Q8: Predict mother's heights using the model.
      #What is the predicted hieght of the first mother in the dataset? (65.6 inches)
      #What is the actual height of the first mother in the dataset?  (67 inches)

female_heights %>%
  mutate(Y_hat = predict(lm(mother ~ daughter, data = .)))

#Use the following 2002 table for the Lahman data to answer the following questions

library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

  #Q9: Compute a similar table but with rates computed over 1999-2001. Keep only rows from 
      #1999-2001 where players have 100 or more plate appearances, then calculate   
      #the average single rate (mean_singles) and average BB rate (mean_bb) per player over those three seasons.

bat_99_01 <- Batting %>%
  filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb, yearID) %>%
  group_by(playerID) %>%
  summarize(mean_singles = mean(singles), mean_bb = mean(bb))


sum(bat_99_01$mean_singles > 0.2) 
sum(bat_99_01$mean_bb > 0.2)

  #Q10: Use inner_join to combine the bat_02 table with the table of 1999-2001 rate averages you created in the previous question
      #What is the correlation between 2002 singles rates and 199-2001 average single rates?
      #What is the correlation between 2002 BB rates and 1999-2001 average BB rates?

inner_join(bat_02, bat_99_01) %>%
summarize (r_singles = cor(singles, mean_singles), r_bb = cor(bb, mean_bb))
  
  #Q11: Make scatterplots of mean_singles versus singles and mean_bb versus bb.
      #Are either of these distributions bivariate normal?  (both are bivariate normal)

inner_join(bat_02, bat_99_01) %>%
  ggplot(aes(singles, mean_singles)) +
  geom_point()

inner_join(bat_02, bat_99_01) %>%
  ggplot(aes(bb, mean_bb)) +
  geom_point()

  #Q12: Fit a linear model to predict 2002 singles given 1999-2001 mean_singles and another to predict 2002 bb given 1999-2001 mean_bb
      #What are the coefficients of mean_singles and mean_bb, the slopes of the fit?

inner_join(bat_02, bat_99_01) %>%
  lm(singles ~ mean_singles, data = .)
     
inner_join(bat_02, bat_99_01) %>%
  lm(bb ~ mean_bb, data = .)