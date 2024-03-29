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

### Section 2.3:Tibbles, do, and broom

install.packages("broom")
library(broom)

dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G, 1), 
         BB = BB/G,
         R = R/G) %>%
  select(HR, BB, R) %>%
  filter(HR >= 0.4 & HR<=1.2)

dat %>%
  group_by(HR) %>%
  do(fit = lm(R ~ BB, data = .))

  #Q5:You want to take the tibble dat and run the linear model R~BB for each strata of HR. Then you want to add 3 new columns: the
      #coefficient, standard errod, and p-values. Having the get_slope function below, what additional code could you write to 
      #accomplish your goal?

get_slope <- function(data) {
  fit <- lm(R ~ BB, data = data)
  sum.fit <- summary(fit)
  data.frame(slope = sum.fit$coefficients[2, "Estimate"], 
             se = sum.fit$coefficients[2, "Std. Error"],
             pvalue = sum.fit$coefficients[2, "Pr(>|t|)"])
}

dat %>%
  group_by(HR) %>%
  do(get_slope(.))


dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = HR/G,
         R = R/G) %>%
  select(lgID, HR, BB, R) 

#Use the following dataset for the next set of questions

library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1, sample.kind = "Rounding")
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

  #Q8:Group by pair and summarize the number of observations in each group
      #How many father-daughter pairs are in the dataset?
      #How many mother-son pairs are in the dataset?

  
galtonfd <- galton %>%              #My answer; could not find the solution using the asked group_by function. Instead, I created 2 data frames, 
  filter(pair=="father_daughter")   #filtering the father_daughters pairs in one and the mother_son pairs in the other.
  nrow(galtonfd)                    #Correct answer obtained albeit by other means than those asked in the question
galtonms <- galton %>%
  filter(pair=="mother_son")
nrow(galtonms)

galton %>%                          #Course answer
  group_by(pair) %>%
  summarize(n = n())

  #Q9: Calculate the correlation coefficients for fathers and daughters, fathers and sons, mothers and daughters and mothers and sons.
      #Which pair has the strongest correlation in heights?   (father_son)
      #Which pair has the weakest correlation in heights?     (mother_son)
      
galton %>%
  group_by(pair) %>%
  summarize(r = cor(parentHeight, childHeight))

  #Q10: Use lm and the broom package to fit regression lines for each parent-child pair type. 
      #Compute the least squares estimates, standard errors, confidence intervals and p-values for the parentHeight coefficient for each pair.
  
      #What is the estimate of the father-daughter coefficient? (0.345 inches)
      #For every 1-inch increase in mother's height, how many inches does the typical son's height increase?  (0.381 inches)
      #Which sets of parent-child heights are significantly correlated at a p-value cut off of 0.05?
      #Which of the following statements are true?
        #All of the confidence intervals overlap each other.
        #At least one confidence interval covers zero.
        #The confidence intervals involving mothers' heights are larger than the confidence intervals involving fathers' heights.
        #The confidence intervals involving daughters' heights are larger than the confidence intervals involving sons' heights.
        #The data are consistent with inheritance of height being independent of the child's gender.
        #The data are consistent with inheritance of height being independent of the parent's gender.

galton %>%
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  filter(term == "parentHeight") %>%
  select (pair, estimate, std.error, p.value, conf.low, conf.high)

galton %>%
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  filter(term == "parentHeight") %>%
  select (pair, estimate, std.error, p.value, conf.low, conf.high) %>%
  ggplot(aes(pair, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point()

### Section 2.4: Regression and Baseball

  #Q3: Imagine you have two teams. Team A is comprised of batters who, on average, get two bases on balls, four singles, one double, and one home run. 
      #Team B is comprised of batters who, on average, get one base on balls, six singles, two doubles, and one triple. Which team scores more runs, as predicted by our model?

      #model was Y_hat = 0.37X1 + 0.52X2 + 0.77X3 + 1.24X4 + 1.44*X5; therefore team B is predicted to score more runs

Runs <- function(BB,S,D,Tri,HR){
  0.37*BB + 0.52*S + 0.77*D + 1.24*Tri + 1.44*HR
}
Team_A <- Runs(2, 4, 1, 0, 1 )
Team_B <- Runs(1, 6, 2, 1, 0)
Team_A >= Team_B


  #Q9: Use the Teams data frame from the Lahman package. Fit a multivariate linear regression model to obtain the effects of BB and HR on Runs (R) in 1971. 
      #Use the tidy function in the broom package to obtain the results in a data frame.

  #Q9A: What is the estimate for the effect of BB on runs? For HR on runs?

library(broom)
 
    #My answer       
Teams %>% 
  filter(yearID == 1971) %>%
  select(BB, HR, R) %>%
  do(tidy(lm(R ~ BB + HR, data = .) , conf.int = TRUE))

    #Course answer
Teams %>%
  filter(yearID == 1971) %>%
  lm(R ~ BB + HR, data = .) %>%
  tidy() %>%
  filter(term == "BB") %>%
  pull(estimate)

Teams %>%
  filter(yearID == 1971) %>%
  lm(R ~ BB + HR, data = .) %>%
  tidy() %>%
  filter(term == "HR") %>%
  pull(estimate)

  #Q9B: Interpret the p-values using a cutoff of 0.05. Which of the options given is the correct interpretation?
      #Both BB and HR have a nonzero effect on runs.
      #HR has a significant effect on runs, but the evidence is not strong enough to suggest BB also does.  (correct)
      #BB has a significant effect on runs, but the evidence is not strong enough to suggest HR also does.
      #Neither BB nor HR have a statistically significant effect on runs.

  #Q10: Repeat the above exercise to find the effects of BB and HR on runs (R) for every year from 1961 to 2018 using do and the broom package.
    #Make a scatterplot of the estimate for the effect of BB on runs over time and add a trend line with confidence intervals.

Teams %>% 
  filter(yearID %in% 1961: 2018) %>%
  group_by(yearID) %>%
  select(yearID, BB, HR, R) %>%
  do(tidy(lm(R ~ BB + HR, data = .), conf.int = TRUE)) %>%
  ungroup() %>%
  filter(term == "BB") %>%
  ggplot(aes(yearID, estimate))+
  geom_point()+
  geom_smooth(method = "lm")

  #Q11: Fit a linear model on the results from Question 10 to determine the effect of year on the impact of BB (effect of year on the estimate).
      #For each additional year, by what value does the impact of BB on runs change? (0.004)
      #What is the p-value for this effect?   (0.008)

Teams %>% 
  filter(yearID %in% 1961: 2018) %>%
  group_by(yearID) %>%
  select(yearID, BB, HR, R) %>%
  do(tidy(lm(R ~ BB + HR, data = .), conf.int = TRUE)) %>%
  ungroup() %>%
  filter(term == "BB") %>%
  do(tidy(lm(estimate ~ yearID, data = .)))