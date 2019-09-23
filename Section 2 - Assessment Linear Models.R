###### Assessment: Linear Models (verified learners only) ######


library(tidyverse)
library(broom)
library(Lahman)
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G)

  #Q1a: Use runs (R) per game to predict average attendance. For every 1 run scored, attendance increases by how much?   (4117)
      #Use home runs (HR) per game to predict average attendance. For every 1 home run hit per game, attendance inscreases by how much? (8113)

Teams_small %>%
  mutate(R_game = R/G) %>%
  lm(avg_attendance ~ R_game, data = .)

Teams_small %>%
  mutate(HR_game = HR/G) %>%
  lm(avg_attendance ~ HR_game, data = .)

  #Q1b: Use number of wins to predict attendance; do not normalize for number of games.For every game won in a season, how much does average attendance increase? (121.1)
      #Suppose a team won zero games in a season. Predict the average attendance. (1129.2)

Teams_small %>%
  lm(avg_attendance ~ W, data =.)

  #Q1c: Use year to predict average attendance. How much does average attendance increase each year?  (244.5)

Teams_small %>%
  lm(avg_attendance ~ yearID, data = .)

  #Q2: What is the correlation coefficient for wins and runs per game?
      #What is the correlation coefficient for wins and home runs per game?

    #My answer
Teams_small %>%
  mutate(R_game = R/G, HR_game = HR/G) %>%
  summarize(r1 = cor(W, R_game), r2 = cor(W, HR_game))

    #Course answer
cor(Teams_small$W, Teams_small$R/Teams_small$G)
cor(Teams_small$W, Teams_small$HR/Teams_small$G)

#Q3: Stratify Teams_small by wins: divide number of wins by 10 and then round to the nearest integer. Keep only strata 5 through 10, which have 20 or more data points.
  #Q3a: How many observations are in the 8 win strata?  (338)

  #Course answer (I could not answer this one)

dat <- Teams_small %>%
  mutate(wins_strata = round(W/10)) %>%
  filter(wins_strata >= 5 & wins_strata <= 10)
sum(dat$wins_strata == 8)

  #Q3b: Calculate the slope of the regression line predicting average attendance given runs per game for each of the win strata.
      #Which win stratum has the largest regression line slope? (5)

  #My answer
get_slope <- function(data){
  fit <- lm (avg_attendance ~ R_game, data = data)
  data.frame(slope = fit$coefficients[2])
}

dat %>% 
  mutate(R_game = R/G) %>%
  group_by(wins_strata) %>%
  do(get_slope(.))

  #Course answer

dat %>%  
  group_by(wins_strata) %>%
  summarize(slope = cor(R/G, avg_attendance)*sd(avg_attendance)/sd(R/G))

      #Calculate the slope of the regression line predicting average attendance given HR per game for each of the win strata.
      #Which win stratum has the largest regression line slope? (5)

  #My answer

get_slope <- function(data){
  fit <- lm (avg_attendance ~ HR_game, data = data)
  data.frame(slope = fit$coefficients[2])
}

dat %>% 
  mutate(HR_game = HR/G) %>%
  group_by(wins_strata) %>%
  do(get_slope(.))

  #Course answer

dat %>%  
  group_by(wins_strata) %>%
  summarize(slope = cor(HR/G, avg_attendance)*sd(avg_attendance)/sd(HR/G))

  #Q3c: Which of the following are true about the effect of win strata on average attendance?
      #Across all win strata, runs per game are positively correlated with average attendance.  (correctly answered true)
      #Runs per game have the strongest effect on attendance when a team wins many games.   (orrectly answered false)
      #After controlling for number of wins, home runs per game are not correlated with attendance. (correctly answered false)
      #Home runs per game have the strongest effect on attendance when a team does not win many games.  (correctly answered true)
      #Among teams with similar numbers of wins, teams with more home runs per game have larger average attendance. (incorrectly answered false)

  #Q4: Using the original Teams_small wins column, fit a multivariate regression determining the effects of runs per game, home runs per game, wins, and year on average attendance. 
      #What is the estimate of the effect of runs per game on average attendance? (321.8)
      #What is the estimate of the effect of home runs per game on average attendance?  (1798.4)
      #What is the estimate of the effect of number of wins in a season on average attendance?  (116.7)

  #My answer (run model and identify values without pulling each estimate one at a time)
Teams_small %>%
  mutate(R_game = R/G, HR_game = HR/G) %>%
  lm(avg_attendance ~ R_game + HR_game + W + yearID, data = .)

  #Course answer (run model and then for each question filter data by term and pull estimate. More code compared to just identifying the proper values)

fit <- Teams_small %>% 
  mutate(R_per_game = R/G,
         HR_per_game = HR/G) %>%
  lm(avg_attendance ~ R_per_game + HR_per_game + W + yearID, data = .)

tidy(fit) %>%
  filter(term == "R_per_game") %>%
  pull(estimate)
  
  #Q5: Use the multivariate regression model from Question 4. Suppose a team averaged 5 runs per game, 1.2 home runs per game, and won 80 games in a season.
      #What would this team's average attendance be in 2002?
      #What would this team's average attendance be in 1960?

  #My answer (marked incorrect as results were different from using predict function, possibly because of rounding error)
avg_attendance_hat <- function (R_game, HR_game, W, yearID){
  -456674.4 + 321.8*R_game + 1798.4*HR_game + 116.7*W + 229.6*yearID
}

avg_attendance_hat(5, 1.2, 80, 2002)
avg_attendance_hat(5, 1.2, 80, 1960)

  #Course answer
fit <- Teams_small %>%
  mutate(R_game = R/G, HR_game = HR/G) %>%
  lm(avg_attendance ~ R_game + HR_game + W + yearID, data = .)
predict(fit, data.frame(R_per_game = 5, HR_per_game = 1.2, W = 80, yearID = 2002))
predict(fit, data.frame(R_per_game = 5, HR_per_game = 1.2, W = 80, yearID = 1960))

  #Yet another answer

fit$coefficients[1] + 5*fit$coefficients[2] + 1.2*fit$coefficients[3] + 80*fit$coefficients[4] + 1960*fit$coefficients[5]
fit$coefficients[1] + 5*fit$coefficients[2] + 1.2*fit$coefficients[3] + 80*fit$coefficients[4] + 2002*fit$coefficients[5]

  #Q6:Use your model from Question 4 to predict average attendance for teams in 2002 in the original Teams data frame.
      #What is the correlation between the predicted attendance and actual average attendance?

  #My answer
Teams %>%
  filter(yearID == 2002) %>%
  mutate(R_game = R/G, HR_game = HR/G, avg_attendance = attendance/G) %>%
  mutate(Y_hat = predict(fit, newdata = .)) %>%
  summarise(r = cor(Y_hat, attendance))

  #Course answer
newdata <- Teams %>%
  filter(yearID == 2002) %>%
  mutate(avg_attendance = attendance/G,
         R_game = R/G,
         HR_game = HR/G)
preds <- predict(fit, newdata)
cor(preds, newdata$avg_attendance)
