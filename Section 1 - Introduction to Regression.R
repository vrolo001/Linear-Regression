###### Section 1: Introduction to Regression ######

###Section 1.1: Baseball as a motivatin example
install.packages("Lahman")
library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()

    #Q1-Q5 were theoretical
    #Q6: Load the Lahman library. Filter the Teams data frame to include years from 1961 to 2001. Make a scatterplot of runs per game versus at bats (AB) per game.
      #Which of the following is true?
      #There is no clear relationship between runs and at bats per game.
      #As the number of at bats per game increases, the number of runs per game tends to increase. (correct answer)
      #As the number of at bats per game increases, the number of runs per game tends to decrease.

      
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(R_per_game = R/G, AB_per_game = AB/G) %>%
  ggplot(aes(R_per_game, AB_per_game)) + 
  geom_point(alpha = 0.5) 

    #Q7: Use the filtered Teams data frame from Question 6. Make a scatterplot of win rate (number of wins per game) versus number of fielding errors (E) per game.
        #Which of the following is true?
        #There is no clear relationship between win rate and errors per game.
        #As the number of errors per game increases, the win rate tends to increase.
        #As the number of errors per game increases, the win rate tends to decrease. (correct answer)

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(win_rate = W/G, errors_per_game = E/G) %>%
  ggplot(aes(win_rate, errors_per_game)) + 
  geom_point(alpha = 0.5)

    #Q8:Use the filtered Teams data frame from Question 6. Make a scatterplot of triples (X3B) per game versus doubles (X2B) per game.
        #Which of the following is true?
        #There is no clear relationship between doubles per game and triples per game.  (correct answer)
        #As the number of doubles per game increases, the number of triples per game tends to increase.
        #As the number of doubles per game increases, the number of triples per game tends to decrease.

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(triples_game = X3B/G, doubles_game = X2B/G) %>%
  ggplot(aes(triples_game, doubles_game)) + 
  geom_point(alpha = 0.5)

###Section 1.2: Correlation

    #Q7: What is the correlation coefficient between number of runs per game and number of at bats per game?

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(R_per_game = R/G, AB_per_game = AB/G) %>%
  summarize(r = cor(R_per_game, AB_per_game))
 
    #Q8:What is the correlation coefficient between win rate (number of wins per game) and number of errors per game?
 
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(win_rate = W/G, errors_per_game = E/G) %>%
  summarize(r = cor(win_rate, errors_per_game))

    #Q9: What is the correlation coefficient between doubles (X2B) per game and triples (X3B) per game?

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(triples_game = X3B/G, doubles_game = X2B/G) %>%
  summarize(r = cor(triples_game, doubles_game))  

### Section 1.3: Stratification and Variance Explained

library(tidyverse)
library(dslabs)
library(dplyr)
library(ggplot2)

set.seed(1989, sample.kind="Rounding")
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

  #Q8: Calculate the mean and standard deviation of mothers' heights, the mean and standard deviation of daughters' heights, and the correlaton coefficient between mother and

head(female_heights)

female_heights %>%
  summarize(r = cor(mother, daughter),
            Mmom = mean(mother), SDmom = sd(mother),
            Mdaughter = mean(daughter), SDdaughter = sd (daughter))

  #Q9: Calculate the slope and intercept of the regression line predicting daughters' heights given mothers' heights. Given an increase in mother's height by 1 inch, how many inches is the daughter's height expected to change?

Mmom <- mean(female_heights$mother)
SDmom <- sd(female_heights$mother)
Mdaughter <- mean(female_heights$daughter)
SDdaughter <- sd(female_heights$daughter)

r <- cor(female_heights$mother, female_heights$daughter)
m <- r * SDdaughter/SDmom
b <- Mdaughter - m * Mmom

r * (SDdaughter/SDmom)

  #Q10: What percent of the variability in daughter heights is explained by the mother's height?
r^2*100

  #Q11: A mother has a height of 60 inches. What is the conditional expected value of her daughter's height given the mother's height?
cond_Y <-  b + m*60


         







