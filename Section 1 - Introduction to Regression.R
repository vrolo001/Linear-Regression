###### Section 1: Introduction to Regression ######

###Section 1.1: Baseball as a motivatin example
install.packages("Lahman")
library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()

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



