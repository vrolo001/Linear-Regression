###### Assessment: Confounding (Verified learners only) ######

library(ggplot2)
library(dslabs)
data("research_funding_rates")
research_funding_rates

  #Q1: Construct a two-by-two table of gender (men/women) by award status (awarded/not) using the total numbers across all disciplines.
      #What is the number of men not awarded? (1345)
      #What is the number of women not awarded? (1011)

  #My answer  (although not precisely a table, but my code still answers the number of men and women not awarded)  

research_funding_rates %>%
  mutate(women_not_awarded = applications_women - awards_women, 
         men_not_awarded = applications_men - awards_men ) %>%
  summarise(sum(men_not_awarded), sum(women_not_awarded))

  #Course answer

two_by_two <- research_funding_rates %>% 
  select(-discipline) %>% 
  summarize_all(funs(sum)) %>%
  summarize(yes_men = awards_men, 
            no_men = applications_men - awards_men, 
            yes_women = awards_women, 
            no_women = applications_women - awards_women) %>%
  gather %>%
  separate(key, c("awarded", "gender")) %>%
  spread(gender, value)
two_by_two

  #Q2: Use the two-by-two table from Question 1 to compute the percentages of men awarded versus women awarded.
      #What is the percentage of men awarded?
      #What is the percentage of women awarded?

  #My answer

research_funding_rates %>%
  summarise(sum(awards_men)*100/sum(applications_men),
            sum(awards_women)*100/sum(applications_women))

  #Course answer

two_by_two %>% 
  mutate(men = round(men/sum(men)*100, 1), women = round(women/sum(women)*100, 1)) %>%
  filter(awarded == "yes") %>%
  pull(men)

two_by_two %>% 
  mutate(men = round(men/sum(men)*100, 1), women = round(women/sum(women)*100, 1)) %>%
  filter(awarded == "yes") %>%
  pull(women)

  #Q3: Run a chi-squared test on the two-by-two table to determine whether the difference in the two success rates is significant. 
      #(You can use tidy to turn the output of chisq.test into a data frame as well.) What is the p-value of the difference in funding rate?
 
  #My answer

two_by_two %>%
  select(-awarded) %>%
  chisq.test()

  #Course answer

two_by_two %>% select(-awarded) %>% chisq.test() %>% tidy() %>% pull(p.value)

  #Q4: Use the dataset below with number of applications, awards, and success rate for each gender:

dat <- research_funding_rates %>% 
  mutate(discipline = reorder(discipline, success_rates_total)) %>%
  rename(success_total = success_rates_total,
         success_men = success_rates_men,
         success_women = success_rates_women) %>%
  gather(key, value, -discipline) %>%
  separate(key, c("type", "gender")) %>%
  spread(type, value) %>%
  filter(gender != "total")

  #To check if this is a case of Simpson's paradox, plot the success rates versus disciplines, which have been ordered by 
  #overall success, with colors to denote the genders and size to denote the number of applications.

  #In which fields do men have a higher success rate than women?  (Chemical sciences, earth/life sciences, medical sciences,
                                                                  #physics, social sciences)
  #Which two fields have the most applications from women?  (Medical sciences and social sciences)
  #Which two fields have the lowest overall funding rates?  (Medical sciences and social sciences)

dat %>% 
  ggplot(aes(discipline, success, size = applications, color = gender)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
 