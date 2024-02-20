library(tidyverse)
library(ggplot2)
library(table1)
library(patchwork)
library(eatRep)

# helper function for tables and data analysis
source( "/Users/Esteban/Desktop/Schools/my_functions/helper_function.R" )

# Get data 
physical <- read.csv("/Users/Esteban/Desktop/Schools/UCI/Qualifiy_exams/Year-2/Data\ Analysis/qual_2023/data/physical.csv")

# clean dataset 
physical_long <- physical %>% 
  group_by(id) %>% 
  mutate(visit = 0:(length(id)-1) ) %>% 
  mutate(year = seq(from = 0, 
                    to = (length(id)-1)*3, 
                    by = 3)) %>% 
  ungroup() %>% 
  mutate(discrim_indicator = ifelse(discrim > 0, 1, 0 ), 
         discrim_indicator = factor(discrim_indicator, 
                                    levels = c(0,1), 
                                    labels = c("No", "Yes"))) %>% 
  mutate(ed_group = case_when( ed < 12 ~ "Less Than High School", 
                               ed >= 12 & ed < 16 ~ "Highschool", 
                               ed >= 16 ~ "College"), 
         ed_group = factor(ed_group, levels = c("Highschool", "Less Than High School", "College"))) %>% 
  mutate(med_group = case_when(med == 0 ~ "No Known Medical Condition", 
                               med == 1 ~ "One Known Medical Condition", 
                               med == 2  ~ "Two Known Medical Condtion", 
                               med >=3 ~ "Three or more Medical Condition"), 
         med_group = factor(med_group, levels = c("No Known Medical Condition", 
                                                  "One Known Medical Condition", 
                                                  "Two Known Medical Condtion", 
                                                  "Three or more Medical Condition"))) %>% 
  mutate(age_group = case_when( age <= 69 ~ "64-69", 
                                age <= 74 ~ "70-74", 
                                age <= 79 ~ "75-79", 
                                age <= 84 ~ "80-84", 
                                age > 84 ~ "85+"  ), 
         age_group = factor(age_group)) %>% 
  mutate(male = factor(male,
                       levels = c(0,1), 
                       labels = c("Female", "Male"))) %>% 
  mutate(black = factor(black, 
                        levels = c(0,1),
                        labels = c("Non-Hispanic White", "Non-Hispanic Black"))) %>% 
  mutate(med = factor(med)) %>% 
  mutate(income = factor(income)) %>% 
  relocate(c(visit, year), .after = id) %>% 
  relocate(discrim_indicator, .after = discrim) %>% 
  relocate(med_group, .after = med) 

# Number of patietns 
#length(unique(physical_long$id)) #7836

# How many visit 
n.obs <- unlist( lapply( split( physical_long$id, physical_long$id ), length ) ) 
table( n.obs )

# baseline
physical_baseline <- physical_long %>% 
  filter(visit == 0)

# Table1 
table1( ~ physical + age + age_group + black + male + stress + BMI + med + med_group + ed_group | discrim_indicator, data = physical_baseline  )

# lets look at the distribution of our response 

## Summary of physical function
summary(physical_baseline$physical)
mean(physical_baseline$physical) # 10.71031
sd(physical_baseline$physical) # 3.355712

# histogram of physical function 
physical_baseline %>% 
  ggplot() + 
  aes(x = physical, 
      y = ..density..) + 
  geom_histogram(color = "black", 
                 fill = "white", 
                 binwidth = 1) + 
  geom_density(alpha = .10, fill = "red") + 
  labs(x = "Physical Function Score", 
       y = "Density", 
       title = "Histogram") + 
  theme(text = element_text(family = "Times")) + 
  theme_minimal()

physical_baseline %>%
  mutate(norm_fx = dnorm(physical, 10.71031, 3.355712)) %>%
  ggplot() +
  aes(x = physical) +
  geom_density() +
  geom_line(aes(y = norm_fx), color = "red") + 
  theme(text = element_text(family = "Times")) + 
  theme_minimal()

# look at age as a confounder 
summary(physical_baseline$age)

physical_baseline %>% 
  ggplot() + 
  aes(x = age, 
      y = physical  ) + 
  geom_jitter(alpha = 0.5) +
  geom_smooth(method = "lm", 
              se = FALSE, 
              col = "red") +
  labs(x = "Age", 
       y = "Physical Function Score") + 
  theme(text = element_text(family = "Times")) + 
  theme_minimal()

physical_baseline %>% 
  ggplot() + 
  aes(x = age, 
      y = discrim  ) + 
  geom_jitter(alpha = 0.5) +
  geom_smooth(method = "lm", 
              se = FALSE, 
              col = "red") +
  labs(x = "Age", 
       y = "Perceived Discrimination Score") + 
  theme(text = element_text(family = "Times")) + 
  theme_minimal()

# look at race as a confounder
physical_baseline %>% 
  ggplot() + 
  aes(x = black, 
      y = physical) + 
  geom_boxplot() + 
  labs(x = "Race", 
       y = "Physical Function Score") + 
  theme(text = element_text(family = "Times")) + 
  theme_minimal()

physical_baseline %>% 
  ggplot() + 
  aes(x = black, 
      y = discrim) + 
  geom_boxplot() + 
  labs(x = "Race", 
       y = "Perceived Discrimination Score") + 
  theme(text = element_text(family = "Times")) + 
  theme_minimal()

# sex 
physical_baseline %>% 
  ggplot() + 
  aes(x = male, 
      y = physical) + 
  geom_boxplot() + 
  labs(x = "Sex", 
       y = "Physical Function Score") + 
  theme(text = element_text(family = "Times")) + 
  theme_minimal()

physical_baseline %>% 
  ggplot() + 
  aes(x = male, 
      y = discrim) + 
  geom_boxplot() + 
  labs(x = "Sex", 
       y = "Perceived Discrimination Score") + 
  theme(text = element_text(family = "Times")) +
  theme_minimal()









# scientific question of intrest: 

# see if there is an assocaiton between discrim and physical 

p1 <- physical_baseline %>% 
  ggplot() + 
  aes(x = discrim, 
      y = physical) + 
  geom_jitter(alpha = 0.5, size = 0.8) + 
  geom_smooth(method = "lm", se = FALSE, col = "red") + 
  labs(x = "Perceived Discrimination Score", 
       y = "Physical Function Score") + 
  theme(text = element_text(family = "Times")) + 
  theme_minimal()


# see if the assocation between discrim and physical differ depening on race 
p2 <- physical_baseline %>% 
  ggplot() + 
  aes(x = discrim, 
      y = physical, 
      color = black, 
      group = black) + 
  geom_jitter(alpha = 0.5, size = 0.8) + 
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) + 
  labs(x = "Perceived Discrimination Score", 
       y = "Physical Function Score", 
       color = "Race/Ethnicity") + 
  theme(text = element_text(family = "Times")) + 
  theme_minimal() +
  scale_colour_viridis_d(option = "D", direction = -1) + 
  scale_x_continuous(breaks = 0:9)

 p_total <- p1 + p2

#save plot in file 
ggsave(filename = "pf_pd.pdf",
       path = "/Users/Esteban/Desktop/Schools/UCI/Qualifiy_exams/Year-2/Data\ Analysis/qual_2023/Figures", 
       plot= p_total,
       device = "pdf", 
       width = 15, 
       height = 7)





