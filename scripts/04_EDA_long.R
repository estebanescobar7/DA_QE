
library(tidyverse)
library(ggplot2)
library(table1)
library(patchwork)

# set seed: 
set.seed(-971190213)

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
         ed_group = factor(ed_group, levels = c("Highschool", 
                                                "Less Than High School", 
                                                "College"))) %>% 
  mutate(med_group = case_when(med == 0 ~ "No Known Medical Condition", 
                               med == 1 ~ "One Known Medical Condition", 
                               med == 2  ~ "Two Known Medical Condition", 
                               med >=3 ~ "Three or more Medical Condition"), 
         med_group = factor(med_group, levels = c("No Known Medical Condition", 
                                                  "One Known Medical Condition", 
                                                  "Two Known Medical Condition", 
                                                  "Three or more Medical Condition"))) %>% 
  mutate(age_group = case_when( age <= 69 ~ "64-69", 
                                age <= 74 ~ "70-74", 
                                age <= 79 ~ "75-79", 
                                age <= 84 ~ "80-84", 
                                age > 84 ~ "85+"  ), 
         age_group = factor(age_group)) %>% 
  mutate(male = factor(male,
                       levels = c(0,1), 
                       labels = c("Female", 
                                  "Male"))) %>% 
  mutate(black = factor(black, 
                        levels = c(0,1),
                        labels = c("Non-Hispanic White", 
                                   "Non-Hispanic Black"))) %>% 
  mutate(med = factor(med)) %>% 
  mutate(income = factor(income)) %>% 
  relocate(c(visit, year), .after = id) %>% 
  relocate(discrim_indicator, .after = discrim) %>% 
  relocate(med_group, .after = med) 

# How many visit 
n.obs <- unlist( lapply( split( physical_long$id, physical_long$id ), length ) ) 
vist_data <-  data.frame(  table( n.obs ) )

# bargraph of total number of people at each visit 
bargraph_plot <- ggplot(vist_data) +
  aes(x = n.obs, 
      y = Freq ) + 
  geom_bar(stat="identity", 
           position = position_dodge(), 
           color = "black") + 
  geom_text(aes(label = Freq), 
            position = position_dodge(width = 0.9), 
            vjust = 2, 
            size = 4, 
            color = "white") +
  labs(x = "Maximum Number of Visit ", 
       y = "Total number of People", 
       title = " "  ) +
  theme(text = element_text(family = "Times")) + 
  theme_minimal() 

#save plot in file 
ggsave(filename = "bargraph_plot.pdf",
       path = "/Users/Esteban/Desktop/Schools/UCI/Qualifiy_exams/Year-2/Data\ Analysis/qual_2023/Figures", 
       plot= bargraph_plot,
       device = "pdf")

# we can only consider people with three or more visit 
# since we are looking at people's trjectory 

# get the ID that do not have three or more visit 
drop.names <- as.numeric( names( table(physical_long$id)[ table(physical_long$id)<3 ]  ) )

# get the subset of the data that we are working with 
physical_long_reduce <- physical_long[ !is.element(physical_long$id,drop.names),]

#length( unique(physical_long_reduce$id) ) #3772 people who have 3 or more visit 

# make an indicator to see people with differnt values of discrim score at baseline 
physical_long_clean <- physical_long_reduce %>% 
  mutate(discrim_group = case_when( discrim == 0 ~ "0", 
                                    discrim <=3 ~ "1-3", 
                                    discrim <= 6 ~ "4-6", 
                                    discrim <= 9 ~ "7-9" ), 
         discrim_group = factor(discrim_group, levels = c( "0", "1-3", "4-6", "7-9") )) %>% 
  relocate(discrim_group, .after = discrim  )

# spaghetti plot 

# for people who have discrim is zero 
p1 <- physical_long_clean %>%
  filter(discrim_group == "0" ) %>% 
  ggplot() +
  aes(x = year, 
      y = physical, 
      color = black ) +
  geom_point(aes(x = year, 
                 y = physical, 
                 color = black, 
                 group = id), 
             alpha = 0.05) +
  geom_line(aes(x = year, 
                y = physical, 
                color = black, 
                group = id), 
            alpha = 0.05) + 
  geom_smooth(se = FALSE,
              method = "lm") +
  labs( x = "Years Since First Wave", 
        y = "Physical Function Score", 
        color = "Race/Ethnicity", 
        title = "People With Perceived Discrimination Score of Zero at Baseline") + 
  theme(text = element_text(family = "Times")) + 
  scale_x_continuous(breaks = c(0,3,6,9,12)) +
  scale_colour_viridis_d(option = "D", direction = -1) + 
  theme_minimal() +
  theme(axis.title.x = element_blank(), 
        legend.position="none")

# for people who have discrim score between 1 and 3 at baseline  
p2 <- physical_long_clean %>%
  filter(discrim_group == "1-3" ) %>% 
  ggplot() +
  aes(x = year, 
      y = physical, 
      color = black) +
  geom_point(aes(x = year, 
                 y = physical, 
                 color = black, 
                 group = id ), alpha = 0.05) +
  geom_line(aes(x = year, 
                y = physical, 
                color = black, 
                group = id), 
            alpha = 0.05) + 
  geom_smooth(se = FALSE, 
              method = "lm") +
  labs( x = "Years Since First Wave", 
        y = "Physical Function Score", 
        color = "Race/Ethnicity", 
        title = "People With Perceived Discrimination Score Between 1 and 3 at Basline") + 
  theme(text = element_text(family = "Times")) + 
  scale_x_continuous(breaks = c(0,3,6,9,12)) +
  scale_colour_viridis_d(option = "D", direction = -1) + 
  theme_minimal()  + 
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank())

# for people who have discrim score between 4 and 6 at baseline  
p3 <- physical_long_clean %>%
  filter(discrim_group == "4-6" ) %>% 
  ggplot() +
  aes(x = year, 
      y = physical, 
      color = black) +
  geom_point(aes(x = year, 
                 y = physical, 
                 color = black, 
                 group = id), 
             alpha = 0.2) +
  geom_line(aes(x = year, 
                y = physical, 
                color = black, 
                group = id), 
            alpha = 0.2) + 
  geom_smooth(se = FALSE, 
              method = "lm") +
  labs( x = "Years Since First Wave", 
        y = "Physical Function Score", 
        color = "Race/Ethnicity", 
        title = "People With Perceived Discrimination Score Between 4 and 6 at Basline") + 
  theme(text = element_text(family = "Times")) + 
  scale_x_continuous(breaks = c(0,3,6,9,12)) +
  scale_colour_viridis_d(option = "D", direction = -1) + 
  theme_minimal() + 
  theme(legend.position="none")

# for people who have discrim score between 7 and 9 at baseline  
p4 <- physical_long_clean %>%
  filter(discrim_group == "7-9" ) %>% 
  ggplot() +
  aes(x = year, 
      y = physical, 
      color = black) +
  geom_point(aes(x = year, 
                 y = physical, 
                 color = black, 
                 group = id), 
             alpha = 0.4) +
  geom_line(aes(x = year, 
                y = physical, 
                color = black, 
                group = id ), 
            alpha = 0.4) + 
  geom_smooth(se = FALSE, 
              method = "lm") +
  labs( x = "Years Since First Wave", 
        y = "Physical Function Score", 
        color = "Race/Ethnicity", 
        title = "People With Perceived Discrimination Score Between 7 and 9 at Basline") + 
  theme(text = element_text(family = "Times")) + 
  scale_x_continuous(breaks = c(0,3,6,9,12)) +
  scale_colour_viridis_d(option = "D", direction = -1) + 
  theme_minimal() +
  theme(axis.title.y = element_blank())
  
spaghetti_plot <- (p1 + p2)/(p3 + p4)

#save plot in file 
ggsave(filename = "spaghetti.pdf",
       path = "/Users/Esteban/Desktop/Schools/UCI/Qualifiy_exams/Year-2/Data\ Analysis/qual_2023/Figures", 
       plot= spaghetti_plot,
       device = "pdf", 
       width = 14, 
       height = 8)



