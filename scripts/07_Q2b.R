library(tidyverse)
library(ggplot2)
library(table1)
library(patchwork)
library(splines)
library(geepack)

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

# we can only consider people with three or more visit 
# since we are looking at people's trjectory 

# get the ID that do not have three or more visit 
drop.names <- as.numeric( names( table(physical_long$id)[ table(physical_long$id)<3 ]  ) )

# get the subset of the data that we are working with 
physical_long_reduce <- physical_long[ !is.element(physical_long$id,drop.names),]

# obtain the estimated profiles for physical function
# over time for different perceived discrimation levles with 
# confidence intervals. 

## GEE MODEL
fit_gee_naive <- geeglm(formula = physical ~ visit + discrim + black + male + med_group + I(age - 74) + visit*discrim*black, 
                        data = physical_long_reduce,
                        family = gaussian,
                        id = id,
                        corstr = "exchangeable"); summary(fit_gee_naive)

linear_contrast_naive1 <- linContr.glm( model = fit_gee_naive,
                                        contr.names = c( "(Intercept)", "discrim", "visit", "blackNon-Hispanic Black"  ), 
                                        contr.coef = c(1,1,1,1), 
                                        transform = FALSE)

rslt.all <- data.frame()
#unique ID
unique_id <- unique(physical_long_reduce$id)
num_replicates <- 40
for(disc_score in 0:9 ){
  print(paste("disc_score = ", disc_score))
  for(time in 0:4 ){
    print(paste("time = ", time))
    # empty matrix to store the replicates coefficient 
    rslt <- matrix(NA, nrow = num_replicates, ncol = 1)
    for(i in 1:num_replicates ){
      ID <- sample(unique_id, replace = TRUE  )
      subset_data <- physical_long_reduce[ !is.element(physical_long_reduce$id,ID), ]
      # fit GEE with subset data
      fit <- geeglm(formula = physical ~ visit + discrim + black + male + med_group + I(age - 74) + visit*discrim*black, 
                    data = subset_data,
                    family = gaussian,
                    id = id,
                    corstr = "exchangeable")
      linear_contrast <- linContr.glm( model = fit,
                                       contr.names = c( "(Intercept)", "discrim", "visit", "blackNon-Hispanic Black"  ), 
                                       contr.coef = c(1,disc_score,time,disc_score*time), 
                                       transform = FALSE)
      rslt[i,] <- linear_contrast$Est
    }
    values <- data.frame(time = time, 
                         disc = disc_score,  
                         beta =  apply(rslt,2,mean), 
                         se =  apply(rslt,2,sd)) %>%  
      mutate(ci95.lo =  beta - qnorm(.975) * se, 
             ci95.hi =  beta + qnorm(.975) * se)
    rslt.all <- rbind(rslt.all, values)
  }
}

rslt.all

# save the results in a csv cuz this took like 9 hours
#write.csv(rslt.all,"/Users/Esteban/Desktop/Schools/UCI/Qualifiy_exams/Year-2/Data\ Analysis/qual_2023/data/rslt3.csv")


rslt.all <- read.csv("/Users/Esteban/Desktop/Schools/UCI/Qualifiy_exams/Year-2/Data\ Analysis/qual_2023/data/rslt3.csv"); rslt.all

p1 <- rslt.all %>% 
  filter( disc %in% c(0,1,2,3,9) ) %>% 
  mutate( disc = factor(disc, levels = c(0,1,2,3,9) ) ) %>% 
  ggplot()   + 
  geom_line( aes(x = time, y = beta, color = disc ) ) + 
  geom_line( aes(x = time, y = ci95.lo, group = disc, color = disc  ), linetype = "dashed", alpha = 0.5 ) + 
  geom_line( aes(x = time, y = ci95.hi, group = disc, color = disc ), linetype = "dashed", alpha = 0.5 ) + 
  labs(x = "Time", 
       y = "Physical Function Score", 
       color = "Discrimination Score") + 
  coord_cartesian(ylim=c(0,15)) + 
  theme(text = element_text(family = "Times")) + 
  scale_x_continuous(breaks = 0:4) +
  scale_colour_viridis_d(option = "D") + 
  theme_minimal() 

#save plot in file 
ggsave(filename = "profiles.pdf",
       path = "/Users/Esteban/Desktop/Schools/UCI/Qualifiy_exams/Year-2/Data\ Analysis/qual_2023/Figures", 
       plot= p1,
       device = "pdf", 
       width = 11, 
       height = 6)


