library(tidyverse)
library(ggplot2)
library(table1)
library(patchwork)
library(splines)
library(geepack)

# set seed: 
set.seed(-1696912617)

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
 
#length( unique(physical_long_reduce$id) ) # 3772

## GEE MODEL
fit_gee_naive <- geeglm(formula = physical ~ visit + discrim + black + male + med_group + I(age - 74) + visit*discrim*black, 
                        data = physical_long_reduce,
                        family = gaussian,
                        id = id,
                        corstr = "exchangeable"); summary(fit_gee_naive)

linear_contrast_naive1 <- linContr.glm( model = fit_gee_naive,
                                        contr.names = c( "discrim:blackNon-Hispanic Black", "visit:discrim:blackNon-Hispanic Black" ), 
                                        contr.coef = c(1,1), 
                                        transform = FALSE); linear_contrast_naive1

linear_contrast_naive2 <- linContr.glm( model = fit_gee_naive,
                                        contr.names = c( "visit", "visit:discrim" ), 
                                        contr.coef = c(1,1), 
                                        transform = FALSE); linear_contrast_naive2

linear_contrast_naive3 <- linContr.glm( model = fit_gee_naive,
                                        contr.names = c( "visit", "visit:blackNon-Hispanic Black", "visit:discrim", "visit:discrim:blackNon-Hispanic Black" ), 
                                        contr.coef = c(1,1,1,1), 
                                        transform = FALSE); linear_contrast_naive3

linear_contrast_naive4 <- linContr.glm( model = fit_gee_naive,
                                        contr.names = c( "visit", "visit:discrim" ), 
                                        contr.coef = c(1,9), 
                                        transform = FALSE); linear_contrast_naive4

linear_contrast_naive5 <- linContr.glm( model = fit_gee_naive,
                                        contr.names = c( "visit", "visit:blackNon-Hispanic Black", "visit:discrim", "visit:discrim:blackNon-Hispanic Black" ), 
                                        contr.coef = c(1,1,9,9), 
                                        transform = FALSE); linear_contrast_naive5

#number of coefficient in our model 
num_coef <- length(coef(fit_gee_naive)) + 5

###### JRR #######

# number of replicates
num_replicates <- 1000 

#unique ID
unique_id <- unique(physical_long_reduce$id)

# empty matrix to store the replicates coefficient 
rslt <- matrix(NA, nrow = num_replicates, ncol = num_coef   )

for(i in 1:num_replicates){
  # random select observation 
  ID <- sample(unique_id, replace = TRUE  )
  subset_data <- physical_long_reduce[ !is.element(physical_long_reduce$id,ID), ]
  
  # fit GEE with subset data
  fit <- geeglm(formula = physical ~ visit + discrim + black + male + med_group + I(age - 74) + visit*discrim*black, 
                data = subset_data,
                family = gaussian,
                id = id,
                corstr = "exchangeable")
  
  # obtain the linear contrast estiamtes 
  linear_contrast_1 <- linContr.glm( model = fit,
                                     contr.names = c( "discrim:blackNon-Hispanic Black", "visit:discrim:blackNon-Hispanic Black" ), 
                                     contr.coef = c(1,1), 
                                     transform = FALSE)
  
  linear_contrast_2 <- linContr.glm( model = fit,
                                     contr.names = c( "visit", "visit:discrim" ), 
                                     contr.coef = c(1,1), 
                                     transform = FALSE)
  
  linear_contrast_3 <- linContr.glm( model = fit,
                                     contr.names = c( "visit", "visit:blackNon-Hispanic Black", "visit:discrim", "visit:discrim:blackNon-Hispanic Black" ), 
                                     contr.coef = c(1,1,1,1), 
                                     transform = FALSE)
  
  linear_contrast_4 <- linContr.glm( model = fit,
                                     contr.names = c( "visit", "visit:discrim" ), 
                                     contr.coef = c(1,9), 
                                     transform = FALSE)
  
  linear_contrast_5 <- linContr.glm( model = fit,
                                     contr.names = c( "visit", "visit:blackNon-Hispanic Black", "visit:discrim", "visit:discrim:blackNon-Hispanic Black" ), 
                                     contr.coef = c(1,1,9,9), 
                                     transform = FALSE)
  # save resutls in the matrix 
  rslt[i,] <- c(coef(fit), linear_contrast_1$Est, linear_contrast_2$Est, linear_contrast_3$Est, linear_contrast_4$Est, linear_contrast_5$Est  )
  
  print(i)
}

rslt_table <- data.frame(covarite = c(names(fit_gee_naive$coefficients), 
                                      "gamma_11 + gamma_12", 
                                      "gamma_1 + gamma_5", 
                                      "gamma_1 + gamma_4 + 1(gamma_5 + gamma_7)", 
                                      "gamma_1 + 9gamma_5",
                                      "gamma_1 + gamma_4 + 9(gamma_5 + gamma_7)" ), 
                         beta = apply(rslt,2,mean),
                         se = apply(rslt,2,sd)) %>% 
  mutate(test_stat = beta/se, 
         ci95.lo =  beta - qnorm(.975) * se,
         ci95.hi =  beta + qnorm(.975) * se, 
         pvalue =  round(2*(1-pnorm(abs(test_stat))),5)) %>% 
  mutate(beta_nice = format(  round(beta,2 ), nsmall = 2  ) ) %>% 
  mutate(CI = paste0( " (",format( round(ci95.lo, 2 ), nsmall = 2), ", ", format(round(ci95.hi, 2), nsmall = 2), ")" ) ) %>% 
  mutate(P_Value = ifelse(pvalue <.001, "$<$.001", format( round(pvalue, 3) , nsmall = 3) )  ); rslt_table

# one-sided test 

1 - pnorm(abs(1.942)) # 0.0261

# make table 

fileout <- "/Users/Esteban/Desktop/Schools/UCI/Qualifiy_exams/Year-2/Data\ Analysis/qual_2023/Latex/mod2a.tex"

cat("\\begin{tabular}{", " l",  rep(" c" ,3), "}", "\n", 
    "\\toprule", "\n", 
    file=fileout )

cat("\\textbf{Covariate} &&  \\multicolumn{2}{c}{\\textbf{Adjusted}}", " \\\\ ",  "\n",
    " \\cline{1-1}            \\cline{3-4}       \\\\", "\n",
    file = fileout,
    append = TRUE)

cat( paste("&&", "Est", " \\quad ",  "(95\\% CI) & P-value", " \\\\", "\n", 
           "\\hline", "\n",
           "\\\\", 
           "\n"), 
     file=fileout, 
     append=TRUE )

write.table( paste( " \\hspace{1em} ",  rslt_table$covarite, 
                    " && ", 
                    format( round(  rslt_table$beta, 2 ),  nsmall = 2),
                    " (",format( round(rslt_table$ci95.lo, 2 ), nsmall = 2), ", ", format(round(rslt_table$ci95.hi, 2), nsmall = 2), ")",
                    " & ", 
                    ifelse(rslt_table$pvalue <.001, "$<$.001", format( round(rslt_table$pvalue, 3) , nsmall = 3) ) , 
                    "\\\\",
                    sep=""),
             file=fileout, 
             quote=FALSE, 
             row.names=FALSE, 
             col.names=FALSE, 
             append=TRUE )
cat( "\\bottomrule", "\n", "\\end{tabular}", file=fileout, append=TRUE )



