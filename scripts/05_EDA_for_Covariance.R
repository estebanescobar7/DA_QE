
library(tidyverse)
library(ggplot2)
library(table1)
library(patchwork)
library(splines)

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

#length( unique(physical_long_reduce$id) ) #3772 people who have 3 or more visit 

# lets compute the empirical correlation matrix 

# Esteban way: 

# Covariance matrix
cov_mat <- pivot_wider(data = physical_long_reduce, 
                       id_cols = id, 
                       names_from = visit, 
                       values_from = physical, 
                       names_prefix = "Visit_") %>% 
  select( starts_with("Visit") ) %>% 
  cov( use = "pairwise.complete.obs" )

# Correlation matrix 
vvec <- diag(emp_corr_mat)
cormat <- emp_corr_mat/( outer( sqrt(vvec), sqrt(vvec) )   )

# Dan way: 

##### pairwise residual scatter plots

fit <- lm( physical ~ ns( visit, knots=c(0,1,2,3) ), data=physical_long_reduce )
resids <- physical_long_reduce$physical - fitted( fit )
nobs <- length( physical_long_reduce$physical )
nsubjects <- length( table( physical_long_reduce$id ) )
rmat <- matrix( NA, nsubjects, 5 )
ycat <- c( 0,1,2,3,4)
nj <- unlist( lapply( split( physical_long_reduce$id, physical_long_reduce$id ), length ) )
mymin <- function(x){ifelse(sum(!is.na(x) )==0,
                            NA, min(x, na.rm=TRUE))}
for( j in 1:5 ){
  legal <- ( physical_long_reduce$visit >= ycat[j]-0.5 )&( physical_long_reduce$visit < ycat[j]+0.5 )
  jtime <- physical_long_reduce$visit + 0.01*rnorm(nobs)
  t0 <- unlist( lapply( split(abs(jtime - ycat[j]) , physical_long_reduce$id), min))
  tj <- rep( t0, nj )
  keep <- ( abs( jtime - ycat[j] )==tj ) & ( legal )
  yj <- rep( NA, nobs )
  yj[keep] <- resids[keep]
  yj <- unlist( lapply( split( yj, physical_long_reduce$id ), mymin ) )
  rmat[ , j ] <- yj
}
dimnames( rmat ) <- list( NULL, paste("visit",c(0:4)) )
pairs( rmat )

# lets make the matrix 

cmat <- matrix(0,5,5)
nmat <- matrix(0,5,5)

for(j in 1:5){
  for(k in j:5){
    njk <- sum(!is.na( rmat[,j]*rmat[,k]  ) )
    sjk <- sum(rmat[,j]*rmat[,k],na.rm = TRUE )/njk
    cmat[j,k] <- sjk
    nmat[j,k]<- njk
  }
}

vvec <- diag(cmat)
cormat <- cmat/(outer(sqrt(vvec),sqrt(vvec)) )

# save plot to file 
pdf(file = "/Users/Esteban/Desktop/Schools/UCI/Qualifiy_exams/Year-2/Data\ Analysis/qual_2023/Figures/variogram.pdf",
    width = 10,
    height = 8)
# varigoram: 
out <- lda.variogram(id = physical_long_reduce$id, y = resids, x =  physical_long_reduce$visit  )
dr <- out$delta.y
dt <- out$delta.x
var.est <- var(resids); var.est
plot(dt,dr,pch= ".", ylim = c(0,1.2*var.est) )
lines( smooth.spline(dt,dr,df = 4), lwd = 3 )
abline(h=var.est, lty = 2, lwd = 2 )
title("Physical Function Score Residual Variogram" )
dev.off()


# Exchangable 

