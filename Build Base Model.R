library(tidyverse)
library(magrittr)
library(readxl)
library(car) # for VIF test
library(lme4) # for HLMs

data <- readRDS("H:/Economists/Ed/Analytics Projects/Title I Analysis/2nd Release/Data/school_data.rds")


#### GENERAL SPENDING REGRESSION MODEL ------------------------------------------

# the "starting model" - or first stab at creating the Title I model

spending_reg <- lm(log(School_Average) ~ title_class + Students + log(class_average) + school_year + lea_type + # rem charterschool
                     homeless + special_ed + migrant_status + chronically_absent + ell_status, data = data)

summary(spending_reg)
BIC(spending_reg)

hist(data$School_Average)
hist(log(data$School_Average))

plot(spending_reg, which = 1) 
plot(spending_reg, which = 2) 
plot(spending_reg, which = 3) 
plot(spending_reg, which = 4) 
# model diagnostics are looking far better than before

(exp(coef(spending_reg)["title_classSW"]) - 1) * 100 
# SW estimate -3.9%



#### test inclusion of school_type variable -------------------------------------

data %<>% 
  mutate(school_type = case_when(school_type %in% c("Elementary", "Elementary School") ~ "Elem",
                                 school_type == "Middle School" ~ "Middle",
                                 school_type == "High School" ~ "High",
                                 TRUE ~ school_type))

## quick descriptive analysis of breakdown by school type x Title I status
data %>% 
  filter(title_class == "SW") %>% 
  group_by(school_type) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(perc = n / sum(n)) # note that 91% of Title I schools are elementary schools!
# high schools 4% and middle schools 5%

data %>% 
  filter(title_class == "TA") %>% 
  group_by(school_type) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(perc = n / sum(n)) # same with TA schools. there are significantly less, but almost entirely elementary (no HS even)

data %>% 
  # distinct(title_class, school_type, lea_code, school) %>% count()
  filter(school_year == 2019) %>% 
  group_by(title_class, school_type) %>% 
  count() %>% 
  spread(title_class, n)


## test including in model
spending_elem_reg <- lm(log(School_Average) ~ title_class + Students + log(class_average) + school_year + lea_type +
                     homeless + special_ed + migrant_status + chronically_absent + ell_status +
                     relevel(as.factor(school_type), ref = "Elem"), data = data)

summary(spending_reg)
summary(spending_elem_reg) # R2 increases substantially
# and SW now significantly higher!

BIC(spending_reg)
BIC(spending_elem_reg) # also big improvement

## compare results across models (removing school type)
tibble(predictors = names(spending_reg$coefficients),
       orig = spending_reg$coefficients,
       w_type = spending_elem_reg$coefficients[-c(13, 14)])
# homeless and special ed and SW flip


## break out by school type
spending_elem_only_reg <- lm(log(School_Average) ~ title_class + Students + log(class_average) + school_year + lea_type +
                               homeless + special_ed + migrant_status + chronically_absent + ell_status, 
                             data = data %>% filter(school_type == "Elem"))

spending_middle_only_reg <- lm(log(School_Average) ~ title_class + Students + log(class_average) + school_year + lea_type +
                               homeless + special_ed + migrant_status + chronically_absent + ell_status, 
                             data = data %>% filter(school_type == "Middle"))

spending_high_only_reg <- lm(log(School_Average) ~ title_class + Students + log(class_average) + school_year + lea_type +
                               homeless + special_ed + migrant_status + chronically_absent + ell_status, 
                             data = data %>% filter(school_type == "High"))

spending_midhigh_only_reg <- lm(log(School_Average) ~ title_class + Students + log(class_average) + school_year + lea_type +
                               homeless + special_ed + migrant_status + chronically_absent + ell_status, 
                             data = data %>% filter(school_type != "Elem"))


summary(spending_elem_reg)

summary(spending_elem_only_reg) # SW insignif, TA +7
summary(spending_middle_only_reg) # both SW and TA insignif
summary(spending_high_only_reg) # SW insignif, no TA HSs
# once broken out by school type, SW insignificant across the board

summary(spending_midhigh_only_reg) # SW does become significantly positive...

## compare results across models
tibble(predictors = names(spending_elem_only_reg$coefficients)[-3], # remove TA since not present in HS
       elem_only = spending_elem_only_reg$coefficients[-3],
       middle_only = spending_middle_only_reg$coefficients[-3],
       high_only = spending_high_only_reg$coefficients)
# demographic predictors start having a more positive relationship with spending approaching HS
# generally, lots of flipping between school type models, which makes this difficult to interpret...
# this is good reason to revisit the demographic variables of choice


# for now, decide whether/how to include school_type variable

data %>% 
  group_by(school_type, title_class) %>% 
  count() %>% 
  spread(title_class, n) %>% 
  mutate(perc_ta = TA / (Non + SW + TA) * 100)
# firstly, may want to leave TA schools entirely out from middle/high school analysis since they make up <1% of cases for both
# may even be irrelevant for elementary schools... loop back to this

data %>% 
  group_by(school_type, title_class) %>% 
  count() %>% 
  spread(title_class, n)
# could combine middle and high schools to make a similar grouping size?

# test grouping middle/high
tibble(predictors = names(spending_elem_only_reg$coefficients),
       elem = spending_elem_only_reg$coefficients,
       midhigh = spending_midhigh_only_reg$coefficients)
# still flipping a lot, and quite confusing


## conclusion:
# definitely worth including school_type in the model. specifying different models based on this field increases R2. estimates
# flipping a lot also points to significant differences between these school types

rm(spending_elem_only_reg, spending_elem_reg, spending_high_only_reg, spending_middle_only_reg, spending_midhigh_only_reg)




#### remove Title I spending from spending baseline ------------------------------

# get Title I PKIDS spending data
t1 <- readRDS("H:/Economists/Ed/Analytics Projects/Title I Analysis/2nd Release/Data/Title I Expense by School V3.rds")
data %<>% left_join(t1 %>% select(-school))

# t1 %>% select(-school) %>% anti_join(data)

# quick check against old totals
t1_old <- readRDS("H:/Economists/Ed/Analytics Projects/Title I Analysis/2nd Release/Data/Title I Expense by School.rds")

summary(t1$t1_total) # average of $700
summary(t1_old$t1_total) # average of $470
(700-470)/ 700 # 33% increase
(7-5)/5 # which is reflected in the new SW estimate


data %<>% 
  mutate(t1_admin = ifelse(is.na(t1_admin), 0, t1_admin),
         t1_exc = ifelse(is.na(t1_exc), 0, t1_exc),
         t1_instr_comp = ifelse(is.na(t1_instr_comp), 0, t1_instr_comp),
         t1_instr_rel = ifelse(is.na(t1_instr_rel), 0, t1_instr_rel),
         t1_nutr = ifelse(is.na(t1_nutr), 0, t1_nutr),
         t1_ops = ifelse(is.na(t1_ops), 0, t1_ops),
         t1_transp = ifelse(is.na(t1_transp), 0, t1_transp),
         t1_total = ifelse(is.na(t1_total), 0, t1_total))

summary(data)

# calculate spending total sans F01
data %<>% 
  mutate(Base_School_Average = School_Average - t1_total)


# new base models
spending_elem <- lm(log(School_Average) ~ title_class + Students + log(class_average) + school_year + lea_type +
                               homeless + special_ed + migrant_status + chronically_absent + ell_status, 
                             data = data %>% filter(school_type == "Elem"))

spending_middle <- lm(log(School_Average) ~ title_class + Students + log(class_average) + school_year + lea_type +
                                 homeless + special_ed + migrant_status + chronically_absent + ell_status, 
                               data = data %>% filter(school_type == "Middle"))

spending_high <- lm(log(School_Average) ~ title_class + Students + log(class_average) + school_year + lea_type +
                               homeless + special_ed + migrant_status + chronically_absent + ell_status, 
                             data = data %>% filter(school_type == "High"))

summary(spending_elem) # all insignificant
summary(spending_middle)
summary(spending_high)


## check SW estimates after subtracting F01 from spending average
spending_elem_base <- lm(log(Base_School_Average) ~ title_class + Students + log(class_average) + school_year + lea_type +
                      homeless + special_ed + migrant_status + chronically_absent + ell_status, 
                    data = data %>% filter(school_type == "Elem"))

spending_middle_base <- lm(log(Base_School_Average) ~ title_class + Students + log(class_average) + school_year + lea_type +
                        homeless + special_ed + migrant_status + chronically_absent + ell_status, 
                      data = data %>% filter(school_type == "Middle"))

spending_high_base <- lm(log(Base_School_Average) ~ title_class + Students + log(class_average) + school_year + lea_type +
                      homeless + special_ed + migrant_status + chronically_absent + ell_status, 
                    data = data %>% filter(school_type == "High"))

summary(spending_elem_base) # SW -5%
summary(spending_middle_base) # still insignificant
summary(spending_high_base) # still insignificant

BIC(spending_elem_base)
BIC(spending_middle_base)
BIC(spending_high_base)

# this only really changes the results for elementary schools, and this makes sense since the majority of Title I schools are
# elementary schools.

hist(data$Base_School_Average)
hist(log(data$Base_School_Average)) # helps somewhat with right skew...
hist(1/data$Base_School_Average) # may be an overcorrection. deal with this later.
# log transform seems to be sufficient enough for now

plot(spending_elem_base, which = 1)

rm(spending_elem, spending_middle, spending_high, spending_elem_base, spending_middle_base, spending_high_base)


#### refine demographic variables ----------------------------------------------

# run this analysis iteratively in the following script: 
# H:\Economists\Ed\Analytics Projects\Title I Analysis\2nd Release\Analysis\Archive\Spending by Category Demographic Groupings.R
# results are summarized below.


# initially, I selected the dem3_rem model, which grouped ELL/migrant/homeless and removed the chronically absent variable.
# in other words, the demographics were specified as: special_ed + disadvantaged3
# this drastically simplified the models and did not sacrifice much model performance.

# however, this model does something weird when just looking at middle schools. for some reason, when ELL is included in the grouping,
# the SW estimate jumps from being insignificant to +4%. I am not sure why this model behaves erratically, but it made me opt for a
# simpler model that behaved similarly across all school type levels.

# the next-best model was dropping all demographic variables but special_ed and ell_status. this demographic grouping behaved similarly
# across all school type levels, which is appealing. the final model results are below.

spending_elem_dem_reg <- lm(log(Base_School_Average) ~ title_class + Students + log(class_average) + school_year + lea_type + 
                              special_ed + ell_status, data = data %>% filter(school_type == "Elem"))

spending_mid_dem_reg <- lm(log(Base_School_Average) ~ title_class + Students + log(class_average) + school_year + lea_type + 
                              special_ed + ell_status, data = data %>% filter(school_type == "Middle"))

spending_high_dem_reg <- lm(log(Base_School_Average) ~ title_class + Students + log(class_average) + school_year + lea_type + 
                              special_ed + ell_status, data = data %>% filter(school_type == "High"))


summary(spending_elem_dem_reg) # SW -5%
summary(spending_mid_dem_reg) # SW insig
summary(spending_high_dem_reg) # SW insig

BIC(spending_elem_dem_reg)
BIC(spending_mid_dem_reg)
BIC(spending_high_dem_reg)

# check how estimates change between school type
tibble(predictors = names(spending_elem_dem_reg$coefficients)[-3], # remove TA since not present in HS
       elem_only = spending_elem_dem_reg$coefficients[-3],
       middle_only = spending_mid_dem_reg$coefficients[-3],
       high_only = spending_high_dem_reg$coefficients)
# special ed status is now the only estimate that's flipping (SW insignificant)

# check multicollinearity
car::vif(spending_elem_dem_reg)
car::vif(spending_mid_dem_reg)
car::vif(spending_high_dem_reg)
# all values < 4, most < 2 - proceed




#### test demographic interaction variables --------------------------------------

# run this analysis iteratively in the following script: 
# H:\Economists\Ed\Analytics Projects\Title I Analysis\2nd Release\Analysis\Archive\Spending by Category Demographic Interactions.R
# results are summarized below.


# Special ed interaction
  # Elementary: SW -6%, R2 0.63, BIC -3770
  # Middle: interaction not significant
  # High: interaction not significant
# ELL interaction
  # Elementary: SW -5%, R2 0.64, BIC -3810
  # Middle: SW +7%, R2 0.63, BIC -1470
  # High: SW +1%, R2 0.75, BIC -970
# Special ed and ELL interaction
  # Elementary: SW -6%, R2 0.64, BIC -3820
  # Middle: similar to just ELL
  # High: similar to just ELL


# I decided to leave interaction terms out because they don't improve predictive power or change the Title I estimates by much,
# but they do significantly complicate the model.



### quick check - what do the models say without demographics included?
# spending_nodem_elem_reg <- lm(log(Base_School_Average) ~ title_class + Students + log(class_average) + school_year + lea_type,
#                             data = data %>% filter(school_type == "Elem"))
# 
# spending_nodem_mid_reg <- lm(log(Base_School_Average) ~ title_class + Students + log(class_average) + school_year + lea_type,
#                            data = data %>% filter(school_type == "Middle"))
# 
# spending_nodem_high_reg <- lm(log(Base_School_Average) ~ title_class + Students + log(class_average) + school_year + lea_type,
#                             data = data %>% filter(school_type == "High"))
# 
# summary(spending_nodem_elem_reg) # SW +3%
# summary(spending_nodem_mid_reg) # SW +14%
# summary(spending_nodem_high_reg) # SW +13%



### final models
# still the same from the last section

spending_elem_dem_reg <- lm(log(Base_School_Average) ~ title_class + Students + log(class_average) + school_year + lea_type + 
                              special_ed + ell_status, data = data %>% filter(school_type == "Elem"))

spending_mid_dem_reg <- lm(log(Base_School_Average) ~ title_class + Students + log(class_average) + school_year + lea_type + 
                             special_ed + ell_status, data = data %>% filter(school_type == "Middle"))

spending_high_dem_reg <- lm(log(Base_School_Average) ~ title_class + Students + log(class_average) + school_year + lea_type + 
                              special_ed + ell_status, data = data %>% filter(school_type == "High"))


summary(spending_elem_dem_reg) # SW -5%
summary(spending_mid_dem_reg) # SW insig
summary(spending_high_dem_reg) # SW insig




#### fine tune decisions (transformations, groupings, etc. ) -----------------------

## start with final models
spending_elem_dem_reg <- lm(log(Base_School_Average) ~ title_class + Students + log(class_average) + school_year + lea_type + 
                              special_ed + ell_status, data = data %>% filter(school_type == "Elem"))

spending_mid_dem_reg <- lm(log(Base_School_Average) ~ title_class + Students + log(class_average) + school_year + lea_type + 
                             special_ed + ell_status, data = data %>% filter(school_type == "Middle"))

spending_high_dem_reg <- lm(log(Base_School_Average) ~ title_class + Students + log(class_average) + school_year + lea_type + 
                              special_ed + ell_status, data = data %>% filter(school_type == "High"))

summary(spending_elem_dem_reg) # SW -5%, TA +5%
summary(spending_mid_dem_reg) # SW insig, TA insig
summary(spending_high_dem_reg) # SW insig, no TA schools

BIC(spending_elem_dem_reg)
BIC(spending_mid_dem_reg)
BIC(spending_high_dem_reg)

# check how estimates change between school type
tibble(predictors = names(spending_elem_dem_reg$coefficients)[-3], # remove TA since not present in HS
       elem_only = spending_elem_dem_reg$coefficients[-3],
       middle_only = spending_mid_dem_reg$coefficients[-3],
       high_only = spending_high_dem_reg$coefficients)



## try removing log-transform of class_average
spending_elem_reg <- lm(log(Base_School_Average) ~ title_class + Students + class_average + school_year + lea_type + 
                              special_ed + ell_status, data = data %>% filter(school_type == "Elem"))

spending_mid_reg <- lm(log(Base_School_Average) ~ title_class + Students + class_average + school_year + lea_type + 
                             special_ed + ell_status, data = data %>% filter(school_type == "Middle"))

spending_high_reg <- lm(log(Base_School_Average) ~ title_class + Students + class_average + school_year + lea_type + 
                              special_ed + ell_status, data = data %>% filter(school_type == "High"))

summary(spending_elem_reg) # same
summary(spending_mid_reg)
summary(spending_high_reg)



## try grouping middle and high schools
spending_midhigh_reg <- lm(log(Base_School_Average) ~ title_class + Students + class_average + school_year + lea_type + 
                             special_ed + ell_status, data = data %>% filter(school_type %in% c("Middle", "High")))

summary(spending_midhigh_reg)


tibble(predictors = names(spending_elem_dem_reg$coefficients)[-3], # remove TA since not present in HS
       elem_only = spending_elem_dem_reg$coefficients[-3],
       middle_only = spending_mid_dem_reg$coefficients[-3],
       high_only = spending_high_dem_reg$coefficients,
       midhigh_only = spending_midhigh_reg$coefficients[-3])
# interesting - sped balances each other out to become insignificant in the combined model
# other than that, results stay similar

rm(spending_elem_dem_reg, spending_mid_dem_reg, spending_high_dem_reg, spending_elem_reg, spending_mid_reg, spending_high_reg,
   spending_midhigh_reg)



## rename school_type to primary/secondary

data %<>% 
  mutate(school_type = ifelse(school_type == "Elem", "Primary", "Secondary"))

spending_prim_reg <- lm(log(Base_School_Average) ~ title_class + Students + class_average + school_year + lea_type + 
                          special_ed + ell_status, data = data %>% filter(school_type == "Primary" & lea_code != "PCSD"))

spending_sec_reg <- lm(log(Base_School_Average) ~ title_class + Students + class_average + school_year + lea_type + 
                             special_ed + ell_status, data = data %>% filter(school_type == "Secondary"))

summary(spending_prim_reg) # note that removing PCSD makes TA insignificant...
summary(spending_sec_reg)

# try removing TA schools
spending_prim_reg <- lm(log(Base_School_Average) ~ title_class + Students + class_average + school_year + lea_type + 
                          special_ed + ell_status, data = data %>% filter(school_type == "Primary" & title_class != "TA"))

spending_sec_reg <- lm(log(Base_School_Average) ~ title_class + Students + class_average + school_year + lea_type + 
                         special_ed + ell_status, data = data %>% filter(school_type == "Secondary" & title_class != "TA"))

summary(spending_prim_reg) # note that removing PCSD makes TA insignificant...
summary(spending_sec_reg)

BIC(spending_prim_reg)
BIC(spending_sec_reg)

tibble(predictors = names(spending_prim_reg$coefficients), # remove TA since not present in HS
       primary = spending_prim_reg$coefficients,
       secondary = spending_sec_reg$coefficients)

(exp(coef(spending_prim_reg)["title_classSW"]) - 1) * 100 

# calculate 95% confidence interval
(exp(confint(spending_prim_reg, level = 0.95)[2,]) - 1) * 100 


(exp(coef(spending_prim_reg)["title_classTA"]) - 1) * 100 

(exp(coef(spending_sec_reg)["title_classSW"]) - 1) * 100 
(exp(coef(spending_sec_reg)["title_classTA"]) - 1) * 100 

# check after Title I added back in
spending_after_reg <- lm(log(School_Average) ~ title_class + Students + class_average + school_year + lea_type + 
                          special_ed + ell_status, data = data %>% filter(school_type == "Primary" & title_class != "TA"))

summary(spending_after_reg) # SW insignificant

(exp(coef(spending_after_reg)["title_classSW"]) - 1) * 100 

# condense these models because middle & high schools have very similar estimates. this simplifies the modeling process.
# and now all variable estimates point in the same direction.


### check plots
# for primary schools
plot(spending_prim_reg, which = 1)
plot(spending_prim_reg, which = 2)
plot(spending_prim_reg, which = 3)
plot(spending_prim_reg, which = 4)

# for secondary schools
plot(spending_sec_reg, which = 1)
plot(spending_sec_reg, which = 2) # this has a really heavy tail
plot(spending_sec_reg, which = 3)
plot(spending_sec_reg, which = 4)




#### map these results into dollar amounts

coefs <- coef(spending_prim_reg)
prim_data <- data %>% filter(school_type == "Primary")

non_est <- exp(coefs[1] +                                         # intercept
                 coefs[2] * 0 +                                     # title class == SW
                 coefs[3] * 0 +                                     # title class == TA
                 coefs[4] * mean(prim_data$Students) +               # students in school
                 coefs[5] * mean(prim_data$class_average) +     # average students in class
                 coefs[6] * 2019 +                                  # school year
                 coefs[7] * 1 +                                     # LEA type: urban vs. rural
                 coefs[8] * mean(prim_data$special_ed) +            # special education
                 coefs[9] * mean(prim_data$ell_status)              # ell
)

sw_est <- exp(coefs[1] +                                         # intercept
                 coefs[2] * 1 +                                     # title class == SW
                 coefs[3] * 0 +                                     # title class == TA
                 coefs[4] * mean(prim_data$Students) +               # students in school
                 coefs[5] * mean(prim_data$class_average) +     # average students in class
                 coefs[6] * 2019 +                                  # school year
                 coefs[7] * 1 +                                     # LEA type: urban vs. rural
                 coefs[8] * mean(prim_data$special_ed) +            # special education
                 coefs[9] * mean(prim_data$ell_status)              # ell
)

non_est - sw_est


spending_fix_reg <- lm(log(Base_School_Average) ~ title_class + Students + class_average + school_year + lea_type + 
                          special_ed + ell_status + factor(lea_code), 
                       data = data %>% filter(school_type == "Primary" & title_class != "TA"))
summary(spending_fix_reg) # still positive and significant (2%)

spending_fix_after_reg <- lm(log(School_Average) ~ title_class + Students + class_average + school_year + lea_type + 
                         special_ed + ell_status + factor(lea_code), 
                       data = data %>% filter(school_type == "Primary" & title_class != "TA"))
summary(spending_fix_after_reg) # positive and significant (8%)



## finally, test exclusion of San Juan

spending_sjsd_prim_reg <- lm(log(Base_School_Average) ~ title_class + Students + class_average + school_year + lea_type + 
                          special_ed + ell_status, data = data %>% filter(school_type == "Primary") %>% filter(lea_code != "SJSD"))

spending_sjsd_sec_reg <- lm(log(Base_School_Average) ~ title_class + Students + class_average + school_year + lea_type + 
                         special_ed + ell_status, data = data %>% filter(school_type == "Secondary") %>% filter(lea_code != "SJSD"))

summary(spending_sjsd_prim_reg)
summary(spending_sjsd_sec_reg)
# this doesn't change SW estimates at all

tibble(predictors = names(spending_prim_reg$coefficients), # remove TA since not present in HS
       primary = spending_prim_reg$coefficients,
       secondary = spending_sec_reg$coefficients,
       primarysj = spending_sjsd_prim_reg$coefficients,
       secondarysj = spending_sjsd_sec_reg$coefficients)
# not a big deal - definitely safe to leave SJSD in the analysis

rm(spending_sjsd_prim_reg, spending_sjsd_sec_reg)


### does this differ when nutrition is removed? (a federal program)

data %<>% mutate(BaseSubNutr = Base_School_Average - nutr)

spending_prim_reg_nutr <- lm(log(BaseSubNutr) ~ title_class + Students + class_average + school_year + lea_type + 
                          special_ed + ell_status, data = data %>% filter(school_type == "Primary"))

spending_sec_reg_nutr <- lm(log(BaseSubNutr) ~ title_class + Students + class_average + school_year + lea_type + 
                         special_ed + ell_status, data = data %>% filter(school_type == "Secondary"))

summary(spending_prim_reg_nutr) # bumps to -6%
summary(spending_sec_reg_nutr)

BIC(spending_prim_reg)
BIC(spending_sec_reg)


## check how much title I spending
# still roughly half of what it should be in 2019....
data %>% 
  filter(school_year == 2019) %>% 
  summarize(total = sum(t1_total * Students))



#### SPENDING BY CATEGORY MODELS ---------------------------------------------------


#### instructional compensation -------------------------------------------------

# first create baseline instr_comp variable (removing Title I funds)
data %<>% 
  mutate(base_instr_comp = instr_comp - t1_instr_comp)

summary(data$base_instr_comp)


hist(data$base_instr_comp)
hist(log(data$base_instr_comp)) # still helps


## run models
comp_prim_reg <- lm(log(base_instr_comp) ~ title_class + Students + class_average + school_year + lea_type + 
                          special_ed + ell_status, data = data %>% filter(school_type == "Primary" & title_class != "TA"))

comp_sec_reg <- lm(log(base_instr_comp) ~ title_class + Students + class_average + school_year + lea_type + 
                         special_ed + ell_status, data = data %>% filter(school_type == "Secondary"))

comp_prim_after_reg <- lm(log(instr_comp) ~ title_class + Students + class_average + school_year + lea_type + 
                      special_ed + ell_status, data = data %>% filter(school_type == "Primary" & title_class != "TA"))


summary(comp_prim_reg) # SW -9%, TA -10%
summary(comp_sec_reg) # SW +10%, TA +14%
summary(comp_prim_after_reg) # 

(exp(coef(comp_prim_reg)["title_classSW"]) - 1) * 100 # -12%
(exp(coef(comp_prim_reg)["title_classTA"]) - 1) * 100 

(exp(coef(comp_prim_after_reg)["title_classSW"]) - 1) * 100 # still -9%

# (exp(coef(comp_sec_reg)["title_classSW"]) - 1) * 100 # both insignificant
# (exp(coef(comp_sec_reg)["title_classTA"]) - 1) * 100

# plot(comp_prim_reg, which = 1)




#### instructional related --------------------------------------------------------

# first create baseline variable (removing Title I funds)
data %<>% 
  mutate(base_instr_rel = instr_rel - t1_instr_rel)

summary(data$base_instr_rel)


hist(data$base_instr_rel)
hist(log(data$base_instr_rel)) # still helps


## run models
rel_prim_reg <- lm(log(base_instr_rel) ~ title_class + Students + class_average + school_year + lea_type + 
                      special_ed + ell_status, data = data %>% filter(school_type == "Primary" & title_class != "TA"))

rel_sec_reg <- lm(log(base_instr_rel) ~ title_class + Students + class_average + school_year + lea_type + 
                     special_ed + ell_status, data = data %>% filter(school_type == "Secondary"))

rel_prim_after_reg <- lm(log(instr_rel) ~ title_class + Students + class_average + school_year + lea_type + 
                     special_ed + ell_status, data = data %>% filter(school_type == "Primary" & title_class != "TA"))

summary(rel_prim_reg)
summary(rel_sec_reg)
summary(rel_prim_after_reg)

(exp(coef(rel_prim_reg)["title_classSW"]) - 1) * 100 # insignificant
(exp(coef(rel_prim_reg)["title_classTA"]) - 1) * 100 
(exp(coef(rel_prim_after_reg)["title_classSW"]) - 1) * 100 # becomes +17%

# (exp(coef(rel_sec_reg)["title_classSW"]) - 1) * 100 # both insignificant
# (exp(coef(rel_sec_reg)["title_classTA"]) - 1) * 100



#### admin ----------------------------------------------------------------------

# first create baseline variable (removing Title I funds)
data %<>% 
  mutate(base_admin = admin - t1_admin)

summary(data$base_admin)

hist(data$base_admin)
hist(log(data$base_admin)) # still helps


## run models
admin_prim_reg <- lm(log(base_admin) ~ title_class + Students + class_average + school_year + lea_type + 
                     special_ed + ell_status, data = data %>% filter(school_type == "Primary" & title_class != "TA"))

admin_sec_reg <- lm(log(base_admin) ~ title_class + Students + class_average + school_year + lea_type + 
                    special_ed + ell_status, data = data %>% filter(school_type == "Secondary"))

admin_prim_after_reg <- lm(log(admin) ~ title_class + Students + class_average + school_year + lea_type + 
                       special_ed + ell_status, data = data %>% filter(school_type == "Primary" & title_class != "TA"))


summary(admin_prim_reg)
summary(admin_sec_reg)
summary(admin_prim_after_reg)

(exp(coef(admin_prim_reg)["title_classSW"]) - 1) * 100 # -9%
(exp(coef(admin_prim_reg)["title_classTA"]) - 1) * 100 
(exp(coef(admin_prim_after_reg)["title_classSW"]) - 1) * 100 # -8%

# (exp(coef(admin_sec_reg)["title_classSW"]) - 1) * 100 # both insignificant
# (exp(coef(admin_sec_reg)["title_classTA"]) - 1) * 100



#### operations -------------------------------------------------------------------

# decided to remove this category since the results seemed pretty unreliable
# these were the prior results:

# Elementary: SW -12%, TA +4%
#   Middle: SW insig, TA insig
# High:  SW +14%, no TA (this is almost completely due to SJSD)
# Removing demographics (sped and ell):
#   Elementary: SW -2%, TA +10%
#   Middle: SW +15%, TA -10%
#   High: SW +35%??
  
# so decided to remove operations entirely because Title I spending is hardly used for this purpose and the results are pretty weird 
# and there is no theoretical significance for why operations would be different at non/Title I schools (unlike depreciation…)





#### WITHIN DISTRICT TITLE I SPENDING HLM ANALYSIS ------------------------------

# how many TA/SW schools of the same type occur within the same district?

# things to consider:
# (1) just elementary schools
# (2) whether results differ with manually removing single T1 schools or not
# (3) with log-transformed response?
# (4) effect interpretations


### create reduced data frame with peer non/Title I observations

# check secondary school breakout
data %>% 
  filter(school_type == "Secondary") %>% 
  group_by(lea_code, title_class) %>% 
  count() %>% 
  spread(title_class, n) # %>% View()
# secondary data is quite sparse. I don't think there's enough districts to make conclusions about the state, and the group sizes are
# also very unbalanced which can lead to modeling issues. restrict this analysis only to elementary schools.

data %>% 
  filter(school_type == "Primary") %>% 
  group_by(lea_code, title_class) %>% 
  count() %>% 
  spread(title_class, n) # %>% View()
# the data looks sufficient for a non vs sw analysis with about 60% of districts having usable observations
# targeted assistance is much more sparse. I don't think there's enough data here for a representative sample of the state either.

## -> restrict the analysis to Non vs SWP elementary schools


### create reduced data set
# get list of districts with both SW and Non schools
red_districts <- data %>% 
  filter(school_type == "Primary") %>% 
  filter(title_class != "TA") %>% 
  group_by(lea_code, title_class) %>% 
  count() %>% 
  spread(title_class, n) %>%
  filter(!(is.na(Non) | is.na(SW))) %>%
  pull(lea_code)

data %>%
  filter(school_type == "Primary") %>%
  filter(title_class != "TA") %>%
  group_by(lea_code, title_class) %>%
  count() %>%
  spread(title_class, n) %>%
  filter((is.na(Non) | is.na(SW))) %>% View()


red_data <- data %>% 
  filter(school_type == "Primary") %>% 
  filter(title_class != "TA") %>% 
  filter(lea_code %in% red_districts)


# how much does this restrict the sample?
length(red_districts)/38 # a little more than half of the state's districts

red_data %>% distinct(lea_code, school) %>% count() # 476 schools
data %>% filter(school_type == "Primary") %>% filter(title_class != "TA") %>% 
  distinct(lea_code, school) %>% count() # out of 519
476/519 # I guess this is 92% of elementary SW/Non schools - not too bad


### test fixed effects model first!
# baseline performance of final statewide model
spending_prim_reg <- lm(log(Base_School_Average) ~ title_class + Students + class_average + school_year + lea_type + 
                          special_ed + ell_status, data = data %>% filter(school_type == "Primary"))
summary(spending_prim_reg)


base_fixed_reg <- lm(log(Base_School_Average) ~ title_class + Students + class_average + school_year + lea_type + 
                  special_ed + ell_status, data = red_data)
summary(base_fixed_reg) # SW still -5% here before controlling for LEA
BIC(base_fixed_reg)


fixed_reg <- lm(log(Base_School_Average) ~ title_class + Students + class_average + school_year + lea_type + 
                          special_ed + ell_status + factor(lea_code), data = red_data)
summary(fixed_reg) # still the same results as HLM: +4% for SW
BIC(fixed_reg) # definitely an improvement (and just as good as HLM)

(exp(coef(fixed_reg)["title_classSW"]) - 1) * 100 # now +1.8%

# use this base model for spending cat differences

comp_fixed_reg <- lm(log(base_instr_comp) ~ title_class + Students + class_average + school_year + lea_type + 
                  special_ed + ell_status + factor(lea_code), data = red_data)
summary(comp_fixed_reg) # SW insignificant

rel_fixed_reg <- lm(log(base_instr_rel) ~ title_class + Students + class_average + school_year + lea_type + 
                       special_ed + ell_status + factor(lea_code), data = red_data)
summary(rel_fixed_reg) # SW +5.5%

admin_fixed_reg <- lm(log(base_admin) ~ title_class + Students + class_average + school_year + lea_type + 
                       special_ed + ell_status + factor(lea_code), data = red_data)
summary(admin_fixed_reg) # SW insignificant






### check results with reduced data set

# (1) verify HLM is an improvement
base_red <- lm(log(Base_School_Average) ~ 1, data = red_data)
BIC(base_red)

level_red <- lmer(log(Base_School_Average) ~ 1 + (1|lea_code), data = red_data)
BIC(level_red) # much better model with levels - add all variables

# (2) run with actual variables
mod_red <- lmer(log(Base_School_Average) ~ title_class + Students + class_average + school_year + lea_type + 
                special_ed + ell_status + (1|lea_code), data = red_data)
BIC(mod_red) # -5960
summary(mod_red) # SW +4%


mod_red_scale <- lmer(log(Base_School_Average) ~ title_class + Students + class_average + school_year + lea_type + 
                        I(special_ed * Students) + I(ell_status * Students) + (1|lea_code), data = red_data)
BIC(mod_red_scale) # -5940
summary(mod_red_scale) # SW +6%!


mod_red_rem <- lmer(log(Base_School_Average) ~ title_class + Students + class_average + school_year + # rem urban
                        I(special_ed * Students) + I(ell_status * Students) + (1|lea_code), data = red_data)
BIC(mod_red_rem) # -5950
summary(mod_red_rem) # SW +6%

plot(mod_red_rem, which = 1) # this looks pretty good

(exp(coef(mod_red_rem)["title_classSW"]) - 1) * 100 
(exp(coef(mod_red_rem)["title_classTA"]) - 1) * 100 


mod_red_scale3 <- lmer(log(Base_School_Average) ~ title_class + scale(Students) + class_average + school_year +
                         special_ed + ell_status + (1|lea_code), data = red_data)
BIC(mod_red_scale3) # -5980
summary(mod_red_scale3) # also +4% here, but ELL becomes insignificant


# check how estimates change removing granite
# mod_red_gtsd <- lmer(log(Base_School_Average) ~ title_class + Students + class_average + school_year + # rem urban
#                       I(special_ed * Students) + I(ell_status * Students) + (1|lea_code), 
#                      data = red_data %>% filter(lea_code != "GTSD"))
# BIC(mod_red_gtsd) # -6290
# summary(mod_red_gtsd) # drops back to +4%


## compare with results from lm
spending_prim_reg <- lm(log(Base_School_Average) ~ title_class + Students + class_average + school_year + lea_type + 
                          special_ed + ell_status, data = data %>% filter(school_type == "Primary"))
summary(spending_prim_reg)




# check whether this is believable - how much is ELL concentrated w/in districts?
# red_data %>% 
#   group_by(lea_code) %>% 
#   summarize(avg = mean(ell_status, na.rm = T), sd = sd(ell_status, na.rm = T)) %>% 
#   arrange(desc(avg))
# it looks like there should be some significance here...



#### run spending by cat models using this base model

mod_red_scale3 <- lmer(log(Base_School_Average) ~ title_class + scale(Students) + class_average + school_year +
                         special_ed + ell_status + (1|lea_code), data = red_data)
BIC(mod_red_scale3) # -5980
summary(mod_red_scale3) # also +4% here, but ELL becomes insignificant

plot(mod_red_scale3, which = 1) # looks decent



comp_mod_red <- lmer(log(base_instr_comp) ~ title_class + scale(Students) + class_average + school_year +
                         special_ed + ell_status + (1|lea_code), data = red_data)
summary(comp_mod_red) # SW insignificant


rel_mod_red <- lmer(log(base_instr_rel) ~ title_class + scale(Students) + class_average + school_year +
                       special_ed + ell_status + (1|lea_code), data = red_data)
summary(rel_mod_red) # SW +10%

admin_mod_red <- lmer(log(base_admin) ~ title_class + scale(Students) + class_average + school_year +
                      special_ed + ell_status + (1|lea_code), data = red_data)
summary(admin_mod_red) # SW insignificant

# these results roughly align with the statewide model, but the estimates are less negative


red_data %>% 
  ggplot(aes(x = title_class, y = log(Base_School_Average))) +
  geom_boxplot()



### compare results using the full data set

# (1) verify HLM is an improvement
base_full <- lm(log(Base_School_Average) ~ 1, data = data)
BIC(base_full)

level_full <- lmer(log(Base_School_Average) ~ 1 + (1|lea_code), data = data)
BIC(level_full) # much better model with levels - add all variables


# (2) run with actual variables
mod_full <- lmer(log(Base_School_Average) ~ title_class + Students + class_average + school_year + lea_type + 
                  special_ed + ell_status + (1|lea_code), data = data)
BIC(mod_full) # -6560
summary(mod_full) # SW -4%! (opposite sign than reduced data set) and TA -4%
# I think this is because elementary and high schools are being compared...


mod_full_scale <- lmer(log(Base_School_Average) ~ title_class + Students + class_average + school_year + lea_type + 
                        I(special_ed * Students) + I(ell_status * Students) + (1|lea_code), data = data)
BIC(mod_full_scale) # -6340
summary(mod_full_scale) # SW insignificant! and so is special ed? and students? these results are funny


plot(mod_full, which = 1) # this looks pretty good



### try full data set (all LEAs) but just elementary schools

mod_elem_full <- lmer(log(Base_School_Average) ~ title_class + Students + class_average + school_year + lea_type + 
                   special_ed + ell_status + (1|lea_code), data = data %>% filter(school_type == "Primary"))
BIC(mod_elem_full) # -6660
summary(mod_elem_full) # yep becomes +4% again
# only special ed sign is different from original model, all other signs are consistent. this seems pretty reliable

mod_elem_full_scale <- lmer(log(Base_School_Average) ~ title_class + Students + class_average + school_year + lea_type + 
                              I(special_ed * Students) + I(ell_status * Students) + (1|lea_code), 
                            data = data %>% filter(school_type == "Primary"))
BIC(mod_elem_full_scale) # -6630
summary(mod_elem_full_scale) # also becomes 6% with scale



## compare these results to base lm
spending_prim_reg <- lm(log(Base_School_Average) ~ title_class + Students + class_average + school_year + lea_type + 
                          special_ed + ell_status, data = data %>% filter(school_type == "Primary"))
summary(spending_prim_reg)



## start with naive HLM
base <- lm(log(Base_School_Average) ~ 1, data = data)
BIC(base)

level <- lmer(log(Base_School_Average) ~ 1 + (1|lea_code), data = data)
BIC(level) # much better model with levels

naive <- lmer(log(Base_School_Average) ~ title_class + Students + class_average + school_year + lea_type +
                special_ed + ell_status + (1|lea_code), data = data)
BIC(naive)
summary(naive)
car::vif(naive)

adj <- lmer(log(Base_School_Average) ~ title_class + Students + class_average + school_year + lea_type + 
              I(special_ed * Students) + I(ell_status * Students) + (1|lea_code), data = data)
BIC(adj)
summary(adj)




complex <- lmer(log(Base_School_Average) ~ title_class + Students + class_average + school_year + lea_type + 
                  special_ed + ell_status + (1 + school_year|lea_code), data = data) # change in LEAs over time

summary(complex)
BIC(complex)

summary(naive)
BIC(naive)
anova(naive)





#### SPENDING BY CATEGORY WITHIN TITLE I SCHOOLS ----------------------------------

# get data just at SWP Title I schools
t1_data <- data %>% 
  filter(title_class == "SW")

# is spending less at high-poverty T1 schools than low-poverty T1 schools?

hist(t1_data$low_income_yn)

t1_data %>% 
  ggplot(aes(x = low_income_yn, y = log(Base_School_Average))) +
  geom_point() + geom_smooth(method = "lm", se = F)
# ok definitely a clear positive relationship between low_income status and spending in general


# start with final spending model
t1_base_reg <- lm(log(Base_School_Average) ~ low_income_yn + Students + class_average + school_year + lea_type + 
                          special_ed + ell_status, data = t1_data %>% filter(school_type == "Primary"))
summary(t1_base_reg) # LI +20%!

# suspect high multicollinearity here
car::vif(t1_base_reg) # actually not bad?

rcorr(t1_base_reg)


