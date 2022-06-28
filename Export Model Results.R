library(tidyverse)
library(magrittr)
library(readxl)
library(car)
library(lme4) # for hlm
library(lmerTest) # for p values
library(broom.mixed) # for formatting hlm output
library(MuMIn) # for R2 from hlm


#### data prep -----------------------------------------------------------------

data <- readRDS("H:/Economists/Ed/Analytics Projects/Title I Analysis/2nd Release/Data/school_data.rds")

data %<>% 
  mutate(school_type = case_when(school_type %in% c("Elementary", "Elementary School") ~ "Primary",
                                 school_type %in% c("Middle School", "High School") ~ "Secondary",
                                 TRUE ~ school_type))

# get Title I PKIDS spending data
t1 <- readRDS("H:/Economists/Ed/Analytics Projects/Title I Analysis/2nd Release/Data/Title I Expense by School V3.rds")
data %<>% left_join(t1 %>% select(-school))

data %<>% 
  mutate(t1_admin = ifelse(is.na(t1_admin), 0, t1_admin),
         t1_exc = ifelse(is.na(t1_exc), 0, t1_exc),
         t1_instr_comp = ifelse(is.na(t1_instr_comp), 0, t1_instr_comp),
         t1_instr_rel = ifelse(is.na(t1_instr_rel), 0, t1_instr_rel),
         t1_nutr = ifelse(is.na(t1_nutr), 0, t1_nutr),
         t1_ops = ifelse(is.na(t1_ops), 0, t1_ops),
         t1_transp = ifelse(is.na(t1_transp), 0, t1_transp),
         t1_total = ifelse(is.na(t1_total), 0, t1_total))

# calculate spending total sans F01
data %<>% 
  mutate(Base_School_Average = School_Average - t1_total)

data %<>% 
  mutate(base_instr_comp = instr_comp - t1_instr_comp)

data %<>% 
  mutate(base_instr_rel = instr_rel - t1_instr_rel)

data %<>% 
  mutate(base_admin = admin - t1_admin)

# remove TA and Secondary schools
data %<>% filter(school_type == "Primary" & title_class != "TA")

# remove 2020 and later
data %<>% filter(school_year <= 2019)

# create unique ID for school
data %<>% 
  mutate(school_name = school) %>% 
  mutate(school = paste0(lea_code, "-", school_number))



#### aggregate across school years

full <- data # store full data set broken out by year

names(data)

data %<>% 
  select(school_year:title_class, school, Students, lea_type, class_average, ell_status, special_ed,
         Base_School_Average, School_Average, base_instr_comp, instr_comp, base_instr_rel, instr_rel,
         base_admin, admin)

# remove schools that changed Title I status across years

title_change <- data %>% 
  group_by(school, title_class) %>% 
  count() %>% 
  spread(title_class, n, fill = 0) %>% 
  ungroup() %>% 
  # count() # 512 schools total
  filter(Non > 0 & SW > 0) %>% 
  pull(school)
# 23 schools changed Title I status across years, or ~5% of all schools

data %<>% 
  filter(!school %in% title_change)

# calculate weighted average (by students) across 2014-19

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

data %<>% 
  group_by(lea_id, lea_code, school_number, school, title_class) %>% 
  mutate(Students_agg = mean(Students),
         lea_type_agg = getmode(lea_type),
         class_average_agg = sum(Students * class_average) / sum(Students),
         ell_status_agg = sum(Students * ell_status) / sum(Students),
         special_ed_agg = sum(Students * special_ed) / sum(Students)) %>% 
  mutate(Base_School_Average_agg = sum(Students * Base_School_Average) / sum(Students),
         School_Average_agg = sum(Students * School_Average) / sum(Students),
         
         base_instr_comp_agg = sum(Students * base_instr_comp) / sum(Students),
         instr_comp_agg = sum(Students * instr_comp) / sum(Students),
         
         base_instr_rel_agg = sum(Students * base_instr_rel) / sum(Students),
         instr_rel_agg = sum(Students * instr_rel) / sum(Students),
         
         base_admin_agg = sum(Students * base_admin) / sum(Students),
         admin_agg = sum(Students * admin) / sum(Students)) %>% 
  ungroup()

data %<>% 
  select(lea_id, lea_code, school_number, school, title_class, ends_with("_agg")) %>% 
  distinct()

data %<>% 
  rename_with(~str_remove(., '_agg'))
  
# quick check: % of Title I schools within districts
data %>% 
  group_by(lea_code) %>% 
  mutate(t1 = ifelse(title_class == "SW", 1, 0)) %>% 
  summarize(perc_t1 = mean(t1)) %>% 
  ggplot(aes(x = perc_t1)) + geom_histogram()


#### functions -----------------------------------------------------------------

# get Title I estimate
get_est <- function(model){
  if(class(model) == "lmerModLmerTest") {
    round((exp(fixef(model)[2]) - 1), 3)
  } else {
    round((exp(coef(model)["title_classSW"]) - 1), 3)
  }
}

# get effect sizes using partial eta squared method
get_effect <- function(model){
  if(class(model) == "lmerModLmerTest") { # need to pull diff column if lmer object
    round(summary(model)$coefficients[, 4]^2/(summary(model)$coefficients[, 4]^2 + df.residual(model)), 3)
  } else {
    round(summary(model)$coefficients[, 3]^2/(summary(model)$coefficients[, 3]^2 + df.residual(model)), 3)
  }
}

# get non-Title I spending estimate for average student
non_est <- function(model){
  if(class(model) == "lmerModLmerTest") { coefs <- fixef(model) } 
  else { coefs <- coef(model) }
  
  non_est <- exp(coefs[1] +                                         # intercept
                   coefs[2] * 0 +                                     # title class == SW
                   coefs[3] * mean(data$Students) +               # students in school
                   coefs[4] * 1 +                                     # LEA type: urban vs. rural
                   coefs[5] * mean(data$special_ed) +            # special education
                   coefs[6] * mean(data$ell_status)              # ell
  )
  
  return(round(non_est))
}

# get SW Title I spending estimate for average student
sw_est <- function(model){
  if(class(model) == "lmerModLmerTest") { coefs <- fixef(model) } 
  else { coefs <- coef(model) }
  
  sw_est <- exp(coefs[1] +                                         # intercept
                  coefs[2] * 1 +                                     # title class == SW
                  coefs[3] * mean(data$Students) +               # students in school
                  coefs[4] * 1 +                                     # LEA type: urban vs. rural
                  coefs[5] * mean(data$special_ed) +            # special education
                  coefs[6] * mean(data$ell_status)              # ell
  )
  
  return(round(sw_est))
}


# get dollar amount difference between Title I and non-Title I students
dollar_diff <- function(model){
  sw_est(model) - non_est(model)
}

# format p-values
get_stars <- function(model){
  model <- summary(model)
  stars <- case_when(
    round(model$coefficients[,4], 4) < 0.001 ~ "***",
    round(model$coefficients[,4], 4) < 0.01 ~ "**",
    round(model$coefficients[,4], 4) < 0.05 ~ "*",
    round(model$coefficients[,4], 4) < 0.1 ~ ".",
    TRUE ~ ""
  )
  paste0(ifelse(as.character(round(model$coefficients[,4], 3)) == "0", "0.000", 
                as.character(round(model$coefficients[,4], 3))), stars)
}



#### preliminary modeling ------------------------------------------------------

## specify statewide model
state_lm <- lm(log(Base_School_Average) ~ title_class + Students + lea_type + ell_status + special_ed, 
               data = data)

round((exp(coef(state_lm)["title_classSW"]) - 1), 3) * 100 # -8.1% - ***
summary(state_lm)

## specify within-district model
state_lmer <- lmer(log(Base_School_Average) ~ title_class + Students + lea_type + ell_status + special_ed +
                     (1 | lea_code), data = data)

round((exp(fixef(state_lmer)[2]) - 1), 3) * 100 # +1.2% - insignificant (alpha = 0.10)
summary(state_lmer)


# compare new model estimates (are they still flipping?)
tidy(state_lm)

state_lm %>% 
  tidy() %>% 
  mutate(eff_size = get_effect(state_lm)) %>% 
  select(term, estimate, p.value, eff_size)

state_lmer %>% 
  tidy() %>% 
  filter(effect == "fixed") %>% 
  mutate(eff_size = get_effect(state_lmer)) %>% 
  select(term, estimate, p.value, eff_size)

# signs are no longer flipping between models! (so this is an improvement)
# however, the effect sizes of ell and sped are still changing drastically




#### STATEWIDE LINEAR MODELS -----------------------------------------------------

#### total spending model -------------------------------------------------------

spending_before_lm <- lm(log(Base_School_Average) ~ title_class + Students + lea_type + special_ed + ell_status, 
                         data = data)

spending_after_lm <- lm(log(School_Average) ~ title_class + Students + lea_type + special_ed + ell_status, 
                         data = data)

coefs <- names(coef(spending_before_lm)) # store for tables


## get estimates and effect sizes for regression predictors table

tibble( # before
  coefs = coefs,
  bf_est = round((exp(coef(spending_before_lm)) - 1), 3),
  bf_stars = get_stars(spending_before_lm),
  bf_eff = get_effect(spending_before_lm)
)

# get R2
summary(spending_before_lm)$r.squared

tibble( # after
  coefs = coefs,
  af_est = round((exp(coef(spending_after_lm)) - 1), 3),
  af_stars = get_stars(spending_after_lm),
  af_eff = get_effect(spending_after_lm)
  
)

# get R2
summary(spending_after_lm)$r.squared

non_est(spending_before_lm)

#### spending by category models -----------------------------------------------

# teacher comp models
comp_before_lm <- lm(log(base_instr_comp) ~ title_class + Students + lea_type + special_ed + ell_status, data = data)
comp_after_lm <- lm(log(instr_comp) ~ title_class + Students + lea_type + special_ed + ell_status, data = data)

# instr related models
rel_before_lm <- lm(log(base_instr_rel) ~ title_class + Students + lea_type + special_ed + ell_status, data = data)
rel_after_lm <- lm(log(instr_rel) ~ title_class + Students + lea_type + special_ed + ell_status, data = data)

# admin spending models
admin_before_lm <- lm(log(base_admin) ~ title_class + Students + lea_type + special_ed + ell_status, data = data)
admin_after_lm <- lm(log(admin) ~ title_class + Students + lea_type + special_ed + ell_status, data = data)

# tidy(comp_before_lm)
# tidy(comp_after_lm)


## get estimate, confidence interval, amount, and difference for summary table

# rotate through models pulling relevant #s
model <- admin_after_lm

round((exp(coef(model)[2]) - 1), 3)
get_stars(model)[2]
round(exp(confint(model))[2,] - 1, 3)

sw_est(model)
dollar_diff(model)

rm(spending_before_lm, spending_after_lm, comp_before_lm, comp_after_lm,
   rel_before_lm, rel_after_lm, admin_before_lm, admin_after_lm)


#### WITHIN-DISTRICT HIERARCHICAL MODELS ---------------------------------------

#### total spending model -------------------------------------------------------

spending_before_lmer <- lmer(log(Base_School_Average) ~ title_class + Students + lea_type + special_ed + ell_status +
                               (1 | lea_code), data = data)

spending_after_lmer <- lmer(log(School_Average) ~ title_class + Students + lea_type + special_ed + ell_status +
                               (1 | lea_code), data = data)

## get estimates and effect sizes for regression predictors table

tibble( # before
  coefs = coefs,
  bf_est = round(exp(fixef(spending_before_lmer)) - 1, 3),
  # bf_stars = get_stars(spending_before_lmer),
  bf_eff = get_effect(spending_before_lmer)
)

# get variance components and diagnostics
summary(spending_before_lmer) # and p-values
r.squaredGLMM(spending_before_lmer)

tibble( # after
  coefs = coefs,
  af_est = round((exp(fixef(spending_after_lmer)) - 1), 3),
  # af_stars = get_stars(spending_after_lmer),
  af_eff = get_effect(spending_after_lmer)
)

# get variance components and diagnostics
summary(spending_after_lmer)
r.squaredGLMM(spending_after_lmer)



#### spending by category models -----------------------------------------------

# teacher comp models
comp_before_lmer <- lmer(log(base_instr_comp) ~ title_class + Students + lea_type + special_ed + ell_status + 
                         (1 | lea_code), data = data)
comp_after_lmer <- lmer(log(instr_comp) ~ title_class + Students + lea_type + special_ed + ell_status + 
                      (1 | lea_code), data = data)

# instr related models
rel_before_lmer <- lmer(log(base_instr_rel) ~ title_class + Students + lea_type + special_ed + ell_status + 
                      (1 | lea_code), data = data)
rel_after_lmer <- lmer(log(instr_rel) ~ title_class + Students + lea_type + special_ed + ell_status + 
                     (1 | lea_code), data = data)

# admin spending models
admin_before_lmer <- lmer(log(base_admin) ~ title_class + Students + lea_type + special_ed + ell_status + 
                        (1 | lea_code), data = data)
admin_after_lmer <- lmer(log(admin) ~ title_class + Students + lea_type + special_ed + ell_status + 
                       (1 | lea_code), data = data)


## get estimate, confidence interval, amount, and difference for summary table

# rotate through models pulling relevant #s
model <- admin_after_lmer

round((exp(fixef(model)[2]) - 1), 3)
round(exp(confint(model))[4,] - 1, 3)
summary(model) # get stars

sw_est(model)
dollar_diff(model)

rm(spending_before_lmer, spending_after_lmer, comp_before_lmer, comp_after_lmer,
   rel_before_lmer, rel_after_lmer, admin_before_lmer, admin_after_lmer)




#### DESCRIPTIVE DIFFERENCES --------------------------------------------------
# between Title I and non-Title I schools to justify modeling strategy

get_stars_desc <- function(ttest){
  pval <- ttest$p.value
  stars <- case_when(
    round(pval, 4) < 0.001 ~ "***",
    round(pval, 4) < 0.01 ~ "**",
    round(pval, 4) < 0.05 ~ "*",
    round(pval, 4) < 0.1 ~ ".",
    TRUE ~ ""
  )
}


## characteristics
tibble( # enrollment
  characteristic = paste0("Enrollment", get_stars_desc(t.test(data$Students[data$title_class == "Non"], data$Students[data$title_class == "SW"]))),
  sw_est = paste0(round(mean(data$Students[data$title_class == "SW"])), " ± ", round(sd(data$Students[data$title_class == "SW"]))),
  non_est = paste0(round(mean(data$Students[data$title_class == "Non"])), " ± ", round(sd(data$Students[data$title_class == "Non"])))
)

urban_data <- data %>% 
  mutate(lea_type = ifelse(lea_type == "Urban", 1, 0))

tibble( # lea type
  characteristic = paste0("LEA Type", get_stars_desc(t.test(urban_data$lea_type[urban_data$title_class == "Non"], urban_data$lea_type[urban_data$title_class == "SW"]))),
  sw_est = paste0(mean(urban_data$lea_type[urban_data$title_class == "SW"]), " ± ", sd(urban_data$lea_type[urban_data$title_class == "SW"])),
  non_est = paste0(mean(urban_data$lea_type[urban_data$title_class == "Non"]), " ± ", sd(urban_data$lea_type[urban_data$title_class == "Non"]))
)

tibble( # special education
  characteristic = paste0("Special Education", get_stars_desc(t.test(data$special_ed[data$title_class == "Non"], data$special_ed[data$title_class == "SW"]))),
  sw_est = paste0(mean(data$special_ed[data$title_class == "SW"]), " ± ", sd(data$special_ed[data$title_class == "SW"])),
  non_est = paste0(mean(data$special_ed[data$title_class == "Non"]), " ± ", sd(data$special_ed[data$title_class == "Non"]))
)

tibble( # english learner
  characteristic = paste0("English Learner", get_stars_desc(t.test(data$ell_status[data$title_class == "Non"], data$ell_status[data$title_class == "SW"]))),
  sw_est = paste0(mean(data$ell_status[data$title_class == "SW"]), " ± ", sd(data$ell_status[data$title_class == "SW"])),
  non_est = paste0(mean(data$ell_status[data$title_class == "Non"]), " ± ", sd(data$ell_status[data$title_class == "Non"]))
)

## spending
tibble( # overall spending
  characteristic = paste0("Overall Spending sans Title I", get_stars_desc(t.test(data$Base_School_Average[data$title_class == "Non"], data$Base_School_Average[data$title_class == "SW"]))),
  sw_est = paste0(mean(data$Base_School_Average[data$title_class == "SW"]), " ± ", sd(data$Base_School_Average[data$title_class == "SW"])),
  non_est = paste0(mean(data$Base_School_Average[data$title_class == "Non"]), " ± ", sd(data$Base_School_Average[data$title_class == "Non"]))
)

tibble( # instructional compensation
  characteristic = paste0("Instructional Comp sans Title I", get_stars_desc(t.test(data$base_instr_comp[data$title_class == "Non"], data$base_instr_comp[data$title_class == "SW"]))),
  sw_est = paste0(mean(data$base_instr_comp[data$title_class == "SW"]), " ± ", sd(data$base_instr_comp[data$title_class == "SW"])),
  non_est = paste0(mean(data$base_instr_comp[data$title_class == "Non"]), " ± ", sd(data$base_instr_comp[data$title_class == "Non"]))
)

tibble( # instructional related
  characteristic = paste0("Instructional Rel sans Title I", get_stars_desc(t.test(data$base_instr_rel[data$title_class == "Non"], data$base_instr_rel[data$title_class == "SW"]))),
  sw_est = paste0(mean(data$base_instr_rel[data$title_class == "SW"]), " ± ", sd(data$base_instr_rel[data$title_class == "SW"])),
  non_est = paste0(mean(data$base_instr_rel[data$title_class == "Non"]), " ± ", sd(data$base_instr_rel[data$title_class == "Non"]))
)

tibble( # admin
  characteristic = paste0("Admin sans Title I", get_stars_desc(t.test(data$base_admin[data$title_class == "Non"], data$base_admin[data$title_class == "SW"]))),
  sw_est = paste0(mean(data$base_admin[data$title_class == "SW"]), " ± ", sd(data$base_admin[data$title_class == "SW"])),
  non_est = paste0(mean(data$base_admin[data$title_class == "Non"]), " ± ", sd(data$base_admin[data$title_class == "Non"]))
)

