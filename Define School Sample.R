library(tidyverse)
library(magrittr)
library(readxl)

all_data <- readRDS("H:/Economists/Ed/Analytics Projects/Title I Analysis/Data/all_data.RDS")

## create a data set with all K-12 schools
reg_data <- all_data[!is.na(all_data$School_Average) & !is.na(all_data$race_white) & !is.na(all_data$class_average),]

reg_data %<>% 
  rename(instr_comp = `Instructional Compensation`, instr_rel = `Instructional Related, Student Support, and Extra Curricular`,
         admin = `Administrative Expenses`, ops = `Operations, Facilities, and Construction`, trans = `Transportation Services`,
         nutr = `Nutrition Services`, dep = `Debt and Depreciation`)

reg_data %<>% filter(!school %in% c("Serv By Appt", "Provo Adult Education")) # remove non-k12 schools from data set


#### remove online schools -------------------------------------------------------

## create a data set excluding online schools

online <- read_csv("H:/Economists/Ed/KIDS Public Facing Dashboard Projects/Data/Spatial/Spatial for Map Final.csv")
online %<>% dplyr::select(LEAID, SchoolNumber, OnlineSchool) %>% mutate(SchoolNumber = as.character(SchoolNumber))
reg_data %<>% left_join(online, by = c("lea_id" = "LEAID", "school_number" = "SchoolNumber"))

brick_data <- reg_data %>% filter(is.na(OnlineSchool)) # remove online schools

brick_data %<>% # remove additional online schools not flagged in USBE's data
  filter(!(school == "Blue Peak High" | (lea_code == "MANA" & school_year == 2014)))


## verify that data set without online schools is most appropriate for spending by cat analyses

# analyze whether online schools can be left in this analysis
online_data <- reg_data %>%
  filter((str_detect(school, "eSchool|Online|Virtual") |
            school %in% c("Mountain Heights Academy", "Utah Connections Academy", "Blue Peak High", "Davis Connect K-6", 
                          "Wasatch Learning Academy") | (lea_code == "MANA" & school_year == 2014))) %>%
  mutate(instr = instr_comp + instr_rel) %>% 
  dplyr::select(school, school_year, instr_comp, instr_rel, instr, admin, ops) # %>% View()

summary(online_data)

# graph impact
reg_data %>%
  mutate(online = ifelse(str_detect(school, "eSchool|Online|Virtual") |
            school %in% c("Mountain Heights Academy", "Utah Connections Academy", "Blue Peak High", "Davis Connect K-6", 
                          "Wasatch Learning Academy") | (lea_code == "MANA" & school_year == 2014), "online", "brick")) %>% 
  ggplot(aes(x = Students, y = log(School_Average), color = online)) + geom_point() + 
  geom_smooth(method = "lm", se  = F)

# test whether means are significantly different
t.test(online_data$instr_comp, brick_data$instr_comp) # significantly diff, p = 0.00
t.test(online_data$instr_rel, brick_data$instr_rel) # not significantly diff, p = 0.27
t.test(online_data$admin, brick_data$admin) # not significantly diff, p = 0.13
t.test(online_data$ops, brick_data$ops) # significantly diff, p = 0.00

## conclusion:
# online schools should definitely be removed for testing differences in instructional compensation and operations, since they
# are structurally different in these respects and their mean differences are significant. likewise, for theoretical reasons, I think
# they should be removed from the other spending cat analyses too. we know that instructional related costs look different at most
# onlines schools (teacher pay) and the administration structure may look different too. remove online schools for all analyses.
# plus, the data for online schools is generally less reliable than for brick & mortar schools. some totals are unbelievably low.

rm(online, online_data)



#### remove charter schools ------------------------------------------------------

charter_data <- brick_data %>% filter(charterschool == "Charter")
district_data <- brick_data %>% filter(charterschool == "District")

# test whether means are significantly different
t.test(district_data$School_Average, charter_data$School_Average) # significantly diff; charter lowers
t.test(district_data$instr_comp, charter_data$instr_comp) # significantly diff; charter lower
t.test(district_data$instr_rel, charter_data$instr_rel) # significantly diff; charter lower
t.test(district_data$admin, charter_data$admin) # significantly diff; charter higher!
t.test(district_data$ops, charter_data$ops) # significantly diff; district higher

# all means are significantly different, mostly in favor of charters being less expensive to operate

# visualize these differences - charters definitely on the lower end of spending distribution
brick_data %>%
  ggplot(aes(x = log(School_Average), fill = charterschool)) +
  geom_histogram(position = "dodge")

brick_data %>%
  ggplot(aes(x = Students, y = log(School_Average), color = charterschool)) +
  geom_point() + geom_smooth(method = "lm", se = F)


# finally, try running models and see how including/excluding charters changes estimates
spending_reg <- lm(log(School_Average) ~ title_class + Students + log(class_average) + school_year + lea_type + charterschool + 
                     homeless + special_ed + migrant_status + chronically_absent + ell_status, data = brick_data)

spending_reg_charter <- lm(log(School_Average) ~ title_class + Students + log(class_average) + school_year + lea_type + 
                     homeless + special_ed + migrant_status + chronically_absent + ell_status, 
                     data = brick_data %>% filter(charterschool == "Charter"))

spending_reg_district <- lm(log(School_Average) ~ title_class + Students + log(class_average) + school_year + lea_type + 
                             homeless + special_ed + migrant_status + chronically_absent + ell_status, 
                           data = brick_data %>% filter(charterschool == "District"))

summary(spending_reg)
summary(spending_reg_charter)
summary(spending_reg_district)
# actually SW estimate becomes more pronounced with removal of charters, TA insignificant

tibble(predictors = names(spending_reg$coefficients)[c(1:7, 9:13)], 
       before = spending_reg$coefficients[c(1:7, 9:13)], 
       after = spending_reg_district$coefficients)

## conclusion: remove charter schools from the analysis on the basis of these schools being structurally different than district
# schools. specifically, their costs are much lower, and controlling for the charterschool variable alone doesn't entirely
# mitigate this difference, as evidenced by the changing model estimates above.

rm(spending_reg, spending_reg_charter, spending_reg_district, charter_data)




#### remove schools with < 100 students ------------------------------------------

## found that removing <100 students is a bit heavy-handed, but we have no choice if we're using the UPEFS data.
# districts are not required to report Title I spending at schools with <100 students to USBE, so we cannot use these 
# small schools in our analysis given our existing methodology. we have no choice but to throw them out.
# (about 5% of data after removing charters and alternative schools)

# visualize this impact on spending
district_data %>% 
  # filter(school_type != "Alternative School") %>% 
  ggplot(aes(x = Students, y = log(School_Average), color = "blue")) +
  geom_point() + # geom_smooth(method = "lm") +
  geom_vline(aes(xintercept = 100, color = "red"))
# < 100 students in the proposed cutoff for excluding schools from the analysis (originating from Title I reporting guidelines)
# this looks like it will take care of most of the really high spenders

# district_data %>% 
#   filter(school_type != "Alternative School" & Students <= 100) %>% 
#   ggplot(aes(x = Students, y = log(School_Average))) +
#   geom_point()

district_data %>% 
  filter(Students > 100) %>% 
  ggplot(aes(x = Students, y = log(School_Average))) +
  geom_point() + geom_smooth(method = "lm")
# there are still a handful of high spenders.. who are these schools?

district_data %>% 
  filter(Students > 100 & log(School_Average) > 10.5) # %>% View()
# these are all alternative schools - these will be taken care of in the next section

# compare histograms before and after
hist(log(district_data$School_Average))
hist(log(district_data$School_Average[district_data$Students > 100]))
# this helps normalize the spending distribution a ton

district_data %>% 
  filter(Students > 100) %>% 
  ggplot(aes(x = log(School_Average))) +
  geom_histogram()


## test how this changes model estimates
spending_reg <- lm(log(School_Average) ~ title_class + Students + log(class_average) + school_year + lea_type + 
                     homeless + special_ed + migrant_status + chronically_absent + ell_status, data = district_data)

spending_reg_100 <- lm(log(School_Average) ~ title_class + Students + log(class_average) + school_year + lea_type + 
                             homeless + special_ed + migrant_status + chronically_absent + ell_status, 
                           data = district_data %>% filter(Students > 100))

summary(spending_reg)
summary(spending_reg_100)
# R2 does decrease with smaller model

tibble(predictors = names(spending_reg$coefficients), 
       before = spending_reg$coefficients, 
       after = spending_reg_100$coefficients)

# and check how this changes plots
plot(spending_reg, which = 1)
plot(spending_reg_100, which = 1) # much better

plot(spending_reg, which = 2)
plot(spending_reg_100, which = 2) # also a huge improvement


## conclusion:
# removing schools with less than 100 students significantly improves model diagnostics. the response variable becomes much more
# normally distributed, and so the qqplot looks much better, as does the residuals vs fitted plot. the R2 of the model does decrease,
# but I think this is well worth the improved model diagnostics. this also pronounces the SW estimate results, and TA still insig.
# the theoretical justification is that districts don't have to report spending at Title I schools with fewer than 100 students

big_data <- district_data %>% filter(Students > 100)

# big_data <- district_data # for skipping students < 100


#### remove alternative schools --------------------------------------------------

big_data %>% 
  mutate(alt = ifelse(school_type == "Alternative School", "alternative", "traditional")) %>% 
  ggplot(aes(x = Students, y = log(School_Average), color = alt)) +
  geom_point()
# this covers pretty much all of the remaining high-spenders

# big_data %>% 
#   ggplot(aes(x = Students, y = log(School_Average), color = title_class)) +
#   geom_point()
# note that these schools are almost all non-Title I, so the SW estimate will likely weaken


### verify listed alternative schools

big_data %>% 
  filter(school_type == "Alternative School") %>% 
  distinct(school) %>% arrange(school) # %>% View()
# manually verified that all of these can be classified as alternative for some reason or another


### check for additional alternative schools

# comb through other schools and pick out additional alternative schools (by school name)
big_data %>% 
  filter(school_type != "Alternative School") %>% 
  filter(str_detect(school, "Voc|Youth"))

big_data %>% 
  filter(school_type != "Alternative School") %>% 
  distinct(school) # %>% View()

# check once small schools are not removed
# big_data %>% 
#   filter(school_type != "Alternative School") %>% 
#   filter(Students <= 100) %>% 
#   distinct(school) %>% View() # these all sound normal

addtl_alt <- c("Granite Technical Institute", "Nebo Advanced Learning Center", "Ashley Valley Educ. Ctr.", 
               "Tooele Community Learning Center","Granite Connection High", "Salt Lake Technology Center", 
               "Weber Innovation High School")

# check where these schools lie in the distribution
# big_data %>% 
#   mutate(alt = ifelse(school %in% addtl_alt, "alternative", "traditional")) %>% 
#   ggplot(aes(x = Students, y = log(School_Average), color = alt)) +
#   geom_point()
# meh not too informative, but they do tend to be smaller schools



### check for other schools with high percentages of specific student groups
# check for missing alternative labels
big_data %>% 
  filter(school_type != "Alternative School") %>% 
  arrange(desc(special_ed)) # %>% select(special_ed)
# max of 38% of student body special ed is reasonable, but double check any ways

# SJSD really high on both of these 
big_data %>% 
  filter(school_type != "Alternative School") %>% 
  arrange(desc(ell_status)) # %>% select(ell_status)

big_data %>% 
  filter(school_type != "Alternative School") %>% 
  arrange(desc(homeless)) # %>% select(homeless)


# get data with traditional schools only
trad_data <- big_data %>% filter(school_type != "Alternative School" & !school %in% addtl_alt)

# this removes the remainder of the high-spenders
trad_data %>% 
  ggplot(aes(x = Students, y = log(School_Average))) +
  geom_point() + geom_smooth(method = "lm")


# check high-spenders once schools < 100 students aren't removed
trad_data %>% filter(log(School_Average) > 10) %>% select(school, Students) # %>% View() # these are all the really small schools


### compare models before and after removal of alt schools

spending_reg <- lm(log(School_Average) ~ title_class + Students + log(class_average) + school_year + lea_type + 
                     homeless + special_ed + migrant_status + chronically_absent + ell_status, data = big_data)

spending_reg_trad <- lm(log(School_Average) ~ title_class + Students + log(class_average) + school_year + lea_type + 
                         homeless + special_ed + migrant_status + chronically_absent + ell_status, 
                       data = trad_data)

summary(spending_reg)
summary(spending_reg_trad)
# R2 does decrease again with smaller model

tibble(predictors = names(spending_reg$coefficients), 
       before = spending_reg$coefficients, 
       after = spending_reg_trad$coefficients)
# drastically changes sped coefficient! and weakens SW estimate

# and check how this changes plots
plot(spending_reg, which = 1)
plot(spending_reg_trad, which = 1) # much better

plot(spending_reg, which = 2)
plot(spending_reg_trad, which = 2) # also an improvement
# there is still a right tail to the spending distribution, but looking better than before

hist(log(trad_data$School_Average))

## conclusion:
# removing alternative schools significantly normalizes the spending distribution. I verified all schools marked as alternative and
# manually looked through other schools to mark them too. there is justification in just looking at "traditional" schools, because
# alternative schools can be structurally different. plus, demographics tend to be more concentrated at alt schools, which makes
# modeling these schools practically difficult. special ed estimates are significantly different after removing alt schools, for example.




#### exploratory analysis of demographic variables ------------------------------

# summary(spending_reg) # get list of demographic variables from original model

## compare histograms before and after school exclusions
# use dataset w online schools removed as baseline
# hist(brick_data$Students)
# hist(brick_data$class_average)
# hist(brick_data$homeless)
# hist(brick_data$special_ed)
# hist(brick_data$migrant_status)
# hist(brick_data$chronically_absent)
# hist(brick_data$ell_status)

# compare to new dataset with alternative schools, charter schools, and schools < 100 students removed
hist(trad_data$Students) # no real diff
hist(trad_data$class_average) # normalizes a bit - removes smaller class sizes
hist(trad_data$homeless) # no diff
hist(trad_data$special_ed) # normalizes significantly - removes high outliers
hist(trad_data$migrant_status) # no diff
hist(trad_data$chronically_absent) # no diff
hist(trad_data$ell_status) # no diff
# helps normalize the class_average and special_ed variables significantly. otherwise, no major differences in dem distr.


## check distributions by Title I status

trad_data %>% 
  ggplot(aes(x = Students, y = log(School_Average), color = title_class)) +
  geom_point()
# Non-Title I schools tend to be larger, with a few exceptions

trad_data %>% 
  ggplot(aes(x = class_average, y = log(School_Average), color = title_class)) +
  geom_point()
# some outliers.... seem to be an even mix though

trad_data %>% 
  ggplot(aes(x = homeless, y = log(School_Average), color = title_class)) +
  geom_point()
# definite clustering by title I classification - be careful about how to handle this variable

trad_data %>% 
  ggplot(aes(x = special_ed, y = log(School_Average), color = title_class)) +
  geom_point()
# SW tends to have slightly higher proportion of sped students

trad_data %>% 
  ggplot(aes(x = migrant_status, y = log(School_Average), color = title_class)) +
  geom_point()
# be careful with this variable too - some extreme outliers that are SW

trad_data %>% 
  ggplot(aes(x = chronically_absent, y = log(School_Average), color = title_class)) +
  geom_point()
# average is definitely much higher by Title I status

trad_data %>% 
  ggplot(aes(x = ell_status, y = log(School_Average), color = title_class)) +
  geom_point()
# average is definitely much higher by Title I status



### check how SJSD looks - are they always outliers?

# higher end of spending distribution / fewer students
trad_data %>% 
  mutate(sjsd = ifelse(lea_code == "SJSD", "San Juan", "Other")) %>% 
  ggplot(aes(x = Students, y = log(School_Average), color = sjsd)) +
  geom_point()

# cycle through all dem variables above and check
trad_data %>% 
  mutate(sjsd = ifelse(lea_code == "SJSD", "San Juan", "Other")) %>% 
  ggplot(aes(x = homeless, y = log(School_Average), color = sjsd)) +
  geom_point()
# class_average: low end of class size distr
# homeless: extreme outliers**
# special ed: normal
# migrant: normal
# chronically absent: pretty normal, maybe a little high
# ell status: also high end of distribution here*

# initial thoughts: the homeless variable almost proxies for SJSD. try removing this variable in the analysis?

# try removing SJSD from analysis
spending_reg <- lm(log(School_Average) ~ title_class + Students + log(class_average) + school_year + lea_type + 
                     homeless + special_ed + migrant_status + chronically_absent + ell_status, data = trad_data)

spending_reg_sjsd <- lm(log(School_Average) ~ title_class + Students + log(class_average) + school_year + lea_type + 
                     homeless + special_ed + migrant_status + chronically_absent + ell_status, 
                   data = trad_data %>% filter(lea_code != "SJSD"))

spending_reg_home <- lm(log(School_Average) ~ title_class + Students + log(class_average) + school_year + lea_type + 
                     special_ed + migrant_status + chronically_absent + ell_status, data = trad_data)

summary(spending_reg)
summary(spending_reg_sjsd)
summary(spending_reg_home)

tibble(predictors = names(spending_reg$coefficients), 
       before = spending_reg$coefficients, 
       after = spending_reg_sjsd$coefficients)
# removing SJSD has the following effects:
# (1) SW estimate -4% -> -5%
# (2) homeless estimate sig increases

# something to play around with in the modeling phase, I suppose...
# but I think San Juan is important to talk about. throwing out an entire district is a bit more difficult to justify than
# removing specialty schools across the state.


#### export final data set ------------------------------------------------------

# final data set removes charter schools, alternative schools, and schools < 100 students
# saveRDS(trad_data, "H:/Economists/Ed/Analytics Projects/Title I Analysis/2nd Release/Data/school_data_full_vars.rds")

# just keep non-testing variables for this analysis and organize by helpfulness
trad_red <- trad_data %>% 
  select(-c(lp_yn:aca_isreadinggradelevel_eoy)) %>% 
  select(school_year:school, title_class, School_Median:School_Average, Students, class_average, school_type, lea_type,
         ell_status:chronically_absent, race_american_indian:race_asian, instr_comp:dep, degree:total_comp, lea_name)

# save reduced data set
# saveRDS(trad_red, "H:/Economists/Ed/Analytics Projects/Title I Analysis/2nd Release/Data/school_data.rds")


# quick check - what % of remaining schools are TAP?
trad_red %>% 
  group_by(title_class) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(perc = n / sum(n)) # 2.5% of all schools

trad_red %>% 
  filter(title_class != "Non") %>% 
  group_by(title_class) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(perc = n / sum(n)) # about 8% of Title I schools
# I think this is consistent with national results


#### calculate percentage of observations removed by exclusion decision

## baseline - brick_data
brick_data %>% distinct(lea_code, school_number, school) %>% count() # 1010 distinct schools
brick_data %>% count() # 6350 observations

## after charter school removal
district_data %>% distinct(lea_code, school_number, school) %>% count() # 880 distinct schools
district_data %>% count() # 5557 observations

## after schools < 100 removal
big_data %>% distinct(lea_code, school_number, school) %>% count() # 828 distinct schools
big_data %>% count() # 5208 observations

## after alternative schools removal
trad_data %>% distinct(lea_code, school_number, school) %>% count() # 798 distinct schools
trad_data %>% count() # 5045 observations

## overall decrease in data
(1010 - 798) / 1010 # 21% of schools removed
(6350 - 5045) / 6350 # and 21% of school observations

# the largest drop by far was the removal of charter schools
1010 - 880 # lost from charters
880 - 828 # lost from small schools
828 - 798 # lost from alt schools
# note that there definitely is crossover between these categories, so these numbers aren't entirely accurate

# but overall: 20% of observations were removed to preserve traditional schools
# still have data on about 800 distinct Utah schools


## graph before and after removals

brick_data %>% 
  mutate(exclusions = ifelse(charterschool == "Charter" | Students <=  100 | 
                            (school_type == "Alternative School" | school %in% addtl_alt), 
                          "exc", "inc")) %>% 
  ggplot(aes(x = Students, y = log(School_Average), color = exclusions)) +
  geom_point() + geom_smooth(method = "lm", se = F)

  