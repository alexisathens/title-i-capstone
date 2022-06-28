library(tidyverse)
library(magrittr)
library(readxl)


#### VERIFY DATA QUALITY -------------------------------------------------------

all <- readRDS("H:/Economists/Ed/Analytics Projects/Title I Analysis/2nd Release/Data/Raw Data/Title I Expenses V3.rds")

# remove charter schools
all %<>% filter(str_detect(lea_code, "SD") | lea_code %in% c("ALPS", "NSUS", "TINT"))

# check what district is missing
all %>% distinct(lea_code) %>% count()
present_ids <- all %>% distinct(lea_id) %>% pull(lea_id)

district_ids <- read_csv("H:/Economists/Ed/KIDS/All LEAs/District IDs Master.csv")
district_ids %>% filter(!district_id %in% present_ids) # missing Daggett

# double check Daggett doesn't have Title I schools
school <- readRDS("H:/Economists/Ed/Analytics Projects/Title I Analysis/2nd Release/Data/school_data.rds")
school %>% filter(lea_code == "DGSD") # confirmed from PF dashboard there are no Title I schools


# check totals by year now
all %>% 
  group_by(fiscal_year) %>% 
  summarize(total = sum(total))
# compare $71mil to USBE's $68mil! so much closer!
71/68 # 4% extra


# check how these funds are classified
all %>% 
  group_by(subcode) %>% 
  summarize(total = sum(total)) %>% 
  arrange(desc(total)) %>% 
  ungroup() %>% 
  mutate(perc = total / sum(total) * 100) # about 3.5% of funds in D50, 2.5% in X99
# it looks like a lot of the D50 expenditures are for large tech investments (e.g., payments to Apple)
# it's too bad this is removed from per-student spending estimates, but we'll just have to remove D50 and X99 for consistency

# all %<>% filter(!subcode %in% c("D50", "X99")) # remove these later after comparing across districts

all %>% 
  filter(!subcode %in% c("D50", "X99")) %>% 
  group_by(fiscal_year) %>% 
  summarize(total = sum(total))
67/68 # 1% lower when excluding capital and nonk12 - really close




#### compare district-level totals to UPEFS data --------------------------------------

## get UPEFS data
up <- read_delim("H:/Economists/Ed/Analytics Projects/UPEFS data/UPEFS_expenses.txt", delim = "|")

up %>% filter(str_detect(ProgramCode, "^78")) %>% distinct(ProgramCode)

up %>% filter(ProgramCode %in% c(7801, 7511, 7662)) %>% distinct(ProgramCode) # all Title I program codes appear to be mapped to 7801

# program 7801 is ESEA Title I A
up %<>% filter(ProgramCode == 7801)

# remove charter schools
up %<>% filter(str_detect(LEA_name, "District"))


## now compare totals by year
up %>% 
  group_by(YearId) %>% 
  summarize(total = sum(Amount))
# total is $72mil, compared to USBE's prelim PDF doc stating $68mil
# so it seems like this data source slightly overestimates, and ours slightly underestimates
# but overall totals indicate that neither is more accurate than the other...


## next, compare district-level totals by data source
up_d <- up %>% 
  filter(YearId <= 2020) %>% 
  group_by(district_id, LEA_name, YearId) %>% 
  summarize(total = sum(Amount, na.rm = T)) %>% 
  ungroup()

kids_d <- all %>% 
  filter(fiscal_year <= 2020) %>% 
  group_by(lea_id, fiscal_year) %>% 
  summarize(total = sum(total, na.rm = T)) %>% 
  ungroup()

compare_d <- up_d %>% 
  left_join(kids_d, by = c("district_id" = "lea_id", "YearId" = "fiscal_year"), suffix = c(".up", ".kids"))

compare_d %<>% 
  mutate(diff = round(total.kids - total.up)) %>% 
  mutate(perc_diff = round((diff) / total.up * 100))

# remove for now years where PKIDS data is missing
compare_d %<>% 
  filter(!is.na(total.kids))

summary(compare_d$diff) # average difference by district: -13K!
summary(compare_d$perc_diff) # average percent difference by district: PKIDS data 3% lower

# look just at 2019
# compare_d %>% filter(YearId == 2019) %>% View()
# this doesn't look so good - it looks like we're undercounting pretty significantly in some districts

# look at 2020 to compare USBE's PDFs
# compare_d %>% filter(YearId == 2020) %>% View()
# yeah, we tend to be the ones undercounting

## check specific egregious districts to see if we can recover any additional program codes

problem_d <- compare_d %>% 
  group_by(LEA_name) %>% 
  filter(YearId <= 2019) %>% 
  summarize(avg = round(mean(perc_diff))) %>% 
  filter(abs(avg) > 10) %>% 
  ungroup()

# View(problem_d)




#### ALLOCATE DISTRICT COSTS TO SCHOOLS ----------------------------------------

#### compare school-level totals to UPEFS data --------------------------------------

### first verify that most PKIDS money is going to SW schools
kids_loc <- all %>%
  group_by(fiscal_year, lea_code, u_location) %>%
  summarize(total = sum(total)) %>%
  ungroup()

comp_kids <- school %>%
  select(school_year, lea_code, school_number, title_class) %>%
  left_join(kids_loc, by = c("school_year" = "fiscal_year", "lea_code", "school_number" = "u_location"))

comp_kids %>%
  mutate(funds = if_else(total > 0, 1, 0, 0)) %>%
  group_by(title_class) %>%
  summarize(avg = mean(funds))
# SW and TA look really good. Non-Title I schools have funding 14% of the time though

# check average amount
comp_kids %>%
  group_by(title_class) %>%
  summarize(avg = mean(total, na.rm = T))
# average of $12K of funds when included

comp_kids %>%
  group_by(title_class) %>%
  mutate(total = ifelse(is.na(total), 0, total)) %>%
  summarize(avg = mean(total, na.rm = T))
# or <$2K including Non 0s

# overall, this is not too bad



#### group by parent code --------------------------------------------------------

exp <- all %>% 
  group_by(lea_id, lea_code, fiscal_year, u_location, school, level, subcode) %>% 
  summarize(total = sum(total)) %>% 
  ungroup()


## group by pc

exp %<>% filter(!(lea_code == "WGSD" & subcode == "Z01")) # remove Title I preschool program

exp %<>% 
  mutate(pc = case_when(
    str_detect(subcode, "A|H|Q") ~ "admin",
    str_detect(subcode, "B|C|E|F|G|S|U|L") | (lea_code %in% c("CASD", "PCSD") & subcode == "Z02") | 
      (lea_code == "NESD" & subcode == "Z06") ~ "instr_rel",
    str_detect(subcode, "M|O") | (lea_code == "TOSD" & subcode == "Z02") ~ "ops",
    str_detect(subcode, "N") ~ "nutr",
    str_detect(subcode, "P") ~ "instr_comp",
    str_detect(subcode, "T") ~ "transp",
    str_detect(subcode, "X99|D50") | (lea_code == "DASD" & subcode == "Z08") | (lea_code == "WASD" & subcode == "Z01") |
      (lea_code == "SLSD" & subcode %in% c("Z01", "Z02", "Z03")) ~ "exc",
    TRUE ~ NA_character_
  ))

exp %>% filter(is.na(pc)) %>% distinct(lea_code, subcode)

exp %<>% mutate(total = ifelse(total < 0, 0, total))

exp %<>% 
  group_by(lea_id, lea_code, fiscal_year, u_location, school, level, pc) %>% 
  summarize(total = sum(total)) %>% 
  mutate(pc = paste0("t1_", pc)) %>% 
  ungroup() %>% 
  spread(pc, total, fill = 0)

exp %>% 
  group_by(lea_code, fiscal_year, u_location) %>% 
  count() %>% 
  ungroup() %>% 
  filter(n > 1)

# # remove duplicate in schools
exp %<>% filter(!(lea_code == "PCSD" & u_location == 500 & level == "District")) # district location same as elementary school
exp %<>% filter(!(lea_code == "PCSD" & u_location == 500 & school == "McPolin Elementary"))
exp %<>% filter(!(lea_code == "SJSD" & u_location == 706 & level == "District"))

# kids_loc %>% anti_join(exp) # check haven't lost locations

exp %<>%
  mutate(t1_total = t1_admin + t1_exc + t1_instr_comp + t1_instr_rel + t1_nutr + t1_ops + t1_transp) # remove dep

exp %<>% 
  filter(fiscal_year <= 2019)

  

#### allocate district costs to schools -----------------------------------------

exp %>% 
  group_by(level) %>% 
  summarize(total = sum(t1_total)) %>% 
  arrange(desc(total))


### get district-only expense totals

# get locations that aren't schools (using all school-level data from 1st release)
rel1 <- readRDS("H:/Economists/Ed/Analytics Projects/Title I Analysis/Data/all_data.rds")
rel1 %<>% filter(school_year <= 2019)

districts <- exp %>% 
  anti_join(rel1, by = c("lea_id", "lea_code", "fiscal_year" = "school_year", "u_location" = "school_number"))

districts %>% 
  group_by(level) %>% 
  summarize(total = sum(t1_total)) %>% 
  arrange(desc(total))

districts %>% 
  filter(!is.na(level) & level != "District") # %>% View()


# a lot of these observations are charters - remove them
districts %<>% 
  filter(str_detect(lea_code, "SD") | lea_code %in% c("ALPS", "NSUS", "TINT"))

# also remove alternative schools
districts %<>% 
  filter(level != "Alternative School" | is.na(level))

# just a small amount of money left at elementary and middle schools - remove this too
districts %<>% 
  filter(!str_detect(level, "Elem|Mid") | is.na(level))

districts %<>% 
  filter(!str_detect(level, "Charter|Preschool") | is.na(level))



## comb through the rest of the NA levels
districts %>% 
  filter(is.na(level)) # %>% View()
# I think it's best to classify these as district expenses as well. one location is actually a Title I district location

districts %<>% 
  mutate(level = ifelse(is.na(level), "District", level))

districts %<>% 
  group_by(lea_id, lea_code, fiscal_year) %>% 
  summarize_at(vars(t1_admin:t1_total), ~sum(.)) %>% 
  ungroup()

districts %<>% 
  pivot_longer(t1_admin:t1_total, names_to = "t1", values_to = "total") %>% 
  mutate(t1 = paste0("d_", t1)) %>% 
  spread(t1, total, fill = 0)

## quick check: total at district by year
districts %>% 
  group_by(fiscal_year) %>% 
  summarize(total = sum(d_t1_total))
21/69 # 30% at district in 2021
15/71 # 20% at district in 2019
# this is better data quality than UPEFS!



### calculate school costs based on how much Title I funding was assigned to schools

# start with just schools in final dataset

final_schools <- school %>% select(school_year:title_class, School_Average, Students)

final_schools %<>% 
  left_join(exp %>% select(-c(school, level)), 
            by = c("school_year" = "fiscal_year", "lea_id", "lea_code", "school_number" = "u_location"))

exp %>% 
  anti_join(final_schools,  by = c("fiscal_year" = "school_year", "lea_id", "lea_code", "u_location" = "school_number")) %>% 
  filter(level != "District") # %>% View()
# these are rightfully small / alternative schools - proceed

# final_schools %>% 
#   pivot_longer(t1_admin:t1_total, names_to = "t1", values_to = "total") %>% 
#   mutate(total = ifelse(is.na(total), 0, total)) %>% 
#   spread(t1, total, fill = 0)


## check spending across non/Title I schools
final_schools %>% 
  mutate(spending = if_else(t1_total > 0, 1, 0, 0)) %>%
  group_by(title_class) %>% 
  summarize(avg = mean(spending))
# this has changed since the original calculation... how? by not including 2020 (data quality improves drastically in later years)
# not perfect, (less perfect than the UPEFS data), but this seems believable


## remove non-Title I schools
final_schools %<>% 
  filter(title_class != "Non")
# assuming these are mis-classified...

## remove 2020 observations
final_schools %<>% 
  filter(school_year <= 2019)

# and remove any Title I schools without any expenses (nothing to subtract out)
final_schools %<>% 
  filter(!is.na(t1_total))
# random ones that don't match over - mostly GTSD, JUSD. poor data quality at these districts?


## join district costs
final_schools %<>% 
  left_join(districts, by = c("lea_id", "lea_code", "school_year" = "fiscal_year"))


# use the same methodology for allocating across schools as with UPEFS data (based on % of total Title I spending)
# base on proportion of overall Title I spending, rather than by category. by category spending is just too heinous and 
# seems too detailed for the data we're working with.

## calculate share of district's resources each school is using
final_schools %<>% 
  group_by(school_year, lea_id, lea_code) %>%
  mutate(lea_t1_total = sum(t1_total)) %>% # get total spending in schools per district
  mutate(lea_t1_perc = t1_total / lea_t1_total) %>%  # get percent of Title I funding at particular school
  ungroup() %>% select(-lea_t1_total)

## calculate spending by category totals based on this breakdown

# check for NA / 0 values
summary(final_schools)

final_schools %<>% 
  replace(is.na(.), 0)

final_schools %<>% 
  mutate(admin = t1_admin + d_t1_admin * lea_t1_perc) %>% 
  mutate(exc = t1_exc + d_t1_exc * lea_t1_perc) %>% 
  mutate(instr_comp = t1_instr_comp + d_t1_instr_comp * lea_t1_perc) %>% 
  mutate(instr_rel = t1_instr_rel + d_t1_instr_rel * lea_t1_perc) %>% 
  mutate(nutr = t1_nutr + d_t1_nutr * lea_t1_perc) %>% 
  mutate(ops = t1_ops + d_t1_ops * lea_t1_perc) %>% 
  mutate(transp = t1_transp + d_t1_transp * lea_t1_perc)

final_schools %<>% 
  select(-starts_with("d_")) %>% 
  select(-starts_with("t1_")) %>% 
  select(-lea_t1_perc)


### calculate at student level

final_schools %<>% 
  mutate(t1_admin = admin / Students,
         t1_exc = exc / Students,
         t1_instr_comp = instr_comp / Students,
         t1_instr_rel = instr_rel / Students,
         t1_nutr = nutr / Students,
         t1_ops = ops / Students,
         t1_transp = transp / Students) %>% 
  select(-c(admin:transp))


### quality check against school total

final_schools %<>% 
  mutate(t1_total = t1_admin + t1_instr_comp + t1_instr_rel + t1_nutr + t1_ops + t1_transp) # not including exc!

final_schools %<>% 
  mutate(perc_t1 = t1_total / School_Average)

final_schools %>% 
  ggplot(aes(x = perc_t1)) +
  geom_histogram()

summary(final_schools$perc_t1)
  
# spot checked the high spenders and these are all coming from 7801...


### clean and export

final_schools %<>% 
  select(-c(School_Average, Students, perc_t1))

summary(final_schools) # instr_rel still highest category by far

# saveRDS(final_schools, "H:/Economists/Ed/Analytics Projects/Title I Analysis/2nd Release/Data/Title I Expense by School V3.rds")

