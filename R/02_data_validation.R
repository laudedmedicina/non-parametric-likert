################ Data validity##############################

#Now, did I delete anything accidently? Let's suuuuuuuuu= Data validity

dim(df)        # should still be 626 rows (no accidental row loss)
ncol(df)       # confirm expected number of columns

##########Confirm Likert recoding worked#####################

#Should show numbers 1-3, NOT text anymore
df %>% select(starts_with("student_")) %>% 
  summarise(across(everything(), ~unique(.)))

# Quick check — no text values remaining
df %>% select(starts_with("student_attitude")) %>%
  summarise(across(everything(), ~class(.)))  # should all say "numeric"


################# Check value ranges make sense #####################
# Likert items should only contain 1, 2, 3
df %>% select(starts_with("student_attitude")) %>%
  summarise(across(everything(), list(min = min, max = max), na.rm = TRUE))

# Age should be realistic (18-30 for students)
range(df$age_write_as_number_please, na.rm = TRUE)

# No negative values in count columns #Pick any col you want as well
summary(df$how_many_research_projects_did_you_participate_in_please_write_zero_if_you_never_participated)

#################### Check factor levels #######################
# Confirm categories are clean — no typos, no duplicates
levels(factor(df$gender))         # should be exactly: "Male" "Female"
levels(factor(df$university))     # should be exactly 5 universities
levels(factor(df$residence))      # Urban, Rural, Refugee Camp
levels(factor(df$academic_year))  # Second through Sixth


#################Re-check missing values########################
colSums(is.na(df))
# Only the 2 skip-logic columns should have ~245 and ~239 NAs
# Everything else should be 0


#############Check composite scores##########################
# Attitude score should be between 1 and 3
summary(df$attitude_score)

# Check for any NaN (happens if all values in a row were NA)
sum(is.nan(df$attitude_score))

#############Spot-check a few rows manually####################

# Compare original Excel vs cleaned df side by side for 5 random rows
df %>% slice_sample(n = 5) %>% 
  select(gender, age_write_as_number_please, university, attitude_score, barrier_score)

#########Final summary snapshot############
# One clean overview of the finished dataset
skimr::skim(df)   