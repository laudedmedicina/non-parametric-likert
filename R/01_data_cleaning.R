# ============================================================
# 01_data_cleaning.R
# Purpose : Import raw data, clean, recode, and compute
#           composite scores. Saves clean dataset to disk.
# Input   : data/raw/dataset.xlsx
# Output  : data/processed/clean_data.csv
# ============================================================

library(tidyverse)
library(janitor)
library(readxl)
library(stringr)

# ------------------------------------------------------------
# 1. Import
# ------------------------------------------------------------
# ============================================================
# 01_data_cleaning.R
# Purpose : Import raw data, clean, recode, and compute
#           composite scores. Saves clean dataset to disk.
# Input   : data/raw/dataset.xlsx
# Output  : data/processed/clean_data.csv
# ============================================================

library(tidyverse)
library(janitor)
library(readxl)
library(stringr)

# ------------------------------------------------------------
# 1. Import
# ------------------------------------------------------------
df <- read_excel("data/raw/dataset.xlsx")
View(df)
#Whenever we call df, our dataset opens--------------------

glimpse(df)          # column types + preview >>>> Console returns 
summary(df)          # basic stats per column
colSums(is.na(df))   # count missing per column>>> remember it's just counting

df <- df %>% clean_names()   # janitor: makes all names lowercase_snake_case= each space is replaced with an underscore (_) character

# Drop rows with any NA
#df <- df %>% drop_na() #Do it cautiously, I used it, I found that half of dataset was deleted.

# Drop NA in specific columns
df <- df %>% drop_na(age_write_as_number_please, gender) #lol i wrongly forget to rename the coulmn variables NVM, I will continue
View(df)
# Replace NA with a value
#df <- df %>% mutate(income = replace_na(family_monthly_income_nis, 0)) #Not needed... it's a form

# Filter out NAs manually
df <- df %>% filter(!is.na(age_write_as_number_please))
View(df)

# The time to recede likert scale------------------------------------

# Create or transform a column
df <- df %>% mutate(
  age_write_as_number_please = case_when(
    age_write_as_number_please < 20 ~ "18-20",
    age_write_as_number_please < 25 ~ "20-24",
    TRUE     ~ "25+"
  ),
  gender = factor(gender)          # convert to factor
)

view(df)
# Recode Likert text → numbers.... since the questions starts with "attitude", we will make R pick the first word
#df <- df %>% Pipes df into the next function and saves the result back into df. The %>% just means "take this, then do..."
#mutate(...) Creates or modifies columns. Whatever you put inside, it changes existing columns or adds new ones.
#Remember, mutase enzymes? It typically change something

#across(starts_with("attitude_"), ...) This is the power tool here. Instead of writing mutate for each column one by one, across() applies the same operation to many columns at once.starts_with("attitude_") is the column selector — it targets every column whose name begins with "attitude_"
df <- df %>% mutate(
  across(starts_with("student_"), ~ recode(.,
                                           "Agree"             = 3,
                                           "No opinion/ uncertain" = 2,
                                           "Disagree"          = 1
  ))
)

df <- df %>% mutate(
  across(starts_with("assess_"), ~ recode(.,
                                          "Agree"             = 3,
                                          "No opinion/ uncertain" = 2,
                                          "Disagree"          = 1
  ))
)


#Alternative way to recode the scale
#likert_map <- c(
  #"Agree"                  = 3,
 # "No opinion/ uncertain"  = 2,
 # "Disagree"               = 1
#)

#df <- df %>%
 # mutate(
  #  across(starts_with("student_"), ~ recode(., !!!likert_map)),
  #  across(starts_with("assess_"),  ~ recode(., !!!likert_map))
  #)

#btw we can select by position if the names are complex: across(19:49, ~ recode(., ...))   # columns 19 to 49, do the same operation
View(df)


# Keep only relevant rows
#df <- df %>% filter(academic_year != "First")

# Keep only relevant columns
#df <- df %>% select(gender, age, university, starts_with("student_")) All coulmn are important 

# Remove columns
#df <- df %>% select(-timestamp), not needed, since we got no column to be removed

View(df)

#df <- df %>% rename(
#participation = "did_you_participate_in_a_research_project_before",
#num_projects  = "how_many_research_projects_did_you_participate_in",

#)

#df <- df %>% distinct()                  # remove fully duplicate rows
#df <- df %>% distinct(student_id, .keep_all = TRUE)  # by key column, if we used a specific id or something

library(stringr)

df <- df %>% mutate(
  university = str_trim(university),        # Removes accidental whitespace from the start and end of each value.
  
  university = str_to_title(university),    # Converts the text to Title Case — first letter of each word capitalized, rest lowercase.
  university = str_replace(university, "Univ\\.", "University") #Finds the abbreviation "Univ." and replaces it with the full word "University".
) #The \\. might look strange — it's because . in R pattern matching means "any character", so you need \\. to mean "a literal dot".
#In our dataset, we dont need stringer.Why? It's a google form, so there's noway to miss with abbriviation or wording stractrue

# Wide → Long (useful for plotting all Likert items)
df_long <- df %>%
  pivot_longer(
    cols = starts_with("student_"), #btw, since I foregt to rename the col, i still can rename it rn
    names_to  = "item",
    values_to = "response"
  )
#cols — which columns to collapse. Here, all columns starting with "attitude" 
#names_to — the old column names become values in a new column called "item" 
#values_to — the old values go into a new column called "response"


# Long → Wide
df_wide <- df_long %>%
  pivot_wider(names_from = item, values_from = response)

view(df_long)


# Row-wise mean of all attitude items [attitude score] and [barrier score]
df <- df %>%
  rowwise() %>%
  mutate(attitude_score = mean(c_across(starts_with("student_")), na.rm = TRUE)) %>%
  ungroup()

df <- df %>%
  rowwise() %>%
  mutate(
    attitude_score = mean(c_across(starts_with("student_")), na.rm = TRUE),
    barrier_score  = mean(c_across(starts_with("assess_the_barriers")), na.rm = TRUE)
  ) %>%
  ungroup()

# ------------------------------------------------------------
# 8. Save clean dataset
# ------------------------------------------------------------
write.csv(df, "data/processed/clean_data.csv", row.names = FALSE)

message(glue::glue(
  "Cleaning complete. Final dataset: {nrow(df)} rows × {ncol(df)} columns."
))
# ------------------------------------------------------------
# 8. Save clean dataset
# ------------------------------------------------------------
write.csv(df, "data/processed/clean_data.csv", row.names = FALSE)

message(glue::glue(
  "Cleaning complete. Final dataset: {nrow(df)} rows × {ncol(df)} columns."
))