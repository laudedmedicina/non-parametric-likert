# ============================================================
# 03_analysis.R
# Purpose : Descriptive statistics, normality assessment,
#           and inferential non-parametric tests.
# Input   : data/processed/clean_data.csv
# Output  : outputs/tables/descriptive_table.docx
# ============================================================

library(tidyverse)
library(gtsummary)
library(flextable)
library(moments)
library(nortest)
library(dunn.test)

# ------------------------------------------------------------
# 1. Load clean data
# ------------------------------------------------------------
df <- read.csv("data/processed/clean_data.csv", stringsAsFactors = FALSE) %>%
  mutate(
    gender        = factor(gender),
    university    = factor(university),
    academic_year = factor(academic_year,
                           levels = c("Second", "Third", "Fourth",
                                      "Fifth",  "Sixth")),
    residence     = factor(residence)
  )

# ============================================================
# SECTION A — Descriptive statistics
# ============================================================

library(tidyverse)

# Frequency tables for all categorical variables
map(c("gender", "academic_year", "university", "residence",
      "fathers_educational_status", "mothers_educational_status"),
    ~ df %>% count(.data[[.x]]) %>%
      mutate(pct = round(n / sum(n) * 100, 1)))

demo_vars <- c("gender", "university", "academic_year", "residence")

freq_tables=map(demo_vars, ~ df %>%
                  count(.data[[.x]]) %>%
                  mutate(percent = round(n / sum(n) * 100, 1)) %>%
                  rename(category = 1)
) %>%
  set_names(demo_vars)

print(freq_tables)

# Numeric summaries
df %>% select(age_write_as_number_please, attitude_score, barrier_score) %>%
  summary()



################ showing descriptive data in figures ################
# Gender — pie chart
df %>%
  count(gender) %>%
  mutate(percent = round(n / sum(n) * 100, 1),
         label   = paste0(gender, "\n", percent, "%")) %>%
  ggplot(aes(x = "", y = n, fill = gender)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 4) +
  scale_fill_manual(values = c("Male" = "#378ADD", "Female" = "#D4537E")) +
  labs(title = "Gender distribution") +
  theme_void() +
  theme(legend.position = "none")

# Residence — pie chart
df %>%
  count(residence) %>%
  mutate(percent = round(n / sum(n) * 100, 1),
         label   = paste0(residence, "\n", percent, "%")) %>%
  ggplot(aes(x = "", y = n, fill = residence)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 4) +
  scale_fill_manual(values = c("Urban" = "#7F77DD", "Rural" = "#AFA9EC", "Refugee Camp" = "#CECBF6")) +
  labs(title = "Residence distribution") +
  theme_void() +
  theme(legend.position = "none")


# University — horizontal bar chart
df %>%
  count(university) %>%
  mutate(percent = round(n / sum(n) * 100, 1),
         university = reorder(university, n)) %>%
  ggplot(aes(x = n, y = university)) +
  geom_col(fill = "#EF9F27") +
  geom_text(aes(label = paste0(n, " (", percent, "%)")),
            hjust = -0.1, size = 3.5) +
  xlim(0, 180) +
  labs(title = "Students by university", x = "Count", y = NULL) +
  theme_minimal()

# Father's education
df %>%
  count(fathers_educational_status) %>%
  mutate(percent = round(n / sum(n) * 100, 1),
         fathers_educational_status = reorder(fathers_educational_status, n)) %>%
  ggplot(aes(x = n, y = fathers_educational_status)) +
  geom_col(fill = "#AFA9EC") +
  geom_text(aes(label = paste0(n, " (", percent, "%)")),
            hjust = -0.1, size = 3.5) +
  xlim(0, 320) +
  labs(title = "Father's educational status", x = "Count", y = NULL) +
  theme_minimal()


ggplot(df, aes(x = attitude_score)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 30, fill = "#378ADD", color = "white", alpha = 0.8) +
  geom_density(color = "#185FA5", linewidth = 1) +
  geom_vline(aes(xintercept = mean(attitude_score)),
             color = "red", linetype = "dashed", linewidth = 0.8) +
  annotate("text", x = mean(df$attitude_score) + 0.05,
           y = 1.2, label = paste0("Mean = ", round(mean(df$attitude_score), 2)),
           hjust = 0, color = "red", size = 3.5) +
  labs(title = "Distribution of attitude toward research score",
       x = "Attitude score (1–3)", y = "Density") +
  theme_minimal()

ggplot(df, aes(x = barrier_score)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 30, fill = "#1D9E75", color = "white", alpha = 0.8) +
  geom_density(color = "#0F6E56", linewidth = 1) +
  geom_vline(aes(xintercept = mean(barrier_score)),
             color = "red", linetype = "dashed", linewidth = 0.8) +
  annotate("text", x = mean(df$barrier_score) + 0.05,
           y = 0.9, label = paste0("Mean = ", round(mean(df$barrier_score), 2)),
           hjust = 0, color = "red", size = 3.5) +
  labs(title = "Distribution of barriers to research score",
       x = "Barrier score (1–3)", y = "Density") +
  theme_minimal()

##################Tables for descriptive data####################

table1 <- df %>%
  select(gender, age_write_as_number_please, university, academic_year, residence) %>%
  tbl_summary(
    statistic = list(
      all_continuous()  ~ "{mean} ± {sd} (range: {min}–{max})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits    = all_continuous() ~ 2,
    label     = list(
      gender        ~ "Gender",
      age_write_as_number_please ~ "Age (years)",
      university    ~ "University",
      academic_year ~ "Academic year",
      residence     ~ "Residence"
    )
  ) %>%
  bold_labels()

table1

table1 %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = "descriptive_statistics_table.docx")


table2 <- df %>%
  select(attitude_score, barrier_score) %>%
  tbl_summary(
    statistic = all_continuous() ~ "{mean} ± {sd} (min: {min}, max: {max})",
    digits    = all_continuous() ~ 3,
    label = list(
      attitude_score ~ "Attitude toward research score",
      barrier_score  ~ "Barriers to research score"
    )
  ) %>%
  bold_labels()

###########I want to merge table1 and table2####################

master_table <- tbl_stack(
  list(table1, table2),
  group_header = c(
    "Sociodemographics",
    "Attitude & barrier scores"
  )
)

master_table


############I want to save the master table #################

master_table %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = "descriptive_statistics_mastertable.docx")


# ============================================================
# SECTION B — Normality assessment
# ============================================================
#### Step 1 — Visual check (histogram + Q-Q plot) ############

# Histogram for attitude score
ggplot(df, aes(x = attitude_score)) +
  geom_histogram(bins = 30, fill = "#378ADD", color = "white") +
  labs(title = "Distribution of attitude score")

#######Alternative with showing the mean
ggplot(df, aes(x = attitude_score)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 30, fill = "#378ADD", color = "white", alpha = 0.8) +
  geom_density(color = "#185FA5", linewidth = 1) +
  geom_vline(aes(xintercept = mean(attitude_score)),
             color = "red", linetype = "dashed", linewidth = 0.8) +
  annotate("text", x = mean(df$attitude_score) + 0.05,
           y = 1.2, label = paste0("Mean = ", round(mean(df$attitude_score), 2)),
           hjust = 0, color = "red", size = 3.5) +
  labs(title = "Distribution of attitude toward research score",
       x = "Attitude score (1–3)", y = "Density") +
  theme_minimal()

# Histogram for barrier score
ggplot(df, aes(x = barrier_score)) +
  geom_histogram(bins = 30, fill = "#1D9E75", color = "white") +
  labs(title = "Distribution of barrier score")

# Q-Q plots
qqnorm(df$attitude_score); qqline(df$attitude_score, col = "red")
qqnorm(df$barrier_score);  qqline(df$barrier_score,  col = "red")


######### step 2 Statistical test (Shapiro-Wilk) ##########

shapiro.test(df$attitude_score)
shapiro.test(df$barrier_score)


###p > 0.05 → data is normal → use parametric tests (t-test, ANOVA)
###p < 0.05 → data is NOT normal → use non-parametric tests (Mann-Whitney U, Kruskal-Wallis)
### NOTE:: with n = 626, Shapiro-Wilk almost always returns p < 0.05 even for minor deviations. In large samples like ours, trust the histogram and Q-Q plot more than the p-value.

####That's why we can do Kolmogorov–Smirnov test

# K-S test against normal distribution
ks.test(df$attitude_score, "pnorm",
        mean = mean(df$attitude_score),
        sd   = sd(df$attitude_score))

ks.test(df$barrier_score, "pnorm",
        mean = mean(df$barrier_score),
        sd   = sd(df$barrier_score))

##Since it's likert scale, many data are similar that's why the Console returns error, however, we can run Lilliefors test based on K-S test or Anderson-Darling test

#Lilliefors test
install.packages("nortest")
library(nortest)

# Lilliefors test
lillie.test(df$attitude_score)
lillie.test(df$barrier_score)


# Anderson-Darling test (most powerful for large n)
ad.test(df$attitude_score)
ad.test(df$barrier_score)

normality_check(df$attitude_score, "Attitude Score")
normality_check(df$barrier_score,  "Barrier Score")

########## Step3 Check skewness & kurtosis#################

skewness(df$attitude_score)
kurtosis(df$attitude_score)

skewness(df$barrier_score)
kurtosis(df$barrier_score)
####Acceptable ranges for normality:

###Skewness between -1 and +1
###Kurtosis between -2 and +2

###Result: 
##Skewness: Both values are between -1 and +1 — this is acceptable, meaning only mild negative skew (slightly left-tailed).
##Kurtosis: Attitude score = 4.35 is slightly above the acceptable threshold of 3 (leptokurtic — more peaked than normal). Barrier score = 3.09 is borderline acceptable.


###### All the tests return non-nornally distributed = non-parametric tests, that means we need to chhoose tests based on the comparisons.
#2 groups (e.g. gender, yes/no)= Mann-Whitney 
#3+ groups (e.g. university, year)= Kruskal-Wallis
#Post-hoc for Kruskal-Wallis= Dunn test with Bonferroni correction
####Continuous variables were reported using median and interquartile range (IQR; Q1–Q3), while categorical variables were reported using frequencies and percentages.


analysis_plan <- data.frame(
  Comparison = c("Attitude/barrier by gender", "Attitude/barrier by academic year", "Attitude/barrier by university",
                 "Attitude/barrier by residence", "Attitude/barrier by research participation", "Attitude/barrier by studied methodology"),
  Groups     = c("2 groups", "5 groups", "5 groups",
                 "3 groups", "2 groups", "2 groups"),
  Test       = c("Mann-Whitney U", "Kruskal-Wallis + Dunn post-hoc",
                 "Kruskal-Wallis + Dunn post-hoc", "Kruskal-Wallis + Dunn post-hoc",
                 "Mann-Whitney U", "Mann-Whitney U")
)

View(analysis_plan)   # opens as a spreadsheet in RStudio


install.packages("dunn.test")
library(dunn.test)

# ============================================================
# SECTION C — Inferential statistics (non-parametric)
# ============================================================
##################### Mann-Whitney U — Gender #################
# Attitude score by gender
wilcox.test(attitude_score ~ gender, data = df)

# Barrier score by gender
wilcox.test(barrier_score ~ gender, data = df)

# With descriptive summary alongside
df %>%
  group_by(gender) %>%
  summarise(
    n      = n(),
    median = median(attitude_score),
    mean   = round(mean(attitude_score), 3),
    sd     = round(sd(attitude_score), 3)
  )



#################### Kruskal-Wallis — Academic year ###############

# Attitude
kruskal.test(attitude_score ~ academic_year, data = df)

# Barrier
kruskal.test(barrier_score ~ academic_year, data = df)

# If p < 0.05 → run Dunn post-hoc to find which years differ
dunn.test(df$attitude_score, df$academic_year,
          method = "bonferroni", kw = TRUE, label = TRUE)

dunn.test(df$barrier_score, df$academic_year,
          method = "bonferroni", kw = TRUE, label = TRUE)


################ Kruskal-Wallis — University ######################

kruskal.test(attitude_score ~ university, data = df)
kruskal.test(barrier_score  ~ university, data = df)

# Post-hoc if significant
dunn.test(df$attitude_score, df$university,
          method = "bonferroni", kw = TRUE, label = TRUE)

dunn.test(df$barrier_score, df$university,
          method = "bonferroni", kw = TRUE, label = TRUE)

############ Kruskal-Wallis — Residence ##########################

kruskal.test(attitude_score ~ residence, data = df)
kruskal.test(barrier_score  ~ residence, data = df)

# Post-hoc if significant
dunn.test(df$attitude_score, df$residence,
          method = "bonferroni", kw = TRUE, label = TRUE)

dunn.test(df$barrier_score, df$residence,
          method = "bonferroni", kw = TRUE, label = TRUE)


# --- Descriptive summary by group ---------------------------
cat("\n\n========== GROUP MEDIANS (IQR) ==========\n")

for (var in c("gender", "academic_year", "university", "residence")) {
  cat("\n---", toupper(var), "---\n")
  df %>%
    group_by(.data[[var]]) %>%
    summarise(
      n              = n(),
      attitude_med   = median(attitude_score, na.rm = TRUE),
      attitude_IQR   = IQR(attitude_score, na.rm = TRUE),
      barrier_med    = median(barrier_score,  na.rm = TRUE),
      barrier_IQR    = IQR(barrier_score,  na.rm = TRUE),
      .groups = "drop"
    ) %>%
    print()
}
