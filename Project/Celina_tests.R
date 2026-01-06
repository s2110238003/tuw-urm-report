#===============================================================================
# DATA SETUP

library(dplyr)

# Import tool data (per participant)
p01_tool <- read.csv("../Data/URM_Study1_P1.csv")
p02_tool <- read.csv("../Data/URM_Study1_P2.csv")
p03_tool <- read.csv("../Data/URM_Study1_P3.csv")
p04_tool <- read.csv("../Data/URM_Study1_P4.csv")
p05_tool <- read.csv("../Data/URM_Study1_P5.csv")
p06_tool <- read.csv("../Data/URM_Study1_P6.csv")
p07_tool <- read.csv("../Data/URM_Study1_P7.csv")
p08_tool <- read.csv("../Data/URM_Study1_P8.csv")
p09_tool <- read.csv("../Data/URM_Study1_P9.csv")
p10_tool <- read.csv("../Data/URM_Study1_P10.csv")
p11_tool <- read.csv("../Data/URM_Study1_P11.csv")
p12_tool <- read.csv("../Data/URM_Study1_P12.csv")
p13_tool <- read.csv("../Data/URM_Study1_P13.csv")
p14_tool <- read.csv("../Data/URM_Study1_P14.csv")
p15_tool <- read.csv("../Data/URM_Study1_P15.csv")
p16_tool <- read.csv("../Data/URM_Study1_P16.csv")
p17_tool <- read.csv("../Data/URM_Study1_P17.csv")
p18_tool <- read.csv("../Data/URM_Study1_P18.csv")
p19_tool <- read.csv("../Data/URM_Study1_P19.csv")
p20_tool <- read.csv("../Data/URM_Study1_P20.csv")

#-------------------------------------------------------------------------------

# Merge tool data into one big table
data_tool <- bind_rows(mget(ls(pattern = "^p\\d+_tool$")))

# Fix: standardize key names + types so merges don't fail without noticing
names(data_tool)[names(data_tool) == "Participant"] <- "participant_id"
names(data_tool)[names(data_tool) == "Run"] <- "run"
data_tool$participant_id <- as.integer(data_tool$participant_id)
data_tool$run <- as.integer(data_tool$run)

# Split tool data by run (1/2)
data_tool_iteration_1 <- subset(data_tool, run == 1)
data_tool_iteration_2 <- subset(data_tool, run == 2)

#-------------------------------------------------------------------------------
# Read and prepare questionnaire data

# Fix: keep Google Forms column names EXACTLY (spaces, ?, etc.)
data_questionnaire <- read.csv("../Data/URM_Study1_FormResponses.csv",
                               check.names = FALSE,
                               stringsAsFactors = FALSE)

# Optional: demographics table
data_demographics <- subset(data_questionnaire, select = 1:9)

# Split questionnaire into the two blocks
data_questionnaire_iteration_1 <- subset(data_questionnaire, select = c(2, 10:32))
data_questionnaire_iteration_2 <- subset(data_questionnaire, select = c(2, 33:55))

# Fix: rename ID col + enforce integer type (prevents join mismatches)
names(data_questionnaire_iteration_1)[1] <- "participant_id"
names(data_questionnaire_iteration_2)[1] <- "participant_id"
data_questionnaire_iteration_1$participant_id <- as.integer(trimws(data_questionnaire_iteration_1$participant_id))
data_questionnaire_iteration_2$participant_id <- as.integer(trimws(data_questionnaire_iteration_2$participant_id))

#-------------------------------------------------------------------------------

# Merge tool + questionnaire per iteration (inner join behavior)
data_iteration_1 <- merge(data_questionnaire_iteration_1, data_tool_iteration_1,
                          by = "participant_id", all = FALSE)
data_iteration_2 <- merge(data_questionnaire_iteration_2, data_tool_iteration_2,
                          by = "participant_id", all = FALSE)

# Add iteration column (maybe useful later)
data_iteration_1$iteration <- 1
data_iteration_2$iteration <- 2

#-------------------------------------------------------------------------------

# Create ONE consistent condition column, then bind into final "long" dataset
# IMPORTANT: In iteration 2 column is also named "Coloured light on?"

data_iteration_1$condition <- ifelse(data_iteration_1[["Coloured light on?"]] == "Yes", "colored", "white")
data_iteration_2$condition <- ifelse(data_iteration_2[["Coloured light on?"]] == "Yes", "colored", "white")

# Final merged dataset (40 rows = 20 participants x 2 runs)
data_long <- rbind(data_iteration_1, data_iteration_2)
data_long <- data_long[order(data_long$participant_id, data_long$iteration), ]

# Final condition tables (20 rows each)
data_color <- data_long[data_long$condition == "colored", ]
data_color <- data_color[order(data_color$participant_id), ]

data_white <- data_long[data_long$condition == "white", ]
data_white <- data_white[order(data_white$participant_id), ]

#===============================================================================

# CHECKS

# Hard sanity checks (script stops if tables are wrong)
stopifnot(nrow(data_long) == 40)
stopifnot(nrow(data_color) == 20)
stopifnot(nrow(data_white) == 20)

# Each participant must appear exactly twice overall (two iterations)
stopifnot(all(table(data_long$participant_id) == 2))

# Each participant must have exactly 1 colored and 1 white
stopifnot(all(table(data_long$participant_id, data_long$condition) == 1))

# Optional: Visual confirmation
head(data_long[, c("participant_id", "iteration", "condition")], 10)

#-------------------------------------------------------------------------------

#CLEAN UP

# Optional: clean environment to keep only "final" objects
rm(list = setdiff(ls(), c("data_long", "data_color", "data_white", "data_demographics")))


#===============================================================================

# EXPLANATION

# data_color
  # 20 rows, 1 per participant
  # colored light summary
  # same as data_long but filtered for colored condition
# data_white
  # same as data-color but for white light condition
# data_long
  # main dataset
  # 40 rows = 20 participants x 2 iterations
# data_demographics
  # 20 rows per participant,
  # demo info, no iteration, no condition

#===============================================================================
#===============================================================================
#===============================================================================

                            # ANALYSIS - PLAN

# 1. NASA-TLX - paired t-test per condition (Wilcoxon if not normally distributed)

# 2. Flow Short Scale - Paired t-test (Wilcoxon if not normally distributed)

# 3. Performance metrics - paired comparison
  # - (not needed for well being and thus not as relevant to our hypothesis but interesting)
  # - (consider: learning effects, short task duration, high variance)
  # - (frame as "exploratory analyses of task performance")

#===============================================================================
#===============================================================================
#===============================================================================

# NASA-TLX

# Checks:
table(data_long$condition)
table(data_long$participant_id, data_long$condition)


# Identify TLX columns programmatically (robust to line breaks & truncation)
tlx_items <- grep(
  "mentally demanding|physically demanding|hurried or rushed|successful were you|hard did you have to work|insecure, discouraged",
  names(data_long),
  ignore.case = TRUE,
  value = TRUE
)

# Sanity check: must be exactly 6 items
stopifnot(length(tlx_items) == 6)

# Inspect structure
str(data_long[, tlx_items])

# Numeric conversion
data_long[, tlx_items] <- lapply(data_long[, tlx_items], as.numeric)

# Compute TLX overall - six answers into one workload score
data_long$TLX_overall <- rowMeans(
  data_long[, tlx_items],
  na.rm = TRUE
)

# Check that TLX_overall exists - yes
names(data_long)

# Now we can compare - white vs colored per participant
install.packages("tidyr")
library(tidyr)

tlx_table <- data_long %>%
  select(participant_id, condition, TLX_overall) %>%
  pivot_wider(
    names_from = condition,
    values_from = TLX_overall,
    names_prefix = "TLX_"
  ) %>%
  mutate(
    TLX_diff = TLX_colored - TLX_white
  ) %>%
  arrange(participant_id)

# Analysis ready NASA-TLX table:
summary(tlx_table)

# INFO: DIFFERENCES - white VS colored light
cat(
  "Mean TLX (white):", mean(tlx_table$TLX_white), "\n",
  "Mean TLX (colored):", mean(tlx_table$TLX_colored), "\n",
  "Mean difference (colored - white):", mean(tlx_table$TLX_diff), "\n"
)
  # The mean difference is very close to zero
  # Differences go both directions across participants
  # If there is an effect, it is very small


# INFO: NORMALITY
# The t-test assumed the within-participant difference (colored − white) is approximately normal
# We need to check TLX_diff for this

  # Numerical check:
  shapiro.test(tlx_table$TLX_diff) # p-value = 0.07388 >= 0.05 -> paired t-test is okay
  
  # Visual Check
    hist(
      tlx_table$TLX_diff,
      main = "NASA-TLX Difference Scores (Colored − White)",
      xlab = "Difference"
    )
    qqnorm(tlx_table$TLX_diff)
    qqline(tlx_table$TLX_diff)
  # Normally distributed -> paired t-test is okay

# T-TEST: compares within-participant differences, not group averages
    t.test(
      tlx_table$TLX_colored,
      tlx_table$TLX_white,
      paired = TRUE
    )

# INFO: t-test    
# Hypothesis: “Does perceived workload differ between colored and white lighting conditions?”
# t ≈ 0.25 -> the difference between conditions is tiny
# p = 0.8059 -> non-significant result -> very big, no evidence that colored and white lighting differ in workload, observed difference can be explained by random variation
# 95% CI: [−0.74, 0.94] -> the true effect could be a small decrease or a small increase, the interval includes 0 comfortably, no plausible meaningful effect is supported










