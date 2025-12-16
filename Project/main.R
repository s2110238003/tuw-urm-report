# Import tool data
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

# Merge tool data into one big table
data_tool <- bind_rows(mget(ls(pattern = "^p\\d+_tool$")))

# Merge tool data with questionnaire data
# TODO

# Split data into a table per iteration
library(dplyr)
data_tool_run1 <- data_tool %>% filter(Run == 1)
data_tool_run2 <- data_tool %>% filter(Run == 2)

# Split tool data into a table per light condition
# TODO

# Do actual analysis
# TODO