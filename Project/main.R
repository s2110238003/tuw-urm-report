library(dplyr)

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

data_tool_iteration_1 <- subset(data_tool, data_tool[,2] == 1)
data_tool_iteration_2 <- subset(data_tool, data_tool[,2] == 2)


# Read and prepare questionnaire data
data_questionnaire <- read.csv("../Data/URM_Study1_FormResponses.csv", check.names = FALSE)
data_demographics <- subset(data_questionnaire, select = 1:9)

data_questionnaire_iteration_1 <- subset(data_questionnaire, select = c(2, 10:32))
data_questionnaire_iteration_2 <- subset(data_questionnaire, select = c(2, 33:55))


# Merge tables to contain tool and questionnaire data
data_iteration_1 <- merge(data_questionnaire_iteration_1, data_tool_iteration_1, by = 1, all = TRUE)
data_iteration_2 <- merge(data_questionnaire_iteration_2, data_tool_iteration_2, by = 1, all = TRUE)


# Split tables per condition
data_iteration_1_color <- subset(data_iteration_1, data_iteration_1[,2] == "Yes")
data_iteration_1_white <- subset(data_iteration_1, data_iteration_1[,2] == "No")

data_iteration_2_color <- subset(data_iteration_2, data_iteration_2[,2] == "Yes")
data_iteration_2_white <- subset(data_iteration_2, data_iteration_2[,2] == "No")

data_color <- rbind(data_iteration_1_color, data_iteration_2_color)
data_color <- data_color[order(data_color[,1]), ]

data_white <- rbind(data_iteration_1_white, data_iteration_2_white)
data_white <- data_white[order(data_white[,1]), ]



# Do actual analysis
# TODO

