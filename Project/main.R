# ==================
# Data setup

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
data_questionnaire_iteration_1 <- subset(data_questionnaire, select = c(2, 10:32))
data_questionnaire_iteration_2 <- subset(data_questionnaire, select = c(2, 33:55))

data_demographics <- subset(data_questionnaire, select = 1:9)


# Merge tables to contain tool and questionnaire data
data_iteration_1 <- merge(data_questionnaire_iteration_1, data_tool_iteration_1, by = 1, all = TRUE)
data_iteration_2 <- merge(data_questionnaire_iteration_2, data_tool_iteration_2, by = 1, all = TRUE)
data_accumulated <- rbind(data_iteration_1, data_iteration_2) # TODO: I am not sure yet if this is the proper way to handle this, we ignore iteration 1 and 2, which might be fine though?

# Split tables per condition
data_iteration_1_color <- subset(data_iteration_1, data_iteration_1[,2] == "Yes")
data_iteration_1_white <- subset(data_iteration_1, data_iteration_1[,2] == "No")

data_iteration_2_color <- subset(data_iteration_2, data_iteration_2[,2] == "Yes")
data_iteration_2_white <- subset(data_iteration_2, data_iteration_2[,2] == "No")

data_color <- rbind(data_iteration_1_color, data_iteration_2_color)
data_color <- data_color[order(data_color[,1]), ]

data_white <- rbind(data_iteration_1_white, data_iteration_2_white)
data_white <- data_white[order(data_white[,1]), ]


# ==================
# Do actual analysis

g_colored_light_on <- data_accumulated[,2]


# Questionnaire 1
# TODO: Note that this is probably not final. I just played around a little bit to understand R and the data better.

# How mentally demanding was the task?
x_1_1 <- data_accumulated[,3]
t_1_1 <- t.test(x_1_1 ~ g_colored_light_on)
p_1_1 <- t_1_1$p.value
boxplot(
  data_accumulated[,3] ~ g_colored_light_on,
  xlab="Coloured light on?",
  ylab="Mental demand rating",
  main=paste("t-test p =", round(p_1_1,4))
)

# How physically demanding was the task?
x_1_2 <- data_accumulated[,4]
t_1_2 <- t.test(x_1_2 ~ g_colored_light_on)
p_1_2 <- t_1_2$p.value
boxplot(
  data_accumulated[,4] ~ g_colored_light_on,
  xlab="Coloured light on?",
  ylab="Physical demand rating",
  main=paste("t-test p =", round(p_1_2,4))
)

# How hurried or rushed was the pace of the task?
x_1_3 <- data_accumulated[,5]
t_1_3 <- t.test(x_1_3 ~ g_colored_light_on)
p_1_3 <- t_1_3$p.value
boxplot(
  data_accumulated[,5] ~ g_colored_light_on,
  xlab="Coloured light on?",
  ylab="Rushed pace of task rating",
  main=paste("t-test p =", round(p_1_3,4))
)

# How successful were you in accomplishing what you were asked to do?
x_1_4 <- data_accumulated[,6]
t_1_4 <- t.test(x_1_4 ~ g_colored_light_on)
p_1_4 <- t_1_4$p.value
boxplot(
  data_accumulated[,6] ~ g_colored_light_on,
  xlab="Coloured light on?",
  ylab="Successful accomplish rating",
  main=paste("t-test p =", round(p_1_4,4))
)

# How hard did you have to work to accomplish your level of performance? 
x_1_5 <- data_accumulated[,7]
t_1_5 <- t.test(x_1_5 ~ g_colored_light_on)
p_1_5 <- t_1_5$p.value
boxplot(
  data_accumulated[,7] ~ g_colored_light_on,
  xlab="Coloured light on?",
  ylab="Hard work rating",
  main=paste("t-test p =", round(p_1_5,4))
)

# How insecure, discouraged, irritated, or stressed did you feel during the task? 
x_1_6 <- data_accumulated[,8]
t_1_6 <- t.test(x_1_6 ~ g_colored_light_on)
p_1_6 <- t_1_6$p.value
boxplot(
  data_accumulated[,8] ~ g_colored_light_on,
  xlab="Coloured light on?",
  ylab="Insecure, discouraged, irritated, or stressed rating",
  main=paste("t-test p =", round(p_1_6,4))
)


# Questionnaire 2
# TODO


# Tool data
# TODO

