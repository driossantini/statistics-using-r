
# Quick tutorial section
# --------------------

library(ggplot2, reshape2)

facebook_data <- read.delim("Datasets/FacebookNarcissism.dat", header = TRUE)

graph <- ggplot(facebook_data, aes(NPQC_R_Total, Rating))

graph + geom_point()
graph + geom_point(shape = 17)
graph + geom_point(size = 6)
graph + geom_point(aes(colour = Rating_Type))
graph + geom_point(aes(colour = Rating_Type), position = "jitter")
graph + geom_point(aes(shape = Rating_Type), position = "jitter")


# Simple scatterplot
# --------------------

exam_data <- read.delim("Datasets/Exam Anxiety.dat", header = TRUE)

scatter <- ggplot(exam_data, aes(Anxiety, Exam))
scatter + geom_point()
scatter + geom_point() + labs(x = "Exam Anxiety", y = "Exam Performance %")
scatter + geom_point() + geom_smooth() +
  labs(x = "Exam Anxiety", y = "Exam Performance %")

scatter + geom_point() + geom_smooth(method = "lm", colour = "red") +
  labs(x = "Exam Anxiety", y = "Exam Performance %")

scatter + geom_point() + geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Exam Anxiety", y = "Exam Performance %")

scatter + geom_point() +
  geom_smooth(method = "lm", alpha = 0.1, fill = "blue") +
  labs(x = "Exam Anxiety", y = "Exam Performance %")


# Grouped scatterplot
# --------------------

scatter <- ggplot(exam_data, aes(Anxiety, Exam, colour = Gender))
scatter + geom_point() + geom_smooth(method = "lm")
scatter + geom_point() + geom_smooth(method = "lm") +
  labs(x = "Exam Anxiety", y = "Exam Performance %", colour = "Gender")

scatter + geom_point() +
geom_smooth(method = "lm", aes(fill = Gender), alpha = 0.1) +
labs(x = "Exam Anxiety", y = "Exam Performance %", colour = "Gender")

## Self-test

test <- ggplot(facebook_data, aes(x = NPQC_R_Total, y = Rating))
test + geom_point(size = 0.5, position = "jitter") +
  geom_smooth(method = "lm", se = FALSE, aes(colour = Rating_Type)) +
  labs(x = "NPQC-R score", y = "Raiting", colour = "Rating Type")

# Histograms

festival_data <- read.delim("Datasets/DownloadFestival.dat", header = TRUE)

festival_histogram <- ggplot(data = festival_data, aes(day1)) +
  theme(legend.position = "none")

festival_histogram + geom_histogram(binwidth = 0.4) +
  labs(x = "Hygiene (Day 1 of Festival)", y = "Frequency")

# Boxplots
# --------------------

festival_boxplot <- ggplot(festival_data,
                           aes(x = gender, y = day1))

festival_boxplot + geom_boxplot() +
  labs(x = "Gender", y = "Hygiene (Day 1 of Festival)")

## Modify and correct the outliers
festival_data <- festival_data[order(festival_data$day1), ]
festival_data[festival_data$day1 == 20.02, ]["day1"] <- 2.02

## Reload data and plot again
festival_boxplot <- ggplot(festival_data,
                           aes(x = gender, y = day1))

festival_boxplot + geom_boxplot() +
  labs(x = "Gender", y = "Hygiene (Day 1 of Festival)")

## Self-test

fbp_day2 <- ggplot(festival_data,
                   aes(x = gender, y = day2))
fbp_day2 + geom_boxplot() +
  labs(x = "Gender", y = "Hygiene (Day 2 of Festival)")


fbp_day3 <- ggplot(festival_data,
                   aes(x = gender, y = day3))
fbp_day3 + geom_boxplot() +
  labs(x = "Gender", y = "Hygiene (Day 3 of Festival)")


# Density plots
# --------------------

density <- ggplot(data = festival_data, aes(day1))
density + geom_density() +
  labs(x = "Hygiene (Day 1 of Festival)", y = "Density Estimate")


# Graphing means
# --------------------

## Bar charts and error bars

### One independent variable
chick_flick <- read.delim(file = "Datasets/ChickFlick.dat", header = TRUE)
bar <- ggplot(data = chick_flick, aes(film, arousal))

bar +
  stat_summary(fun = mean, geom = "bar", fill = "white", colour = "black") +
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange") +
  labs(x = "Film", y = "Mean Arousal")

### Self-test

bar + # error bar
  stat_summary(fun = mean, geom = "bar", fill = "white", colour = "black") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", colour = "red") +
  labs(x = "Film", y = "Mean Arousal")

bar + # bootstrapped confidence intervals
  stat_summary(fun = mean, geom = "bar", fill = "white", colour = "black") +
  stat_summary(fun.data = mean_cl_boot, geom = "pointrange", colour = "red") +
  labs(x = "Film", y = "Mean Arousal")

### Several Independent variables

bar <- ggplot(data = chick_flick, aes(x = film, y = arousal, fill = gender))

bar +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",
               position = position_dodge(width = 0.9), width = 0.2) +
  labs(x = "Film", y = "Mean Arousal", fill = "Gender")

bar <- ggplot(chick_flick, aes(x = film, y = arousal, fill = film))
bar + stat_summary(fun = mean, geom = "bar") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  facet_wrap(~ gender) + labs(x = "Film", y = "Mean Arousal") +
  theme(legend.position = "none")

## Line graphs

### Single independent variables
hiccups_data <- read.delim("Datasets/Hiccups.dat", header = TRUE)
hiccups <- stack(hiccups_data)
names(hiccups) <- c("Hiccups", "Intervention")
hiccups$Intervention_Factor <- factor(hiccups$Intervention,
                                      levels = unique(hiccups$Intervention))

line <- ggplot(data = hiccups, aes(x = Intervention_Factor, Hiccups))

line + stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), colour = "blue",
               linetype = "dashed") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.15) +
  labs(x = "Intervention", y = "Mean Number of Hiccups")


### Multiple independent variables

text_data <- read.delim("Datasets/TextMessages.dat", header = TRUE)

text_messages <- reshape2::melt(text_data,
                                id = c("Group"))
names(text_messages) <- c("Group", "Time", "Grammar_Score")

line <- ggplot(data = text_messages, aes(Time, Grammar_Score, colour = Group))

line + stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line", aes(group = Group)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.15) +
  labs(x = "Time", y = "Mean Grammar Score", colour = "Group")

#### Self-Test (Skipped)
# Use what you have learnt to repeat the text message data plot but to
# also have different symbols for text messagers and controls and different
# types of lines.

# Themes and options

facebook_data <- read.delim("Datasets/FacebookNarcissism.dat", header = TRUE)
graph <- ggplot(facebook_data, aes(NPQC_R_Total, Rating))

graph + geom_point() +
  theme(panel.grid.major = element_line(colour = "blue")) +
  theme(axis.line = element_line(colour = "blue")) +
  theme(axis.line = element_line(linetype = "dashed"))

# End of chapter