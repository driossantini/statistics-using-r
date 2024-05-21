library(ggplot2)

dlf <- read.delim("Datasets/DownloadFestival(No Outlier).dat", header = TRUE)

# Checking for normality visually
# --------------------

## Day 1

hist_day1 <- ggplot(data = dlf, aes(x = day1)) +
  theme(legend.position = "none") +
  geom_histogram(aes(y = after_stat(density)),
                 colour = "black", fill = "white") +
  labs(x = "Hygiene Score on Day 1", y = "Density")

hist_day1 +
  stat_function(fun = dnorm,
                args = list(mean = mean(dlf$day1, na.rm = TRUE),
                            sd = sd(dlf$day1, na.rm = TRUE)),
                colour = "black", size = 1)

qqplot_day1 <- ggplot(data = dlf) + stat_qq(aes(sample = day1), na.rm = TRUE)
qqplot_day1


## Day 2

hist_day2 <- ggplot(data = dlf, aes(x = day2)) +
  theme(legend.position = "none") +
  geom_histogram(aes(y = after_stat(density)),
                 colour = "black", fill = "white") +
  labs(x = "Hygiene Score on Day 2", y = "Density")

hist_day2 +
  stat_function(fun = dnorm,
                args = list(mean = mean(dlf$day2, na.rm = TRUE),
                            sd = sd(dlf$day2, na.rm = TRUE)),
                colour = "black", size = 1)

qqplot_day2 <- ggplot(data = dlf) + stat_qq(aes(sample = day2), na.rm = TRUE)
qqplot_day2


## Day 3

hist_day3 <- ggplot(data = dlf, aes(x = day3)) +
  theme(legend.position = "none") +
  geom_histogram(aes(y = after_stat(density)),
                 colour = "black", fill = "white") +
  labs(x = "Hygiene Score on Day 3", y = "Density")

hist_day3 +
  stat_function(fun = dnorm,
                args = list(mean = mean(dlf$day3, na.rm = TRUE),
                            sd = sd(dlf$day3, na.rm = TRUE)),
                colour = "black", size = 1)

qqplot_day3 <- ggplot(data = dlf) + stat_qq(aes(sample = day3), na.rm = TRUE)
qqplot_day3

# Quantifying normality
# --------------------

library(psych)
psych::describe(dlf$day1)

library(pastecs)
pastecs::stat.desc(dlf$day1, basic = FALSE, norm = TRUE)

describe(dlf[, c("day1", "day2", "day3")])
stat.desc(dlf[, c("day1", "day2", "day3")], basic = FALSE, norm = TRUE)

round(stat.desc(dlf[, c("day1", "day2", "day3")], basic = FALSE, norm = TRUE),
      digits = 3)


# Exploring groups of data
# --------------------

rexam <- read.delim("Datasets/RExam.dat", header = TRUE)

# Factorisation has been completed already in this dataset
# rexam$uni <- factor(rexam$uni, levels = c(0:1),
#                     labels = c("Duncetown University", "Sussex University"))


