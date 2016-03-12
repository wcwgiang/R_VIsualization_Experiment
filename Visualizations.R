Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

Scenario = data.frame()

NUM_SAMPLES = 50
NUM_REPLICATIONS = 4
MEAN_LOW = 30
MEAN_HIGH = 80
LOW_SD = 0.1
HIGH_SD = 0.3
PLOT_WIDTH = 800
PLOT_HEIGHT = 300
BLOCK = 1
JUDGEMENT_DIRECTION = -.3


# Scenario Num (One number per sample)
SCENARIO_NUM = 0

scenarios <- read.csv('C:/Dropbox/experimentplots/scenario_samples.csv')
sample <- scenarios[,2]
sample_median <- median(sample)
sample_mean <- mean(sample)
# Median
boxplot(sample_median, ylim = c(0,150), horizontal=TRUE)

# Mean & SD
plot(0,type='n', xlim = c(0,150), ylim = c(0,2),yaxt = 'n',ylab = "", xlab = "")
segments(mean(sample)-sd(sample), 1, mean(sample)+sd(sample), 1)
points(mean(sample), 1, pch = 16 , cex = 1.5)

# Boxplot
boxplot(sample, ylim = c(0,150), range = 0, horizontal=TRUE)

# Violin Plot
plot(0,type='n', xlim = c(0,150), ylim = c(0,2),yaxt = 'n',yaxs = "i",ylab = "", xlab = "")
vioplot(sample, col = "transparent", ylim = c(0,150), horizontal = TRUE, add = TRUE)


# Judgemnt Segment
segments(round(quantile(sample,c(.5+JUDGEMENT_DIRECTION),names=FALSE)),0,round(quantile(sample,c(.5+JUDGEMENT_DIRECTION),names=FALSE)),2,lwd = 4, col = "red")

segments(round(quantile(sample,c(.5+JUDGEMENT_DIRECTION),names=FALSE)),-1,round(quantile(sample,c(.5+JUDGEMENT_DIRECTION),names=FALSE)),3,lwd = 4, col = "blue")
