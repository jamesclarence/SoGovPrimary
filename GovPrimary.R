# Attempt at Replicating Harry Enten's FiveThirtyEight
# Primary Margin of Victory and General Election Margin of Victory at the state level
# Using Southern state gubernatorial primary and general election results

# Article: "Clinton Is Polling Like an Incumbent, And That Could Help Her in 2016" 
# URL: http://fivethirtyeight.com/features/clinton-is-polling-like-an-incumbent-and-that-could-help-her-in-2016/

library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("scales", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")

results <- read.csv("~/Documents/govprimary.csv")

State <- results$State
Year <- results$Year
prim <- results$Primary.Margin
gen <- results$General.Margin

prim <- as.numeric(sub("%","",prim))/100
gen <- as.numeric(sub("%","",gen))/100

df <- data.frame(State, Year, prim, gen)

# Need to adjust Y AXIS so scale is same as X axis)
ggplot(df) + geom_point(aes(prim, gen, color=State, size=2)) +
  scale_color_manual(values=c("red", "dark green","black","orange")) +
  scale_size(guide='none') +
  scale_y_continuous(labels = percent, limits=c(-0.5,0.5)) +
  scale_x_continuous(labels = percent) +
  geom_hline(aes(yintercept=0)) + 
  geom_vline(aes(xintercept=0)) +
  labs(title="Primary vs. General: Southern Gubernatorial Performance", x="Difference between incumbent-party & \nchallenging-party primary margins", y="General election margin for incumbent-party") + 
  geom_smooth(aes(prim,gen),method=lm, se=FALSE) +
  theme(panel.grid.minor=element_blank()) +
  theme_bw()

# Slope of regression line:
# y = 0.03793x + 0.05603
# prim = 0.5, y = 0.03793*0.5 + 0.05603
# y = 0.074995