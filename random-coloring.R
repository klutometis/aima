library(debug)
library(ggplot2)
data <- read.csv('random-coloring-data.csv', header=T, sep=',')

png("random-coloring.png", width=1600, height=900)
ggplot(data=data, aes(x=n, y=time, col=index)) +
  geom_point() +
  geom_smooth() +
  scale_color_discrete(name="Method",
                       labels=c("Backtracking",
                         "Foward checking",
                         "Min-conflicts")) +
  scale_x_continuous(name="n") +
  scale_y_continuous(name="Time (s)")
dev.off()

png("random-coloring-no-outliers.png", width=1600, height=900)
ggplot(data=data, aes(x=n, y=time, col=index)) +
  geom_point() +
  geom_smooth() +
  scale_color_discrete(name="Method",
                       labels=c("Backtracking",
                         "Foward checking",
                         "Min-conflicts")) +
  scale_x_continuous(name="n") +
  scale_y_continuous(name="Time (s)", limits=c(0, 1.5))
dev.off()
