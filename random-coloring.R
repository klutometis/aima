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
                         "Min-conflicts"))
dev.off()
