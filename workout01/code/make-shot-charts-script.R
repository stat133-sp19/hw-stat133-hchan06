# title      : Shot Charts for GSW Players
# description: Exporting shot charts for GSW players in different image files
# input(s)   : 
#     curry     : stephen curry data frame
#     iguodala  : andre iguodala data frame
#     durant    : kevin durant data frame
#     green     : draymond green data frame
#     thompson  : klay thompson data frame
#     court_file: string vector that contains the relative path of court background image
# output(s)  :
#     andre-iguodala-shot-chart.pdf : andre iguodala's shot chart in pdf
#     draymond-green-shot-chart.pdf : draymond green's shot chart in pdf
#     kevin-durant-shot-chart.pdf   : kevin durant's shot chart in pdf
#     klay-thompson-shot-chart.pdf  : klay thompson's shot chart in pdf
#     stephen-curry-shot-chart.pdf  : stephen curry's shot chart in pdf
#     gsw-shot-charts.pdf           : all GSW players' shot charts in pdf
#     gsw-shot-charts.png           : all GSW players' shot charts in png

library(ggplot2)
library(jpeg)
library(grid)


court_file <- "../images/nba-court.jpg"


court_image <- rasterGrob(
  readJPEG(court_file),
  width = unit(1, "npc"),
  height = unit(1, "npc"))


thompson_shot_chart <- ggplot(data = thompson) +
  annotation_custom(court_image, -250, 250, -50, 420) + 
  geom_point(aes(x = x, y = y, color = shot_made_flag)) + 
  ylim(-50, 420) +
  ggtitle('Shot Chart: Klay Thompson (2016 season)') +
  theme_minimal()

curry_shot_chart <- ggplot(data = curry) +
  annotation_custom(court_image, -250, 250, -50, 420) + 
  geom_point(aes(x = x, y = y, color = shot_made_flag)) + 
  ylim(-50, 420) +
  ggtitle('Shot Chart: Stephen Curry (2016 season)') +
  theme_minimal()

durant_shot_chart <- ggplot(data = durant) +
  annotation_custom(court_image, -250, 250, -50, 420) + 
  geom_point(aes(x = x, y = y, color = shot_made_flag)) + 
  ylim(-50, 420) +
  ggtitle('Shot Chart: Kevin Durant (2016 season)') +
  theme_minimal()

green_shot_chart <- ggplot(data = green) +
  annotation_custom(court_image, -250, 250, -50, 420) + 
  geom_point(aes(x = x, y = y, color = shot_made_flag)) + 
  ylim(-50, 420) +
  ggtitle('Shot Chart: Draymond Green (2016 season)') +
  theme_minimal()

iguodala_shot_chart <- ggplot(data = iguodala) +
  annotation_custom(court_image, -250, 250, -50, 420) + 
  geom_point(aes(x = x, y = y, color = shot_made_flag)) + 
  ylim(-50, 420) +
  ggtitle('Shot Chart: Andre Iguodala (2016 season)') +
  theme_minimal()

gsw_shot_chart <- ggplot(data = five_players) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Charts: GSW (2016 season)') +
  theme_minimal() +
  theme(legend.position = "top") +
  theme(legend.title = element_blank()) +
  facet_wrap(~ name)



pdf(file = "../images/andre-iguodala-shot-chart.pdf", width = 6.5, height = 5)
iguodala_shot_chart
dev.off()

pdf(file = "../images/draymond-green-shot-chart.pdf", width = 6.5, height = 5)
green_shot_chart
dev.off()

pdf(file = "../images/kevin-durant-shot-chart.pdf", width = 6.5, height = 5)
durant_shot_chart
dev.off()

pdf(file = "../images/klay-thompson-shot-chart.pdf", width = 6.5, height = 5)
thompson_shot_chart
dev.off()

pdf(file = "../images/stephen-curry-shot-chart.pdf", width = 6.5, height = 5)
curry_shot_chart
dev.off()

pdf(file = "../images/gsw-shot-charts.pdf", width = 8, height = 7)
gsw_shot_chart
dev.off()

png(filename = "../images/gsw-shot-charts.png", units = "in", width = 8, height = 7, res = 150)
gsw_shot_chart
dev.off()
