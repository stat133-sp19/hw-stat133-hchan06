# title       : Summary Data of GSW Players
# description : Export ouput summary data of the GSW players' shooting statistics
# input(s)    : 
#     curry     : stephen curry data frame
#     iguodala  : andre iguodala data frame
#     durant    : kevin durant data frame
#     green     : draymond green data frame
#     thompson  : klay thompson data frame
# output(s)   : what are the outputs created when running the script?
#    stephen-curry-summary.txt  : stephen curry shooting statistic summary in txt file
#    andre-thompson-summary.txt : andre thompson shooting statistic summary in txt file
#    draymond-green-summary.txt : draymond green shooting statistic summary in txt file
#    kevin-durant-summary.txt   : kevin durant shooting statistic summary in txt file
#    klay-thompson-summary.txt  : klay thompson shooting statistic summary in txt file
#    shots-data.csv             : all GSW players shooting data in csv file


curry <-  read.csv("../data/stephen-curry.csv", stringsAsFactors = FALSE)
iguodala <- read.csv("../data/andre-iguodala.csv", stringsAsFactors = FALSE)
green <- read.csv("../data/draymond-green.csv", stringsAsFactors = FALSE)
durant <- read.csv("../data/kevin-durant.csv", stringsAsFactors = FALSE)
thompson <- read.csv("../data/klay-thompson.csv", stringsAsFactors = FALSE)

curry$name = 'Stephen Curry'
iguodala$name = 'Andre Iguodala'
durant$name = 'Kevin Durant'
green$name = 'Draymond Green'
thompson$name = 'Klay Thompson'


curry$shot_made_flag[curry$shot_made_flag == 'y'] = "shot_yes"
curry$shot_made_flag[curry$shot_made_flag == 'n'] = "shot_no"
iguodala$shot_made_flag[iguodala$shot_made_flag == 'y'] = "shot_yes"
iguodala$shot_made_flag[iguodala$shot_made_flag == 'n'] = "shot_no"
green$shot_made_flag[green$shot_made_flag == 'y'] = "shot_yes"
green$shot_made_flag[green$shot_made_flag == 'n'] = "shot_no"
durant$shot_made_flag[durant$shot_made_flag == 'y'] = "shot_yes"
durant$shot_made_flag[durant$shot_made_flag == 'n'] = "shot_no"
green$shot_made_flag[green$shot_made_flag == 'y'] = "shot_yes"
green$shot_made_flag[green$shot_made_flag == 'n'] = "shot_no"
thompson$shot_made_flag[thompson$shot_made_flag == 'y'] = "shot_yes"
thompson$shot_made_flag[thompson$shot_made_flag == 'n'] = "shot_no"


curry$minute = 12*curry$period - curry$minutes_remaining
iguodala$minute = 12*iguodala$period - iguodala$minutes_remaining
durant$minute = 12*durant$period - durant$minutes_remaining
green$minute = 12*green$period - green$minutes_remaining
thompson$minute = 12*thompson$period - thompson$minutes_remaining



sink(file = '../output/stephen-curry-summary.txt')
summary(curry)
sink()

sink(file = '../output/andre-iguodala-summary.txt')
summary(iguodala)
sink()

sink(file = '../output/draymond-green-summary.txt')
summary(green)
sink()

sink(file = '../output/kevin-durant-summary.txt')
summary(durant)
sink()

sink(file = '../output/klay-thompson-summary.txt')
summary(thompson)
sink()

five_players <- rbind(curry, iguodala, durant, green, thompson)

write.csv(
  x = five_players,
  file = '../data/shots-data.csv'
)

sink(file = '../output/shots-data-summary.txt')
summary(five_players)
sink()
