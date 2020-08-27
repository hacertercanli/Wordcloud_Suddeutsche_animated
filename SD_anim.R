

#Run the 30 selected keywords from the corpus list across the dataset, for 2.5 years of time


Keywords = c("politik","bayern","sport","wirtschaft","kriminalität","gesundheit","coronavirus","fußball","gesellschaft","usa","trump","bundesliga","verkehr","instagram","polizei","freizeit","regionalsport","karriere","familie","gericht","kommunalwahl","spd","digital","covid19","csu","europäische union","auto","wohnen","medizin","bildung")
data = read.delim("Ordered_raw_HT.csv", sep = ";")
data = data[data$Date !="",]
data$Date = as.Date(data$Date, format = "%d/%m/%Y")
data$date = format(data$Date, "%m-%Y")


theset = data.frame("keyword"=rep(Keywords, time=31), date = rep(seq(from = as.Date('2018-01-01'), to = as.Date('2020-07-31'), by='months'), each = 30), size = 0)


#create for loop & two nested for loops, with i and j.

for(i in 1:length(theset$keyword)) {
  train = data[data$date==format(as.Date(theset$date[i], format="%Y/%m/%d"),"%m-%Y"),]
  k = 0
  for (j in 1:length(train)){
    if (str_contains(train$Keywords[j],theset$keyword[i],ignore.case = TRUE)){
      k = k+1
    }
  }
  theset$size[i] = k
  
}
#with geom_text_wordcloud add text to the plot (this blog is super useful >> https://www.datanovia.com/en/blog/gganimate-how-to-create-plots-with-beautiful-animation-in-r/)

gg <- theset %>%
  ggplot(aes(label = keyword, size=size)) +
  geom_text_wordcloud() +
  theme_classic()

#values will change linearly (https://gganimate.com/ helps)
gg2 <- gg + transition_time(date) +
  ease_aes('linear')+
  labs(title = 'date: {frame_time}')

#now animate but the size is tiny, I cannot change it.. 

animate(gg2, duration = 30, fps = 2, width = 400, height = 400, renderer = gifski_renderer())
anim_save("gg_anim_wc.gif", animation = last_animation())





