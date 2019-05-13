if(!is.element("tidyverse", installed.packages()[,1]))
  install.packages("tidyverse")
library(tidyverse)

# if needed:
getwd()
setwd("workspace/nn_comparisson/results/")

# Gets all the files on the working dir, and select all of the "NB" files for plotting
files = list.files()
files = grep("NB[0-9]*L[0-9]*.csv", files, perl = T, value = T)

index = 1
values = c()
for(i in files){
  try({
    a = read.csv(i)
    values[index] = min(a$MSE)
    index = index + 1
  })
}

ind = paste(files)
ids = seq(1:length(files))

# For some reason, values is starting at 2.
data = data.frame(id=ids, 
                  individual = ind,
                  value=values)

label_data = data
bar_number = nrow(label_data)
angle=90-360*(label_data$id-0.5)/bar_number
label_data$hjust<-ifelse(angle < -90, 1, 0)
label_data$angle<-ifelse(angle<-90, angle+180, angle)

p = ggplot(data, aes(x=as.factor(ids), y=values))+
  geom_bar(stat="identity", fill=alpha("skyblue", 0.7))+
  ylim(-10, 50)+
  theme_minimal()+
  theme(
    axis.text=element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")
  )+
  coord_polar(start=0)+
  geom_text(data=label_data, aes(x=ids, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold", alpha=0.6, size=2.6, angle=label_data$angle, inherit.aes = F)


# for testing:
#p = ggplot(data)
p
