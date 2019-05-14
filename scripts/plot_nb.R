if(!is.element("tidyverse", installed.packages()[,1]))
  install.packages("tidyverse")
library(tidyverse)

# if needed:
getwd()
setwd("workspace/nn_comparisson/results/")

# Gets all the files on the working dir, and select all of the "NB" files for plotting
files = list.files()
files = grep("NB[0-9]*L[0-9]*.csv", files, perl = T, value = T)

plots = c()
p = 1
for(i in 1:32){
  fi = grep(paste("NB", i, "L[0-9]*.csv", sep=""), files, perl=T, value = T)
  index = 1
  best = c()
  avg = c()
  worst = c()
  try({
    for( j in fi ){
      a = read.csv(j)
      best[index] = min(a$MSE)
      avg[index] = mean(a$MSE)
      worst[index] = max(a$MSE)
      index = index + 1
    }
  })
  dm = data.frame(rbind(best, avg, worst))
  plots[p] = plot(dm)
  p = p + 1
}

# wat
#data = data.frame(id=ids, 
#                  individual = ind,
#                  value=values)
