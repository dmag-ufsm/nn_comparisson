library(readr) # CSVs from URL
dataURL <- vector()
base <- list()

n <- 32
noConv <- c(2,5,11,15,22)#no convergence
for (i in 1:n){
  if (!(i %in% noConv)){
    dataURL[i] <- paste('https://raw.githubusercontent.com/dmag-ufsm/nn_comparisson/master/results/b',i,'.csv', sep='')
    base[[i]] <- read_csv(dataURL[i])
  }
}



# ------------------------- Let's see what happens with B1 ------------------------
b1 <- base[[1]]
summary(b1$MSE)

# Best MSE with 12 neurons in the first layer.
# 8.28
b1[b1$MSE == min(b1$MSE), ]

# So, the average for 8 neurons is...
# 56.3
# min = 8.68
summary(b1[b1$`1st layer` == 8, 5])
# -------------------------------------------------



# ------- Now let's see the best MSE  for all datasets  ------------
foo <- ''
for (i in 1:32){
  if (!(i %in% noConv)){
    foo <- rbind(foo, (base[[i]][base[[i]]$MSE == min(base[[i]]$MSE), ]))
  }
}
foo<- foo[-1,]
foo <- cbind(seq(1:32)[!(1:32 %in% noConv)], foo)
names(foo)[1] <- 'Base'
write.csv(foo,'confByMSE.csv',row.names=F)
# -------------------------------------------------
