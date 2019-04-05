getwd()

if (!"doParallel" %in% rownames(installed.packages())){
  message ("Installing doParallel")
  install.packages("doParallel")
}

if (!"curl" %in% rownames(installed.packages())){
  message ("Installing curl")
  install.packages("curl")
}

if (!"gtools" %in% rownames(installed.packages())){
  message ("Installing gtools")
  install.packages("gtools")
}

library(neuralnet)
library(parallel)
library(doParallel)
library(readr)
library(gtools)

scale_column = function(x){
  return (x-min(x))/(max(x) - min(x))
}

normalize = function(data) {
  data.norm <- as.data.frame(sapply(data, unclass))
  
  # vars = names(data)
  # must_convert <- sapply(data, is.factor)
  # temp <- sapply(data[,must_convert], unclass)
  # if (class(temp) != "list") {
  # 
  #   data.norm <- cbind(data[,!must_convert], temp)
  #   if (class(temp) == "integer") {
  #     x <- names(data.norm)
  #     x[length(x)] <- vars[must_convert]
  #     names(data.norm) <- x
  #   }
  #   data.norm = data.norm[,vars]
  # } else {
  #   data.norm = data
  # }
  return(as.data.frame(lapply(data.norm, scale_column)))
}

#core numbers and config
no_cores = max(1, detectCores()-1)

calculate = function(data,layer_range = 3,layers = 4){
  amostra = 0.7 * ncol(data)
  set.seed(23)
  indice = sample(seq_len(ncol(data)), size=amostra)
  
  data.norm = normalize(data)
  
  vars = names(data.norm)
  goal_class = tail(vars, n=1)
  formula = as.formula(paste(paste(goal_class, " ~ ", collapse = ""), paste(vars[vars!=goal_class], collapse = " + ")))
  ll = max(ncol(data.norm) - layer_range, -1)
  lh = ncol(data.norm) + layer_range
  
  base <- permutations(n=lh-ll+1,r=layers,ll:lh, repeats.allowed=TRUE)
  
  data.train = data.norm[indice, ]
  data.test = data.norm[-indice, ]

  cluster <- makeCluster(no_cores)
  registerDoParallel(cluster)

  result <- foreach(index = 1:nrow(base),
                    .combine = rbind,
                    .packages = c("neuralnet")
  ) %dopar% {
    set.seed(42)
    time <- system.time(NN <- neuralnet(formula, data.train, hidden = base[index,], stepmax = 1e+07, linear.output=T))
    previsao <- compute(NN, data.test[,-ncol(data.test)])
    
    mse <- sum((data.test[,ncol(data.test)] - previsao$net.result)^2 / nrow(data.test))
    return (c(base[index,],mse, time[1], time[2]))
  }

  stopCluster(cluster)
  
  colnames(result) = c(sapply(1:layers,function(i){ 
      return(paste("Layer ",i)) 
  }), "MSE", "User Time", "CPU Time")
  return(result)
}

dataURL <- vector()
base <- list()
n <- 32
noConv <- c(2)#no convergence
for (i in 1:n){
  if (!(i %in% noConv)){
    dataURL[i] <- paste('https://raw.githubusercontent.com/dmag-ufsm/nn_comparisson/master/datasets/B',i,'.csv', sep='')
    message("Processing B",i,".csv")
    try({
      data <- read.csv(dataURL[i])
      for (j in 1:4) {
        result <- calculate(data,layer_range = 3,layers = j)
        write.csv(result, file=paste('results/B',i,'L',j,'.csv', sep=""), row.names=FALSE)
      }
    })
  }
}
