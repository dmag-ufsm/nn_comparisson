getwd()

for (package in c("neuralnet","doSNOW","curl","readr","gtools")) {
  if (!package %in% rownames(installed.packages())){
    message (paste("Installing", package))
    install.packages(package)
  }
}

library(neuralnet)
library(parallel)
library(snow)
library(doSNOW)
library(readr)
library(gtools)

scale_column = function(x){
  y = x[!is.na(x)]
  avg = sum(y)/length(y)
  x[is.na(x)] = avg
  d = max(x)-min(x)
  n = x-min(x)
  
  # In case max and min are same value
  if (d == 0){
    d = max(x)
    n = x
  }
  return (n/d)
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
no_cores <- max(1, detectCores()-1)
reuse_previous_results <- FALSE
layer_range <- 3
min_layers <- 1
max_layers <- 2


tryCatch({
  log.socket <- make.socket(port=4000)
},error = function(e){
  if (!exists("log.socket") || class(log.socket) != 'socket'){
    log.socket <- NULL
    message("Not connected to a socket, log will not be available")
    message(e)
    message("To listen to the log on port 4000 run he following command on Linux (or WSL):")
    message("nc -l 4000")
  }
})


Log = function(text, ...) {
  msg <- sprintf(paste0(as.character(Sys.time()), ": ", text, "\n"), ...)
  cat(msg)
  if (exists("log.socket") && !is.null(log.socket)) {
    write.socket(log.socket, msg)
  }
}

calculate = function(data,data_name,existing_data = NULL,layer_range = 3,layers = 4){
  amostra = 0.7 * ncol(data)
  set.seed(23)
  indice = sample(seq_len(ncol(data)), size=amostra)
  
  data.norm = normalize(data)
  
  vars = names(data.norm)
  goal_class = tail(vars, n=1)
  formula = as.formula(paste(paste(goal_class, " ~ ", collapse = ""), paste(vars[vars!=goal_class], collapse = " + ")))
  ll = max(ncol(data.norm) - layer_range, 1)
  lh = ncol(data.norm) + layer_range
  
  base <- permutations(n=lh-ll+1,r=layers,ll:lh, repeats.allowed=TRUE)
  
  data.train = data.norm[indice, ]
  data.test = data.norm[-indice, ]
  
  progressBar <- txtProgressBar(min = 0, max = nrow(base), style=3)
  progress <- function(n) setTxtProgressBar(progressBar, n)
  opts <- list(progress=progress)
  
  
  if(!exists("log.socket")){
    log.socket <- NULL
  }

  cluster <- makeSOCKcluster(no_cores)
  registerDoSNOW(cluster)
  
  result <- foreach(index = 1:nrow(base),
                    .combine = rbind,
                    .packages = c("neuralnet"),
                    .export = c("Log","log.socket"),
                    .options.snow = opts
  ) %dopar% {
    
    hidden <- base[index,]
    
    if (!exists("existing_data")) {
      existing_data <- NULL
    }
    
    tryCatch({previous <- existing_data}, error = function(e) { previous <- NULL })
    
    data_line <- data_name
    for (i in 1:length(hidden)){
      if (is.null(previous)) {
        previous <- previous[previous[,paste("Layer.",i,sep="")]==hidden[i],]
      }
      data_line <- paste(data_line,"[",hidden[i],"]", sep="")
    }

    if (!is.null(previous) && nrow(previous) == 1) {
      Log("recovering %s",data_line)
      return (c(base[index,], previous$MSE, previous$User.Time, previous$CPU.Time, TRUE))
    }

    Log("processing %s",data_line)

    set.seed(42)
    
    time <- system.time(NN <- neuralnet(formula, data.train, hidden, stepmax = 1e+07, linear.output=T))
    previsao <- compute(NN, data.test[,-ncol(data.test)])
    
    mse <- sum((data.test[,ncol(data.test)] - previsao$net.result)^2 / nrow(data.test))
    return (c(base[index,], mse, time[1], time[2], FALSE))
  }

  stopCluster(cluster)
  
  close(progressBar)
  
  colnames(result) = c(sapply(1:layers,function(i){ 
      return(paste("Layer",i)) 
  }), "MSE", "User Time", "CPU Time", "Reutilized")
  return(result)
}

dataURL <- vector()
base <- list()
n <- 32
noConv <- c()#no convergence
for (i in 1:n){
  if (!(i %in% noConv)){
    dataURL[i] <- paste('https://raw.githubusercontent.com/dmag-ufsm/nn_comparisson/master/datasets/B',i,'.csv', sep='')
    try({
      data <- read.csv(dataURL[i])
      for (j in min_layers:max_layers) {
        
        layer <- ifelse(j == 1 , "layer" , "layers")
        message(format(Sys.time(), "[%Y-%m-%d %X]"), " Processing B",i,".csv with ",j," hidden ",layer)
        
        data_name = paste('B',i,'L',j, sep="")
        
        if (reuse_previous_results && file.exists(paste('results/',data_name,'.csv', sep=""))){
          existing_data <- read.csv(paste('results/',data_name,'.csv', sep=""))
        } else {
          existing_data <- NULL
        }
        
        result <- calculate(data,data_name,existing_data,layer_range,layers=j)
        write.csv(result, file=paste('results/N',data_name,'.csv', sep=""), row.names=FALSE)
      }
    })
  }
}
message(format(Sys.time(), "[%Y-%m-%d %X]"), " Finished")
