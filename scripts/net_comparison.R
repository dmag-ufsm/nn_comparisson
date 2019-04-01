getwd()
setwd("DMAG/nn_comparisson/datasets/")

library(neuralnet)
library(parallel)
library(doParallel)

abalone = read.csv("b1.csv")
arrythmia = read.csv("b2.csv")
audiology = read.csv("b3.csv")
balance = read.csv("b4.csv")
breast_cancer = read.csv("b5.csv")
car_eval = read.csv("b6.csv")
CM1 = read.csv("b7.csv")
datatrieve = read.csv("b8.csv")
desharnais = read.csv("b9.csv")
ecoli = read.csv("b10.csv") #Error, "names" of wrong size maybe
echo_cardiogram = read.csv("b11.csv")
glass = read.csv("b12.csv")
heart_cleveland = read.csv("b13.csv")
heart_statlog = read.csv("b14.csv")
hepatitis = read.csv("b15.csv")
JM1 = read.csv("b16.csv")
kr_vs_kp = read.csv("b17.csv")
MW1 = read.csv("b18.csv")
pima_diabetes = read.csv("b19.csv")
post_operative = read.csv("b20.csv")
primary_tumor = read.csv("b21.csv")
reuse = read.csv("b22.csv")
solar_flare = read.csv("b23.csv")
tic_tac_toe = read.csv("b24.csv") #Also error
thyroid_allhyper = read.csv("b25.csv")
thyroid_hypothyroid = read.csv("b26.csv") #and here
thyroid_sick_euthyroid = read.csv("b27.csv")
wbdc = read.csv("b28.csv")
wisconsin = read.csv("b29.csv")
wine = read.csv("b30.csv")
yeast = read.csv("b31.csv") #and i guess here
zoo = read.csv("b32.csv")



normalize = function(x){
  return (x-min(x))/(max(x) - min(x))
}


#defines the range of hidden layer sizes
layer_range = 3
hidden_layers = 4

#core numbers and config
no_cores = max(1, detectCores()-1)


work <<- function(l1, l2, l3, l4, formula, data.train, data.test){
  set.seed(42)
  NN = neuralnet(formula, data.train, hidden = c(l1, l2, l3, l4), stepmax = 1e+07, linear.output=T)
  previsao <- compute(NN, data.test[,-ncol(data.test)])

  mse <- sum((data.test[,ncol(data.test)] - previsao$net.result)^2 / nrow(data.test))
  return (c(l1,l2,l3,l4,mse))
}

data <- desharnais
data_name <- "desharnais"
f_work <- work

calculate = function(data, data_name, f_work){
  amostra = 0.7 * ncol(data)
  set.seed(23)
  indice = sample(seq_len(ncol(data)), size=amostra)
  
  vars = names(data)  
  
  must_convert <- sapply(data, is.factor)
  temp <- sapply(data[,must_convert], unclass)
  data.norm <- cbind(data[,!must_convert], temp)
  if (class(temp) == "integer") { 
    x <- names(data.norm)
    x[length(x)] <- vars[must_convert]
    names(data.norm) <- x 
  }
  data.norm = data.norm[,vars]
  
  
  data.norm = as.data.frame(lapply(data.norm, normalize))
  
  goal_class = tail(vars, n=1)
  formula = as.formula(paste(paste(goal_class, " ~ ", collapse = ""), paste(vars[vars!=goal_class], collapse = " + ")))
  ll = max(ncol(data.norm) - layer_range, -1)
  lh = ncol(data.norm) + layer_range
  tamanho_base = (lh-ll+1)**hidden_layers
  base = matrix(nrow = tamanho_base, ncol=hidden_layers)
  a <- 1
  for(i in ll:lh){
    for(j in ll:lh){
      for(k in ll:lh){
        for(l in ll:lh){
          base[a, 1] <- i
          base[a, 2] <- j
          base[a, 3] <- k
          base[a, 4] <- l
          a <- a + 1
        }
      }
    }
  }
  
  data.train = data.norm[indice, ]
  data.test = data.norm[-indice, ]
  
  cluster <- makeCluster(no_cores)
  registerDoParallel(cluster)
  
  result <- foreach(index = 1:tamanho_base,
                    .combine = rbind,
                    # .export = c("formula", "data.train", "data.test"),
                    .packages = c("neuralnet")
  ) %dopar% f_work(base[index,1],base[index,2],base[index,3],base[index,4], formula, data.train, data.test)
  
  stopCluster(cluster)
  colnames(result) = c("1st layer", "2nd layer", "3rd layer", "4th layer", "MSE")
  write.csv(result, file=paste(data_name, "csv", collapse = "."), row.names=F)
  
}

calculate(abalone,"abalone", work)
calculate(arrythmia,"arrythmia", work)
calculate(audiology,"audiology", work)
calculate(balance,"balance", work)
calculate(breast_cancer,"breast_cancer", work)
calculate(car_eval,"car_eval", work)
calculate(CM1,"CM1", work)
calculate(datatrieve,"datatrieve", work)
calculate(desharnais,"desharnais", work)
calculate(ecoli,"ecoli", work)
calculate(echo_cardiogram,"echo_cardiogram", work)
calculate(glass,"glass", work)
calculate(heart_cleveland,"heart_cleveland", work)
calculate(heart_statlog,"heart_statlog", work)
calculate(hepatitis,"hepatitis", work)
calculate(JM1,"JM1", work)
calculate(kr_vs_kp,"kr_vs_kp", work)
calculate(MW1,"MW1", work)
calculate(pima_diabetes,"pima_diabetes", work)
calculate(post_operative,"post_operative", work)
calculate(primary_tumor,"primary_tumor", work)
calculate(reuse,"reuse", work)
calculate(solar_flare,"solar_flare", work)
calculate(tic_tac_toe,"tic_tac_toe", work)
calculate(thyroid_allhyper,"thyroid_allhyper", work)
calculate(thyroid_hypothyroid,"thyroid_hypothyroid", work)
calculate(thyroid_sick_euthyroid,"thyroid_sick_euthyroid", work)
calculate(wbdc,"wbdc", work)
calculate(wisconsin,"wisconsin", work)
calculate(wine,"wine", work)
calculate(yeast,"yeast", work)
calculate(zoo,"zoo", work)
