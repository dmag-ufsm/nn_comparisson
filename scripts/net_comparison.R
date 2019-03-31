setwd("../datasets/")
library(neuralnet)

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


calculate = function(data, n_hidden){
  amostra = 0.7 * nrow(data)
  set.seed(23)
  indice = sample(seq_len(nrow(data), size=amostra))
  data = normalize(data)
  vars = names(data)
  goal_class = tail(vars, n=1)
  formula = as.formula(paste(paste(goal_class, " ~ ", collapse = ""), paste(vars[vars!=goal_class], collapse = " + ")))
  ll = ncol(data) - 3
  lh = ncol(data) + 3
  tamanho_base = (lh-ll+1)*(lh-ll+1)
  base = matrix(nrow = tamanho_base, ncol=n_hidden)

}


