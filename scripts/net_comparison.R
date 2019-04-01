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



normalize = function(x){
	return (x-min(x))/(max(x) - min(x))
}


#defines the range of hidden layer sizes
layer_range = 3
hidden_layers = 4

work = function(l1, l2, l3, l4, formula, data.train, data.test){
	set.seed(42)
	NN = neuralnet(formula, data.train, hidden = c(l1, l2, l3, l4), stepmax = 1e+07, linear.output=T)
	previsao <- compute(NN, data.test[,-tail(data.test)])

	mse <- sum((data.test[,tail(data.test)] - previsao$net.result) **2 / nrow(data.test))
	return mse

}

calculate = function(data){
	amostra = 0.7 * ncol(data)
	set.seed(23)
	indice = sample(seq_len(ncol(data), size=amostra))
	data.norm = as.matrix(lapply(data, normalize))
	vars = names(data.norm)
	goal_class = tail(vars, n=1)
	formula = as.formula(paste(paste(goal_class, " ~ ", collapse = ""), paste(vars[vars!=goal_class], collapse = " + ")))
	ll = max(ncol(data.norm) - layer_range, -1)
	lh = ncol(data.norm) + layer_range
	tamanho_base = (lh-ll+1)**hidden_layers
	base = matrix(nrow = tamanho_base, ncol=hidden_layers)
	a = 1
	for(i in ll:lh){
		for(j in ll:lh){
			for(k in ll:lh){
				for(l in ll:lh){
					base[a, 1] <- i
					base[a, 2] <- j
					base[a, 3] <- k
					base[a, 4] <- l
					a = a + 1
				}
			}
		}
	}

	


}


