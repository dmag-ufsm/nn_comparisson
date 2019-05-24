for (package in c("fmsb")) {
  if (!package %in% rownames(installed.packages())){
    message (paste("Installing", package))
    install.packages(package)
  }
}

library(fmsb)

# Create data: note in High school for Jonathan:
# data=as.data.frame(matrix( sample( 2:20 , 29 , replace=T) , ncol=29))
data.1=c()
data.2=c()
data.3=c()
data.4=c()
data=c()
skip=c(2,10)
for (i in 1:31) {
  if (!(i %in% skip)){
    dataset.1 <- cbind(read.csv(paste("neo_results/NB",i,"L1.csv",sep="")),source=c(paste("B",i,sep="")))
    dataset.2 <- cbind(read.csv(paste("neo_results/NB",i,"L2.csv",sep="")),source=c(paste("B",i,sep="")))
    dataset.3 <- cbind(read.csv(paste("neo_results/NB",i,"L3.csv",sep="")),source=c(paste("B",i,sep="")))
    dataset.4 <- cbind(read.csv(paste("neo_results/NB",i,"L4.csv",sep="")),source=c(paste("B",i,sep="")))
    best.1 <- dataset.1[order(dataset.1$MSE),][1:1,]
    best.2 <- dataset.2[order(dataset.2$MSE),][1:1,]
    best.3 <- dataset.3[order(dataset.3$MSE),][1:1,]
    best.4 <- dataset.4[order(dataset.4$MSE),][1:1,]
    data.1 <- merge(data.1,best.1,all=T)
    data.2 <- merge(data.2,best.2,all=T)
    data.3 <- merge(data.3,best.3,all=T)
    data.4 <- merge(data.4,best.4,all=T)
  }
}

# lower <- min(data$MSE)
# higher <- max(data$MSE)

lowest <- min(data.1$MSE,data.2$MSE,data.3$MSE,data.4$MSE)

data.1$MSEN = log10(data.1$MSE * 1 / lowest)
data.2$MSEN = log10(data.2$MSE * 1 / lowest)
data.3$MSEN = log10(data.3$MSE * 1 / lowest)
data.4$MSEN = log10(data.4$MSE * 1 / lowest)

# highest <- max(data$MSEN)

data$L1 = data.1$MSEN
data$L2 = data.2$MSEN
data$L3 = data.3$MSEN
data$L4 = data.4$MSEN
data$source = data.1$source

data <- data.frame(data)

row.names(data) <- data$source

data.o <- data.frame(data[order(data$source, decreasing = T), ])
data.t <- as.data.frame(t(data.o[c("L1","L2","L3","L4")]))

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
data.t.mm=rbind(5, 0, data.t)

# The default radar chart proposed by the library:
# radarchart(data.t.mm, maxmin=T)

colors_border=c( 
  rgb(0.2,0.5,0.5,0.9), 
  rgb(0.8,0.2,0.5,0.9), 
  rgb(0.7,0.5,0.1,0.9),
  rgb(0.5,0.5,0.5,0.9) 
)

colors_in=c( 
  rgb(0.2,0.5,0.5,0.2), 
  rgb(0.8,0.2,0.5,0.2), 
  rgb(0.7,0.5,0.1,0.2), 
  rgb(0.5,0.5,0.5,0.2) 
)


# Custom the radarChart !
radarchart( data.t.mm  , axistype=0 ,  maxmin=T,

            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=2 , plty=1

            #custom the grid
            # cglcol="gray", cglty=1, axislabcol="gray", caxislabels=exp(log(10)*seq(0,highest,by=1)), cglwd=0.8,

            #custom labels
            # vlcex=.8
)

legend(x=0.7, y=1, legend = c("A","B","C","D"), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)
