classes <- function(dataframe){
  col_names <- names(dataframe)
  x <- rbind(class(dataframe[,col_names[1]]))
  for (i in 2:length(col_names)){
    x <- rbind(x,class(dataframe[,col_names[i]]))
  }
  row.names(x) <- col_names
  colnames(x) <- "Class"
  return(x)
}

mrange <- function(data){return(max(data) - min(data))}


get_mode <- function(data){
  unique_data <- unique(data)
  unique_data[which.max(tabulate(match(data,unique_data)))]
}

# I use excess kurtosis to match against a normal distribution
mykurtosis <- function(x) {  
  m4 <- mean((x - mean(x, na.rm = T))^4,na.rm = T)
  kurt <- m4/(sd(x, na.rm = T)^4)-3  
  kurt
}

myskewness <-  function(x) {
  m3 <- (mean(x, na.rm = T) - median(x, na.rm = T))*3
  skew <- m3/(sd(x, na.rm = T))
  skew
}

###### Optimising Conf Matrix ####
#test <- matrix(data = c(5,6,2,4,9,8,25,47,3,5,6,89,2,0,7,2,46,866,2,8,200,15,6,79,8), nrow = 5, ncol = 5, byrow = T)

permutations <- function(n){
  if(n==1){
    return(matrix(1))
  } else {
    sp <- permutations(n-1)
    p <- nrow(sp)
    A <- matrix(nrow=n*p,ncol=n)
    for(i in 1:n){
      A[(i-1)*p+1:p,] <- cbind(i,sp+(sp>=i))
    }
    return(A)
  }
}

optim_confusion <- function(matrix){
  lenght <- nrow(matrix)
  width <- ncol(matrix)
  best <- sum(diag(matrix))
  perms <- permutations(width)
  best_cols <- perms[1,]
    for(rows in 2:nrow(perms)){
    new <- sum(diag(matrix[,perms[rows,]]))
    if(new > best){
      best <- new
      best_cols <- perms[rows,]
    }
  }
  return(matrix[,best_cols])
}

Truepos <- function(optimised_conf){
  return(sum(diag(optimised_conf))/sum(optimised_conf))
}

Purity <- function(optimised_conf){ 
  col_max = apply(optimised_conf, 2, max)
  return(sum(col_max)/sum(optimised_conf))
}


Precision <- function(cm){ 
  # This allows dataframes to be input as well
  cm <- as.matrix(cm)
  return(diag(cm) / colSums(cm))
}
Recall <-  function(cm){
  cm <- as.matrix(cm)
  return(diag(cm)/rowSums(cm))
}

F1 <- function(precision,recall){
  2 * precision * recall / (precision + recall)
}


confusion_table_tiles <- function(confusion_table, title){
  confuzzed <- as.data.frame(confusion_table)
  confuzzed$Var1 <- factor(confuzzed$Var1,levels = rev(unique(confuzzed$Var1)))
  confuzzed$Var2 <- factor(confuzzed$Var2 )
  
  p <- ggplot(confuzzed,aes(x = Var2, y = Var1, fill = Freq)) + geom_tile(colour = "black") +
    geom_text(aes(label=Freq), color="black") +
    ggtitle(title) +
    scale_fill_distiller(palette="Blues", direction=1) +
    xlab("Cluster") +
    ylab("Class")
  print(p)
  ggsave(paste(title,".pdf",sep = "_"))
}

internal_metrics <- function(distance,main_cluster,second_cluster){
  statisticsKM5 <-  cluster.stats(distance, main_cluster, second_cluster)
  km_adim <- mean(statisticsKM5$diameter)
  km_mad <- mean(statisticsKM5$average.distance)
  km_mmd <- mean(statisticsKM5$median.distance)
  km_e <- statisticsKM5$entropy
  tab <- rbind(km_adim,km_mad,km_mmd,km_e)
  return(tab)
}

aggs <- function(cm){
  tps <- sum(diag(cm))
  tpp <- Truepos(cm)
  pur <- Purity(cm)
  tab <- cbind(tps,tpp,pur)
  return(tab)
}


externals <- function(cm){
  pre <- mean(Precision(cm))
  rec <- mean(Recall(cm))
  f1 <- mean(F1(pre,rec))
  tab <- cbind(pre,rec,f1)
  colnames(tab) <- c("Precision", "Recall", "F1")
  return(tab)
}

con_DF_maker <- function(group,data_vector){
  return(data.frame(Data = group,
                    Class = c(rep("A",5),rep("B",5),rep("C",5),rep("D",5),rep("E",5)),
                    Classification = rep(c("a","b","c","d","e")),
                    Values = data_vector))
}
