

df <- read_and_prepare_titanic_dataset("~/Downloads/titanic3.csv")
df <- mtcars
str(df)
str(df2)

df <- read.csv('https://raw.githubusercontent.com/jbryer/CompStats/master/Data/titanic3.csv')
df <- data.frame(df$survived, df$pclass, df$sex, df$age, df$sibsp, df$parch)
colnames(df) <- c("survived","pclass","sex","age","sibsp","parch")
df <- df[complete.cases(df),]
df$cyl <- as.factor(df$cyl)
df$vs <- as.factor(df$vs)
df$am <- as.factor(df$am)
#I should I more things 

df$pclass <- as.factor(df$pclass)
df$survived <- as.factor(df$survived)
df$sex <- as.factor(df$sex)
str(df)


log_reg <- function(df, size=10) {
  N <- nrow(df)
  size=10
  
  df <- df[sample(N),]
  
  num <- floor(N/size)
  rest <- N - num * size
  ncv <- cumsum(c(rep(size,num), rest))
  
  predictions <- data.frame(vs = df$vs, pred = NA)
  
  for(n in ncv) {
    v <- rep(TRUE, N)
    v[(n-size+1):n] <- FALSE
    
    lr <- glm(vs ~ ., data = df[v,], family = binomial(logit))
    predictions[!v,"pred"] <- predict(lr, newdata=df[!v,], type="response")
  }
  
  return(predictions)
}

predictions <- log_reg(df, size=10)
str(predictions)



plot_pred_type_distribution <- function(df, threshold) {
  v <- rep(NA, nrow(df))
  v <- ifelse(df$pred >= threshold & df$vs == 1, "TP", v)
  v <- ifelse(df$pred >= threshold & df$vs == 0, "FP", v)
  v <- ifelse(df$pred < threshold & df$vs == 1, "FN", v)
  v <- ifelse(df$pred < threshold & df$vs == 0, "TN", v)
  
  df$pred_type <- v
  
  ggplot(data=df, aes(x=vs, y=pred)) + 
    geom_violin(fill=rgb(1,1,1,alpha=0.6), color=NA) + 
    geom_jitter(aes(color=pred_type), alpha=0.6) +
    geom_hline(yintercept=threshold, color="red", alpha=0.6) +
    scale_color_discrete(name = "type") +
    labs(title=sprintf("Threshold at %.2f", threshold))
}

plot_pred_type_distribution(predictions, 0.7)


calculate_roc <- function(df, cost_of_fp, cost_of_fn, n=100) {
  tpr <- function(df, threshold) {
    sum(df$pred >= threshold & df$am == 1) / sum(df$am  == 1)
  }
  
  fpr <- function(df, threshold) {
    sum(df$pred >= threshold & df$am  == 0) / sum(df$am  == 0)
  }
  
  cost <- function(df, threshold, cost_of_fp, cost_of_fn) {
    sum(df$pred >= threshold & df$survived == 0) * cost_of_fp + 
      sum(df$pred < threshold & df$survived == 1) * cost_of_fn
  }
  
  roc <- data.frame(threshold = seq(0,1,length.out=n), tpr=NA, fpr=NA)
  roc$tpr <- sapply(roc$threshold, function(th) tpr(df, th))
  roc$fpr <- sapply(roc$threshold, function(th) fpr(df, th))
  roc$cost <- sapply(roc$threshold, function(th) cost(df, th, cost_of_fp, cost_of_fn))
  
  return(roc)
}

roc <- calculate_roc(predictions, 1, 2, n = 100)
## I should add more things here 




plot_roc <- function(roc, threshold, cost_of_fp, cost_of_fn) {
  library(gridExtra)
  norm_vec <- function(v) (v - min(v))/diff(range(v))
  
  idx_threshold = which.min(abs(roc$threshold-threshold))
  
  col_ramp <- colorRampPalette(c("green","orange","red","black"))(100)
  col_by_cost <- col_ramp[ceiling(norm_vec(roc$cost)*99)+1]
  p_roc <- ggplot(roc, aes(fpr,tpr)) + 
    geom_line(color=rgb(0,0,1,alpha=0.3)) +
    geom_point(color=col_by_cost, size=4, alpha=0.5) +
    coord_fixed() +
    geom_line(aes(threshold,threshold), color=rgb(0,0,1,alpha=0.5)) +
    labs(title = sprintf("ROC")) + xlab("FPR") + ylab("TPR") +
    geom_hline(yintercept=roc[idx_threshold,"tpr"], alpha=0.5, linetype="dashed") +
    geom_vline(xintercept=roc[idx_threshold,"fpr"], alpha=0.5, linetype="dashed")
  
  p_cost <- ggplot(roc, aes(threshold, cost)) +
    geom_line(color=rgb(0,0,1,alpha=0.3)) +
    geom_point(color=col_by_cost, size=4, alpha=0.5) +
    labs(title = sprintf("cost function")) +
    geom_vline(xintercept=threshold, alpha=0.5, linetype="dashed")
  
  sub_title <- sprintf("threshold at %.2f - cost of FP = %d, cost of FN = %d", threshold, cost_of_fp, cost_of_fn)
  
  grid.arrange(p_roc, p_cost, ncol=2, sub=textGrob(sub_title, gp=gpar(cex=1), just="bottom"))
}

plot_roc(roc, 0.7, 1, 2)



library(pROC)
auc(predictions$vs, predictions$pred)

