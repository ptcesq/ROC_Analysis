
rocdata <- function(grp, pred){
  # Author: Kate Nambiar 
  # Date: 3/21/12
  # Site: http://www.rrandomness.com/r/simple-roc-plots-with-ggplot2-part-1/
  # Produces x and y co-ordinates for ROC curve plot
  # Arguments: grp - labels classifying subject status
  #            pred - values of each observation
  # Output: List with 2 components:
  #         roc = data.frame with x and y co-ordinates of plot
  #         stats = data.frame containing: area under ROC curve, p value, upper and lower 95% confidence interval
  
  grp <- as.factor(grp)
  if (length(pred) != length(grp)) {
    stop("The number of classifiers must match the number of data points")
  } 
  
  if (length(levels(grp)) != 2) {
    stop("There must only be 2 values for the classifier")
  }
  
  cut <- unique(pred)
  tp <- sapply(cut, function(x) length(which(pred > x & grp == levels(grp)[2])))
  fn <- sapply(cut, function(x) length(which(pred < x & grp == levels(grp)[2])))
  fp <- sapply(cut, function(x) length(which(pred > x & grp == levels(grp)[1])))
  tn <- sapply(cut, function(x) length(which(pred < x & grp == levels(grp)[1])))
  tpr <- tp / (tp + fn)
  fpr <- fp / (fp + tn)
  roc = data.frame(x = fpr, y = tpr)
  roc <- roc[order(roc$x, roc$y),]
  
  i <- 2:nrow(roc)
  auc <- (roc$x[i] - roc$x[i - 1]) %*% (roc$y[i] + roc$y[i - 1])/2
  
  pos <- pred[grp == levels(grp)[2]]
  neg <- pred[grp == levels(grp)[1]]
  q1 <- auc/(2-auc)
  q2 <- (2*auc^2)/(1+auc)
  se.auc <- sqrt(((auc * (1 - auc)) + ((length(pos) -1)*(q1 - auc^2)) + ((length(neg) -1)*(q2 - auc^2)))/(length(pos)*length(neg)))
  ci.upper <- auc + (se.auc * 0.96)
  ci.lower <- auc - (se.auc * 0.96)
  
  se.auc.null <- sqrt((1 + length(pos) + length(neg))/(12*length(pos)*length(neg)))
  z <- (auc - 0.5)/se.auc.null
  p <- 2*pnorm(-abs(z))
  
  stats <- data.frame (auc = auc,
                       p.value = p,
                       ci.upper = ci.upper,
                       ci.lower = ci.lower
  )
  
  return (list(roc = roc, stats = stats))
}


genSample <- function(n = 1000, threshold = 0.5, positive_percent = 0.50, 
                      true_positive_percent = 0.90, true_negative_percent = 0.9){
# creates a martix of truth values and predicted values 
# for AUC analysis 

 
    below_threshold = threshold-0.00001 # gives us a below threshold number for cut off
    above_threshold = threshold+0.00001 # gives us an above threshold number for cut off 
    negative_percent = 1 - positive_percent # False Percentage - The percentage of the sample N which is marked Negative (Default 0.5 observations)
    
    true_positive = n * true_positive_percent # True Positvive 
    false_positive = n * (1 - true_positive_percent) # False Positive - Truth (Label) (Default 25 Observations)
    
    true_negative <- n * negative_percent 
    false_negative <- n * (1 - true_negative_percent)
    
    
    # Positive labels 
    true_positive_label <- rep(1, true_positive)
    false_positive_label <- rep(0, false_positive)
    true_positive_percentage = runif(true_positive, threshold, 1.0)
    false_positive_percentage = runif(false_positive, below_threshold, 1.0)
    truth_1 <- c(true_positive_label, false_positive_label)
    pred_1 <- c(true_positive_percentage, false_positive_percentage)
    positive <- cbind(truth_1, pred_1)
    
    # Negative numbers 
    true_negative_label <- rep(0, true_negative)
    false_negative_label <- rep(1, false_negative)
    true_negative_percentage <- runif(true_negative, 0.0, below_threshold)
    false_negative_percentage <- runif(false_negative, 0.0, above_threshold)
    truth_2 <- c(true_negative_label, false_negative_label)
    pred_2 <- c(true_negative_percentage, false_negative_percentage) 
    negative <- cbind(truth_2, pred_2)
    
    
    returnMatrix <- rbind(positive, negative)
    colnames(returnMatrix) <- c("truth", "prediction")
    return(returnMatrix)
}


# 
# Main Entry Point 
#


library(ROCR)
library(ggplot2)

# Generate with a 1:1 ratio 
set.seed(1234)
returnMatrix <- as.data.frame(genSample(threshold=0.75))
rm_a <- returnMatrix 
rocData <- as.data.frame(rocdata(returnMatrix$truth, returnMatrix$prediction))
auc <- signif(mean(rocData$stats.auc), 2)
ptitle <- paste("ROC Graph with a 50% Concentration ~ AUC = ", auc)
ggplot(rocData, aes(roc.x, roc.y)) + geom_line() + 
      annotate("segment", x=-Inf, xend=Inf,y=-Inf, yend=Inf, colour="red") + 
      ggtitle(ptitle) +
      xlab("False Positive Rate") + ylab("True Positive Rate")


# PRCurve 
pred <- prediction(returnMatrix$prediction, returnMatrix$truth)
perf1 <- performance(pred, "prec", "rec")
x <- unlist(perf1@x.values)
y <- unlist(perf1@y.values)
PR.data <- as.data.frame(cbind(x,y))


ggplot(PR.data, aes(x,y)) + geom_line() + 
  ggtitle('Precision/Recall with 1:1 Ratio')


# Generate with a 1:10 ratio 


# ROC Graph 
set.seed(1234)
returnMatrix <- as.data.frame(genSample(positive_percent = 0.02, threshold=0.75, true_negative_percent = 0.9)) 
rm_b <- returnMatrix
rocData <- as.data.frame(rocdata(returnMatrix$truth, returnMatrix$prediction))
auc <- signif(mean(rocData$stats.auc), 2)
ptitle <- paste("ROC Graph with 2% Concentration ~ AUC = ", auc)
ggplot(rocData, aes(roc.x, roc.y)) + geom_line() + 
  annotate("segment", x=-Inf, xend=Inf,y=-Inf, yend=Inf, colour="red") + 
  ggtitle(ptitle) +
  xlab("False Positive Rate") + ylab("True Positive Rate")


# PRCurve 
pred <- prediction(returnMatrix$prediction, returnMatrix$truth)
perf1 <- performance(pred, "prec", "rec")
x <- unlist(perf1@x.values)
y <- unlist(perf1@y.values)
PR.data <- as.data.frame(cbind(x,y))


ggplot(PR.data, aes(x,y)) + geom_line() + 
    ggtitle('Precision/Recall with 1:10 Ratio')


#
#  Isolation of Precision Shift 
#

pred <- prediction(rm_a$prediction, rm_a$truth)
perf1 <- performance(pred, "prec")
#plot(perf1, main="Precision 1:1")
#abline(v=0.75, col="red")
x <- unlist(perf1@x.values)
y <- unlist(perf1@y.values)
PR.data1 <- as.data.frame(cbind(x,y))


pred <- prediction(rm_b$prediction, rm_b$truth)
perf2 <- performance(pred, "prec")
#plot(perf2, main="Precision 1:10")
#abline(v=0.75, col="red")
x <- unlist(perf2@x.values)
y <- unlist(perf2@y.values)
PR.data2 <- as.data.frame(cbind(x,y))

precision.err <- as.data.frame(cbind(PR.data1$x, PR.data1$y, PR.data2$y))
colnames(precision.err) <- c("x", "y1", "y2")
precision.err <- subset(precision.err, x<0.75)
precision.err$diff <- precision.err$y1 - precision.err$y2

# plot the graphs togeather to show the differentials 

err.plot <- ggplot(precision.err, aes(x)) + 
            geom_line(aes(y = y1), colour="red") + 
            geom_line(aes(y = y2), colour="blue") + 
            ggtitle('Precision Curves Below 0.75 Cutoff Threshold') +
            xlab("Cut Off") + ylab("Precision")
err.plot






















