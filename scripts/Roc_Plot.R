rocplot.single <- function(grp, pred, title = "ROC Plot", p.value = FALSE){
  require(ggplot2)
  plotdata <- rocdata(grp, pred)
  
  if (p.value == TRUE){
    annotation <- with(plotdata$stats, paste("AUC=",signif(auc, 2), " (P=", signif(p.value, 2), ")", sep=""))
  } else {
    annotation <- with(plotdata$stats, paste("AUC=",signif(auc, 2), " (95%CI ", signif(ci.upper, 2), " - ", signif(ci.lower, 2), ")", sep=""))
  }
  
  p <- ggplot(plotdata$roc, aes(x = x, y = y)) +
    geom_line(aes(colour = "")) +
    geom_abline (intercept = 0, slope = 1) +
    theme_bw() +
    scale_x_continuous("False Positive Rate (1-Specificity)") +
    scale_y_continuous("True Positive Rate (Sensitivity)") +
    scale_colour_manual(labels = annotation, values = "#000000") +
    opts(title = title,
         plot.title = theme_text(face="bold", size=14), 
         axis.title.x = theme_text(face="bold", size=12),
         axis.title.y = theme_text(face="bold", size=12, angle=90),
         panel.grid.major = theme_blank(),
         panel.grid.minor = theme_blank(),
         legend.justification=c(1,0), 
         legend.position=c(1,0),
         legend.title=theme_blank(),
         legend.key = theme_blank()
    )
  return(p)
}
