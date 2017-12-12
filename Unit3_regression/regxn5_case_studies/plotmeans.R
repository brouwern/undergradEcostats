#setwd("C:/Users/lisanjie2/Desktop/TEACHING/1_STATS_CalU/1_STAT_CalU_2016_by_NLB/Lecture/Unit3_regression/week_after_thanksgiving")


# means = means
# SEs =  standard errors
# categories = the names of the categories/groups, in the order that they appear
# x.axis.label = what should be plotted on the x axis
# y.axis.label = what should be plotted on the y axis
# adjust.y.max = allows you to adjust the y axis
# adjust.y.min
# adjust.x.spacing
#### The function STARTS here ####
plot.means <- function(means = NULL,
                       SEs = NULL,
                       #groups = 3,
                       categories = NULL,
                       x.axis.label = "Groups",
                       y.axis.label = "'y.axis.label' sets the axis label",
                       adjust.y.max = 0,
                       adjust.y.min = 0,
                       adjust.x.spacing = 5){
  
  
  
  if(is.null(means)==T){
    stop("No means entered") } 
  
  if(is.null(SEs)==T){
    stop("No standard errors entered") } 
  
  
  #Check if the number of means matches the number of SE
  n.means <- length(means)
  n.SEs    <- length(SEs)
  
  if(n.means != n.SEs){
    error.message <- paste("The number of means is",n.means,"but the number of standard errors is",n.SEs,sep = " ")
    stop(error.message) } 
  
  
  
  #assign arbitrary categories
  if(is.null(categories) == T) {
    categories <- paste("Group",1:n.means)
    print("Set categoris labls with 'categories=' ")
  }
  
  
  # calculate values for plotting limits            
  y.max <- max(means+2*SEs) + adjust.y.max
  y.min <- min(means-2*SEs) - adjust.y.min
  
  
  #determine where to plot points along x-axis
  x.values <- 1:n.means
  x.values <- x.values/adjust.x.spacing
  
  #set x axis min/max
  x.axis.min <- min(x.values)-0.05
  x.axis.max <- max(x.values)+0.05
  
  x.limits <- c(x.axis.min,x.axis.max)
  
  #Plot means
  plot(means ~ x.values,
       xlim = x.limits,
       ylim = c(y.min,y.max),
       xaxt = "n",
       xlab = "",
       ylab = "",
       cex = 1.25,
       pch = 16)
  
  #Add x labels
  axis(side = 1, 
       at = x.values,
       labels = categories
  )
  
  #Plot upper error bar 
  lwd. <- 2
  arrows(y0 = means,
         x0 = x.values,
         y1 = means+2*SEs,
         x1 = x.values,
         length = 0,
         lwd = lwd.)
  
  #Plot lower error bar
  arrows(y0 = means,
         x0 = x.values,
         y1 = means-2*SEs,
         x1 = x.values,
         length = 0,
         lwd = lwd.) 
  
  mtext(text = x.axis.label,side = 1,line = 2)
  mtext(text = y.axis.label,side = 2,line = 2)
  mtext(text = "Error bars = 95% CI",side = 3,line = 0,adj = 0)
  
  print("This function plots ~95% conf. ints. based on SEs input by user.  DO NOT enter 95% CIs.")
  
}

#### The function ENDS here ####
#### The function ENDS here ####
#### The function ENDS here ####
#### The function ENDS here ####
#### The function ENDS here ####