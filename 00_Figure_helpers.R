
library(ggplot2)

# Epanechnikov kernel (weights for smoothing)
# Observations close to point of interest get higher weight, far ones get lower
# beyond certain distance the weight becomes 0
epan <- function(z) ifelse(abs(z)<sqrt(5), 0.75*(1-z^2/5)/sqrt(5), 0)

# Smooth curve (local linear smoothing, like Stata's lpoly)
lpoly_stata <- function(x,y,h,n_grid = 50) {
  # remove missing values
  check = !is.na(x) & !is.na(y)
  x = x[check]
  y = y[check]
  
  # points where the curve is evaluated
  grid = seq(min(x),max(x),length.out=min(length(x),n_grid))
  out = data.frame(x=grid,fit=NA,se=NA)
  
  for(i in seq_along(grid)) {
    # Weights of nearby towns count more
    w = epan((x-grid[i])/h)
    # straight line regression trough nearby towns
    fit = lm(y ~ I(x-grid[i]), weights=w, subset=w>0)
    
    # curve value and its standard error
    out$fit[i] = coef(fit)[1]
    out$se[i] = summary(fit)$coefficients[1,2]
  }
  # calculate 95% confidence band
  out$lower = out$fit - qnorm(0.975)*out$se
  out$upper = out$fit + qnorm(0.975)*out$se
  out
}

# dots: average of x and y within win margin bins
bin_means <- function(data,y,bin,in_lim) {
  check = !is.na(data$iwm94)
  x = data$iwm94[check]
  # left or right of cutoff
  side = x>0
  # bin number
  b = trunc(x*bin/2)
  # group side + bin so each side doesn't share bins
  group = paste(side,b)
  
  # average x and y for each bin
  dots = data.frame(x=tapply(x,group,mean),
                    y=tapply(data[[y]][check],group,mean,na.rm=TRUE),
                    shown=tapply(in_lim[check],group,any))
  # keep only bins with a valid average inside plot range
  dots[dots$shown & is.finite(dots$y),c("x","y")]
}

# RD graph data = dots + smooth curve with 95% band
rd_graph_data <- function(data,y,bin,bw,in_lim) {
  dots = bin_means(data,y,bin,in_lim)
  # remove invalid observations
  check = in_lim & !is.na(data$iwm94) & !is.na(data[[y]])
  # separate curves left and right
  left = lpoly_stata(data$iwm94[check & data$iwm94<=0], data[[y]][check & data$iwm94<=0],bw)
  right = lpoly_stata(data$iwm94[check & data$iwm94>0], data[[y]][check & data$iwm94>0],bw)
  left$side = "left"
  right$side = "right"
  list(dots=dots, curve=rbind(left,right))
}

# Draw a panel for plots
rd_plot <- function(g,title,ylim=NULL) {
  ggplot() +
    # dashed vertical line at cutoff
    geom_vline(xintercept=0, linetype="dashed", colour="grey50") +
    # dots for binned averages
    geom_point(data=g$dots,aes(x, y), colour="grey40", size=1.6) +
    # grey lines for lower/upper 95% band
    geom_line(data=g$curve,aes(x,lower,group=side), colour="grey60",linewidth=0.3, na.rm=TRUE) +
    geom_line(data=g$curve, aes(x,upper,group=side), colour="grey60", linewidth=0.3, na.rm=TRUE) +
    # black line for smooth fit
    geom_line(data=g$curve, aes(x,fit,group=side), linewidth=0.6) +
    # zoom without dropping data
    coord_cartesian(ylim=ylim) +
    # title and theme
    labs(title=title, x=NULL, y=NULL) +
    theme_classic(base_size=9)}

