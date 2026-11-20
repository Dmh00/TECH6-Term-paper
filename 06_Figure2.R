source("00_Setup.R")
library(ggplot2)

# win margin of municipalities
x = d$iwm94[!is.na(d$iwm94)]

# break margins into bins and count amount in each
breaks = seq(min(x), max(x), length.out=100)
h = hist(x, breaks=breaks, right=FALSE, include.lowest=TRUE, plot=FALSE)
bins = data.frame(mid=h$mids, percent=100*h$counts/length(x))

# Figure2a: bar chart of the shares, dashed line at cutoff
figure2a <- ggplot(bins, aes(mid, percent)) +
  geom_col(width=diff(breaks)[1], fill="grey70", colour="grey20", linewidth = 0.2) +
  geom_vline(xintercept=0, linetype="dashed", colour="grey40") +
  labs(x="Islamic win margin", y="Percent") + theme_classic(base_family="serif")

# save figure as png
ggsave("outputs/figure2a.png",figure2a, width=6, height=4.5, dpi=200)


# Figure 2b: MCrary density test
# Translated from authors DCdensity.ado

# sample margins between -0.6 to 0.6
s = d[complete.cases(d[,c("hischshr1520f","iwm94")]), ]
s = s[abs(s$iwm94)<0.6,]
Z = s$iwm94

# number of municipalities
n = length(Z)
# cutoff
c0 = 0


# bin size
b = 2*sd(Z)*n^(-1/2) 
# midpoint lowest bin
l = floor((min(Z)-c0)/b)*b + b/2 + c0 
# midpoint highest bin
r = floor((max(Z)-c0)/b)*b + b/2 + c0
# number of bins
j = floor((max(Z)-min(Z))/b) + 2  

# count municipalities in each bin
binnum = round(((floor((Z-c0)/b)*b + b/2 + c0)-l)/b+1)
counts = tabulate(binnum, nbins=j)
# bar heights
cellval = counts/(n*b)
# bar midpoints
cellmp = floor(((l + (1:j-1)*b)-c0)/b)*b + b/2 + c0

# Fit a 4th-order polynomial trough bar heights to measure curvature
side_h <- function(left) {
  # pick bars on side
  use = if (left) cellmp<c0 else cellmp>c0
  # fit trough bars
  fit = lm(cellval[use] ~ cellmp[use] + I(cellmp[use]^2) + I(cellmp[use]^3) + I(cellmp[use]^4))
  beta = coef(fit)
  # bumpiness at each bar
  fpp = 2*beta[3] + 6*beta[4]*cellmp[use] + 12*beta[5]*cellmp[use]^2
  # reach of bars
  span = if (left) c0 - l else r - c0
  # Mcrary's formula for window
  3.348*(sigma(fit)^2 * span/sum(fpp^2))^(1/5)
}
# average of left and right is final window
h = 0.5*(side_h(TRUE)+side_h(FALSE))

# Pad the histogram with zero cells on both sides
pad = ceiling(h/b)
cellval_p = c(rep(0,pad),cellval,rep(0,pad))
cellmp_p = c(l-(pad:1)*b,cellmp,r+(1:pad)*b)


# smooths histogram at one point with only one side of cutoff
llr <- function(at,right) {
  w = pmax(0, 1 - abs((cellmp_p-at)/h))*((cellmp_p>=c0) == right)
  coef(lm(cellval_p ~ I(cellmp_p - at), weights=w))[1]
}

# smoothed heights left and right
fhatl = llr(c0, right=FALSE)                             
fhatr = llr(c0, right=TRUE)

# log difference
theta = log(fhatr)-log(fhatl)
# standard error
se_theta = sqrt((1/(n * h))*(24/5)*(1/fhatr + 1/fhatl))


# Smooth density at every bar midpoint
fhat = sapply(cellmp, function(a) llr(a, right = a>=c0))
# standard error at every bar with McCrary's formulas
se_f = sapply(seq_along(cellmp), function(i) {
  if (cellmp[i] > c0) {
    m = max(-1,(c0 - cellmp[i])/h)
    v = (2-3*m^11-24*m^10-83*m^9-72*m^8+42*m^7+18*m^6-18*m^5+18*m^4-3*m^3+18*m^2-15*m) /
      (1+m^6+6*m^5-3*m^4-4*m^3+9*m^2-6*m)^2
    
  } else {
    m = min((c0 - cellmp[i])/h, 1)
    v =(2+3*m^11-24*m^10+83*m^9-72*m^8-42*m^7+18*m^6+18*m^5+18*m^4-3*m^3+18*m^2+15*m) /
      (1+m^6-6*m^5-3*m^4+4*m^3+9*m^2+6*m)^2
  }
  sqrt((12*fhat[i])/(5*n*h)*v)
})

# make table for the plot
dens = data.frame(x=cellmp, height=cellval, fhat=fhat,
                   lo = fhat - 1.96*se_f, hi=fhat + 1.96*se_f,
                   side = ifelse(cellmp<c0,"left","right"))

# show only margins from -0.5 to 0.5
dens = dens[dens$x>-0.5 & dens$x<0.5,]

# plot figure2b
figure2b <- ggplot(dens,aes(x)) +
  geom_point(aes(y=height), colour="grey55", size=1) +
  geom_line(aes(y=fhat, group=side), linewidth=0.7) +
  geom_line(aes(y=lo, group=side), colour="grey55", linetype="longdash", linewidth=0.3) +
  geom_line(aes(y=hi, group=side), colour="grey55", linetype="longdash", linewidth=0.3) +
  geom_vline(xintercept=0, linetype="dashed", colour="grey50") +
  scale_y_continuous(breaks = c(0, 1.25, 2.5)) +
  labs(x = "Islamic win margin 1994", y="Density",
       caption = sprintf("Discontinuity est. = %.3f, s.e. = %.3f",theta,se_theta)) +
  theme_classic(base_family="serif")

ggsave("outputs/figure2b.png",figure2b,width=6,height=4.5,dpi=200)

