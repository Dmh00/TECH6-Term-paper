library(haven)
library(estimatr)

# load regdata0.dta
d <- read_dta("regdata0.dta")

# Remove Stata value labels with numeric values
d <- zap_labels(d)

# variable controls from data
controls = c("vshr_islam1994", # Islamic vote shares 1994
              "partycount",     # Number of parties who received votes 1994
              "shhs",           # Household size
              "lpop1994",       # log population 1994
              "ageshr19",       # Population share below 19
              "ageshr60",       # Population share above 60
              "sexr",           # gender ratio
              "merkezi",        # district centre (dummies)
              "merkezp",        # province centre
              "subbuyuk",       # sub-metro centre
              "buyuk")          # metro centre

# find all province dummy variables ("pd_")
pd_vars = grep("^pd_",names(d),value=TRUE)
# Full control set
contr = c(controls,pd_vars)

# Margin terms: v1-v4 margin powers, _i interaction terms Islamic mayor
vi1 <- c("vi1","vi1_i")
vi2 <- c("vi2","vi2_i")
vi3 <- c("vi3","vi3_i")
vi4 <- c("vi4","vi4_i")

# global star level: 10%(*), 5%(**), 1%(***)
stars <- function(p) {
  ifelse(p<0.01,'***',
         ifelse(p<0.05,'**',
                ifelse(p<0.1,'*','')))
}


# Imbens-Kalyanaraman optimal bandwidth for RD design
# follows Stata command rdob.ado
# y = outcome, x=running variable, c=cutoff
rdob_bandwidth <- function(y,x,c=0) {
  # remove observations were running variable missing
  check = !is.na(x)
  x = x[check]
  y = y[check]
  # number of observations
  N = length(x)
  # starting bandwidht
  h1 = 1.84*sd(x)*N^(-1/5)
  
  # observations just left and right of cutoff
  left1 = (c-h1<=x & x<c)
  right1 = (c<=x & x<= (c+h1))
  # Number of observations each window
  Nh1n = sum(left1)
  Nh1p = sum(right1)
  
  # average outcome each window
  Yh1n = mean(y[left1],na.rm=TRUE)
  Yh1p = mean(y[right1],na.rm=TRUE)
  
  # density of x at cutoff
  fxc = (Nh1p+Nh1n)/(2*N*h1)
  # variance of outcome near cutoff (average squared distance)
  sigma2c = (sum((y[left1] - Yh1n)^2,na.rm=TRUE) +
                sum((y[right1] - Yh1p)^2,na.rm=TRUE))/(Nh1p+Nh1n)
  
  # median of x each side cutoff
  medXp = median(x[x>=c])
  medXn = median(x[x<c])
  # x distance from cutoff
  t = x-c
  # observations between medians (middle part)
  mid = (x>=medXn & x<=medXp)
  
  # fit cubic curve in middle part
  cubic_fit = lm(y ~ I(x>=c) + t+I(t^2)+I(t^3),subset=mid)
  m3c = 6*coef(cubic_fit)[5]
  
  # number of observations each side
  Np = sum(x>=c)
  Nn = sum(x<c)
  
  # second starting bandwidth for each side
  h2p = 3.56*(sigma2c / (fxc*max(m3c^2,0.01)))^(1/7) * Np^(-1/7)
  h2n = 3.56*(sigma2c / (fxc*max(m3c^2,0.01)))^(1/7) * Nn^(-1/7)
  
  # observations in windows left and right
  right2 = x>=c & x<=(c+h2p)
  left2 = x>=(c-h2n) & x<c
  
  # fit quadratic each side
  m2pc = 2*coef(lm(y ~ t+I(t^2),subset=right2))[3]
  m2nc = 2*coef(lm(y ~ t+I(t^2),subset=left2))[3]
  
  # regularization to keep data stable when little data
  rp = 720*sigma2c / (sum(right2)*h2p^4)
  rn = (720*sigma2c) / (sum(left2)*h2n^4)
  
  # Optimal bandwidth
  h_opt = 3.4375*((2*sigma2c) / (fxc*((m2pc-m2nc)^2+(rp+rn))))^(1/5) * N^(-1/5) 
  unname(h_opt)
}
# runs one RD regression and return results in table
rd_column <- function(data,y,terms,h=Inf) {
  # keep municipalities within bandwidth
  d = data[abs(data$iwm94)<h,]
  # fit robust lm with standard errors clustered by province
  fit = lm_robust(reformulate(terms, y), data=d, clusters=pcode, se_type="stata")
  # create table with metrics
  data.frame(b = unname(coef(fit)["i94"]), 
             se = unname(fit$std.error["i94"]),
             p = unname(fit$p.value["i94"]), 
             mean = mean(d[[y]], na.rm=TRUE),
             r2 = fit$r.squared, n=fit$nobs)
}
  
