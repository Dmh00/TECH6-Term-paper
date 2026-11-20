source("00_Setup.R")

s = d[complete.cases(d[,c("hischshr1520f","hischshr1520m","i94","iwm94",contr)]), ]

# Panels A and B
run_panel = function(g) {
  
  # high school share, age 15-20
  y = paste0("hischshr1520",g)
  # enrolled share, age 15-30
  y_e = paste0("studshr1530",g)
  
  # bandwidth for each outcome
  h = rdob_bandwidth(s[[y]],s$iwm94)
  he = rdob_bandwidth(s[[y_e]],s$iwm94)
  
  # The Nine columns of Table II:
  # 1-2 Global OLS, 3-8 RD within bandwidth, 9 enrollment outcome
  res <- rbind(
    rd_column(s,y,"i94"),
    rd_column(s,y,c("i94",contr)),
    rd_column(s,y,c("i94",vi1), h),
    rd_column(s,y,c("i94",vi1,contr), h),
    rd_column(s,y,c("i94",vi1,contr), h/2),
    rd_column(s,y,c("i94",vi1,contr), h*2),
    rd_column(s,y,c("i94",vi1,vi2,contr), h),
    rd_column(s,y,c("i94",vi1,vi2,vi3,contr), h),      
    rd_column(s,y_e,c("i94",vi1,contr), he)) 
  
  res$h = c(1,1,h,h,h/2,h*2,h,h,he)
  data.frame(sex=g,column=1:9,res)
}


# Panel A: women
women = run_panel("f")
# Panel B: men
men = run_panel("m")

table2 = rbind(women,men)
table2$cell = paste0(sprintf("%.3f", table2$b), stars(table2$p))
print(table2[,c("sex","column","cell","se","mean","r2","n","h")], digits=3, row.names=FALSE)
write.csv(table2,"outputs/table2_panels_AB.csv", row.names=FALSE)


# Panel C: is women's effect different from men's?

# The author uses Suest function to solve Panel C, but function is not in R
# therefore, we use the method from Suest manual example 3 
# of stacking women and men into one regression to test the differences


# suest function with stacking
suest_p = function(yf,ym,terms, hf=Inf, hm=Inf) {
  
  # Different potential windows for women and men
  wf = s[abs(s$iwm94)<hf,]
  wm = s[abs(s$iwm94)<hm,]
  
  # stack the two samples
  # first women's row (female=1), then men's row (female=0)
  stacked = rbind(data.frame(yy=wf[[yf]], female=1,wf[,terms], pcode=wf$pcode),
              data.frame(yy=wm[[ym]], female=0,wm[,terms], pcode=wm$pcode))
  
  # give every variable a coefficient for women(_F) and men(_M)
  for (v in terms) {
    stacked[[paste0(v,"_F")]] = stacked[[v]]*stacked$female
    stacked[[paste0(v,"_M")]] = stacked[[v]]*(1-stacked$female)
  }
  
  # fit robust regression, clustered by province to assesa correlation between rows
  stacked$male = 1-stacked$female
  fit = lm_robust(reformulate(c("0","female","male",paste0(terms,"_F"), paste0(terms,"_M")),"yy"),
                   data=stacked, clusters=pcode, se_type="CR0")
  # number of provinces
  G = length(unique(stacked$pcode))
  # covariance matrix with small sample factor
  V = vcov(fit)*G/(G-1)
  
  # difference in Islamic-mayor coefficient between women and men
  dif = coef(fit)["i94_F"] - coef(fit)["i94_M"]
  # standard error difference
  se = sqrt(V["i94_F","i94_F"] + V["i94_M","i94_M"] - 2*V["i94_F","i94_M"])
  # p-value of difference = 0 test
  unname(2*pnorm(-abs(dif/se)))
}

# high school share of women and men
f = "hischshr1520f"
m = "hischshr1520m"

# Optimal bandwidths for the four outcomes in Panel C
h = c(f = rdob_bandwidth(s$hischshr1520f,s$iwm94),
      m = rdob_bandwidth(s$hischshr1520m,s$iwm94),
      ef = rdob_bandwidth(s$studshr1530f,s$iwm94),
      em = rdob_bandwidth(s$studshr1530m,s$iwm94))

# calculate p-value for each column in Table II
panelC <- c(
  suest_p(f,m,"i94"),
  suest_p(f,m,c("i94",contr)),
  suest_p(f,m,c("i94",vi1), h["f"],h["m"]),
  suest_p(f,m,c("i94",vi1,contr), h["f"],h["m"]),
  suest_p(f,m,c("i94",vi1,contr), h["f"]/2,h["m"]/2),
  suest_p(f,m,c("i94",vi1,contr), h["f"]*2,h["m"]*2),
  suest_p(f,m,c("i94",vi1,vi2,contr), h["f"],h["m"]),
  suest_p(f,m,c("i94",vi1,vi2,vi3,contr), h["f"],h["m"]),
  
  # enrollment instead of high school
  suest_p("studshr1530f","studshr1530m",c("i94",vi1,contr),h["ef"],h["em"]))
print(round(panelC,3))

# save results Panel C as csv
write.csv(data.frame(column=1:9,p_value=panelC),"outputs/table2_panelC.csv", row.names=FALSE)

