source("00_Setup.R")
library(rdrobust)
library(rddensity)


# sample same municipalities as Table II
s = d[complete.cases(d[,c("hischshr1520f","i94",contr,"iwm94","hischshr1520m")]),]

# make district id (province + district code) so each municipality knows it district
s$district = paste(s$province_pre, s$ilce_pre)

# Get 1994 election result for each district centre
cen = s[s$merkezi==1,c("district","iwm94","i94")]
names(cen) = c("district","iwm_c","i_c")

# one centre per district, and match with each township
cen = cen[!duplicated(cen$district),]
tw = merge(s[s$merkezi==0,],cen,by="district")

# Controls for townships
contr_tw = c(setdiff(controls,c("merkezi","merkezp","buyuk")),pd_vars)

# Use district centre's win margin as running variable
x = tw$iwm_c
# save output as txt
sink("outputs/E1_results.txt", split=TRUE)


# Validity checks

# Density test: check for sorting around the cutoff
dist = tw[!duplicated(tw$district),]
summary(rddensity(dist$iwm_c, c=0))

# Check whether 1994 township characteristics jump at cutoff
summary(rdrobust(tw$vshr_islam1994,x,c=0,cluster=tw$district))
summary(rdrobust(tw$partycount,x,c=0,cluster=tw$district))
summary(rdrobust(tw$lpop1994,x,c=0,cluster=tw$district))


# Run the analysis for women and men
# store spillover and SUTVA-check results
res = data.frame()
sutva = data.frame()

for (g in c("f","m")) {
  # outcome: township share with high school education aged 15-20
  tw$y = tw[[paste0("hischshr1520",g)]]
  y = tw$y
  
  # Plot RD relationship for township education
  png(paste0("outputs/E1_rdplot_",g,".png"), width=1200, height=900, res=200,family="serif")
  rdplot(y,x,c=0, title=ifelse(g =="f","Women","Men"),
         x.label="Islamic win margin in the district centre, 1994",
         y.label="Townships: share 15-20 with high school")
  dev.off()
  
  # RD Estimate with rdrobust optimal bandwidth
  rd_opt = rdrobust(y,x,c=0,cluster=tw$district)
  print(summary(rd_opt))
  
  # same for half and double bandwidths
  h_opt = rd_opt$bws["h","left"]
  print(summary(rdrobust(y,x,c=0, h=h_opt/2, cluster=tw$district)))
  print(summary(rdrobust(y,x,c=0, h=h_opt*2, cluster=tw$district)))
  
  # Placebo test using education in 1990 
  print(summary(rdrobust(tw[[paste0("c90hischshr1520",g)]],x,c=0, cluster=tw$district)))
  
  
  # Local linear RD method for spillover analysis
  hc = rdob_bandwidth(y, x)
  # Repeat for h, h/2 and h*2 and separate models
  for (k in c(1,0.5,2)) {
    w = tw[abs(tw$iwm_c)<k*hc,]
    
    # Without controls
    f0 = lm_robust(y ~ i_c+iwm_c+iwm_c:i_c, data=w, clusters=pcode, se_type="stata")
    # Controls, clustered by province
    f1 = lm_robust(reformulate(c("i_c","iwm_c","iwm_c:i_c",contr_tw),"y"), data=w, clusters=pcode, se_type="stata")
    # Controls, clustered by district
    f2 = lm_robust(reformulate(c("i_c","iwm_c","iwm_c:i_c",contr_tw),"y"), data=w, clusters=district, se_type="stata")
    # Control for township's own Islamic mayor
    f3 = lm_robust(reformulate(c("i_c","iwm_c","iwm_c:i_c","i94",vi1,contr_tw),"y"), data=w, clusters=pcode, se_type="stata")
    
    # Store calculated values
    res = rbind(res,data.frame(sex=g, h=k*hc, n=f1$nobs, mean=mean(w$y),
                               no_controls=coef(f0)["i_c"], no_controls_se=f0$std.error["i_c"],
                               no_controls_p=f0$p.value["i_c"], controls=coef(f1)["i_c"],
                               se=f1$std.error["i_c"],
                               p=f1$p.value["i_c"],
                               district_se=f2$std.error["i_c"], district_p=f2$p.value["i_c"],
                               own_mayor=coef(f3)["i_c"],own_mayor_se=f3$std.error["i_c"], own_mayor_p=f3$p.value["i_c"],
                               row.names=NULL))
  }
  

  # Check if township's own mayor effect changes when controlling for district centre's mayor
  hg = rdob_bandwidth(s[[paste0("hischshr1520",g)]],s$iwm94)
  w  = tw[abs(tw$iwm94)<hg,]
  
  # Township's Islamic mayor
  f1 = lm_robust(reformulate(c("i94",vi1,contr_tw),"y"), data=w, clusters=pcode, se_type="stata")
  # Township's mayor and distrct centre's mayor
  f2 = lm_robust(reformulate(c("i94",vi1,"i_c",contr_tw),"y"), data=w, clusters=pcode, se_type="stata")
  
  # Save comparison
  sutva = rbind(sutva, data.frame(sex=g, h=hg, n=f1$nobs,
                                   own=coef(f1)["i94"], own_se=f1$std.error["i94"], own_p=f1$p.value["i94"],
                                   own_ctrl=coef(f2)["i94"], own_ctrl_se=f2$std.error["i94"], own_ctrl_p=f2$p.value["i94"],
                                   centre = coef(f2)["i_c"], centre_se=f2$std.error["i_c"], centre_p=f2$p.value["i_c"], row.names=NULL))
}

# Calculate smallest detectable effect (80% power, 5% significance level)
res$mde = (qnorm(0.80)+qnorm(0.975))*res$se

# print both spillover and SUTVA check results
print(res,digits=3)
print(sutva,digits=3)

# stop writing to file
sink()
# save results as csv files
write.csv(res,"outputs/E1_spillover.csv",row.names=FALSE)
write.csv(sutva,"outputs/E1_sutva.csv",row.names=FALSE)
