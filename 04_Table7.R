source("00_Setup.R")

# The six outcomes (education in 2011)
# high school share for ages 15-19, 15-29 and never married ages 15-19
# each for women and men
outcomes = c("ahischshr15_19f","ahischshr15_19m","ahischshr15_29f",
              "ahischshr15_29m","nevm15_19f","nevm15_19m")

table7 = data.frame()
for (y in outcomes) {
  
  # Panel A, OLS on all municipalities
  sA = d[complete.cases(d[,c(y,"i94",contr)]), ]
  # effect of an Islamic mayor with controls
  m2 = rd_column(sA,y,c("i94", contr)) 
  
  # effect of an Islamic mayor without controls 
  m1 = rd_column(sA,y,"i94")
  
  # Panel B: RD, close elections only
  sB = d[complete.cases(d[,c(y,"i94",vi1,contr)]), ]
  # optimal bandwidth for outcome
  h = rdob_bandwidth(sB[[y]],sB$iwm94)
  
  # RD with margin terms and controls at h,h/2, and h*2
  terms = c("i94",vi1,contr)
  b1 = rd_column(sB,y,terms, h)
  b2 = rd_column(sB,y,terms, h/2)
  b3 = rd_column(sB,y,terms, h*2)
  
  # save outcomes to table
  table7 = rbind(table7,data.frame(
    outcome = y,
    model = c("A1 OLS no controls","A2 OLS controls",
            "B1 RD h","B2 RD h/2","B3 RD 2h"),
    h = c(1,1,h,h/2,h*2), rbind(m1,m2,b1,b2,b3)))
}

