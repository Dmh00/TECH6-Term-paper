source("00_Setup.R")

# sample only municipalities with data needed
s <- d[complete.cases(d[,c("hischshr1520f","i94",contr,"iwm94")]), ]

# Bandwidths for Table III
bandwidths = c(1,0.5,0.25,0.1,0.05)

# Variables for each shape (vi1-vi4 win margins and powers)
terms0 = c("i94",contr)                 # None
terms1 = c("i94",vi1,contr)             # linear
terms2 = c("i94",vi1,vi2,contr)         # quadratic
terms3 = c("i94",vi1,vi2,vi3,contr)     # cubic
terms4 = c("i94",vi1,vi2,vi3,vi4,contr) # quartic

# run every bandwidth and all orders for women and men 
table3 = data.frame()
for(g in c("f","m")) {
  y = paste0("hischshr1520",g)
  for(h in bandwidths) {
    # save results to table3
    table3 <- rbind(table3,
                    data.frame(sex=g,order="None", h=h,rd_column(s,y,terms0,h)),
                    data.frame(sex=g,order="Linear", h=h,rd_column(s,y,terms1,h)),
                    data.frame(sex=g,order="Quadratic", h=h,rd_column(s,y,terms2,h)),
                    data.frame(sex=g,order="Cubic", h=h,rd_column(s,y,terms3,h)),
                    data.frame(sex=g,order="Quartic", h=h,rd_column(s,y,terms4,h)))
  }
}

# estimate with 3 decimals and stars
table3$cell = paste0(sprintf("%.3f",table3$b),stars(table3$p))
# print and save table as csv
print(table3[,c("sex","order","h","cell","se","n")], digits=3)
write.csv(table3,"outputs/table3.csv",row.names=FALSE)

