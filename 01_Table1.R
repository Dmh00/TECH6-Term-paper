source("00_Setup.R")

# variables that needs to be included for a municipality to be in sample
sumstats <- c("hischshr1520f","hischshr1520m","i94",
              "vshr_islam1994","partycount","lpop1994",
              "ageshr19","ageshr60","sexr","merkezi",
              "merkezp","subbuyuk","buyuk")

# sample municipalities with no missing values for variables
s <- d[complete.cases(d[,c("iwm94",sumstats)]), ]

# Variables in Table I
variables <- c("hischshr1520f","hischshr1520m","i94","vshr_islam1994",
               "partycount","lpop1994","ageshr19","ageshr60","sexr",
               "shhs","merkezi","merkezp","subbuyuk")


# Municipalities that elected an Islamic mayor in 1994
isl = s$i94 == 1

table1 = data.frame()
# one row per variable 
# variable, values Islamic/secular municipalities
for (v in variables) {
  x = s[[v]]
  x1 = x[isl]
  x0 = x[!isl]
  
  # Islamic mayor dummy defines group (no test)
  if (v=='i94') {
    se = 0
    p = NA_real_
  } else {
    # t-test for difference in means (equal variance assumed)
    test = t.test(x1,x0,var.equal=TRUE)
    se = test$stderr
    p = test$p.value
  }
  
  # store variables and metrics in table
  table1 <- rbind(table1, data.frame(
  variable = v,
  mean_all = mean(x),
  sd_all = sd(x),
  mean_islamic = mean(x1),
  sd_islamic = sd(x1),
  mean_secular = mean(x0),
  sd_secular = sd(x0),
  difference = mean(x1) - mean(x0),
  se = se, 
  p_value = p))
}

# stars for significance levels
table1$stars = stars(table1$p_value)
table1$stars[is.na(table1$p_value)] = ""

# write table as csv file
write.csv(table1,"outputs/table1.csv",row.names=FALSE)

