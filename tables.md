---
title: "R Notebook"
output:
  html_document:
    df_print: paged
  word_document: default
  html_notebook: default
  pdf_document: default
---



## Helper functions


``` r
# three helper functions for easier formatting
# number with 3 decimals
fmt = function(x) sprintf("%.3f",x)
# number with 4 decimals (for RMSE and alpha)
fmt4 = function(x) sprintf("%.4f",x)
# number in brackets ()
se_fmt = function(x) sprintf("(%.3f)",x)
```


## Table I


``` r
# labels for the 13 variables
labels = c("Share women aged 15–20 with high school education",
           "Share men aged 15–20 with high school education",
           "Islamic mayor in 1994",
           "Islamic vote share 1994",
           "Number of parties receiving votes 1994",
           "Log population in 1994",
           "Population share below 19 in 2000",
           "Population share above 60 in 2000",
           "Gender ratio in 2000",
           "Household size in 2000",
           "District center",
           "Province center",
           "Sub-metro center")

# create empty matrix
show = matrix("",nrow=0,ncol=5)
for (i in 1:nrow(table1)) {
  # add heading row for variable 1,3 and 4
  if (i == 1) show = rbind(show,c("*Main outcome variables*","","","",""))
  if (i == 3) show = rbind(show, c("*Main explanatory variable*","","","",""))
  if (i == 4) show = rbind(show, c("*Covariates*","","","",""))

  # first row: means for each variable
  show = rbind(show, c(labels[i],
                        fmt(table1$mean_all[i]),
                        fmt(table1$mean_islamic[i]),
                        fmt(table1$mean_secular[i]),
                        paste0(fmt(table1$difference[i]),table1$stars[i])))
  # second row: SD with brackets, SE for diference
  show = rbind(show, c("",
                       se_fmt(table1$sd_all[i]),
                       se_fmt(table1$sd_islamic[i]),
                       se_fmt(table1$sd_secular[i]),
                       se_fmt(table1$se[i])))
}
# number of municipalities
show = rbind(show,c("Observations",nrow(s),sum(isl),sum(!isl),nrow(s)))
# columns names
colnames(show)=c("","All","Islamic","Secular","Difference")

knitr::kable(show,align=c("l",rep("r",4)), caption="TABLE I: SUMMARY STATISTICS")
```



Table: TABLE I: SUMMARY STATISTICS

|                                                  |     All| Islamic| Secular| Difference|
|:-------------------------------------------------|-------:|-------:|-------:|----------:|
|*Main outcome variables*                          |        |        |        |           |
|Share women aged 15–20 with high school education |   0.163|   0.140|   0.166|  -0.026***|
|                                                  | (0.096)| (0.090)| (0.096)|    (0.006)|
|Share men aged 15–20 with high school education   |   0.192|   0.196|   0.192|      0.004|
|                                                  | (0.077)| (0.076)| (0.078)|    (0.005)|
|*Main explanatory variable*                       |        |        |        |           |
|Islamic mayor in 1994                             |   0.120|   1.000|   0.000|      1.000|
|                                                  | (0.325)| (0.000)| (0.000)|    (0.000)|
|*Covariates*                                      |        |        |        |           |
|Islamic vote share 1994                           |   0.139|   0.415|   0.101|   0.313***|
|                                                  | (0.154)| (0.114)| (0.116)|    (0.007)|
|Number of parties receiving votes 1994            |   5.541|   5.889|   5.494|   0.395***|
|                                                  | (2.192)| (3.019)| (2.050)|    (0.131)|
|Log population in 1994                            |   7.840|   8.315|   7.775|   0.540***|
|                                                  | (1.188)| (1.767)| (1.070)|    (0.071)|
|Population share below 19 in 2000                 |   0.405|   0.445|   0.400|   0.046***|
|                                                  | (0.083)| (0.075)| (0.082)|    (0.005)|
|Population share above 60 in 2000                 |   0.092|   0.073|   0.095|  -0.022***|
|                                                  | (0.040)| (0.031)| (0.040)|    (0.002)|
|Gender ratio in 2000                              |   1.073|   1.076|   1.073|      0.003|
|                                                  | (0.253)| (0.117)| (0.266)|    (0.015)|
|Household size in 2000                            |   5.835|   6.445|   5.752|   0.693***|
|                                                  | (2.360)| (2.147)| (2.376)|    (0.141)|
|District center                                   |   0.345|   0.394|   0.338|     0.056*|
|                                                  | (0.475)| (0.489)| (0.473)|    (0.029)|
|Province center                                   |   0.023|   0.067|   0.017|   0.050***|
|                                                  | (0.149)| (0.250)| (0.129)|    (0.009)|
|Sub-metro center                                  |   0.022|   0.076|   0.014|   0.062***|
|                                                  | (0.146)| (0.266)| (0.119)|    (0.009)|
|Observations                                      |    2629|     315|    2314|       2629|


## TABLE II


``` r
# 10 column matrix for table
show = matrix("",nrow=0,ncol=10)

# add results for women and men
for (g in c("f","m")) {
  r = table2[table2$sex==g,]
  r = r[order(r$column),]                  
  
  # one block for headings, outcome mean, Islamic mayor
  show = rbind(show,c(ifelse(g=="f","**Panel A: Women**","**Panel B: Men**"),rep("",9)),
               c("Outcome mean",fmt(r$mean)),c("Islamic mayor in 1994",r$cell),c("",se_fmt(r$se)),
               c("Bandwidth",fmt(r$h)),c("R2",sprintf("%.2f",r$r2)),c("Observations",r$n))
}

# Panel C: p-values of the test that women and men are equal
show = rbind(show,
             c("**Panel C: women = men**",rep("",9)),
             c("p-value",fmt(panelC)))

colnames(show) = c("",paste0("(",1:9,")"))

knitr::kable(show, align=c("l",rep("r",9)), caption="TABLE II: ISLAMIC RULE AND HIGH SCHOOL EDUCATION")
```



Table: TABLE II: ISLAMIC RULE AND HIGH SCHOOL EDUCATION

|                         |       (1)|     (2)|      (3)|      (4)|      (5)|      (6)|      (7)|      (8)|      (9)|
|:------------------------|---------:|-------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|
|**Panel A: Women**       |          |        |         |         |         |         |         |         |         |
|Outcome mean             |     0.163|   0.163|    0.152|    0.152|    0.144|    0.166|    0.152|    0.152|    0.127|
|Islamic mayor in 1994    | -0.026***| 0.012**| 0.032***| 0.028***| 0.032***| 0.022***| 0.028***| 0.043***| 0.014***|
|                         |   (0.006)| (0.006)|  (0.010)|  (0.007)|  (0.011)|  (0.006)|  (0.011)|  (0.016)|  (0.005)|
|Bandwidth                |     1.000|   1.000|    0.240|    0.240|    0.120|    0.480|    0.240|    0.240|    0.205|
|R2                       |      0.01|    0.55|     0.03|     0.65|     0.65|     0.58|     0.65|     0.65|     0.48|
|Observations             |      2629|    2629|     1020|     1020|      589|     2049|     1020|     1020|      904|
|**Panel B: Men**         |          |        |         |         |         |         |         |         |         |
|Outcome mean             |     0.192|   0.192|    0.194|    0.194|    0.195|    0.193|    0.194|    0.194|    0.197|
|Islamic mayor in 1994    |     0.004|   0.009|    0.007|    0.010|   0.015*|   0.011*|   0.015*|    0.019|    0.014|
|                         |   (0.005)| (0.006)|  (0.008)|  (0.007)|  (0.008)|  (0.006)|  (0.009)|  (0.012)|  (0.009)|
|Bandwidth                |     1.000|   1.000|    0.323|    0.323|    0.161|    0.646|    0.323|    0.323|    0.230|
|R2                       |      0.00|    0.41|     0.00|     0.43|     0.47|     0.41|     0.43|     0.43|     0.41|
|Observations             |      2629|    2629|     1341|     1341|      747|     2584|     1341|     1341|      993|
|**Panel C: women = men** |          |        |         |         |         |         |         |         |         |
|p-value                  |     0.000|   0.472|    0.001|    0.004|    0.031|    0.027|    0.169|    0.042|    0.993|


## TABLE III


``` r
# top two rows: bandwidths and column numbers
show = rbind(c("",1,0.5,0.25,0.1,0.05),
              c("","(1)","(2)","(3)","(4)","(5)"))

# Panels and polynomial orders
for (g in c("f","m")) {
  panel = ifelse(g=="f","**Panel A: Women**","**Panel B: Men**")
  show = rbind(show,c(panel,rep("",5)))
  show = rbind(show,c("*Polynomial order of control function*",rep("",5)))
  
  # Rows for polynomial order
  for (i in c("None", "Linear","Quadratic","Cubic","Quartic")) {
    r = table3[table3$sex==g & table3$order==i,]
    r = r[order(-r$h),]                     
    show = rbind(show,c(i, r$cell),c("",se_fmt(r$se)))
  }
  show = rbind(show,c("Observations",r$n))
}
colnames(show) = c("","","","Bandwidth","","")

knitr::kable(show,align=c("l", rep("r", 5)),caption="TABLE III: ALTERNATIVE RD SPECIFICATIONS")
```



Table: TABLE III: ALTERNATIVE RD SPECIFICATIONS

|                                       |         |         | Bandwidth|         |        |
|:--------------------------------------|--------:|--------:|---------:|--------:|-------:|
|                                       |        1|      0.5|      0.25|      0.1|    0.05|
|                                       |      (1)|      (2)|       (3)|      (4)|     (5)|
|**Panel A: Women**                     |         |         |          |         |        |
|*Polynomial order of control function* |         |         |          |         |        |
|None                                   |  0.012**|  0.015**|  0.018***| 0.025***|  0.018*|
|                                       |  (0.006)|  (0.006)|   (0.006)|  (0.007)| (0.010)|
|Linear                                 |  0.014**| 0.021***|  0.025***|  0.028**| 0.039**|
|                                       |  (0.007)|  (0.006)|   (0.007)|  (0.012)| (0.019)|
|Quadratic                              | 0.027***| 0.030***|  0.033***|   0.032*|   0.051|
|                                       |  (0.007)|  (0.007)|   (0.010)|  (0.018)| (0.032)|
|Cubic                                  | 0.031***| 0.026***|   0.036**|  0.057**|   0.054|
|                                       |  (0.007)|  (0.010)|   (0.015)|  (0.028)| (0.042)|
|Quartic                                | 0.030***|  0.032**|   0.044**|  0.067**|   0.028|
|                                       |  (0.009)|  (0.012)|   (0.017)|  (0.033)| (0.056)|
|Observations                           |     2628|     2177|      1049|      489|     257|
|**Panel B: Men**                       |         |         |          |         |        |
|*Polynomial order of control function* |         |         |          |         |        |
|None                                   |    0.009|   0.011*|    0.012*|   0.015*|   0.010|
|                                       |  (0.006)|  (0.006)|   (0.007)|  (0.008)| (0.011)|
|Linear                                 |   0.010*|  0.014**|    0.012*|    0.011|   0.017|
|                                       |  (0.006)|  (0.006)|   (0.007)|  (0.012)| (0.013)|
|Quadratic                              |  0.016**|  0.016**|    0.019*|    0.024|  0.048*|
|                                       |  (0.007)|  (0.007)|   (0.010)|  (0.016)| (0.025)|
|Cubic                                  |  0.017**|    0.016|    0.025*|   0.038*|   0.048|
|                                       |  (0.007)|  (0.010)|   (0.013)|  (0.023)| (0.041)|
|Quartic                                |   0.017*|  0.028**|    0.026*|  0.071**|   0.035|
|                                       |  (0.010)|  (0.012)|   (0.015)|  (0.028)| (0.058)|
|Observations                           |     2628|     2177|      1049|      489|     257|

## Table VII


``` r
# estimate with stars
table7$cell = paste0(fmt(table7$b),stars(table7$p))

# model versions
models = c("A1 OLS no controls","A2 OLS controls",
            "B1 RD h","B2 RD h/2","B3 RD 2h")

# model titles
model_titles = c("Model 1. Unconditional estimates",
           "Model 2. Unconditional estimates",
           "Model 1. Bandwidth = h",
           "Model 2. Bandwidt = h/2",
           "Model 3. Bandwidth = 2h")

# header rows: age cohort, gender,column numbers, outcome
show = rbind(
  c("Age Cohort", "15-19","15-19","15-29","15-29","15-19","15-19"),
  c("Gender","Women","Men","Women","Men","Women","Men"),
  c("","(1)","(2)","(3)","(4)","(5)","(6)"),
  c("Outcome","High School","High School",
    "High School","High School","Unmarried","Unmarried"))

# one block per model, 5 total
for (i in 1:5) {
  # put the six outcomes in same order as columns
  r = table7[table7$model==models[i],]
  r =r[match(outcomes, r$outcome),]     
  
  # Panel headings and outcome means
  if (i==1) {
    show = rbind(show,
                 c("**Panel A: Global OLS**",rep("",6)),
                 c("Outcome mean",fmt(r$mean)))
  }
  if (i==3) {
    show = rbind(show,
                 c("**Panel B: Conditonal local linear RD**",rep("",6)),
                 c("Outcome mean",fmt(r$mean)))
  }
  # Model title, estimates and standard errors
  show = rbind(show,
               c(paste0("*",model_titles[i],"*"),rep("",6)),
               c("Islamic mayor in 1994",r$cell),
               c("",se_fmt(r$se)))
  
  # Bandwidths and observations
  if (i>=2) {
    show = rbind(show,
                 c("Bandwidth",fmt(r$h)),
                 c("Observations",r$n))
  }
}

knitr::kable(show,align=c("l",rep("r",6)),caption = "TABLE VII: EDUCATION IN 2011")
```



Table: TABLE VII: EDUCATION IN 2011

|                                        |            |            |            |            |          |          |
|:---------------------------------------|-----------:|-----------:|-----------:|-----------:|---------:|---------:|
|Age Cohort                              |       15-19|       15-19|       15-29|       15-29|     15-19|     15-19|
|Gender                                  |       Women|         Men|       Women|         Men|     Women|       Men|
|                                        |         (1)|         (2)|         (3)|         (4)|       (5)|       (6)|
|Outcome                                 | High School| High School| High School| High School| Unmarried| Unmarried|
|**Panel A: Global OLS**                 |            |            |            |            |          |          |
|Outcome mean                            |       0.193|       0.189|       0.347|       0.420|     0.915|     0.995|
|*Model 1. Unconditional estimates*      |            |            |            |            |          |          |
|Islamic mayor in 1994                   |   -0.021***|      -0.007|   -0.047***|      -0.010|    -0.002|     0.000|
|                                        |     (0.007)|     (0.005)|     (0.013)|     (0.009)|   (0.006)|   (0.000)|
|*Model 2. Unconditional estimates*      |            |            |            |            |          |          |
|Islamic mayor in 1994                   |       0.006|      0.010*|       0.010|     0.022**|     0.006|     0.001|
|                                        |     (0.007)|     (0.006)|     (0.009)|     (0.009)|   (0.004)|   (0.001)|
|Bandwidth                               |       1.000|       1.000|       1.000|       1.000|     1.000|     1.000|
|Observations                            |        2338|        2338|        2338|        2338|      2338|      2338|
|**Panel B: Conditonal local linear RD** |            |            |            |            |          |          |
|Outcome mean                            |       0.186|       0.182|       0.334|       0.415|     0.912|     0.995|
|*Model 1. Bandwidth = h*                |            |            |            |            |          |          |
|Islamic mayor in 1994                   |      0.017*|       0.011|     0.022**|      0.027*|   0.013**|    -0.000|
|                                        |     (0.010)|     (0.011)|     (0.011)|     (0.015)|   (0.006)|   (0.001)|
|Bandwidth                               |       0.239|       0.199|       0.263|       0.227|     0.280|     0.271|
|Observations                            |         879|         766|         950|         840|      1009|       976|
|*Model 2. Bandwidt = h/2*               |            |            |            |            |          |          |
|Islamic mayor in 1994                   |     0.033**|      -0.007|      0.032*|     0.034**|     0.008|    -0.000|
|                                        |     (0.013)|     (0.014)|     (0.016)|     (0.015)|   (0.008)|   (0.001)|
|Bandwidth                               |       0.119|       0.100|       0.131|       0.114|     0.140|     0.136|
|Observations                            |         508|         420|         544|         482|       581|       554|
|*Model 3. Bandwidth = 2h*               |            |            |            |            |          |          |
|Islamic mayor in 1994                   |     0.017**|       0.013|     0.021**|      0.022*|    0.008*|    -0.000|
|                                        |     (0.007)|     (0.012)|     (0.010)|     (0.013)|   (0.005)|   (0.001)|
|Bandwidth                               |       0.477|       0.399|       0.525|       0.454|     0.560|     0.542|
|Observations                            |        1795|        1486|        2066|        1710|      2191|      2143|


``` r
# Spillover results
f = spill[spill$sex=="f",]
m = spill[spill$sex=="m",]

# each specification gets 2 rows (estimate with stars, and standard error in brackets)
show = rbind(
  c("Main estimate (h)",
    paste0(fmt(f$controls[1]),stars(f$p[1])),
    paste0(fmt(m$controls[1]),stars(m$p[1]))),
  c("",se_fmt(f$se[1]),se_fmt(m$se[1])),

  c("Half bandwidth (h/2)",
    paste0(fmt(f$controls[2]),stars(f$p[2])),
    paste0(fmt(m$controls[2]),stars(m$p[2]))),
  c("",se_fmt(f$se[2]),se_fmt(m$se[2])),

  c("Double bandwidth (2h)",
    paste0(fmt(f$controls[3]),stars(f$p[3])),
    paste0(fmt(m$controls[3]),stars(m$p[3]))),
  c("",se_fmt(f$se[3]),se_fmt(m$se[3])),

  c("No controls",
    paste0(fmt(f$no_controls[1]),stars(f$no_controls_p[1])),
    paste0(fmt(m$no_controls[1]),stars(m$no_controls_p[1]))),
  c("",se_fmt(f$no_controls_se[1]),se_fmt(m$no_controls_se[1])),

  c("Control for township's own mayor",
    paste0(fmt(f$own_mayor[1]),stars(f$own_mayor_p[1])),
    paste0(fmt(m$own_mayor[1]),stars(m$own_mayor_p[1]))),
  c("",se_fmt(f$own_mayor_se[1]),se_fmt(m$own_mayor_se[1])),

  c("Cluster by district",
    paste0(fmt(f$controls[1]),stars(f$district_p[1])),
    paste0(fmt(m$controls[1]),stars(m$district_p[1]))),
  c("",se_fmt(f$district_se[1]),se_fmt(m$district_se[1])),
  
  c("Bandwidth",fmt(f$h[1]),fmt(m$h[1])),
  c("Townships",f$n[1],m$n[1]),
  c("Minimum detectable effect",fmt(f$mde[1]),fmt(m$mde[1]))
)
colnames(show) = c("","Women","Men")

knitr::kable(show,align=c("l","r","r"),caption="TABLE E1.1: SPILLOVER EFFECTS")
```



Table: TABLE E1.1: SPILLOVER EFFECTS

|                                 |   Women|     Men|
|:--------------------------------|-------:|-------:|
|Main estimate (h)                |   0.010|  0.021*|
|                                 | (0.012)| (0.012)|
|Half bandwidth (h/2)             |   0.018|   0.004|
|                                 | (0.018)| (0.031)|
|Double bandwidth (2h)            |   0.007|   0.003|
|                                 | (0.010)| (0.011)|
|No controls                      |  -0.006|  0.036*|
|                                 | (0.018)| (0.019)|
|Control for township's own mayor |   0.009|  0.022*|
|                                 | (0.011)| (0.011)|
|Cluster by district              |   0.010|   0.021|
|                                 | (0.014)| (0.017)|
|Bandwidth                        |   0.155|   0.173|
|Townships                        |     619|     678|
|Minimum detectable effect        |   0.033|   0.033|


``` r
# SUTVA check results
sf = sutva[sutva$sex=="f",]
sm = sutva[sutva$sex=="m",]

# Own mayor effect without centre
show = rbind(
  c("Own Islamic mayor (paper's specification)",
    paste0(fmt(sf$own),stars(sf$own_p)),paste0(fmt(sm$own),stars(sm$own_p))),
  c("",se_fmt(sf$own_se),se_fmt(sm$own_se)),
  
  # Own mayor effect with centre
  c("Own Islamic mayor, controlling for centre",
    paste0(fmt(sf$own_ctrl),stars(sf$own_ctrl_p)),paste0(fmt(sm$own_ctrl),stars(sm$own_ctrl_p))),
  c("",se_fmt(sf$own_ctrl_se),se_fmt(sm$own_ctrl_se)),
  
  c("Islamic mayor in district centre",
    paste0(fmt(sf$centre),stars(sf$centre_p)),paste0(fmt(sm$centre),stars(sm$centre_p))),
  c("",se_fmt(sf$centre_se),se_fmt(sm$centre_se)),
  c("Bandwidth (own win margin)",fmt(sf$h),fmt(sm$h)),
  c("Townships",sf$n,sm$n))

colnames(show) = c("","Women","Men")
knitr::kable(show, align = c("l","r","r"),caption="TABLE E1.2: SUTVA CHECK")
```



Table: TABLE E1.2: SUTVA CHECK

|                                          |    Women|     Men|
|:-----------------------------------------|--------:|-------:|
|Own Islamic mayor (paper's specification) | 0.024***|   0.014|
|                                          |  (0.009)| (0.010)|
|Own Islamic mayor, controlling for centre |  0.023**|   0.014|
|                                          |  (0.009)| (0.010)|
|Islamic mayor in district centre          |  -0.013*|  -0.009|
|                                          |  (0.008)| (0.011)|
|Bandwidth (own win margin)                |    0.240|   0.323|
|Townships                                 |      544|     704|





``` r
show = data.frame()
for (v in c("A","B","BC","C")) {
  l = res[res$version==v & res$model=="Lasso",]
  o = res[res$version==v & res$model=="OLS",]
  show = rbind(show,data.frame(Model=v,
                               Lasso=fmt(mean(l$r2)),
                               OLS=fmt(mean(o$r2)),
                               Lasso_better=paste(sum(l$rmse<o$rmse),"of 6")))
}
knitr::kable(show,align=c("l","r","r","r"),caption="TABLE B3: MEAN TEST R2 OVER SIX SPLITS")
```



Table: TABLE B3: MEAN TEST R2 OVER SIX SPLITS

|Model | Lasso|   OLS| Lasso_better|
|:-----|-----:|-----:|------------:|
|A     | 0.209| 0.209|       5 of 6|
|B     | 0.221| 0.212|       6 of 6|
|BC    | 0.199| 0.199|       2 of 6|
|C     | 0.620| 0.600|       6 of 6|


``` r
# names for the four versions, used as titles in table
model_names = c(A="(1) Election-year variables, linear",
                B="(2) Election-year variables, squares and interactions",
                BC="(3) Same as (2)",
                C="(4) Adds 1990 census variables")

# training + test sample for sample sizes
n_full=format(res$n_train[res$version=="A"][1] +
                res$n_test[res$version=="A"][1],big.mark=",")
n_1990=format(res$n_train[res$version=="C"][1] +
                res$n_test[res$version=="C"][1],big.mark=",")

# Use main split (1234) for table
main = res[res$seed==1234,]
show = matrix("",nrow=0,ncol=6)

# Repeat for all versions
for (v in c("A","B","BC","C")) {
  # place panel headings
  if (v == "A") show=rbind(show,c(paste0("**Panel A: full sample (N = ",n_full,")**"),rep("",5)))
  if (v == "BC") show=rbind(show,c(paste0("**Panel B: 1990 sample (N = ",n_1990,")**"),rep("",5)))

  # give titles to versions, and put Lasso and OLS in rows
  show = rbind(show,c(model_names[v],rep("",5)))
  for (m in c("Lasso","OLS")) {
    r = main[main$version==v & main$model==m,]
    show = rbind(show,c(m, r$degree,
                        # alpha for lasso
                        ifelse(m=="Lasso",fmt4(r$alpha),""),
                        ifelse(m=="Lasso",paste(r$nonzero,"of",r$ncoef),""),
                        fmt4(r$rmse),fmt(r$r2)))
  }

  # 1990 female outcome only (version C)
  if (v=="C") show=rbind(show,c("1990 female high school share only,OLS",1,"","",
                                    fmt4(only90$Test.RMSE),fmt(only90$Test.R2)))
  # benchmark: predict training mean
  if (v %in% c("B","C")) {
    r = main[main$version==v & main$model=="Predict the mean",]
    show = rbind(show,c("Training mean","","","",fmt4(r$rmse),ifelse(abs(r$r2)<0.0005,"0.000",fmt(r$r2))))
  }
}
colnames(show) = c("","Degree","Alpha","Non-zero coefs","Test RMSE","Test R2")

knitr::kable(show,align=c("l","c","c",rep("r",3)),caption="TABLE B1: PREDICTING FEMALE HIGH SCHOOL COMPLETION IN 2000")
```



Table: TABLE B1: PREDICTING FEMALE HIGH SCHOOL COMPLETION IN 2000

|                                                      | Degree | Alpha  | Non-zero coefs| Test RMSE| Test R2|
|:-----------------------------------------------------|:------:|:------:|--------------:|---------:|-------:|
|**Panel A: full sample (N = 2,630)**                  |        |        |               |          |        |
|(1) Election-year variables, linear                   |        |        |               |          |        |
|Lasso                                                 |   1    | 0.0007 |         6 of 7|    0.0869|   0.216|
|OLS                                                   |   1    |        |               |    0.0870|   0.216|
|(2) Election-year variables, squares and interactions |        |        |               |          |        |
|Lasso                                                 |   2    | 0.0002 |       20 of 35|    0.0863|   0.227|
|OLS                                                   |   2    |        |               |    0.0865|   0.224|
|Training mean                                         |        |        |               |    0.0982|  -0.001|
|**Panel B: 1990 sample (N = 1,610)**                  |        |        |               |          |        |
|(3) Same as (2)                                       |        |        |               |          |        |
|Lasso                                                 |   2    | 0.0003 |       17 of 35|    0.0798|   0.192|
|OLS                                                   |   2    |        |               |    0.0800|   0.187|
|(4) Adds 1990 census variables                        |        |        |               |          |        |
|Lasso                                                 |   2    | 0.0014 |      29 of 119|    0.0561|   0.600|
|OLS                                                   |   1    |        |               |    0.0583|   0.568|
|1990 female high school share only,OLS                |   1    |        |               |    0.0611|   0.526|
|Training mean                                         |        |        |               |    0.0888|   0.000|
