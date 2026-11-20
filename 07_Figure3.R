source("00_Setup.R")
source("00_Figure_helpers.R")

# keep margins from -0.3 to 0.3
in_lim = d$iwm94<0.3 & d$iwm94>-0.3

# titles for panels
panels = c("Islamic vote share"="vshr_islam1994",
           "Age 60+"="ageshr60",
           "Age 19-"="ageshr19",
           "Log population"="lpop1994",
           "Gender"="sexr",
           "Any center municipality"="anyc",
           "Number of parties"="partycount",
           "Household size"="shhs")

# create one plot per variable
plots = list()
for (i in seq_along(panels)) {
  g = rd_graph_data(d,panels[i],bin=25,bw=0.06,in_lim=in_lim)
  p = rd_plot(g,names(panels)[i]) + theme(text=element_text(family="serif"))
  # only bottom panels have x-label
  if (i>=7) p=p +labs(x="Islamic win margin")
  plots[[i]] = p
}

# save as png
png("outputs/figure3.png", width=1500, height=2000, res=220)
gridExtra::grid.arrange(grobs=plots,ncol=2)
dev.off()
