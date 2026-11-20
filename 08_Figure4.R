source("00_Setup.R")
source("00_Figure_helpers.R")

# Titles for panels
panels <- c("(a) Women in 2000"="hischshr1520f",
            "(b) Men in 2000"="hischshr1520m",
            "(c) Women in 1990"="c90hischshr1520f",
            "(d) Men in 1990"="c90hischshr1520m")

# make on plot per panel
plots = list()
for (i in seq_along(panels)) {
  g = rd_graph_data(d,panels[i],bin=50,bw=0.08,
                    in_lim=d$iwm94<=0.5 & d$iwm94>=-0.5)
  # same y-axis range and bold titles
  plots[[i]] = rd_plot(g,names(panels)[i],ylim=c(-0.05,0.25)) +
                         theme(plot.title=element_text(face="bold",hjust=0.5))
}

# save figure as png
png("outputs/Figure4.png",width=1600,height=1600,res=220,family="serif")
gridExtra::grid.arrange(grobs=plots,ncol=2,
                        bottom="islamic win margin 1994",
                        left="share of 15-20-year-olds with high school degree")
dev.off()

