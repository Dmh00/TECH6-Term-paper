source("00_Setup.R")
library(ggplot2)

# sample data
s <- d[complete.cases(d[,c("hischshr1520f","i94",contr)]),]

# color grey municipalities with an Islamic vote share under 0.75
grey = s[s$vshr_islam1994<0.75,]
# color close elections black (win margin within 0.02 of cutoff)
black = s[abs(s$iwm94)<0.02,]

# Plot Islamic win margin against Islamic vote share
figure1 <- ggplot() +
  geom_hline(yintercept=0,colour="grey70",linewidth=0.2) +
  geom_point(data=grey,aes(vshr_islam1994,iwm94),colour="grey60",size=0.6) +
  geom_point(data=black,aes(vshr_islam1994,iwm94),colour="black",size=0.6) +
  # win margin if only two parties ran
  geom_function(fun=function(x) -1+2*x,xlim=c(0,0.75), colour="grey30", linewidth=0.3) +
  labs(x="Islamic vote share", y="Islamic win margin") +
  scale_x_continuous(breaks=seq(0,0.75,0.25)) +
  scale_y_continuous(breaks=seq(-1,0.5,0.5)) +
  theme_classic(base_family="serif")

ggsave("outputs/figure1.png", figure1, width=6, height=4.5, dpi=200)

