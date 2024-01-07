library(ggplot2)
library(ggdark)


############## CDF continuos 

x <- seq(-10, 10, 0.01)
est_norm <- dnorm(x)
norm1 <- dnorm(x, mean = 5)
norm2 <- dnorm(x, mean = -5)
norm3 <- dnorm(x, sd = 5)
norm4 <- dnorm(x, sd = 10)
label_col <- c("\u03BC = 0, \u03C3 = 1" = "Blue",
      "\u03BC = 5, \u03C3 = 1" = "Pink",
      "\u03BC = -5, \u03C3 = 1" = "White", 
      "\u03BC = 0, \u03C3 = 5" = "Green",
      "\u03BC = 0, \u03C3 = 10" = "Orange")

ggplot() +
    geom_line(aes(x = x, y = cumsum(est_norm), colour = "\u03BC = 0, \u03C3 = 1"), size = 1.25) + 
    geom_line(aes(x = x, y = cumsum(norm1), colour = "\u03BC = 5, \u03C3 = 1"), size = 1.25) +
    geom_line(aes(x = x, y = cumsum(norm2), colour = "\u03BC = -5, \u03C3 = 1"), size = 1.25) +
    geom_line(aes(x = x, y = cumsum(norm3), colour = "\u03BC = 0, \u03C3 = 5"), size = 1.25) +
    geom_line(aes(x = x, y = cumsum(norm4), colour = "\u03BC = 0, \u03C3 = 10"), size = 1.25) + 
    dark_theme_gray() + 
    ylab("Probabilidad Acumulada") + 
    xlab(TeX("X")) +
    guides(color=guide_legend(title=NULL)) + 
    scale_colour_manual(values = label_col) +
    theme(axis.text = element_text(size = 30),
          axis.title = element_text(size = 40),
          plot.title = element_text(size = 50), 
          legend.key.size = unit(4, 'cm'), #change legend key size
          legend.key.height = unit(2, 'cm'), #change legend key height
          legend.key.width = unit(2, 'cm'), #change legend key width
          legend.title = element_text(size=10), #change legend title font size
          legend.text = element_text(size=22)) #change legend text font size 

######################### CDF Discrete

x <- seq(0, 15, 1)
pois1 <- cumsum(dpois(x, 1))
pois2 <- cumsum(dpois(x, 5))
pois3 <- cumsum(dpois(x, 0.2))

label_col <- c("\u03BB = 1" = "Pink",
       "\u03BB = 5" = "White", 
       "\u03BB = 0.2" = "Orange")

ggplot() +
    geom_step(aes(x = x, y = pois1, colour = "\u03BB = 1"), size = 1.25) + 
    geom_step(aes(x = x, y = pois2, colour = "\u03BB = 5"), size = 1.25) +
    geom_step(aes(x = x, y = pois3, colour = "\u03BB = 0.2"), size = 1.25) +
    dark_theme_gray() + 
    ylab("Probabilidad Acumulada") + 
    xlab(TeX("X")) +
    scale_x_continuous(breaks = seq(0, 15, 1)) +
    guides(color=guide_legend(title=NULL)) + 
    scale_colour_manual(values = label_col) +
    theme(axis.text = element_text(size = 30),
          axis.title = element_text(size = 40),
          plot.title = element_text(size = 50), 
          legend.key.size = unit(4, 'cm'), #change legend key size
          legend.key.height = unit(2, 'cm'), #change legend key height
          legend.key.width = unit(2, 'cm'), #change legend key width
          legend.title = element_text(size=10), #change legend title font size
          legend.text = element_text(size=22)) #change legend text font size 
    
