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
    

######################### Ridge vs Lasso vs OLS

install.packages("glmnet")
library(gmlnet)
library(ggplot2)
library(ggdark)

data(mtcars)

x <- as.matrix(mtcars[, -1])
y <- mtcars$mpg

linear_model <- lm(y ~ x)
betas <- as.vector(coef(linear_model))

ridge_model <- cv.glmnet(x, y, alpha = 0)
best_lambda <- ridge_model$lambda.min
best_model_ridge <- glmnet(x, y, alpha = 0, lambda = best_lambda)
betas_ridge <- as.vector(coef(best_model_ridge))

lasso_model <- cv.glmnet(x, y, alpha = 1)
best_lambda <- lasso_model$lambda.min
best_model_lasso <- glmnet(x, y, alpha = 1, lambda = best_lambda)
betas_lasso <- as.vector(coef(best_model_lasso))

group <- c(rep("Mínimos cuadrados", length(betas)),
           rep("Ridge", length(betas_ridge)),
           rep("Lasso", length(betas_lasso)))

beta_hat <- data.frame(c(betas, betas_ridge, betas_lasso), group)
colnames(beta_hat) <- c("beta", "group")

ggplot(beta_hat, aes(x = beta, y = group, color = group)) +
    geom_point(size = 4) +
    geom_vline(xintercept = 0, color = "white", size = 1.5) +
    labs(x = "\u03B2", y = "") +
    dark_theme_gray() +
    theme(axis.text = element_text(size = 30),
          axis.title = element_text(size = 40),
          plot.title = element_text(size = 50)) +
    theme(legend.position="none")


###################################### Simpson`s Paradox

install.packages("bayestestR")
library(bayestestR)
library(ggplot2)
library(ggdark)

# Simulamos los datos
data <- simulate_simpson(n = 50, r = 0.5, groups = 4)
colnames(data) <- c("x", "y", "grupo")

ggplot(data, aes(x = x, y = y, color = grupo)) +
    geom_point(size = 6) +
    stat_smooth(method = "lm", size = 1, level = 0.95, alpha = 0.3) +
    geom_smooth(method = "lm", color = "white", 
                size = 1.5, level = 0.95, alpha = 0.6) +
    labs(y = "Variable Dependiente", x = "Variable Independiente") +
    dark_theme_gray() +
    theme(axis.text = element_text(size = 20),
          axis.title = element_text(size = 30),
          plot.title = element_text(size = 50))
