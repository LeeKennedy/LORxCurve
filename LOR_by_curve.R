#### Clean Up environment -----------------------------
rm(list=ls())
setwd(here())

#### Packages -----------------------------
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

#### Functions -----------------------------


#### Data Input -----------------------------

setwd(here())
data.in <- read_excel("Caffeine.xlsx")

compound <- data.in$Compound[1]

data.in$Response <- as.numeric(data.in$Response)

y <- data.in$Response
x <- data.in$Conc

fit <- lm(y~x)
sf <- summary(fit)
summary(fit)

se <- summary(fit)$sigma
slope <- summary(fit)$coefficients[2, 1]
blank <- summary(fit)$coefficients[1, 1]

LOD <- (3*se)/slope
LOR <- (10*se)/slope

# Plotting result-------------------------------------------------------------
plot1 <- ggplot(data.in, aes(x=Conc, y=Response)) +
  geom_point(size=4, shape=21, col="black", fill = "cornflowerblue") +
  scale_x_continuous(limits = c(0, LOR*1.5))+
  scale_y_continuous(limits = c(0, slope/LOR*1.5))+
  geom_smooth(method = lm, se = FALSE, level = 0.99) +
  geom_abline(slope = fit[[1]][2], intercept = fit[[1]][1]+3*se, colour = "red") +
  geom_abline(slope = fit[[1]][2], intercept = fit[[1]][1]-3*se, colour = "red") +
  geom_abline(slope = fit[[1]][2], intercept = fit[[1]][1], colour = "blue") +
  geom_hline(yintercept = fit[[1]][1]+3*se) +
  geom_hline(yintercept = fit[[1]][1]+10*se) +
  geom_vline(xintercept = LOD) +
  geom_vline(xintercept = LOR, lty=2, col="darkblue") +
  labs(title = paste(compound,"Standard Curve with estimated LOD and LOR\n", sep=" ")) +
  annotate("text", x= LOD*1.07, y= 0.3*max(data.in$Response), label = "LOD") +
  annotate("text", x= LOR*1.02, y= 0.35*max(data.in$Response), label = "LOR") +
  theme_bw() +
  theme(panel.grid.major = element_line(size = 0.5, color = "grey"), 
        axis.line = element_line(size = 0.7, color = "black"), 
        text = element_text(size = 14))
plot1

