install.packages("janitor")
install.packages("skimr") 
install.packages("gtsummary") #gtsummary is the best package for this — it produces formatted tables ready for copy-paste into Word.   
install.packages("flextable") # to save as docs
install.packages("moments") #to measure kurtosis and skewness
install.packages("ggpubr")
install.packages("patchwork")  # to combine all figures in on figure
install.packages("showtext") #to change the fonts in the graphs
install.packages("sysfonts")
install.packages("nortest")
install.packages("dunn.test")
library(dunn.test)
library(nortest)
library(tidyverse)   # loads dplyr, tidyr, readr, stringr, forcats, etc.
library(janitor)     # extra cleaning helpers
library(ggplot2)
library(ggpubr)    # adds p-values directly on plots
library(patchwork)
library(showtext)
library(sysfonts)
library(moments)
library(flextable)
library(gtsummary)
library(dplyr)
library(skimr)     
library(readxl) #So, here we upload our dataset and we assign it to df. 
