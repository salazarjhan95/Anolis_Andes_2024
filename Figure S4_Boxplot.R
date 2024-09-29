library(ggplot2)
library(plyr)
library(grid)
library(gridExtra)
library(cowplot)


##########################################################################
######                                                              ######
######         Box plot for CTmax, CTmin and Bio                    ######
######         vs Bio 5, Bio 6 and Te minimum and maximum           ######
######         uses "New_PGLS_CTmax_Sept24.csv"                     ######
######         uses "New_PGLS_CTmin_Sept24.csv"                     ######
######         uses "New_PGLS_Tb_Sept24.csv"                        ######
######                                                              ######
##########################################################################



setwd("")


###############
####       ####
#### CTmax ####
####       ####
###############
data <- read.csv("New_PGLS_CTmax_Sept24.csv", header=1)
attach(data)
head(data)

# CTmax vs Bio 5
bio5_ctmax <- ggplot(data, aes(x = bio5, y = ctmax, shape = clade)) +
  geom_point(aes(color = Sp), size = 6, alpha = 0.7) +
  geom_abline(intercept = 15.9310, slope = 0.5716, linewidth=1.5, linetype="dashed") + #pgls.SEy regression results
  geom_abline(intercept = 11.452779, slope = 0.713292, linewidth=1.5, color = "#d8572a") + #Draconura
  geom_abline(intercept = 18.866082, slope = 0.459749, linewidth=1.5, color = "#004e98") + #Dactyloa
  
  scale_x_continuous(breaks = seq(20, 30, 2), limit = c(20, 30)) +
  scale_y_continuous(breaks = seq(26, 34, 2), limit = c(26, 34)) +
  theme_classic() +
  guides(col = guide_legend(nrow = 4, override.aes = list(size = 5)))+
  
  scale_color_manual(values=c("black", "#004949","#009292", "#ff6db6",
                              "#ffb6db","#490092", "#006ddb","#b66dff", 
                              "#6db6ff","#b6dbff", "#920000", "#924900",
                              "#24ff24","#ffff6d"),
                     breaks = c("antonii", "calimae", "chloris","chocorum", "danieli",
                                "heterodermus", "maculigula", "maculiventris",
                                "notopholis", "princeps", "ventrimaculatus"),
                     labels = c("anto", "cali", "chlr","purp", "dani",
                                "hetr", "macg", "macv",
                                "noto", "prin", "vent")) +
  
  theme(legend.position = "none",
        plot.margin = margin(0, 5, 1, 0.1, "cm"))+
  
  guides(color=guide_legend(ncol =2)) +
  
  xlab(expression(Bio~5~("°"*C))) +
  ylab(expression(CT[max]~("°"*C))) +
  
  theme(axis.text.x = element_text(family="serif", size = 26, lineheight = 0.9, vjust = 1)) +
  theme(axis.text.y = element_text(family="serif",size = 26, lineheight = 0.9, vjust = 1)) +
  theme(axis.title.y = element_text(family="serif",vjust=1.2, size=32)) +
  theme(axis.title.x = element_text(family="serif",vjust=-0.5, size=32))

# CTmax vs Te maximum
temax_ctmax <- ggplot(data, aes(x = Te_max, y = ctmax, shape = clade)) +
  geom_point(aes(color = Sp), size = 6, alpha = 0.7) +
  geom_abline(intercept = -6.844151, slope = 0.760776, linewidth=1.5, linetype="dashed") + #pgls.SEy regression results
  geom_abline(intercept = -29.422281, slope = 1.222725, linewidth=1.5, color = "#d8572a") + #Draconura
  geom_abline(intercept = 18.71631, slope = 0.2559653, linewidth=1.5, color = "#004e98") + #Dactyloa
  
  scale_x_continuous(breaks = seq(46, 54, 2), limit = c(46, 54)) +
  scale_y_continuous(breaks = seq(26, 34, 2), limit = c(26, 34)) +
  theme_classic() +
  guides(col = guide_legend(nrow = 4, override.aes = list(size = 5)))+
  
  scale_color_manual(values=c("black", "#004949","#009292", "#ff6db6",
                              "#ffb6db","#490092", "#006ddb","#b66dff", 
                              "#6db6ff","#b6dbff", "#920000", "#924900",
                              "#24ff24","#ffff6d"),
                     breaks = c("antonii", "calimae", "chloris","chocorum", "danieli",
                                "heterodermus", "maculigula", "maculiventris",
                                "notopholis", "princeps", "ventrimaculatus"),
                     labels = c("anto", "cali", "chlr","purp", "dani",
                                "hetr", "macg", "macv",
                                "noto", "prin", "vent")) +
  
  theme(legend.position = "none",
        plot.margin = margin(0, 5, 1, 0.1, "cm"))+
  
  guides(color=guide_legend(ncol =2)) +
  
  xlab(expression(T[e]~maximum~("°"*C))) +
  ylab(expression(CT[max]~("°"*C))) +
  
  theme(axis.text.x = element_text(family="serif", size = 26, lineheight = 0.9, vjust = 1)) +
  theme(axis.text.y = element_text(family="serif",size = 26, lineheight = 0.9, vjust = 1)) +
  theme(axis.title.y = element_text(family="serif",vjust=1.2, size=32)) +
  theme(axis.title.x = element_text(family="serif",vjust=-0.5, size=32))

# CTmax vs Bio 6
bio6_ctmax <- ggplot(data, aes(x = bio6, y = ctmax, shape = clade)) +
  geom_point(aes(color = Sp), size = 6, alpha = 0.7) +
  geom_abline(intercept = 20.315720, slope = 0.641102, linewidth=1.5, linetype="dashed") + #pgls.SEy regression results
  geom_abline(intercept = 17.242996 , slope = 0.768857, linewidth=1.5, color = "#d8572a") + #Draconura
  geom_abline(intercept = 21.853700, slope = 0.561776, linewidth=1.5, color = "#004e98") + #Dactyloa
  
  scale_x_continuous(breaks = seq(10, 20, 2), limit = c(10, 20)) +
  scale_y_continuous(breaks = seq(26, 34, 2), limit = c(26, 34)) +
  theme_classic() +
  guides(col = guide_legend(nrow = 4, override.aes = list(size = 5)))+
  
  scale_color_manual(values=c("black", "#004949","#009292", "#ff6db6",
                              "#ffb6db","#490092", "#006ddb","#b66dff", 
                              "#6db6ff","#b6dbff", "#920000", "#924900",
                              "#24ff24","#ffff6d"),
                     breaks = c("antonii", "calimae", "chloris","chocorum", "danieli",
                                "heterodermus", "maculigula", "maculiventris",
                                "notopholis", "princeps", "ventrimaculatus"),
                     labels = c("anto", "cali", "chlr","purp", "dani",
                                "hetr", "macg", "macv",
                                "noto", "prin", "vent")) +
  
  theme(legend.position = "none",
        plot.margin = margin(0, 5, 1, 0.1, "cm"))+
  
  guides(color=guide_legend(ncol =1)) +
  
  xlab(expression(Bio~6~("°"*C))) +
  ylab(expression(CT[max]~("°"*C))) +
  
  theme(axis.text.x = element_text(family="serif", size = 26, lineheight = 0.9, vjust = 1)) +
  theme(axis.text.y = element_text(family="serif",size = 26, lineheight = 0.9, vjust = 1)) +
  theme(axis.title.y = element_text(family="serif",vjust=1.2, size=32)) +
  theme(axis.title.x = element_text(family="serif",vjust=-0.5, size=32))

# CTmax vs Te minimum
temin_ctmax <- ggplot(data, aes(x = Te_min, y = ctmax, shape = clade)) +
  geom_point(aes(color = Sp), size = 6, alpha = 0.7) +
  geom_abline(intercept = 19.753145, slope = 0.539588, linewidth=1.5, linetype="dashed") + #pgls.SEy regression results
  geom_abline(intercept = 18.200217 , slope = 0.574302, linewidth=1.5, color = "#d8572a") + #Draconura
  geom_abline(intercept = 20.348435, slope = 0.534191, linewidth=1.5, color = "#004e98") + #Dactyloa
  
  scale_x_continuous(breaks = seq(14, 25, 3), limit = c(14, 25)) +
  scale_y_continuous(breaks = seq(26, 34, 2), limit = c(26, 34)) +
  theme_classic() +
  guides(col = guide_legend(nrow = 4, override.aes = list(size = 5)))+
  
  scale_color_manual(values=c("black", "#004949","#009292", "#ff6db6",
                              "#ffb6db","#490092", "#006ddb","#b66dff", 
                              "#6db6ff","#b6dbff", "#920000", "#924900",
                              "#24ff24","#ffff6d"),
                     breaks = c("antonii", "calimae", "chloris","chocorum", "danieli",
                                "heterodermus", "maculigula", "maculiventris",
                                "notopholis", "princeps", "ventrimaculatus"),
                     labels = c("anto", "cali", "chlr","purp", "dani",
                                "hetr", "macg", "macv",
                                "noto", "prin", "vent")) +
  
  theme(legend.key = element_rect(colour = "white"), 
        legend.background = element_rect(colour = "white"),
        legend.text=element_text(family="serif", size=15, face="italic"),
        legend.title=element_blank()) +
  
  guides(color=guide_legend(ncol =1)) +
  
  xlab(expression(T[e]~minimum~("°"*C))) +
  ylab(expression(CT[max]~("°"*C))) +
  
  theme(axis.text.x = element_text(family="serif", size = 26, lineheight = 0.9, vjust = 1)) +
  theme(axis.text.y = element_text(family="serif",size = 26, lineheight = 0.9, vjust = 1)) +
  theme(axis.title.y = element_text(family="serif",vjust=1.2, size=32)) +
  theme(axis.title.x = element_text(family="serif",vjust=-0.5, size=32))




###############
####       ####
#### CTmin ####
####       ####
###############
data2 = read.csv(file="New_PGLS_CTmin_Sept24.csv", header=TRUE)
attach(data2)
head(data2)

# CTmin vs Bio 5
bio5_ctmin <- ggplot(data2, aes(x = bio5, y = ctmin, shape = clade)) +
  geom_point(aes(color = Sp),  size = 6, alpha = 0.7) +
  geom_abline(intercept = 0.5804877, slope = 0.5354485, linewidth=1.5, linetype="dashed") + #pgls.SEy regression results
  geom_abline(intercept = 4.440307 , slope = 0.392901, linewidth=1.5, color = "#d8572a") + #Draconura
  geom_abline(intercept = -5.215360, slope = 0.773273, linewidth=1.5, color = "#004e98") + #Dactyloa
  
  scale_x_continuous(breaks = seq(20, 30, 2), limit = c(20, 30)) +
  scale_y_continuous(breaks = seq(6, 22, 4), limit = c(6, 22)) +
  theme_classic() +
  
  guides(col = guide_legend(nrow = 4, override.aes = list(size = 5)))+
  
  scale_color_manual(values=c("black", "#004949","#009292", "#ff6db6",
                              "#ffb6db","#490092", "#006ddb","#b66dff", 
                              "#6db6ff","#b6dbff", "#920000", "#924900",
                              "#24ff24","#ffff6d"),
                     breaks = c("antonii", "calimae", "chloris","chocorum", "granuliceps",
                                "heterodermus", "maculigula", "maculiventris",
                                "notopholis", "princeps", "ventrimaculatus"),
                     labels = c("anto", "cali", "chlr","purp", "gran",
                                "hetr", "macg", "macv",
                                "noto", "prin", "vent")) +
  
  theme(legend.position = "none",
        plot.margin = margin(0, 5, 1, 0.1, "cm"))+
  
  guides(color=guide_legend(ncol =2)) +
  
  xlab(expression(Bio~5~("°"*C))) +
  ylab(expression(CT[min]~("°"*C))) +
  
  theme(axis.text.x = element_text(family="serif", size = 26, lineheight = 0.9, vjust = 1)) +
  theme(axis.text.y = element_text(family="serif",size = 26, lineheight = 0.9, vjust = 1)) +
  theme(axis.title.y = element_text(family="serif",vjust=1.2, size=32)) +
  theme(axis.title.x = element_text(family="serif",vjust=-0.5, size=32))

# CTmin vs Te maximum
temax_ctmin <- ggplot(data2, aes(x = Te_max, y = ctmin, shape = clade)) +
  geom_point(aes(color = Sp),  size = 6, alpha = 0.7) +
  geom_abline(intercept = -25.762206, slope = 0.820738, linewidth=1.5, linetype="dashed") + #pgls.SEy regression results
  geom_abline(intercept = -18.120911, slope = 0.674655, linewidth=1.5, color = "#d8572a") + #Draconura
  geom_abline(intercept = -45.67668, slope = 1.21404, linewidth=1.5, color = "#004e98") + #Dactyloa
  
  scale_x_continuous(breaks = seq(46, 54, 2), limit = c(46, 54)) +
  scale_y_continuous(breaks = seq(6, 22, 4), limit = c(6, 22)) +
  theme_classic() +
  
  guides(col = guide_legend(nrow = 4, override.aes = list(size = 5)))+
  
  scale_color_manual(values=c("black", "#004949","#009292", "#ff6db6",
                              "#ffb6db","#490092", "#006ddb","#b66dff", 
                              "#6db6ff","#b6dbff", "#920000", "#924900",
                              "#24ff24","#ffff6d"),
                     breaks = c("antonii", "calimae", "chloris","chocorum", "granuliceps",
                                "heterodermus", "maculigula", "maculiventris",
                                "notopholis", "princeps", "ventrimaculatus"),
                     labels = c("anto", "cali", "chlr","purp", "gran",
                                "hetr", "macg", "macv",
                                "noto", "prin", "vent")) +
  
  theme(legend.position = "none",
        plot.margin = margin(0, 5, 1, 0.1, "cm"))+
  
  guides(color=guide_legend(ncol =2)) +
  
  xlab(expression(T[e]~maximum~("°"*C))) +
  ylab(expression(CT[min]~("°"*C))) +
  
  theme(axis.text.x = element_text(family="serif", size = 26, lineheight = 0.9, vjust = 1)) +
  theme(axis.text.y = element_text(family="serif",size = 26, lineheight = 0.9, vjust = 1)) +
  theme(axis.title.y = element_text(family="serif",vjust=1.2, size=32)) +
  theme(axis.title.x = element_text(family="serif",vjust=-0.5, size=32))


# CTmin vs Bio 6
bio6_ctmin <- ggplot(data2, aes(x = bio6, y = ctmin, shape = clade)) +
  geom_point(aes(color = Sp),  size = 6, alpha = 0.7) +
  geom_abline(intercept = 4.908335, slope = 0.585877, linewidth=1.5, linetype="dashed") + #pgls.SEy regression results
  geom_abline(intercept = 7.629717 , slope = 0.423507, linewidth=1.5, color = "#d8572a") + #Draconura
  geom_abline(intercept = 0.8420650, slope = 0.8721925, linewidth=1.5, color = "#004e98") + #Dactyloa
  
  scale_x_continuous(breaks = seq(10, 20, 2), limit = c(10, 20)) +
  scale_y_continuous(breaks = seq(6, 22, 4), limit = c(6, 22)) +
  theme_classic() +
  
  guides(col = guide_legend(nrow = 4, override.aes = list(size = 5)))+
  
  scale_color_manual(values=c("black", "#004949","#009292", "#ff6db6",
                              "#ffb6db","#490092", "#006ddb","#b66dff", 
                              "#6db6ff","#b6dbff", "#920000", "#924900",
                              "#24ff24","#ffff6d"),
                     breaks = c("antonii", "calimae", "chloris","chocorum", "granuliceps",
                                "heterodermus", "maculigula", "maculiventris",
                                "notopholis", "princeps", "ventrimaculatus"),
                     labels = c("anto", "cali", "chlr","purp", "gran",
                                "hetr", "macg", "macv",
                                "noto", "prin", "vent")) +
  
  theme(legend.position = "none",
        plot.margin = margin(0, 5, 1, 0.1, "cm"))+
  
  guides(color=guide_legend(ncol =1)) +
  
  xlab(expression(Bio~6~("°"*C))) +
  ylab(expression(CT[min]~("°"*C))) +
  
  theme(axis.text.x = element_text(family="serif", size = 26, lineheight = 0.9, vjust = 1)) +
  theme(axis.text.y = element_text(family="serif",size = 26, lineheight = 0.9, vjust = 1)) +
  theme(axis.title.y = element_text(family="serif",vjust=1.2, size=32)) +
  theme(axis.title.x = element_text(family="serif",vjust=-0.5, size=32))


# CTmin vs Te minimum
temin_ctmin <- ggplot(data2, aes(x = Te_min, y = ctmin, shape = clade)) +
  geom_point(aes(color = Sp),  size = 6, alpha = 0.7) +
  geom_abline(intercept = 5.529719, slope = 0.440040, linewidth=1.5, linetype="dashed") + #pgls.SEy regression results
  geom_abline(intercept = 8.156981 , slope = 0.316341, linewidth=1.5, color = "#d8572a") + #Draconura
  geom_abline(intercept = 2.4650459, slope = 0.6094424, linewidth=1.5, color = "#004e98") + #Dactyloa
 
  scale_x_continuous(breaks = seq(14, 25, 3), limit = c(14, 25)) +
  scale_y_continuous(breaks = seq(6, 22, 4), limit = c(6, 22)) +
  theme_classic() +
  
  guides(col = guide_legend(nrow = 4, override.aes = list(size = 5)))+
  
  scale_color_manual(values=c("black", "#004949","#009292", "#ff6db6",
                              "#ffb6db","#490092", "#006ddb","#b66dff", 
                              "#6db6ff","#b6dbff", "#920000", "#924900",
                              "#24ff24","#ffff6d"),
                     breaks = c("antonii", "calimae", "chloris","chocorum", "granuliceps",
                                "heterodermus", "maculigula", "maculiventris",
                                "notopholis", "princeps", "ventrimaculatus"),
                     labels = c("anto", "cali", "chlr","purp", "gran",
                                "hetr", "macg", "macv",
                                "noto", "prin", "vent")) +
  
  theme(legend.key = element_rect(colour = "white"), 
        legend.background = element_rect(colour = "white"),
        legend.text=element_text(family="serif", size=15, face="italic"),
        legend.title=element_blank()) +
  
  guides(color=guide_legend(ncol =1)) +
  
  xlab(expression(T[e]~minimum~("°"*C))) +
  ylab(expression(CT[min]~("°"*C))) +
  
  theme(axis.text.x = element_text(family="serif", size = 26, lineheight = 0.9, vjust = 1)) +
  theme(axis.text.y = element_text(family="serif",size = 26, lineheight = 0.9, vjust = 1)) +
  theme(axis.title.y = element_text(family="serif",vjust=1.2, size=32)) +
  theme(axis.title.x = element_text(family="serif",vjust=-0.5, size=32))




###############
####       ####
#### CTmin ####
####       ####
###############
data3 = read.csv(file="New_PGLS_Tb_Sept24.csv", header=TRUE)
attach(data3)
head(data3)

# Tb vs Bio 5
bio5_breadth <- ggplot(data3, aes(x = bio5, y = breadth, shape = clade)) +
  geom_point(aes(color = species),  size = 6, alpha = 0.7) +
  geom_abline(intercept = 20.69766, slope = -0.16847, linewidth=1.5, linetype="dashed") + #pgls regression results
  geom_abline(intercept = 13.087843 , slope = 0.099272, linewidth=1.5, color = "#d8572a") + #Draconura
  geom_abline(intercept = 23.42790, slope = -0.26674, linewidth=1.5, color = "#004e98") + #Dactyloa
  
  scale_x_continuous(breaks = seq(20, 30, 2), limit = c(20, 30)) +
  scale_y_continuous(breaks = seq(10, 26, 4), limit = c(10, 26)) +
  theme_classic() +
  
  guides(col = guide_legend(nrow = 4, override.aes = list(size = 5)))+
  
  scale_color_manual(values=c("black", "#004949","#009292", "#ff6db6",
                              "#ffb6db","#490092", "#006ddb","#b66dff", 
                              "#6db6ff","#b6dbff", "#920000", "#924900",
                              "#24ff24","#ffff6d"),
                     breaks = c("antonii", "calimae", "chloris","chocorum",
                                "heterodermus", "maculigula", "maculiventris",
                                "notopholis", "princeps", "ventrimaculatus"),
                     labels = c("anto", "cali", "chlr","purp",
                                "hetr", "macg", "macv",
                                "noto", "prin", "vent")) +
  
  theme(legend.position = "none",
        plot.margin = margin(0, 4, 1, 0.1, "cm"))+
  
  guides(color=guide_legend(ncol =2)) +
  
  xlab(expression(Bio~5~("°"*C))) +
  ylab(expression(T[br]~("°"*C))) +
  
  theme(axis.text.x = element_text(family="serif", size = 26, lineheight = 0.9, vjust = 1)) +
  theme(axis.text.y = element_text(family="serif",size = 26, lineheight = 0.9, vjust = 1)) +
  theme(axis.title.y = element_text(family="serif",vjust=1.2, size=32)) +
  theme(axis.title.x = element_text(family="serif",vjust=-0.5, size=32))


# Tb vs Te maximum
temax_breadth <- ggplot(data3, aes(x = te_max, y = breadth, shape = clade)) +
  geom_point(aes(color = species),  size = 6, alpha = 0.7) +
  geom_abline(intercept = 32.17876, slope = -0.32228, linewidth=1.5, linetype="dashed") + #pgls regression results
  geom_abline(intercept = 7.07250 , slope = 0.17674, linewidth=1.5, color = "#d8572a") + #Draconura
  geom_abline(intercept = 39.21984, slope = -0.45852, linewidth=1.5, color = "#004e98") + #Dactyloa
   
  scale_x_continuous(breaks = seq(46, 54, 2), limit = c(46, 54)) +
  scale_y_continuous(breaks = seq(10, 26, 4), limit = c(10, 26)) +
  theme_classic() +
  
  guides(col = guide_legend(nrow = 4, override.aes = list(size = 5)))+
  
  scale_color_manual(values=c("black", "#004949","#009292", "#ff6db6",
                              "#ffb6db","#490092", "#006ddb","#b66dff", 
                              "#6db6ff","#b6dbff", "#920000", "#924900",
                              "#24ff24","#ffff6d"),
                     breaks = c("antonii", "calimae", "chloris","chocorum",
                                "heterodermus", "maculigula", "maculiventris",
                                "notopholis", "princeps", "ventrimaculatus"),
                     labels = c("anto", "cali", "chlr","purp",
                                "hetr", "macg", "macv",
                                "noto", "prin", "vent")) +
  
  theme(legend.position = "none",
        plot.margin = margin(0, 5, 1, 0.1, "cm"))+
  
  guides(color=guide_legend(ncol =2)) +
  
  xlab(expression(T[e]~maximum~("°"*C))) +
  ylab(expression(T[br]~("°"*C))) +
  
  theme(axis.text.x = element_text(family="serif", size = 26, lineheight = 0.9, vjust = 1)) +
  theme(axis.text.y = element_text(family="serif",size = 26, lineheight = 0.9, vjust = 1)) +
  theme(axis.title.y = element_text(family="serif",vjust=1.2, size=32)) +
  theme(axis.title.x = element_text(family="serif",vjust=-0.5, size=32))


# Tb vs Bio 6
bio6_breadth <- ggplot(data3, aes(x = bio6, y = breadth, shape = clade)) +
  geom_point(aes(color = species),  size = 6, alpha = 0.7) +
  geom_abline(intercept = 19.29288, slope = -0.18169, linewidth=1.5, linetype="dashed") + #pgls regression results
  geom_abline(intercept = 13.89369 , slope =  0.10701, linewidth=1.5, color = "#d8572a") + #Draconura
  geom_abline(intercept = 20.73941 , slope = -0.25760, linewidth=1.5, color = "#004e98") + #Dactyloa
  
  scale_x_continuous(breaks = seq(10, 20, 2), limit = c(10, 20)) +
  scale_y_continuous(breaks = seq(10, 26, 4), limit = c(10, 26)) +
  theme_classic() +
  
  guides(col = guide_legend(nrow = 4, override.aes = list(size = 5)))+
  
  scale_color_manual(values=c("black", "#004949","#009292", "#ff6db6",
                              "#ffb6db","#490092", "#006ddb","#b66dff", 
                              "#6db6ff","#b6dbff", "#920000", "#924900",
                              "#24ff24","#ffff6d"),
                     breaks = c("antonii", "calimae", "chloris","chocorum",
                                "heterodermus", "maculigula", "maculiventris",
                                "notopholis", "princeps", "ventrimaculatus"),
                     labels = c("anto", "cali", "chlr","purp",
                                "hetr", "macg", "macv",
                                "noto", "prin", "vent")) +
  
  theme(legend.position = "none",
        plot.margin = margin(0, 5, 1, 0.1, "cm"))+
  
  guides(color=guide_legend(ncol =1)) +
  
  xlab(expression(Bio~6~("°"*C))) +
  ylab(expression(T[br]~("°"*C))) +
  
  theme(axis.text.x = element_text(family="serif", size = 26, lineheight = 0.9, vjust = 1)) +
  theme(axis.text.y = element_text(family="serif",size = 26, lineheight = 0.9, vjust = 1)) +
  theme(axis.title.y = element_text(family="serif",vjust=1.2, size=32)) +
  theme(axis.title.x = element_text(family="serif",vjust=-0.5, size=32))


# Tb vs Te minimum
temin_breadth <- ggplot(data3, aes(x = te_min, y = breadth, shape = clade)) +
  geom_point(aes(color = species),  size = 6, alpha = 0.7) +
  geom_abline(intercept = 18.92102, slope = -0.12676, linewidth=1.5, linetype="dashed") + #pgls regression results
  geom_abline(intercept = 14.026913 , slope =  0.079928, linewidth=1.5, color = "#d8572a") + #Draconura
  geom_abline(intercept = 20.39978 , slope = -0.18857, linewidth=1.5, color = "#004e98") + #Dactyloa
  
  scale_x_continuous(breaks = seq(14, 25, 3), limit = c(14, 25)) +
  scale_y_continuous(breaks = seq(10, 26, 4), limit = c(10, 26)) +
  theme_classic() +
  
  guides(col = guide_legend(nrow = 4, override.aes = list(size = 5)))+
  
  scale_color_manual(values=c("black", "#004949","#009292", "#ff6db6",
                              "#ffb6db","#490092", "#006ddb","#b66dff", 
                              "#6db6ff","#b6dbff", "#920000", "#924900",
                              "#24ff24","#ffff6d"),
                     breaks = c("antonii", "calimae", "chloris","chocorum",
                                "heterodermus", "maculigula", "maculiventris",
                                "notopholis", "princeps", "ventrimaculatus"),
                     labels = c("anto", "cali", "chlr","purp",
                                "hetr", "macg", "macv",
                                "noto", "prin", "vent")) +
  
  theme(legend.key = element_rect(colour = "white"), 
        legend.background = element_rect(colour = "white"),
        legend.text=element_text(family="serif", size=15, face="italic"),
        legend.title=element_blank()) +
  
  guides(color=guide_legend(ncol =1)) +
  
  xlab(expression(T[e]~minimum~("°"*C))) +
  ylab(expression(T[br]~("°"*C))) +
  
  theme(axis.text.x = element_text(family="serif", size = 26, lineheight = 0.9, vjust = 1)) +
  theme(axis.text.y = element_text(family="serif",size = 26, lineheight = 0.9, vjust = 1)) +
  theme(axis.title.y = element_text(family="serif",vjust=1.2, size=32)) +
  theme(axis.title.x = element_text(family="serif",vjust=-0.5, size=32))






################################
####                        ####
####  Let's save our plot   ####
####                        ####
################################

##############
## Let's save our plot
png("BoxPlot_Sept25_Supp.png", height = 16, width = 24, units = "in", res = 300)


plot_grid(bio5_breadth, temax_breadth, bio6_breadth,  temin_breadth, 
          bio5_ctmin, temax_ctmin, bio6_ctmin, temin_ctmin,
          bio5_ctmax, temax_ctmax, bio6_ctmax, temin_ctmax,
          labels = c("(A)", "(B)", "(C)","(D)", "(E)", "(F)", "(G)", "(H)", "(I)","(J)", "(K)", "(L)"), 
          ncol = 4, nrow = 3, label_fontfamily = "serif", label_size = 23, 
          align = "h", label_x = -0.03, label_y = 1.009, label_fontface = "bold")

dev.off()
