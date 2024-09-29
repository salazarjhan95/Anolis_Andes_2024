library(ggplot2)
library(plyr)
library(grid)
library(gridExtra)
library(cowplot)
library(ggrepel)


##########################################################################
######                                                              ######
######         Box plot for CTmax, CTmin and Bio                    ######
######         vs Elevation, Bio 1 and Te average                   ######
######         uses "New_PGLS_CTmax_Sept24.csv"                     ######
######         uses "New_PGLS_CTmin_Sept24.csv"                     ######
######         uses "New_PGLS_Tb_Sept24.csv"                        ######
######                                                              ######
##########################################################################

setwd("C:/Users/jhanc/Box/Investigacion/Investigaciones/Side projects/Anolis/CT/Last_analysis/NewTree")



###############
####       ####
#### CTmax ####
####       ####
###############
data <- read.csv("New_PGLS_CTmax_Sept24.csv", header=1)
attach(data)
head(data)

## CTmax vs Bio 1
bio1_ctmax <- ggplot(data, aes(x = bio1, y = ctmax, shape = clade)) +
  geom_point(aes(color = Sp), size = 6, alpha = 0.7) +
  geom_abline(intercept = 17.870792, slope = 0.612534, linewidth=1.5, linetype="dashed") + # pgls.SEy regression result
  geom_abline(intercept = 14.214171 , slope = 0.743273, linewidth=1.5, color = "#d8572a") + #Draconura
  geom_abline(intercept = 19.974116, slope = 0.520007, linewidth=1.5, color = "#004e98") + #Dactyloa
  
  scale_x_continuous(breaks = seq(16, 24, 2), limit = c(16, 24)) +
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
                     labels = c("anto", "calim", "chlr","purp", "dani",
                                "hetr", "macg", "macv",
                                "noto", "prin", "vent")) +

  theme(legend.position = "none",
        plot.margin = margin(0, 5, 1, 0.1, "cm"))+
  
  guides(color=guide_legend(ncol =2)) +
  
  xlab(expression(Bio~1~("°"*C))) +
  ylab(expression(CT[max]~("°"*C))) +
  
  theme(axis.text.x = element_text(family="serif", size = 26, lineheight = 0.9, vjust = 1)) +
  theme(axis.text.y = element_text(family="serif",size = 26, lineheight = 0.9, vjust = 1)) +
  theme(axis.title.y = element_text(family="serif",vjust=1.2, size=32)) +
  theme(axis.title.x = element_text(family="serif",vjust=-0.5, size=32))



## CTmax vs Elevation
ele_ctmax <- ggplot(data, aes(x = mid.ele, y = ctmax, shape = clade)) +
  geom_point(aes(color = Sp), size = 6, alpha = 0.7) +
  geom_abline(intercept = 33.71600, slope = -0.00275, linewidth=1.5, linetype="dashed") + #pgls.SEy regression results
  geom_abline(intercept = 34.21586, slope = -0.00422, linewidth=1.5, color = "#d8572a") + #Draconura
  geom_abline(intercept = 33.73064, slope = -0.00239, linewidth=1.5, color = "#004e98") + #Dactyloa

  scale_x_continuous(breaks = seq(200, 2600, 800), limit = c(200, 2600)) +
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
  
  xlab(expression(Mean~elevation~(m))) +
  ylab(expression(CT[max]~("°"*C))) +

  theme(axis.text.x = element_text(family="serif", size = 26, lineheight = 0.9, vjust = 1)) +
  theme(axis.text.y = element_text(family="serif",size = 26, lineheight = 0.9, vjust = 1)) +
  theme(axis.title.y = element_text(family="serif",vjust=1.2, size=32)) +
  theme(axis.title.x = element_text(family="serif",vjust=-0.5, size=32))



# CTmax vs Te average
te_ctmax <- ggplot(data, aes(x = Te_ave, y = ctmax, shape = clade)) +
  geom_point(aes(color = Sp), size = 6, alpha = 0.7) +
  geom_abline(intercept = 7.538916, slope = 0.716098, linewidth=1.5, linetype="dashed") + #pgls.SEy regression results
  geom_abline(intercept = 3.543057 , slope = 0.819291, linewidth=1.5, color = "#d8572a") + #Draconura
  geom_abline(intercept = 10.359650, slope = 0.633801, linewidth=1.5, color = "#004e98") + #Dactyloa
  
  scale_x_continuous(breaks = seq(28, 36, 2), limit = c(28, 36)) +
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
  
  xlab(expression(T[e]~average~("°"*C))) +
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

# CTmin vs Bio 1
bio1_ctmin <- ggplot(data2, aes(x = bio1, y = ctmin, shape = clade)) +
  geom_point(aes(color = Sp),  size = 6, alpha = 0.7) +
  geom_abline(intercept = 2.5443051, slope = 0.5664828, linewidth=1.5, linetype="dashed") + #pgls.SEy regression results
  geom_abline(intercept = 5.961357, slope = 0.409415, linewidth=1.5, color = "#d8572a") + #Draconura
  geom_abline(intercept = -2.8124277, slope = 0.8464473, linewidth=1.5, color = "#004e98") + #Dactyloa
  
  scale_x_continuous(breaks = seq(16, 24, 2), limit = c(16, 24)) +
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
  
  xlab(expression(Bio~1~("°"*C))) +
  ylab(expression(CT[min]~("°"*C))) +
  
  theme(axis.text.x = element_text(family="serif", size = 26, lineheight = 0.9, vjust = 1)) +
  theme(axis.text.y = element_text(family="serif",size = 26, lineheight = 0.9, vjust = 1)) +
  theme(axis.title.y = element_text(family="serif",vjust=1.2, size=32)) +
  theme(axis.title.x = element_text(family="serif",vjust=-0.5, size=32))


# CTmin vs Elevation
ele_ctmin <- ggplot(data2, aes(x = mid.ele, y = ctmin, shape = clade)) +
  geom_point(aes(color = Sp),  size = 6, alpha = 0.7) +
  geom_abline(intercept = 16.940582, slope = -0.002407, linewidth=1.5, linetype="dashed") + #pgls.SEy regression results
  geom_abline(intercept = 16.183112, slope = -0.001695, linewidth=1.5, color = "#d8572a") + #Draconura
  geom_abline(intercept = 18.892566, slope =  -0.003549, linewidth=1.5, color = "#004e98") + #Dactyloa
  
  scale_x_continuous(breaks = seq(200, 2600, 800), limit = c(200, 2600)) +
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
  
  xlab(expression(Mean~elevation~(m))) +
  ylab(expression(CT[min]~("°"*C))) +
  
  theme(axis.text.x = element_text(family="serif", size = 26, lineheight = 0.9, vjust = 1)) +
  theme(axis.text.y = element_text(family="serif",size = 26, lineheight = 0.9, vjust = 1)) +
  theme(axis.title.y = element_text(family="serif",vjust=1.2, size=32)) +
  theme(axis.title.x = element_text(family="serif",vjust=-0.5, size=32))


# CTmin vs Te average
te_ctmin <- ggplot(data2, aes(x = Te_ave, y = ctmin, shape = clade)) +
  geom_point(aes(color = Sp), size = 6, alpha = 0.7) +
  geom_abline(intercept = -6.083262, slope = 0.638362, linewidth=1.5, linetype="dashed") + #pgls.SEy regression results
  geom_abline(intercept = 0.0401898, slope = 0.4527722, linewidth=1.5, color = "#d8572a") + #Draconura
  geom_abline(intercept = -12.281122, slope = 0.837458, linewidth=1.5, color = "#004e98") + #Dactyloa
  
  scale_x_continuous(breaks = seq(28, 36, 2), limit = c(28, 36)) +
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
  
  xlab(expression(T[e]~average~("°"*C))) +
  ylab(expression(CT[min]~("°"*C))) +
  
  theme(axis.text.x = element_text(family="serif", size = 26, lineheight = 0.9, vjust = 1)) +
  theme(axis.text.y = element_text(family="serif",size = 26, lineheight = 0.9, vjust = 1)) +
  theme(axis.title.y = element_text(family="serif",vjust=1.2, size=32)) +
  theme(axis.title.x = element_text(family="serif",vjust=-0.5, size=32))




###############
####       ####
####  Tb   ####
####       ####
###############
data3 = read.csv(file="New_PGLS_Tb_Sept24.csv", header=TRUE)
attach(data3)
head(data3)

# Tb vs Bio 1
bio1_breadth <- ggplot(data3, aes(x = bio1, y = breadth, shape = clade)) +
  geom_point(aes(color = species),  size = 6, alpha = 0.7) +
  geom_abline(intercept = 20.01982, slope = -0.17527, linewidth=1.5, linetype="dashed") + #pgls regression results
  geom_abline(intercept = 13.47216, slope = 0.10344, linewidth=1.5, color = "#d8572a") + #Draconura
  geom_abline(intercept =  22.03476, slope =  -0.26161, linewidth=1.5, color = "#004e98") + #Dactyloa
  
  scale_x_continuous(breaks = seq(16, 24, 2), limit = c(16, 24)) +
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
  
  xlab(expression(Bio~1~("°"*C))) +
  ylab(expression(T[br]~("°"*C))) +
  
  theme(axis.text.x = element_text(family="serif", size = 26, lineheight = 0.9, vjust = 1)) +
  theme(axis.text.y = element_text(family="serif",size = 26, lineheight = 0.9, vjust = 1)) +
  theme(axis.title.y = element_text(family="serif",vjust=1.2, size=32)) +
  theme(axis.title.x = element_text(family="serif",vjust=-0.5, size=32))


## Tb vs Elevation
ele_breadth <- ggplot(data3, aes(x = mid.ele, y = breadth, shape = clade)) +
  geom_point(aes(color = species),  size = 6, alpha = 0.7) +
  geom_abline(intercept = 1.5688e+01, slope = 6.3415e-04, linewidth=1.5, linetype="dashed") + #pgls regression results
  geom_abline(intercept = 16.4687773, slope = -0.0010010, linewidth=1.5, color = "#d8572a") + #Draconura
  geom_abline(intercept = 1.5553e+01, slope =  9.5644e-04, linewidth=1.5, color = "#004e98") + #Dactyloa
  
  scale_x_continuous(breaks = seq(200, 2600, 800), limit = c(200, 2600)) +
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
  
  xlab(expression(Mean~elevation~(m))) +
  ylab(expression(T[br]~("°"*C))) +
  
  theme(axis.text.x = element_text(family="serif", size = 26, lineheight = 0.9, vjust = 1)) +
  theme(axis.text.y = element_text(family="serif",size = 26, lineheight = 0.9, vjust = 1)) +
  theme(axis.title.y = element_text(family="serif",vjust=1.2, size=32)) +
  theme(axis.title.x = element_text(family="serif",vjust=-0.5, size=32))



te_breadth <- ggplot(data3, aes(x = te_ave, y = breadth, shape = clade)) +
  geom_point(aes(color = species), size = 6, alpha = 0.7) +
  geom_abline(intercept = 23.38013, slope = -0.21846, linewidth=1.5, linetype="dashed") + #pgls regression results
  geom_abline(intercept = 11.96686, slope = 0.11462, linewidth=1.5, color = "#d8572a") + #Draconura
  geom_abline(intercept = 27.12141, slope = -0.32811, linewidth=1.5, color = "#004e98") + #Dactyloa
  
  scale_x_continuous(breaks = seq(28, 36, 2), limit = c(28, 36)) +
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
  
  xlab(expression(T[e]~average~("°"*C))) +
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

png("BoxPlot_Sept25_Main.png", height = 16, width = 18, units = "in", res = 300)

plot_grid(bio1_breadth, te_breadth, ele_breadth,  bio1_ctmin, te_ctmin, ele_ctmin,  bio1_ctmax, te_ctmax, ele_ctmax, 
          labels = c("(A)", "(B)", "(C)","(D)", "(E)", "(F)","(G)", "(H)", "(I)"),
          ncol = 3, nrow = 3, label_fontfamily = "serif", label_size = 23,
          align = "h", label_x = -0.03, label_y = 1.009, label_fontface = "bold")

dev.off()
