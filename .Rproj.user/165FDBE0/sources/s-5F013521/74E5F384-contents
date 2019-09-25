# Assess adult oyster length after temperature treatment, pH treatment. 

broodstock.length <- read.csv("Data/broodstock-length-data.csv", header=T, na.strings = NA)
broodstock.length$Length <- as.numeric(as.character(broodstock.length$Length))
broodstock.length$Temperature <- as.factor(as.character(broodstock.length$Temperature))

# Mean lengths 
aggregate(Length ~ Population + pH + Temperature, data=broodstock.length, mean)
aggregate(Length ~ Population + pH, data=broodstock.length, mean)
aggregate(Length ~ Population, data=broodstock.length, mean)

aggregate(Length ~ Population, data=subset(broodstock.length, pH!="Pre-pH"), mean)

# Check length data for normality 
qqnorm(broodstock.length$Length)
hist(broodstock.length$Length)

# Cohort difference before pH treatmemt, not including O-2?
summary(aov(Length ~ Population, data=subset(broodstock.length, pH=="Pre-pH" & Population!="K"))) #yes 
TukeyHSD(aov(Length ~ Population, data=subset(broodstock.length, pH=="Pre-pH" & Population!="K"))) # NF > HL only 

# Length difference after temperature treatment, before pH treatmemt?
summary(aov(Length ~ Temperature, data=subset(broodstock.length, pH=="Pre-pH"))) #no. 

# Length difference after temperature and pH treatment? 
summary(aov(Length ~ Temperature*pH, data=subset(broodstock.length, pH!="Pre-pH"))) #no 

# Length difference by cohort after all treatments? 
summary(aov(Length ~ Population, data=subset(broodstock.length, pH!="Pre-pH" & Population!="K"))) #yes
TukeyHSD(aov(Length ~ Population, data=subset(broodstock.length, pH!="Pre-pH" & Population!="K"))) #yes

# Did oysters grow/change in ambient pH? 
summary(aov(Length ~ pH, data=subset(broodstock.length, pH!="Low"))) # No 

# Did oysters grow/change in low pH? 
summary(aov(Length ~ pH, data=subset(broodstock.length, pH!="Ambient"))) # No 

# By cohort, did they grow in ambient? 
summary(aov(Length ~ pH*Temperature, data=subset(broodstock.length, pH!="Low" & Population=="NF")))
0.0472*4 #correct p
summary(aov(Length ~ pH*Temperature, data=subset(broodstock.length, pH!="Low" & Population=="HL"))) #NO 
summary(aov(Length ~ pH*Temperature, data=subset(broodstock.length, pH!="Low" & Population=="SN"))) #YES
0.00297*4 #correct p
kruskal.test(x = subset(broodstock.length, pH!="Pre-pH" & Population=="K")$Length, g = subset(broodstock.length, pH!="Ambient" & Population=="K")$Length) #NO

# Grow in LOW pH? 
summary(aov(Length ~ pH*Temperature, data=subset(broodstock.length, pH!="Ambient" & Population=="NF"))) #NO 
summary(aov(Length ~ pH*Temperature, data=subset(broodstock.length, pH!="Ambient" & Population=="HL"))) #NO 
summary(aov(Length ~ pH*Temperature, data=subset(broodstock.length, pH!="Ambient" & Population=="SN"))) #NO
summary(aov(Length ~ pH*Temperature, data=subset(broodstock.length, pH!="Ambient" & Population=="K"))) #YES, but after p-adj, no

# Change factor orders for plots 
broodstock.length$pH <- factor(broodstock.length$pH, levels = c("Pre-pH",  "Low", "Ambient"))
broodstock.length$Population <- factor(broodstock.length$Population, levels = c("HL", "NF", "SN", "K"))
broodstock.length$Temperature <- factor(broodstock.length$Temperature, levels = c(6, 10))
brood.col.lengths <- c("white","gray55","gray85")[as.numeric(broodstock.length$pH)]
names(brood.col.lengths) <-broodstock.length$pH

# =======
# Supplemental Materials - adult shell lengths 

pdf(file="Figures/broodstock-height.pdf", width=5, height = 5)
ggplot(broodstock.length, aes(x=Population, y= Length, fill=pH)) +  geom_boxplot() + 
  geom_point(size=1.25, position=position_jitterdodge(jitter.width = 0.18, jitter.height = 0, dodge.width = 0.75),aes(group=pH)) + labs(title=(expression(paste("Shell height by cohort, ", pCO[2], " exposure"))), y=expression("Shell height (mm)")) + 
  theme_bw(base_size = 12) + 
  theme(plot.title = element_text(size = 14, hjust = 0, colour = "gray30"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.title.x = element_blank(), legend.position="bottom") +  geom_vline(xintercept = c(1.5, 2.5, 3.5), colour="gray") + scale_fill_manual(values=brood.col.lengths, name=element_blank(), labels = c(expression(pre-pCO[2]), expression(High-pCO[2]), expression(Ambient-pCO[2])))+ scale_x_discrete(labels = c('Dabob Bay','Fidalgo Bay', "Oyster Bay C1",'Oyster Bay C2')) + scale_y_continuous(limits=c(min=min(broodstock.length$Length),max=max(subset(broodstock.length, Population!="K")$Length)))
dev.off()
