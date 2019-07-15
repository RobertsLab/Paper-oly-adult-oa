# Analyzie juvenile deployment data 

Deploy.data <- read.csv("Data/juvenile-deployment-data.csv", header=T, na.strings = c("NA", "?", "Unknown", "TBD"))
Deploy.data <- droplevels.data.frame(subset(Deploy.data, SURVIVED != "NA"))
bays <- levels(Deploy.data$BAY) 

# Plot % survival in each pouch for initial inspection. # adjacent to points = pouch number during deployment. 

plot_list_surv = list()
for (i in 1:4) {
p <- ggplot(subset(Deploy.data, BAY==bays[i]), aes(x=HABITAT:POPULATION:PH, y= 100*(SURVIVED/DEPLOYED))) + geom_boxplot(aes(fill=PH)) + geom_vline(xintercept = c(2.5, 4.5, 6.5, 10.5, 12.5, 14.5), colour="gray") + geom_vline(xintercept = c(8.5), colour="gray20") + scale_fill_manual(values=c('#d1e5f0', '#67a9cf'), name="pCO2", labels = c("Amb.", "High")) + labs(title=paste("Bay = ", bays[i], sep=""),y=expression("% Survival")) + theme(legend.position="none")  + geom_point(aes(group=HABITAT:POPULATION:PH), position=position_jitter(width = 0.25), size=1.8) + geom_text(aes(label=POUCH),hjust=0, vjust=0)
plot_list_surv[[i]] <- p
}
plot_list_surv

# Stats to compare survival. Find best fit model, then compare. 

# ONE BIG MODEL & EACH FACTOR SEPARATELY 
Anova(glm.outplant1 <- glm(cbind(SURVIVED, DEPLOYED) ~ POPULATION, data=Deploy.data, binomial), test="Wald") 
Anova(glm.outplant2 <- glm(cbind(SURVIVED, DEPLOYED) ~ BAY, data=Deploy.data, binomial), test="Wald")
Anova(glm.outplant3 <- glm(cbind(SURVIVED, DEPLOYED) ~ PH, data=Deploy.data, binomial), test="Wald")
Anova(glm.outplant4 <- glm(cbind(SURVIVED, DEPLOYED) ~ POPULATION*BAY*PH, data=Deploy.data, binomial), test="Wald") 
Anova(glm.outplant5 <- glm(cbind(SURVIVED, DEPLOYED) ~ POPULATION*BAY*PH- POPULATION:BAY:PH , data=Deploy.data, binomial), test="Wald") 
Anova(glm.outplant6 <- glm(cbind(SURVIVED, DEPLOYED) ~ (POPULATION+BAY)*PH, data=Deploy.data, binomial), test="Wald") # <-- final model, same variables sign. as in full model
AIC(glm.outplant1,glm.outplant2,glm.outplant3,glm.outplant4,glm.outplant5,glm.outplant6)

# Survival differences between bay, population, and interaction? Change reference factor for all comparisons. 
Deploy.data$POPULATION <- relevel(Deploy.data$POPULATION, ref = "HC")
Deploy.data$BAY <- relevel(Deploy.data$BAY, ref = "PG")
Deploy.data$PH <- relevel(Deploy.data$PH, ref = "L")
summary(glm.outplant6) 

# Inspect population differences as sole factor
summary(glm(cbind(SURVIVED, DEPLOYED) ~ POPULATION, data=Deploy.data, binomial), test="Chi")

# Inspect bays as sole factor
summary(glm(cbind(SURVIVED, DEPLOYED) ~ BAY, data=Deploy.data, binomial), test="Chi")

# Inspect PH differences as sole factor
summary(glm(cbind(SURVIVED, DEPLOYED) ~ PH, data=Deploy.data, binomial), test="Chi")

# Inspect PH differences by population 
summary(glm(cbind(SURVIVED, DEPLOYED) ~ POPULATION:PH, data=Deploy.data, binomial), test="Chi")

# Inspect pH differences by bay  
summary(glm(cbind(SURVIVED, DEPLOYED) ~ BAY:PH, data=Deploy.data, binomial), test="Chi")

# Summary stats

# mean & sd survival by parental pH                    
aggregate((SURVIVED/DEPLOYED) ~ PH, Deploy.data, FUN = function(x) c(mean = mean(x), sd = sd(x)))

# mean & sd survival by parental pH & cohort 
aggregate((SURVIVED/DEPLOYED) ~ PH + POPULATION, subset(Deploy.data, HABITAT != "NA"), FUN = function(x) c(mean = mean(x), sd = sd(x)))

# mean & sd survival by cohort
aggregate((SURVIVED/DEPLOYED) ~ POPULATION, Deploy.data, FUN = function(x) c(mean = mean(x), sd = sd(x)))

# mean & sd survival by bay 
aggregate((SURVIVED/DEPLOYED) ~ BAY, Deploy.data, FUN = function(x) c(mean = mean(x), sd = sd(x)))

# mean & sd survival by bay, cohort, parental pH
aggregate((SURVIVED/DEPLOYED) ~ PH + BAY, Deploy.data, FUN = function(x) c(mean = mean(x), sd = sd(x)))

# total animals deployed by parental pH, cohort & bay
aggregate(DEPLOYED ~ PH + POPULATION + BAY, Deploy.data, sum, na.rm=TRUE) 

#  Survival boxplots

# Re-order and rename factors 
Deploy.data$PH <- as.factor(Deploy.data$PH)
Deploy.data$PH <- gsub("A", "Ambient", Deploy.data$PH)
Deploy.data$PH <- gsub("L", "Low", Deploy.data$PH)
Deploy.data$POPULATION <- factor(Deploy.data$POPULATION, levels=c("FB", "HC", "SSF1", "SSF2"))


# Fidalgo Bay
pdf(file="Figures/deployment-survival-FB.pdf", width=3.23, height = 4.5)
ggplot(subset(Deploy.data, HAB.PH !="NA" & BAY=="FB"), aes(x=PH, y= 100*(SURVIVED/DEPLOYED), fill=PH)) + 
  geom_boxplot(width=0.35, position = position_dodge(width = .55)) +
  scale_fill_manual(values=c('#d1e5f0', '#67a9cf'), name="Parental pCO2", labels = c("Ambient", "High")) + 
  geom_point(size=2.75, color="gray20", aes(shape=POPULATION, group=PH), position=position_jitterdodge(jitter.width = 0.75, jitter.height = .1, dodge.width = 0.5)) + 
  scale_shape_manual(values=c(15, 17, 19, 8), labels=c("Fidalgo Bay", "Dabob Bay", "Oyster Bay C1", "Oyster Bay C2")) + 
  labs(title="Fidalgo Bay",y=expression("Percent Survival")) + 
  theme_bw(base_size = 12) + 
  theme(plot.title = element_text(size = 14, hjust = 0, vjust=-0.6, colour="gray30"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none", axis.title.y = element_text(colour = "gray30", size=12)) + 
  geom_vline(xintercept = c(1.5), colour="gray") + 
  scale_x_discrete(labels = element_blank()) + 
  scale_y_continuous(limits=c(min=-1, max=110), breaks = c(0, 25, 50, 75, 100))
dev.off()

# Port Gamble Bay
pdf(file="Figures/deployment-survival-PG.pdf", width=2.75, height = 4.5)
ggplot(subset(Deploy.data, HAB.PH !="NA" & BAY=="PG"), aes(x=PH, y= 100*(SURVIVED/DEPLOYED), fill=PH)) + 
  geom_boxplot(width=0.35, position = position_dodge(width = .55)) +
  scale_fill_manual(values=c('#d1e5f0', '#67a9cf'), name="Parental pCO2", labels = c("Ambient", "High")) + 
  geom_point(size=2.75, color="gray20", aes(shape=POPULATION, group=PH), position=position_jitterdodge(jitter.width = 0.75, jitter.height = .1, dodge.width = 0.5)) + 
  scale_shape_manual(values=c(15, 17, 19, 8), labels=c("Fidalgo Bay", "Dabob Bay", "Oyster Bay C1", "Oyster Bay C2")) + 
  labs(title="Port Gamble Bay",y=expression("Percent Survival")) + 
  theme_bw(base_size = 12) + 
  theme(plot.title = element_text(size = 14, hjust = 0, vjust=-0.6, colour="gray30"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none", axis.text.y=element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank()) + 
  geom_vline(xintercept = c(1.5), colour="gray") +  
  scale_x_discrete(labels = element_blank()) + scale_y_continuous(limits=c(min=-1, max=110), breaks = c(0, 25, 50, 75, 100))
dev.off()

# Skokomish River Delta 
pdf(file="Figures/deployment-survival-SK.pdf", width=2.75, height = 4.5)
ggplot(subset(Deploy.data, HAB.PH !="NA" & BAY=="SK"), aes(x=PH, y= 100*(SURVIVED/DEPLOYED), fill=PH)) + 
  geom_boxplot(width=0.35, position = position_dodge(width = .55)) +
  scale_fill_manual(values=c('#d1e5f0', '#67a9cf'), name="Parental pCO2", labels = c("Ambient", "High")) + 
  geom_point(size=2.75, color="gray20", aes(shape=POPULATION, group=PH), position=position_jitterdodge(jitter.width = 0.75, jitter.height = .1, dodge.width = 0.5)) + 
  scale_shape_manual(values=c(15, 17, 19, 8), labels=c("Fidalgo Bay", "Dabob Bay", "Oyster Bay C1", "Oyster Bay C2")) + labs(title="Skokomish River Delta",y=expression("Percent Survival")) + 
  theme_bw(base_size = 12) + 
  theme(plot.title = element_text(size = 14, hjust = 0, vjust=-0.6, colour="gray30"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none", axis.text.y=element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank()) + 
  geom_vline(xintercept = c(1.5), colour="gray") + 
  scale_x_discrete(labels = element_blank()) + scale_y_continuous(limits=c(min=-1, max=110), breaks = c(0, 25, 50, 75, 100))
dev.off()

# Case Inlet 
pdf(file="Figures/deployment-survival-CI.pdf", width=2.75, height = 4.5)
ggplot(subset(Deploy.data, HAB.PH !="NA" & BAY=="CI"), aes(x=PH, y= 100*(SURVIVED/DEPLOYED), fill=PH)) + 
  geom_boxplot(width=0.35, position = position_dodge(width = .55)) +
  scale_fill_manual(values=c('#d1e5f0', '#67a9cf'), name="Parental pCO2", labels = c("Ambient", "High")) + 
  geom_point(size=2.75, color="gray20", aes(shape=POPULATION, group=PH), position=position_jitterdodge(jitter.width = 0.75, jitter.height = .1, dodge.width = 0.5)) + 
  scale_shape_manual(values=c(15, 17, 19, 8), labels=c("Fidalgo Bay", "Dabob Bay", "Oyster Bay C1", "Oyster Bay C2")) + labs(title="Case Inlet",y=expression("Percent Survival")) + 
  theme_bw(base_size = 12) + 
  theme(plot.title = element_text(size = 14, hjust = 0, vjust=-0.6, colour="gray30"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none", axis.text.y=element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank()) + 
  geom_vline(xintercept = c(1.5), colour="gray") +
  scale_x_discrete(labels = element_blank()) + scale_y_continuous(limits=c(min=-1, max=110), breaks = c(0, 25, 50, 75, 100))
dev.off()

# Plot for legend only 
pdf(file="Figures/deployment-survival-legend.pdf", width=5, height = 5)
ggplot(subset(Deploy.data, HAB.PH !="NA" & BAY=="FB"), aes(x=PH, y= 100*(SURVIVED/DEPLOYED), fill=PH)) + 
  geom_point(aes(shape=POPULATION)) + 
  geom_boxplot() +
  scale_fill_manual(values=c('#d1e5f0', '#67a9cf'), name=(expression(paste("Parental ", pCO[2]))), labels = c("Ambient", "High")) + 
  scale_shape_manual(values=c(15, 17, 19, 8), labels=c("Fidalgo Bay", "Dabob Bay", "Oyster Bay C1", "Oyster Bay C2"), name="Cohort") + labs(title="plot for title") + 
  theme(legend.text=element_text(size=14, colour = "gray30"), legend.title=element_text(size=14, colour = "gray30"), legend.position="right") + guides(shape = guide_legend(override.aes = list(size=5)))
dev.off()



# =============================================
#-------- Assess shell height data before & after deployment

Deploy.data.length <- melt(Deploy.data, id.vars = c("DATE", "BAY", "HABITAT", "POUCH", "POPULATION", "PH", "SIZE", "HAB.PH", "POP.PH.HAB", "DEPLOYED", "SURVIVED", "MASS.F"), variable.name = "oyster.rep", value.name = "length.mm")
Deploy.data.length <- droplevels.data.frame(subset(Deploy.data.length, length.mm != "NA"))
Deploy.data.length[c("PH", "POUCH")] <- lapply(Deploy.data.length[c("PH", "POUCH")], as.factor)

# Check out length between habitat, within parental history, across all locations
aggregate(length.mm ~ PH + POPULATION, Deploy.data.length, FUN = function(x) c(mean = mean(x), sd = sd(x)))

# plot lengths by bay, site in bay (habitat), cohort, parental pH

plot_list_length = list()
for (i in 1:4) {
  p <- ggplot(subset(Deploy.data.length, BAY==bays[i]), aes(x=HABITAT:POPULATION:PH, y=length.mm)) + geom_boxplot(aes(fill=PH)) + geom_vline(xintercept = c(2.5, 4.5, 6.5, 10.5, 12.5, 14.5), colour="gray") + geom_vline(xintercept = c(8.5), colour="gray20") + scale_fill_manual(values=c('#d1e5f0', '#67a9cf'), name="pCO2", labels = c("Amb.", "High")) + labs(title=paste("Shell Heights, Bay = ", bays[i], sep=""),y=expression("Shell height (mm)")) + theme(legend.position="none")  + geom_point(aes(group=HABITAT:POPULATION:PH), position=position_jitter(width = 0.25), size=1.1)
  plot_list_length[[i]] <- p
}
plot_list_length

colors3 <- c("Ambient"= "#d1e5f0", "Low"="#67a9cf")
# length distribution after deployment, by parental pH (all cohorts, all bays)
ggdensity(data=Deploy.data.length, x = "length.mm",
          add = "mean", rug = TRUE,
          color = "PH", fill = "PH",  palette = colors3) +
  labs(title="Shell length (mm)\n Post-Deployment (@ 15 months)",x="shell length (mm)") +
  font("title", size = 20, face = "bold") +
  font("xlab", size = 18) +
  font("ylab", size = 18) +
  font("xy.text", size = 18) +
  font("legend.title", size=18) +
  font("legend.text", size=18)

ggplot(Deploy.data.length, aes(x=POPULATION:PH, y=length.mm, fill=PH, colour=POPULATION)) + 
  geom_boxplot() +
  labs(title="Shell length by population\nafter deployment",y=expression("Growth (mean length change within pouch, mm)")) + 
  theme_bw(base_size = 14) + xlab("Parental pH and Habitat") +
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0)) + 
  scale_fill_manual(values=c('#d1e5f0', '#67a9cf'), name=(expression(paste("Parental ", pCO[2]))), labels = c("Ambient", "High")) +
  scale_colour_manual(values=c('red', 'yellow', "green", "purple"))


# ----- Calculate growth: mean final length - mean initial length 

# read in initial shell height data prior to deployment 
Oly.size <- read.csv("Data/1yr-offspring-size.csv", header = T, stringsAsFactors = T)

# Extract only pouches that were deployed for initial length 
Pre.length.deployed <- melt(Oly.size[Oly.size$BAG %in% Deploy.data$POUCH,][,c(-2,-4,-5,-8)], id.vars = c("BAG", "COHORT", "PH", "SIZE.CLASS"), na.action = na.omit)
Pre.length.deployed$value <- as.numeric(Pre.length.deployed$value)
Pre.length.deployed <- subset(Pre.length.deployed, BAG != "NA" & (value != "NA" & value != "s" & value != ""))
Pre.length.deployed$BAG <- as.factor(Pre.length.deployed$BAG)
Pre.length.mean <- aggregate(value ~ BAG + COHORT + PH + SIZE.CLASS, Pre.length.deployed, mean, na.rm=TRUE)
colnames(Pre.length.mean) <- c("BAG", "COHORT", "PH", "SIZE.CLASS", "PRE.LENGTH")


# Plot initial shell length 
ggplot(Pre.length.deployed, aes(x=COHORT:PH, y=value)) + geom_boxplot(aes(fill=PH, colour=COHORT)) + scale_fill_manual(values=c('#d1e5f0', '#67a9cf'), name="pCO2", labels = c("Amb.", "High")) + scale_color_manual(values=c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3"), name="Cohort", labels=c("D", "O-2", "F", "O-1")) + labs(title="Juvenile shell height prior to deployment", y=expression("shell height"), x=expression("Cohort : Parental pH")) + theme(legend.position="right")  + geom_point(size=1, aes(group=COHORT:PH), position=position_jitter(width = 0.25))

# Mean initial size by cohort & parental pCO2 (befoe deployment)
aggregate(PRE.LENGTH ~  PH + COHORT, Pre.length.mean, FUN = function(x) c(mean = mean(x), sd = sd(x)))

# Calculate mean initial length within each pouch. Only use 6C groups (10C not deployed)

# Calculate mean final length within each pouch 
Deploy.length.mean <- aggregate(length.mm ~ POUCH + PH + BAY + HABITAT + POPULATION + SIZE, Deploy.data.length, mean, na.rm=TRUE)
colnames(Deploy.length.mean) <- c("POUCH", "PH", "BAY", "HABITAT", "POPULATION", "SIZE", "POST.LENGTH")

# Merge pre and post length data 
Deploy.growth <- merge(x=Pre.length.mean, y=Deploy.length.mean, by.x="BAG", by.y="POUCH")
Deploy.growth$Growth <- Deploy.growth$POST.LENGTH - Deploy.growth$PRE.LENGTH

# Inspect with plots 
plot_list_growth = list()
for (i in 1:4) {
  p <- ggplot(subset(Deploy.growth, BAY==bays[i]), aes(x=POPULATION:PH.y, y=Growth)) + geom_boxplot(aes(fill=PH.y)) + geom_vline(xintercept = c(2.5, 4.5, 6.5, 10.5, 12.5, 14.5), colour="gray") + geom_vline(xintercept = c(8.5), colour="gray20") + scale_fill_manual(values=c('#d1e5f0', '#67a9cf'), name="pCO2", labels = c("Amb.", "High")) + labs(title=paste("Relative Shell Height, Bay = ", bays[i], sep=""),y=expression("Mean shell height change"), x=expression("Cohort : Parental pH")) + theme(legend.position="none")  + geom_point(aes(group=POPULATION:PH.y), position=position_jitter(width = 0.25), size=1.8) + geom_text(aes(label=BAG),hjust=0, vjust=0)
  plot_list_growth[[i]] <- p
}
plot_list_growth

#Statistics 
hist((Deploy.growth$Growth+(-1*(min(Deploy.growth$Growth))+1))) #gamma dist. 
Deploy.growth$Growth.t <- (Deploy.growth$Growth+(-1*(min(Deploy.growth$Growth))+1))

# Fit model 
Anova(glm.grw.1 <- glm(Growth.t ~ BAY*POPULATION*PH.y,family=Gamma(link="log"), data=Deploy.growth)) 
Anova(glm.grw.2 <- glm(Growth.t ~ BAY*POPULATION*PH.y-BAY:POPULATION:PH.y,family=Gamma(link="log"), data=Deploy.growth)) 
Anova(glm.grw.3 <- glm(Growth.t ~ BAY*POPULATION*PH.y-BAY:POPULATION:PH.y-PH.y,family=Gamma(link="log"), data=Deploy.growth)) 
Anova(glm.grw.4 <- glm(Growth.t ~ BAY*POPULATION*PH.y-BAY:POPULATION:PH.y-PH.y-BAY:POPULATION,family=Gamma(link="log"), data=Deploy.growth)) 
Anova(glm.grw.5 <- glm(Growth.t ~ BAY*POPULATION*PH.y-BAY:POPULATION:PH.y-PH.y-BAY:POPULATION-BAY,family=Gamma(link="log"), data=Deploy.growth)) 
Anova(glm.grw.6 <- glm(Growth.t ~ POPULATION+POPULATION:PH.y,family=Gamma(link="log"), data=Deploy.growth)) 
AIC(glm.grw.1, glm.grw.2, glm.grw.3, glm.grw.4, glm.grw.5, glm.grw.6)
summary(glm.grw.6)

ggplot(Deploy.growth, aes(x=BAY:PH.y, y=Growth)) + geom_boxplot(aes(fill=PH.y)) + geom_vline(xintercept = c(2.5, 4.5, 6.5, 10.5, 12.5, 14.5), colour="gray") + geom_vline(xintercept = c(8.5), colour="gray20") + scale_fill_manual(values=c('#d1e5f0', '#67a9cf'), name="pCO2", labels = c("Amb.", "High")) + labs(title=paste("Relative Shell Height"),y=expression("Mean shell height change"), x=expression("Cohort : Parental pH")) + theme(legend.position="right") 

# ===========================
#----------- Assess group mass data 

Pre.mass <- subset(Oly.size, TEMP==6 & BAG!="NA")[,c(1:8)]
Post.mass <- subset(Deploy.data, POUCH != "NA" & (POUCH != 167 & POUCH != 12))[,c(1:12)]
Pre.mass <- merge(x=Pre.mass, y=Post.mass[c("POUCH", "DEPLOYED")], by.x = "BAG", by.y="POUCH")

Pre.mass$Mass.pre.per <- Pre.mass$BAG.WEIGHT/Pre.mass$DEPLOYED
Post.mass$Mass.post.per <- Post.mass$MASS.F/Post.mass$SURVIVED
Post.mass$POP.PH.HAB <- droplevels(Post.mass$POP.PH.HAB)
plot(Pre.mass$Mass.pre.per ~ Pre.mass$PH)
plot(Post.mass$Mass.post.per ~ Post.mass$POP.PH.HAB)
Post.mass$BAY.PH.HAB <- as.factor(paste(Post.mass$BAY, Post.mass$PH, Deploy.growth$HABITAT, sep = "."))
Post.mass$PH <- as.factor(Post.mass$PH)

# Compare mean mass per oyster between parental pH, habitat 
qqPlot(subset(Post.mass, Mass.post.per!="NA" & PH != "NA")$Mass.post.per)
summary(aov(Mass.post.per ~ PH*HABITAT*BAY, data=subset(Post.mass, Mass.post.per!="NA" & PH != "NA")))

# Plot mean oyster mass by parental pCO2, bay 
ggplot(subset(Post.mass, Mass.post.per!="NA" & PH != "NA"),aes(x=BAY:PH, y=Mass.post.per, fill=PH)) + 
  geom_boxplot() +
  geom_point(size=2.75, color="gray20", aes(shape=POPULATION, group=PH), position=position_jitterdodge(jitter.width = 0.75, jitter.height = .1, dodge.width = 0.5)) + 
  scale_shape_manual(values=c(15, 17, 19, 8), labels=c("Fidalgo Bay", "Dabob Bay", "Oyster Bay C1", "Oyster Bay C2")) + 
  labs(title="Mean mass per oyster (g) after deployment",y=expression("Mass/oyster (g)")) + 
  theme_bw(base_size = 14) + xlab("Parental pCO2, bay") +
  theme(plot.title = element_text(face = 'bold',size = 20, hjust = 0)) + scale_fill_manual(values=c('#d1e5f0', '#67a9cf'))






# ===================================
# Old plots, showing separate cohorts (probably remove) 

pdf(file="Figures/deployment-survival-FB.pdf", width=5, height = 2.5)
ggplot(subset(Deploy.data, HAB.PH !="NA" & BAY=="FB"), aes(x=POPULATION, y= 100*(SURVIVED/DEPLOYED), fill=PH)) + geom_boxplot(width=0.3, position = position_dodge(width = .55)) +
  geom_point(position=position_jitterdodge(jitter.width = 0.13, jitter.height = 0, dodge.width = 0.5),aes(group=PH)) + labs(title="Fidalgo Bay",y=expression("Percent Survival")) + 
  theme_bw(base_size = 12) + 
  theme(plot.title = element_text(size = 14, hjust = 0, vjust=-0.6, colour="gray30"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none", axis.title.y = element_text(colour = "gray30", size=12)) + geom_vline(xintercept = c(1.5, 2.5, 3.5), colour="gray") + scale_fill_manual(values=c('#d1e5f0', '#67a9cf'), name="Parental pCO2", labels = c("Ambient", "High")) + scale_x_discrete(labels = element_blank())
dev.off()

pdf(file="Figures/deployment-survival-PG.pdf", width=5, height = 2.5)
ggplot(subset(Deploy.data, HAB.PH !="NA" & BAY=="PG"), aes(x=POPULATION, y= 100*(SURVIVED/DEPLOYED), fill=PH)) + geom_boxplot(width=0.3, position = position_dodge(width = .55)) +
  geom_point(position=position_jitterdodge(jitter.width = 0.13, jitter.height = 0, dodge.width = 0.5),aes(group=PH)) +
  labs(title="Port Gamble Bay",y=expression("Percent Survival")) + 
  theme_bw(base_size = 12) + 
  theme(plot.title = element_text(size = 14, hjust = 0, vjust=-0.6, colour="gray30"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none", axis.title.y = element_text(colour = "gray30", size=12)) + geom_vline(xintercept = c(1.5, 2.5, 3.5), colour="gray") + scale_fill_manual(values=c('#d1e5f0', '#67a9cf'), name="Parental pCO2", labels = c("Ambient", "High")) + scale_x_discrete(labels = element_blank())
dev.off()

pdf(file="Figures/deployment-survival-SK.pdf", width=5, height = 2.5)
ggplot(subset(Deploy.data, HAB.PH !="NA" & BAY=="SK"), aes(x=POPULATION, y= 100*(SURVIVED/DEPLOYED), fill=PH)) + geom_boxplot(width=0.3, position = position_dodge(width = .55)) +
  geom_point(position=position_jitterdodge(jitter.width = 0.13, jitter.height = 0, dodge.width = 0.5),aes(group=PH)) +
  labs(title="Skokomish River Delta",y=expression("Percent Survival")) + 
  theme_bw(base_size = 12) + 
  theme(plot.title = element_text(size = 14, hjust = 0, vjust=-0.6, colour="gray30"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none", axis.title.y = element_text(colour = "gray30", size=12)) + geom_vline(xintercept = c(1.5, 2.5, 3.5), colour="gray")  + scale_fill_manual(values=c('#d1e5f0', '#67a9cf'), name="Parental pCO2", labels = c("Ambient", "High")) + scale_x_discrete(labels = element_blank())
dev.off()

pdf(file="Figures/deployment-survival-CI.pdf", width=5.1, height = 3.2)
ggplot(subset(Deploy.data, HAB.PH !="NA" & BAY=="CI"), aes(x=POPULATION, y= 100*(SURVIVED/DEPLOYED), fill=PH)) + geom_boxplot(width=0.3, position = position_dodge(width = .55)) +
  geom_point(position=position_jitterdodge(jitter.width = 0.13, jitter.height = 0, dodge.width = 0.5),aes(group=PH)) +
  labs(title="Case Inlet",y=expression("Percent Survival")) + 
  theme_bw(base_size = 12) + 
  theme(plot.title = element_text(size = 14, hjust = 0, vjust=-0.6, colour="gray30"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank(), legend.text=element_text(size=14, colour = "gray30"), legend.title=element_text(size=14, colour = "gray30"), axis.title.y = element_text(colour = "gray30", size=12), legend.position="bottom") + geom_vline(xintercept = c(1.5, 2.5, 3.5), colour="gray") + scale_fill_manual(values=c('#d1e5f0', '#67a9cf'), name=(expression(paste("Parental ", pCO[2]))), labels = c("Ambient", "High")) + scale_x_discrete(labels = c('Fidalgo Bay','Dabob Bay', "Oyster Bay C1", 'Oyster Bay C2'))
dev.off()

# plot for title only 
pdf(file="Figures/deployment-survival-plot-title.pdf", width=6.6, height = 3.5)
ggplot(subset(Deploy.data, HAB.PH !="NA" & BAY=="CI"), aes(x=POPULATION, y= 100*(SURVIVED/DEPLOYED), fill=PH)) + geom_boxplot() + 
  labs(title=expression(paste("Survival by bay, cohort and parental ", pCO[2], " exposure"))) + 
  theme_bw(base_size = 12) + 
  theme(plot.title = element_text(size = 16, hjust = 0, vjust=2, colour="gray30"))
dev.off()


# Other options 


pdf(file="Figures/deployment-survival-FB.pdf", width=3.23, height = 4.5)
ggplot(subset(Deploy.data, HAB.PH !="NA" & BAY=="FB"), aes(x=PH, y= 100*(SURVIVED/DEPLOYED)))  + 
  geom_boxplot(colour="gray30", width=0.35, size=.8, fill="gray95") + 
  geom_point(size=3.5, aes(shape=POPULATION, colour=PH),position = position_jitter(w = 0.3, h = 0)) + 
  scale_color_manual(values=c('#67a9cf','#2166ac'), label=c("Ambient", "High"), name="Parental pCO2") + 
  scale_shape_manual(values=c(15, 17, 19, 8), labels=c("Fidalgo Bay", "Dabob Bay", "Oyster Bay C1", "Oyster Bay C2"), name="Cohort") + theme_bw(base_size = 10) + 
  labs(title="Fidalgo Bay",y=expression("Percent Survival")) + 
  theme_bw(base_size = 12) + 
  theme(plot.title = element_text(size = 14, hjust = 0, vjust=-0.6, colour="gray30"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none", axis.title.y = element_text(colour = "gray30", size=12)) + 
  geom_vline(xintercept = c(1.5), colour="gray") + 
  scale_x_discrete(labels = element_blank()) + 
  scale_y_continuous(limits=c(min=-1, max=110), breaks = c(0, 25, 50, 75, 100))
dev.off()

pdf(file="Figures/deployment-survival-PG.pdf", width=2.75, height = 4.5)
ggplot(subset(Deploy.data, HAB.PH !="NA" & BAY=="PG"), aes(x=PH, y= 100*(SURVIVED/DEPLOYED)))  + 
  geom_boxplot(colour="gray30", width=0.35, size=.8, fill="gray95") + 
  geom_point(size=3.5, aes(shape=POPULATION, colour=PH),position = position_jitter(w = 0.3, h = 0)) + 
  scale_color_manual(values=c('#67a9cf','#2166ac'), label=c("Ambient", "High"), name="Parental pCO2") + 
  scale_shape_manual(values=c(15, 17, 19, 8), labels=c("Fidalgo Bay", "Dabob Bay", "Oyster Bay C1", "Oyster Bay C2"), name="Cohort") + theme_bw(base_size = 10) + 
  labs(title="Port Gamble Bay",y=expression("Percent Survival")) + 
  theme_bw(base_size = 12) + 
  theme(plot.title = element_text(size = 14, hjust = 0, vjust=-0.6, colour="gray30"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none", axis.text.y=element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank()) + 
  geom_vline(xintercept = c(1.5), colour="gray") +  
  scale_x_discrete(labels = element_blank()) + scale_y_continuous(limits=c(min=-1, max=110), breaks = c(0, 25, 50, 75, 100))
dev.off()

pdf(file="Figures/deployment-survival-SK.pdf", width=2.75, height = 4.5)
ggplot(subset(Deploy.data, HAB.PH !="NA" & BAY=="SK"), aes(x=PH, y= 100*(SURVIVED/DEPLOYED)))  + 
  geom_boxplot(colour="gray30", width=0.35, size=.8, fill="gray95") + 
  geom_point(size=3.5, aes(shape=POPULATION, colour=PH),position = position_jitter(w = 0.3, h = 0)) + 
  scale_color_manual(values=c('#67a9cf','#2166ac'), label=c("Ambient", "High"), name="Parental pCO2") + 
  scale_shape_manual(values=c(15, 17, 19, 8), labels=c("Fidalgo Bay", "Dabob Bay", "Oyster Bay C1", "Oyster Bay C2"), name="Cohort") + theme_bw(base_size = 10) + 
  labs(title="Skokomish River Delta",y=expression("Percent Survival")) + 
  theme_bw(base_size = 12) + 
  theme(plot.title = element_text(size = 14, hjust = 0, vjust=-0.6, colour="gray30"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none", axis.text.y=element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank()) + 
  geom_vline(xintercept = c(1.5), colour="gray") +  
  scale_x_discrete(labels = element_blank()) + scale_y_continuous(limits=c(min=-1, max=110), breaks = c(0, 25, 50, 75, 100))
dev.off()

pdf(file="Figures/deployment-survival-CI.pdf", width=2.75, height = 4.5)
ggplot(subset(Deploy.data, HAB.PH !="NA" & BAY=="CI"), aes(x=PH, y= 100*(SURVIVED/DEPLOYED)))  + 
  geom_boxplot(colour="gray30", width=0.35, size=.8, fill="gray95") + 
  geom_point(size=3.5, aes(shape=POPULATION, colour=PH),position = position_jitter(w = 0.3, h = 0)) + 
  scale_color_manual(values=c('#67a9cf','#2166ac'), label=c("Ambient", "High"), name="Parental pCO2") + 
  scale_shape_manual(values=c(15, 17, 19, 8), labels=c("Fidalgo Bay", "Dabob Bay", "Oyster Bay C1", "Oyster Bay C2"), name="Cohort") + theme_bw(base_size = 10) + 
  labs(title="Case Inlet",y=expression("Percent Survival")) + 
  theme_bw(base_size = 12) + 
  theme(plot.title = element_text(size = 14, hjust = 0, vjust=-0.6, colour="gray30"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none", axis.text.y=element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank()) + 
  geom_vline(xintercept = c(1.5), colour="gray") +  
  scale_x_discrete(labels = element_blank()) + 
  scale_y_continuous(limits=c(min=-1, max=110), breaks = c(0, 25, 50, 75, 100))
dev.off()