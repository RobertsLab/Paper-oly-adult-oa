Oly.size <- read.csv("Data/9-month-Oly-Size.csv", header = T, stringsAsFactors = T)
Oly.size.long <- melt(Oly.size[c(-1, -7, -8)], id.vars = c("GROUP", "COHORT", "TREATMENT", "TEMP", "PH"), na.action = na.omit) 
Oly.size.long$value <- as.numeric(Oly.size.long$value)
Oly.size.long$TEMP <- as.factor(Oly.size.long$TEMP)

# Create dataframe with pop/treat groups, stocking density 
groups <- c("HL10-AMB", "HL10-LOW", "HL6-AMB", "HL6-LOW", "K10-AMB", "K10-LOW", "K6-AMB", "K6-LOW", "NF10-AMB", "NF10-LOW", "NF6-AMB", "NF6-LOW", "SN10-AMB", "SN10-LOW", "SN6-AMB", "SN6-LOW",  "SN10-AMB-Exp", "SN10-LOW-Exp", "SN6-AMB-Exp", "SN6-LOW-Exp")
stocked <- c(1311, 1091, 501, 834, 259, 122, 372, 341, 661, 77, 720, 684, 54, 35, 128, 211, 4, 19, 68, 194)
stock.dens <- data.frame(cbind(groups, stocked), stringsAsFactors = F)
stock.dens$stocked <- as.numeric(stock.dens$stocked)

Oly.size.summary <- cbind(
  aggregate(value ~ GROUP, Oly.size.long, length, na.action = na.omit),
  aggregate(value ~ GROUP, Oly.size.long, mean, na.action = na.omit)[2],
  aggregate(value ~ GROUP, Oly.size.long, sd, na.action = na.omit)[2],
  aggregate(value ~ GROUP, Oly.size.long, median, na.action = na.omit)[2]
)
colnames(Oly.size.summary) <- c("Group", "Count", "Mean", "SD", "Median")

aggregate(value ~ PH, subset(Oly.size.long, TEMP==6& (GROUP!="SN6-LOW-Exp" & GROUP !="SN6-AMB-Exp" & GROUP !="SN10-LOW-Exp" & GROUP !="SN10-AMB-Exp")), mean)
aggregate(value ~ PH, subset(Oly.size.long, TEMP==6& (GROUP!="SN6-LOW-Exp" & GROUP !="SN6-AMB-Exp" & GROUP !="SN10-LOW-Exp" & GROUP !="SN10-AMB-Exp")), sd)

# merge stocking density dataframe with size summary data 
Oly.size.summary2 <- merge(x=Oly.size.summary, y=stock.dens, by.x="Group", by.y="groups", all.x=T, all.y=F)
Oly.size.summary2$Perc.count <- (Oly.size.summary2$Count/Oly.size.summary2$stocked)*100
Oly.size.summary3 <- merge(x=Oly.size.summary2, y=unique(Oly.size[,c("GROUP", "COHORT", "TREATMENT", "TEMP", "PH")]), by.x = "Group", by.y="GROUP", all.x = T, all.y=F)
pch.list <- as.numeric(Oly.size.summary3$TREATMENT)

write.csv(file="Data/Oly.10-month-size.summary.csv", x=Oly.size.summary3)

# Remove the mini-experiment data, very small stocking density and only 1 spawning group 
Oly.size.summary4 <- dplyr::filter(Oly.size.summary3, !grepl("Exp",TREATMENT))
#Oly.size.summary4 <- filter(Oly.size.summary4, !grepl("NF10",Group))
?filter

# Inspect length by stocking density, 6C
plot(x=log(subset(Oly.size.summary4, TEMP==6)$stocked), y=subset(Oly.size.summary4, TEMP==6)$Mean, main="All Pops, 6C\nmean length ~ stocking density\ncolor = pH", xlab="Stocking Density", ylab = "Mean Length (mm)", col=Oly.size.summary4$PH, pch=19, cex=2)
text(x=log(subset(Oly.size.summary4, TEMP==6)$stocked)+.1, y=subset(Oly.size.summary4, TEMP==6)$Mean+.1, labels =subset(Oly.size.summary4, TEMP==6)$COHORT)
# mean length definitely a function of stocking density - include as a random effect in models 
anova(lm(subset(Oly.size.summary4, TEMP==6)$Median ~ subset(Oly.size.summary4, TEMP==6)$stocked + factor(subset(Oly.size.summary4, TEMP==6)$PH)))

# Inspect length by stocking density, 10C
plot(x=log(subset(Oly.size.summary4, TEMP==10)$stocked), y=subset(Oly.size.summary4, TEMP==10)$Mean, main="All Pops, 10C\nmean length ~ stocking density\ncolor = pH", xlab="Stocking Density", ylab = "Mean Length (mm)", col=Oly.size.summary4$PH, pch=19, cex=2)
text(x=log(subset(Oly.size.summary4, TEMP==10)$stocked)+.1, y=subset(Oly.size.summary4, TEMP==10)$Mean+.1, labels =subset(Oly.size.summary4, TEMP==10)$COHORT)
# mean length definitely a function of stocking density - include as a random effect in models 
anova(lm(subset(Oly.size.summary4, TEMP==10)$Median ~ subset(Oly.size.summary4, TEMP==10)$stocked + factor(subset(Oly.size.summary4, TEMP==10)$PH)))

# Inspect length by stocking density, all 
plot(x=log(Oly.size.summary3$stocked), y=Oly.size.summary3$Mean, main="All Pops,\nmean length ~ stocking density\ncolor = pH", xlab="Stocking Density", ylab = "Mean Length (mm)", col=Oly.size.summary3$COHORT, pch=19, cex=2)
text(x=log(Oly.size.summary3$stocked)+.1, y=Oly.size.summary3$Mean+.1, labels =Oly.size.summary3$TEMP)
# mean length definitely a function of stocking density - include as a random effect in models 

plot(x=log(Oly.size.summary3$stocked), y=Oly.size.summary3$Mean, main="All Pops,\nmean length ~ stocking density\ncolor = pH", xlab="Stocking Density", ylab = "Mean Length (mm)", col=Oly.size.summary3$COHORT, pch=19, cex=2)
text(x=log(Oly.size.summary3$stocked)+.1, y=Oly.size.summary3$Mean+.1, labels =Oly.size.summary3$COHORT)
# mean length definitely a function of stocking density - include as a random effect in models 


anova(lm(Oly.size.summary3$Mean ~ Oly.size.summary3$stocked + factor(Oly.size.summary3$PH)))
anova(lm(Oly.size.summary3$Mean ~ Oly.size.summary3$stocked + factor(Oly.size.summary3$TEMP)))
anova(lm(Oly.size.summary3$Mean ~ Oly.size.summary3$stocked + factor(Oly.size.summary3$COHORT)))


anova(lm(Mean ~ PH*stocked, data=Oly.size.summary4))
anova(lm(Mean ~ TEMP*stocked, data=Oly.size.summary4))
anova(lm(Mean ~ COHORT*stocked, data=Oly.size.summary4))
summary(lm(Mean ~ COHORT*stocked, data=Oly.size.summary4))
anova(lm(Mean ~ PH*COHORT, data=Oly.size.summary4))

# Mean stocking density vary by temp & pH treatment? 
shapiro.test(log(Oly.size.summary4$stocked))  #log-normal OK
hist(log(Oly.size.summary4$stocked)) #normal OK
bartlett.test(log(Oly.size.summary4$stocked) ~ Oly.size.summary4$PH) #variance OK
bartlett.test(log(Oly.size.summary4$stocked) ~ Oly.size.summary4$TEMP) #variance diff, outlier 
anova(lm(log(stocked) ~ COHORT/TEMP/PH, data=Oly.size.summary4)) #diff between cohort, but not by temp & pH w/n cohorts
anova(lm(log(stocked) ~ COHORT/PH, data=Oly.size.summary4)) #not by temp & pH w/n cohorts

# merge stocking density with full dataframe to include in stats 
Oly.size.long2 <- merge(x=Oly.size.long, y=stock.dens, by.x="GROUP", by.y="groups", all.x=T, all.y=F)

# Remove mini-exp. groups 
Oly.size.long2 <- filter(Oly.size.long2, !grepl("Exp",GROUP))

#Assess distribution of length data 
hist(Oly.size.long2$value) #right skewed 
shapiro.test(Oly.size.long2$value) #not normal 

# plot length histogram & overlay normal distribution - definitely not normal. Looks like gamma distribution. 
hist(subset(Oly.size.long2, value != "NA")$value,  breaks=0:26, col="green", main="")
lines(seq(0,26,0.1), length(subset(Oly.size.long2, value != "NA")$value)*dnorm(seq(0,26,.1), mean(subset(Oly.size.long2, value != "NA")$value), sqrt(var(subset(Oly.size.long2, value != "NA")$value))))

# replot histogram, and overlay gamma probability distribution. First calculate distribution's rate and shape (rate = mean/var, shape = rate*mean). Also need max length to assign bins and gamma dist. range 
length.rate <- mean(subset(Oly.size.long2, value != "NA")$value) / var(subset(Oly.size.long2, value != "NA")$value)
length.shape <- length.rate*mean(subset(Oly.size.long2, value != "NA")$value)
max(subset(Oly.size.long2, value != "NA")$value) #need max for hist. bins 
hist(subset(Oly.size.long2, value != "NA")$value, breaks=-0.5:24.5, col="green", main="") #plot histogram
lines(seq(0.01,25,0.1), length(subset(Oly.size.long2, value != "NA")$value)*dgamma(seq(0.01,25,0.1),length.shape,length.rate)) #overlay gamma distribution density function 

# conclusion: gamma distribution fits length data adequately 

# quick models looking at pH with population (cohort) and stocked as random effects 
Anova(glmer(value ~ PH + (1|COHORT/stocked),family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA"& TEMP==6))) #Significant in chilled group 
Anova(glmer(value ~ PH + (1|COHORT/stocked),family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA"& TEMP==10))) #not significant in unchilled group 
Anova(glmer(value ~ PH*TEMP + (1|COHORT/stocked),family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA"))) # all sign. 
# glm to compare size between 6-amb and 10-low 
hist(subset(Oly.size.long2, value != "NA"& TREATMENT=="10-LOW" | TREATMENT =="6-AMB")$value)
Anova(glmer(value ~ PH + (1|COHORT/stocked),family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA"& TREATMENT=="10-LOW" | TREATMENT =="6-AMB"))) 

# Compare 

# Just 6  
Anova(glmer(value ~ PH+COHORT+(1|stocked),family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA" & TEMP==6)))
summary(glmer(value ~ PH+COHORT+(1|stocked),family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA" & TEMP==6)))

# Just 10  
Anova(glmer(value ~ PH+COHORT+(1|stocked),family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA" & TEMP==10)))
summary(glmer(value ~ PH+COHORT+(1|stocked),family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA" & TEMP==10)))

# Both 6 and 10 groups 
Anova(glmer(value ~ PH+COHORT+(1|stocked),family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA")))


# Just look at population differences
Anova(glmer(value ~ COHORT+(1|stocked),family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA")))
summary(glmer(value ~ COHORT+(1|stocked),family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA")))

# Fit glms with gamma errors and stocked as random effect, compare with AIC. 
# first 6C only
summary(glm.l1 <- glm(value~  stocked,family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA" & TEMP==6)))
summary(glm.l2 <- glm(value~  PH,family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA" & TEMP==6)))
summary(glm.l3 <- glm(value~  COHORT,family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA" & TEMP==6)))
summary(glm.l4 <- glmer(value~  PH+(1|stocked),family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA" & TEMP==6)))
summary(glm.l5 <- glmer(value~  COHORT+(1|stocked),family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA" & TEMP==6)))
summary(glm.l6 <- glmer(value~  PH*COHORT+(1|stocked),family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA" & TEMP==6)))
summary(glm.l7 <- glmer(value~  PH+COHORT+(1|stocked),family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA" & TEMP==6)))
summary(glm.l8 <- glmer(value ~ PH + (1|COHORT/stocked),family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA"& TEMP==6)))

#10c only
summary(glm.l9 <- glm(value~  stocked,family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA" & TEMP==10)))
summary(glm.l10 <- glm(value~  PH,family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA" & TEMP==10)))
summary(glm.l11 <- glm(value~  COHORT,family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA" & TEMP==10)))
summary(glm.l12 <- glmer(value~  PH+(1|stocked),family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA" & TEMP==10)))
summary(glm.l13 <- glmer(value~  COHORT+(1|stocked),family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA" & TEMP==10)))
summary(glm.l14 <- glmer(value~  PH*COHORT+(1|stocked),family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA" & TEMP==10)))
summary(glm.l15 <- glmer(value~  PH+COHORT+(1|stocked),family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA" & TEMP==10)))
summary(glm.l16 <- glmer(value ~ PH + (1|COHORT/stocked),family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA"& TEMP==10)))

#all
summary(glm.l17 <- glm(value~  stocked,family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA")))
summary(glm.l18 <- glm(value~  PH,family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA")))
summary(glm.l19 <- glm(value~  COHORT,family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA")))
summary(glm.l20 <- glm(value~  TEMP,family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA")))
summary(glm.l21 <- glmer(value~  TEMP*PH+(1|stocked),family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA")))
summary(glm.l22 <- glmer(value~  COHORT*TEMP+(1|stocked),family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA")))
summary(glm.l23 <- glmer(value~  PH*COHORT+(1|stocked),family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA")))
summary(glm.l24 <- glmer(value~  PH+COHORT+(1|stocked),family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA")))
summary(glm.l25 <- glmer(value~  PH+COHORT+TEMP+(1|stocked),family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA")))
summary(glm.l26 <- glmer(value~  COHORT+TEMP+(1|stocked),family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA")))

AIC(glm.l1,glm.l2,glm.l3,glm.l4,glm.l5,glm.l6,glm.l7,glm.l8) #lowest AIC = glm.l6
summary(glm.l6) # smallest AIC
anova(glm.l6, test="Chi")
Anova(glm.l6) # Use `car` package for Chisq test on analysis of deviance table 
plot(glm.l6)

# Run best fit to compare 10-low and 6-amb 
Anova(glmer(value ~ PH*COHORT + (1|stocked),family=Gamma(link="log"), data=subset(Oly.size.long2, value != "NA"& TREATMENT=="10-LOW" | TREATMENT =="6-AMB"))) #Significant in chilled group 

# Make table of estimates (exp. transformed) with 95% CI
se <- sqrt(diag(vcov(glm.l6)))
print(glm.l.ci <- exp(cbind(Est = fixef(glm.l6), LL = fixef(glm.l6) - 1.96*se, UL = fixef(glm.l6) + 1.96*se)))
1-glm.l.ci[2,] # % smaller due to low pH 
plot(glm.l7) #plot residuals 

AIC(glm.l9,glm.l10,glm.l11,glm.l12,glm.l13,glm.l14,glm.l15,glm.l16) #lowest AIC = glm.l14
summary(glm.l14) # 
anova(glm.l14, test="Chi")
Anova(glm.l15) # Use `car` package for Chisq test on analysis of deviance table 

AIC(glm.l17,glm.l18,glm.l19,glm.l20,glm.l21,glm.l22,glm.l23,glm.l24,glm.l25,glm.l26) #lowest AIC = glm.l23
summary(glm.l26) # 
anova(glm.l25, glm.l26)
Anova(glm.l26) # Use `car` package for Chisq test on analysis of deviance table 


# compare bag densities between PH groups using summary dataframe
shapiro.test(log(Oly.size.summary4$stocked))
anova(lm(log(stocked) ~ PH, data=Oly.size.summary4))

### Kruskal wallis tests - nonparametric rank-order test 

kruskal.test(value ~ PH, data=Oly.size.long2) # ~0 

# KW test for each cohort 
kruskal.test(value ~ PH, data=subset(Oly.size.long2, COHORT=="HL")) #diff
kruskal.test(value ~ PH, data=subset(Oly.size.long2, COHORT=="HL" & value !=2)) #diff, even without the 2.0 mm data points 
kruskal.test(value ~ PH, data=subset(Oly.size.long2, COHORT=="K")) #diff
kruskal.test(value ~ PH, data=subset(Oly.size.long2, COHORT=="NF")) #diff 
kruskal.test(value ~ PH, data=subset(Oly.size.long2, COHORT=="SN")) #NOT diff 

summary(subset(Oly.size.long2, COHORT=="HL" & value !=2 & PH=="AMBIENT" & TEMP == 6)$value)
summary(subset(Oly.size.long2, COHORT=="HL" & value !=2 & PH=="LOW")$value)
summary(subset(Oly.size.long2, COHORT=="HL" & PH=="AMBIENT")$value)
summary(subset(Oly.size.long2, COHORT=="HL" & PH=="LOW")$value)
summary(subset(Oly.size.long2, COHORT=="NF" & PH=="AMBIENT"& PH=="AMBIENT")$value)
summary(subset(Oly.size.long2, COHORT=="NF" & PH=="LOW")$value)
summary(subset(Oly.size.long2, COHORT=="K" & PH=="AMBIENT")$value)
summary(subset(Oly.size.long2, COHORT=="K" & PH=="LOW")$value)

# plots length data as distributions 

colors <- c("10-AMB"="mediumseagreen", "10-LOW"="indianred2", "6-AMB"="blue", "6-LOW"="orange")
colors2 <- c("AMBIENT"="gray70", "LOW"="lightsteelblue")

Oly.size.long$TREAT <- Oly.size.long$TREATMENT 
Oly.size.long$TREAT <- as.factor(gsub("-Exp", "", Oly.size.long$TREAT))
levels(Oly.size.long$TREAT) #confirm only 4 treatment levels

# Density plots 
ggdensity(data=subset(Oly.size.long2,  value!="NA" & TEMP==6), x = "value",
          add = "mean", rug = TRUE,
          color = "PH", fill = "PH",  palette = colors2) +
  labs(title="Shell length (mm)\n @ ~10months",x="shell length (mm)") +
  font("title", size = 20, face = "bold") +
  font("xlab", size = 18) +
  font("ylab", size = 18) +
  font("xy.text", size = 18) +
  font("legend.title", size=18) +
  font("legend.text", size=18)

ggdensity(data=subset(Oly.size.long2,  value!="NA" & TEMP==10), x = "value",
          add = "mean", rug = TRUE,
          color = "PH", fill = "PH",  palette = colors2) +
  labs(title="Shell length (mm)\n @ ~10months",x="shell length (mm)") +
  font("title", size = 20, face = "bold") +
  font("xlab", size = 18) +
  font("ylab", size = 18) +
  font("xy.text", size = 18) +
  font("legend.title", size=18) +
  font("legend.text", size=18)




ggplot(Oly.size.long2, aes(x = value)) +
geom_histogram(aes(color = PH, fill = PH),
                   alpha = 0.4, position = "identity") +
  scale_fill_manual(values = colors2) +
  scale_color_manual(values = colors2)


# Each population separately 
png("Results/NF-9month-Length.png", width = 700, height = 500)
ggdensity(data=subset(Oly.size.long, COHORT=="NF"), x = "value",
          add = "mean", rug = TRUE,
          color = "PH", fill = "PH",  palette = colors2) +
  labs(title="North Sound Size (mm)\n @ ~10months, n=1,017",x="Length (mm)")
dev.off()

png("Results/HC-9month-Length.png", width = 700, height = 500)
ggdensity(data=subset(Oly.size.long, COHORT=="HL"), x = "value",
          add = "mean", rug = TRUE,
          color = "PH", fill = "PH",  palette = colors2) + 
  labs(title="Hood Canal Size (mm)\n @ ~10months, n=1,537", x="length (mm)")
dev.off()

png("Results/SSF1-9month-Length.png", width = 700, height = 500)
ggdensity(data=subset(Oly.size.long, COHORT=="SN"), x = "value",
          add = "mean", rug = TRUE,
          color = "PH", fill = "PH",  palette = colors2) + 
  labs(title="South Sound F1 Size (mm)\n @ ~10months, n=671", x="length (mm)")
dev.off()

png("Results/SSF2-9month-Length.png", width = 700, height = 500)
ggdensity(data=subset(Oly.size.long, COHORT=="K"), x = "value",
          add = "mean", rug = TRUE,
          color = "PH", fill = "PH", palette = colors2) + 
  labs(title="South Sound F2 Size (mm)\n @ ~10months, n=826", x="length (mm)")
dev.off()
