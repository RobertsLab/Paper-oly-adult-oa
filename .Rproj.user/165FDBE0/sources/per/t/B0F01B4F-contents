# read in data 
histology <- read.csv("Data/histology_data.csv", header=T, stringsAsFactors = T, na.strings = "NA")

# Convert a few columns to factors & relevel 
histology$TEMPERATURE <- as.factor(histology$TEMPERATURE)         
histology$Female.Stage <- as.factor(histology$Female.Stage)   
histology$Male.Stage <- as.factor(histology$Male.Stage)
histology$Sex.redo <- as.factor(histology$Sex.redo)
histology$Sex.redo <- droplevels(histology$Sex.redo, exclude = "")
histology$Sex.redo <- factor(histology$Sex.redo, levels=c("I", "M", "HPM", "H", "HPF", "F"))
histology$Dom.Stage.redo <- as.factor(histology$Dom.Stage.redo)
histology$Dom.Stage.redo <- droplevels(histology$Dom.Stage.redo, exclude = "#N/A")
histology$PCO2 <- factor(histology$PCO2, levels = c("Pre","High","Amb")) 

# make an empty dataframe for test statistic results 
tests <-  data.frame(test=character(), chisquare=character(), pvalue=numeric(), stringsAsFactors=FALSE) 

# Compare 6C to 10C - effect of temperature?  alpha = 0.0125 (n=4 comparisons)

CT.domsex.stage.pre <- table(subset(histology, SAMPLING=="FEBRUARY")$TEMPERATURE, subset(histology, SAMPLING=="FEBRUARY")$Dom.Stage.redo)
tests[1,] <- as.vector(unlist(chisq.test(CT.domsex.stage.pre, simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")]))
#10 vs. 6C: X-squared = 15.842 p=0.0026

CT.malestage.pre <- table(subset(histology, SAMPLING=="FEBRUARY")$TEMPERATURE, subset(histology, SAMPLING=="FEBRUARY")$Male.Stage)
tests[2,] <- as.vector(unlist(chisq.test(CT.malestage.pre[,-1], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")]))
#10 vs. 6C: 31.081, p-value = 9.999e-05 

CT.femstage.pre <- table(subset(histology, SAMPLING=="FEBRUARY")$TEMPERATURE, subset(histology, SAMPLING=="FEBRUARY")$Female.Stage)
tests[3,] <- as.vector(unlist(chisq.test(CT.femstage.pre[,-1], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")]))
#10 vs. 6C: 2.1488 p-value = 0.669
?chisq.test
CT.sex.pre <- table(subset(histology, SAMPLING=="FEBRUARY")$TEMPERATURE, subset(histology, SAMPLING=="FEBRUARY")$Sex.simple)
tests[4,] <- as.vector(unlist(chisq.test(CT.sex.pre, simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")]))
#10 vs. 6C: X-squared = 7.9551, p-value = 0.1634

# ------- Compare pH treatments - effect of pH? 

# Prepare contingency tables 
CT.sex <- table(histology$PCO2, histology$Sex.simple, histology$TEMPERATURE)
CT.sex.plots <- table(histology$PCO2, histology$Sex.redo, histology$TEMPERATURE)
CT.sex.pop <- table(histology$PCO2, histology$Sex.simple, histology$POPULATION, histology$TEMPERATURE)
CT.sex.pop.plots <- table(histology$PCO2, histology$Sex.redo, histology$POPULATION, histology$TEMPERATURE)

CT.sex.temp <- table(histology$PCO2, histology$Sex.simple, histology$TEMPERATURE)
CT.domsex.stage <- table(histology$PCO2, histology$Dom.Stage.redo, histology$TEMPERATURE)
CT.domsex.stage.pop <- table(histology$PCO2, histology$Dom.Stage.redo, histology$POPULATION, histology$TEMPERATURE)
CT.malestage <- table(histology$PCO2, histology$Male.Stage, histology$TEMPERATURE)
CT.malestage.pop <- table(histology$PCO2, histology$Male.Stage, histology$POPULATION, histology$TEMPERATURE)
CT.femstage <- table(histology$PCO2, histology$Female.Stage, histology$TEMPERATURE)
CT.femstage.pop <- table(histology$PCO2, histology$Female.Stage, histology$POPULATION, histology$TEMPERATURE)

colnames(CT.sex.pop.plots) <- c("Indeterminate", "Male", "Male dominant", "Hermaphroditic", "Female dominant", "Female")
colnames(CT.sex.plots) <- c("Indeterminate", "Male", "Male dominant", "Hermaphroditic", "Female dominant", "Female")
colnames(CT.domsex.stage) <- c("Empty/No Follicles (0)", "Early (1)", "Advanced (2)", "Ripe (3)", "Spawned/Regressing (4)")

# Compare using chi-square if N is large enough, fisher exact test if not.  n=6 comparisons per factor, alpha=0.0083333

# Dominant stage, alpha=0.0083333
tests[5,] <- as.vector(unlist(chisq.test(CT.domsex.stage[-1,,"6"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")])) 
#ambient vs. low, X-squared= 9.738, p-value = 0.0364   
tests[6,] <- as.vector(unlist(chisq.test(CT.domsex.stage[-2,,"6"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")])) 
#pre to ambient, X-squared=16.514 p=0.0019          
tests[7,] <- as.vector(unlist(chisq.test(CT.domsex.stage[-3,,"6"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")])) 
#pre to low, X-squared=4.5654, p=0.3521             

tests[8,] <- as.vector(unlist(chisq.test(CT.domsex.stage[-1,,"10"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")])) 
#ambient vs. low, X-squared=12.458,  p-value = 0.009099
tests[9,] <- as.vector(unlist(chisq.test(CT.domsex.stage[-2,,"10"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")])) 
#pre to ambient, X-squared=12.682, p-value = 0.009999     
tests[10,] <- as.vector(unlist(chisq.test(CT.domsex.stage[-3,,"10"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")])) 
#pre to low, X-squared=5.2335, p-value = 0.285        

# female only, alpha=0.0083333
tests[11,] <- as.vector(unlist(chisq.test(CT.femstage[-1,-1,"6"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")])) 
#ambient vs. low, p=0.2185                                        
tests[12,] <- as.vector(unlist(chisq.test(CT.femstage[-2,-1,"6"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")])) 
#pre to ambient, p=0.07639                                        
tests[13,] <- as.vector(unlist(chisq.test(CT.femstage[-3,-1,"6"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")])) 
#pre to low, p=0.3464                                             

tests[14,] <- as.vector(unlist(chisq.test(CT.femstage[-1,2:4,"10"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")])) 
#ambient vs. low, p=1                                  
tests[15,] <- as.vector(unlist(chisq.test(CT.femstage[-2,2:4,"10"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")])) 
#pre to ambient, p=0.1274                                        
tests[16,] <- as.vector(unlist(chisq.test(CT.femstage[-3,2:4,"10"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")])) 
#pre to low, p=0.06159                                          

# male only, (alpha=0.0083333)
tests[17,] <- as.vector(unlist(chisq.test(CT.malestage[-1,-1,"6"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")])) 
#ambient vs. low, X-squared = 8.975, p-value = 0.0297              
tests[18,] <- as.vector(unlist(chisq.test(CT.malestage[-2,-1,"6"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")])) 
#pre to ambient, X-squared=24.197, p=9.999e-05                     
tests[19,] <- as.vector(unlist(chisq.test(CT.malestage[-3,-1,"6"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")])) 
#pre to low, X-squared=15.159, p=0.0011   

tests[20,] <- as.vector(unlist(chisq.test(CT.malestage[-1,-1,"10"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")])) 
#ambient vs. low, X-squared = 9.5, p-value = 0.0221        
tests[21,] <- as.vector(unlist(chisq.test(CT.malestage[-2,-1,"10"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")])) 
#pre to ambient, X-squared=15.387, p-value = 0.0036                    
tests[22,] <- as.vector(unlist(chisq.test(CT.malestage[-3,-1,"10"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")]))
#pre to low, X-squared=0.55353, p=0.9742   

# Sex, alpha=0.0083333
tests[23,] <- as.vector(unlist(chisq.test(CT.sex[-1,,"6"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")])) 
#sex btwn ambient and low pH: X-squared=5.3909, p=0.1542                 
tests[24,] <- as.vector(unlist(chisq.test(CT.sex[-2,,"6"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")])) 
#sex btwn pre and ambient pH: X-squared=              
tests[25,] <- as.vector(unlist(chisq.test(CT.sex[-3,,"6"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")])) 
#sex btwn pre and low pH: X-squared=4.6477, p=0.2019                       

tests[26,] <- as.vector(unlist(chisq.test(CT.sex[-1,,"10"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")])) 
#sex btwn ambient and low pH: X-squared=0.62657, p-value = 0.9091              
tests[27,] <- as.vector(unlist(chisq.test(CT.sex[-2,,"10"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")])) 
#sex btwn pre and ambient pH: X-squared=6.8457, p-value = 0.07399              
tests[28,] <- as.vector(unlist(chisq.test(CT.sex[-3,,"10"], simulate.p.value = T, B = 10000)[c("data.name", "statistic", "p.value")])) 
#sex btwn pre and low pH: X-squared=3.869, p-value = 0.6058         

# Compare 6-amb to 10-low ... do T/pH treatments cancel? n=4 comparisons, alpha=0.0125

CT.domsex.stage <- table(histology$PCO2, histology$Dom.Stage.redo, histology$TEMPERATURE)
tests[29,] <- as.vector(unlist(chisq.test(rbind(CT.domsex.stage[3,,c("6")],CT.domsex.stage[2,,c("10")]), simulate.p.value = T, B=10000)[c("data.name", "statistic", "p.value")])) #X=2.8185, p=0.63

CT.sex <- table(histology$PCO2, histology$Sex.simple, histology$TEMPERATURE)
tests[30,] <- as.vector(unlist(chisq.test(rbind(CT.sex[3,,c("6")],CT.sex[2,,c("10")]), simulate.p.value = T, B=10000)[c("data.name", "statistic", "p.value")])) 
# X=11.7, p=0.037 

CT.malestage <- table(histology$PCO2, histology$Male.Stage, histology$TEMPERATURE)
tests[31,] <- as.vector(unlist(chisq.test(rbind(CT.malestage[3,-1,c("6")],CT.malestage[2,-1,c("10")]), simulate.p.value = T, B=10000)[c("data.name", "statistic", "p.value")])) 
# p=0.6495

CT.femstage <- table(histology$PCO2, histology$Female.Stage, histology$TEMPERATURE)
tests[32,] <- as.vector(unlist(chisq.test(rbind(CT.femstage[3,2:4,c("6")],CT.femstage[2,2:4,c("10")]), simulate.p.value = T, B=10000)[c("data.name", "statistic", "p.value")])) 
#  p=0.7785

tests$holm <- p.adjust(tests$pvalue, "holm")
tests$BH <- p.adjust(tests$pvalue, "BH")
View(tests) # review - opting to go with the less conservative, due to many different correlations in data 


round(prop.table(CT.malestage[-1,,"6"],1 ), digits = 3)
round(prop.table(CT.malestage[-1,,"10"],1 ), digits = 3)

# =============
# After BH correction, sigificance retained for ... 

# temp treatment differences
CT.malestage.pre[, -1]      # Male stage after temp treatment 
CT.domsex.stage.pre         # Dominant sex's stage after temp treatment 

# High vs. ambient pCO2 treatment differences 
CT.domsex.stage[-1, , "10"] # Dominant sex's stage between ambient and high pCO2, 10C group 

# Ambient pCO2 treatment 
CT.domsex.stage[-2, , "6"]  # Dominant sex's stage after ambient pCO2
CT.domsex.stage[-2, , "10"] # Dominant sex's stage after ambient pCO2 treatment, 10C group
CT.malestage[-2, , "6"]     # Male stage after ambient pCO2, 6C group 
CT.malestage[-2, , "10"]    # Male stage after ambient pCO2, 10C group 

# High pCO2 treatment 
CT.malestage[-3, , "6"]     # Male stage between ambient and high pCO2, 6C group 


# ===================
# PLOTS for figures 

# plots with all cohorts combined 
colnames(CT.malestage) <- c("Empty/No Follicles (0)", "Early (1)", "Advanced (2)", "Ripe (3)", "Spawned/Regressing (4)")
colnames(CT.femstage) <- c("Empty/No Follicles (0)", "Early (1)", "Advanced (2)", "Ripe (3)", "Spawned/Regressing (4)")

pdf(file="Figures/gonad-stage-all-cohorts", height = 6.25, width = 4.62)
par(mfrow=c(2,2), oma=c(5,4,0,3), mar=c(0,3,4.5,0), mgp=c(2.6,0.6,0))

barplot(t(prop.table(CT.malestage[,,"6"], 1)), xlab=F, las=1, col=c("#f7f7f7", "#cccccc", "#636363", "#252525",  "#969696"), cex.lab=1.4, cex.axis = 1.2, col.axis = "gray30", col.lab = "gray30", legend.text = F)
title(main = "Male", line = 0.8, cex.main=1.4, col.main = "gray30", font.main = 1)

barplot(t(prop.table(CT.femstage[,,"6"], 1)), xlab=F, las=1, col=c("#f7f7f7", "#cccccc", "#636363", "#252525",  "#969696"), cex.lab=1.4, cex.axis = 1.2, col.axis = "gray30", col.lab = "gray30", legend.text = F)
title(main = "Female", line = 0.8, cex.main=1.4, col.main = "gray30", font.main = 1)

barplot(t(prop.table(CT.malestage[,,"10"], 1)), xlab=F, las=1, col=c("#f7f7f7", "#cccccc", "#636363", "#252525",  "#969696"), cex.lab=1.4, cex.axis = 1.2, col.axis = "gray30", col.lab = "gray30", legend.text = F)
title(main = "Male", line = 0.8, cex.main=1.4, col.main = "gray30", font.main = 1)

barplot(t(prop.table(CT.femstage[,,"10"], 1)), xlab=F, las=1, col=c("#f7f7f7", "#cccccc", "#636363", "#252525",  "#969696"), cex.lab=1.4, cex.axis = 1.2, col.axis = "gray30", col.lab = "gray30", legend.text = F)
title(main = "Female", line = 0.8, cex.main=1.4, col.main = "gray30", font.main = 1)

mtext(side=1,text=(expression(paste(pCO[2], " treatment"))), outer=T,line=3, col="gray30", font=1, cex=1.1)
mtext(side=2,text="Proportion Sampled", outer=T,line=-0.5, col="gray30", font=1, cex=1, at=0.2)
mtext(side=2,text="Proportion Sampled", outer=T,line=-0.5, col="gray30", font=1, cex=1, at=0.7)
mtext(side=2,text="6°C treatment", outer=T,line=1.5, col="gray30", font=3, cex=1.2, at=0.7)
mtext(side=2,text="10°C treatment", outer=T,line=1.5, col="gray30", font=3, cex=1.2, at=0.2)
mtext(side=3,outer=T,line=-2.5, col="gray30", font=3, cex=1.2, text=expression(paste("Male & female gamete developmental stages")))
dev.off()

# make a plot for the legend only 
colnames(CT.domsex.stage.pop) <- c("Empty/No follicles (0)", "Early (1)", "Advanced (2)", "Ripe (3)", "Spawned/Regressing (4)")

pdf(file="Figures/gonad-stage-legend", height = 5.2, width = 5.6)
par(mar=c(0,0,0,18))
barplot(t(prop.table(CT.domsex.stage.pop[,,plot.cohort[i], plot.temps[i]], 1)), xlab=F, las=1, col=c("#f7f7f7", "#cccccc", "#636363", "#252525",  "#969696"), cex.lab=1.4, cex.axis = 1.2, col.axis = "gray30", col.lab = "gray30", legend.text = T, args.legend = list(x = "topright", bty = "n", inset=c(-1.8, 0), title="Gonad Stage", cex=1.5, text.col="gray30", text.font=1))
dev.off()

pdf(file="Figures/gonad-sex-all-cohorts", height = 6.25, width = 3.35)
par(mfrow=c(2,1), oma=c(4,4,0,3), mar=c(0,3,3,0), mgp=c(2.6,0.5,0))

barplot(t(prop.table(CT.sex.plots[,,"6"], 1)), xlab=F, las=1, col=c("#f7f7f7", "#67a9cf", "#d1e5f0","gray85", "#fddbc7","#ef8a62" ), cex.names=0.8, cex.axis = 1, col.axis = "gray30", col.lab = "gray30", legend.text = F)
title(main = "Gonad sex", line = 1, cex.main=1.2, col.main = "gray30", font.main = 1)

barplot(t(prop.table(CT.sex.plots[,,"10"], 1)), xlab=F, las=1, col=c("#f7f7f7", "#67a9cf", "#d1e5f0","gray85", "#fddbc7","#ef8a62" ), cex.names=0.8, cex.axis = 1, col.axis = "gray30", col.lab = "gray30", legend.text = F)

mtext(side=1,text=(expression(paste(pCO[2], " treatment"))), outer=T,line=2.5, col="gray30", font=1, cex=1.1, at=0.6)
mtext(side=2,text="Proportion Sampled", outer=T,line=-0.5, col="gray30", font=1, cex=1, at=0.2)
mtext(side=2,text="Proportion Sampled", outer=T,line=-0.5, col="gray30", font=1, cex=1, at=0.7)
mtext(side=2,text="10°C Treatment", outer=T,line=1.2, col="gray30", font=3, cex=1.2, at=0.2)
mtext(side=2,text="6°C Treatment", outer=T,line=1.2, col="gray30", font=3, cex=1.2, at=0.7)
dev.off()

# make a plot for the legend 
colnames(CT.domsex.stage.pop) <- c("Empty/No follicles (0)", "Early (1)", "Advanced (2)", "Ripe (3)", "Spawned/Regressing (4)")

pdf(file="Figures/gonad-stage-legend", height = 5.2, width = 5.6)
par(mar=c(0,0,0,18))
barplot(t(prop.table(CT.domsex.stage.pop[,,plot.cohort[i], plot.temps[i]], 1)), xlab=F, las=1, col=c("#f7f7f7", "#cccccc", "#636363", "#252525",  "#969696"), cex.lab=1.4, cex.axis = 1.2, col.axis = "gray30", col.lab = "gray30", legend.text = T, args.legend = list(x = "topright", bty = "n", inset=c(-1.8, 0), title="Gonad Stage", cex=1.5, text.col="gray30", text.font=1))
dev.off()


# ===============================
# Supplemental Materials 

# cohort differences (not by treatment)

# Compare sex ratios between cohorts ... all treatments combined! 
CT.stage.pop.allph <- t(table(histology$Dom.Stage.redo, histology$POPULATION))
tests[33,] <- as.vector(unlist(chisq.test(CT.stage.pop.allph[,-1], simulate.p.value = T, B=10000)[c("data.name", "statistic", "p.value")])) 

CT.sex.pop.allph <- t(table(histology$Sex.simple, histology$POPULATION))
tests[34,] <- as.vector(unlist(chisq.test(CT.sex.pop.allph, simulate.p.value = T, B=10000)[c("data.name", "statistic", "p.value")])) #very sign. 
pairwiseNominalIndependence(CT.sex.pop.allph, fisher=FALSE, gtest=FALSE, chisq=TRUE,  method="fdr", simulate.p.value=T)

CT.male.pop.allph <- t(table(histology$Male.Stage, histology$POPULATION))
tests[35,] <- as.vector(unlist(chisq.test(CT.male.pop.allph[,-1], simulate.p.value = T, B=10000)[c("data.name", "statistic", "p.value")])) 

CT.female.pop.allph <- t(table(histology$Female.Stage, histology$POPULATION))
tests[36,] <- as.vector(unlist(chisq.test(CT.female.pop.allph[,-1], simulate.p.value = T, B=10000)[c("data.name", "statistic", "p.value")])) 


# Supplemental Plots showing 


plot.cohort <- c("NF","HL", "SN", "K", "NF","HL", "SN", "K")
plot.names <- c("Fidalgo Bay","Dabob Bay", "Oyster Bay C1", "Oyster Bay  C2", "Fidalgo Bay","Dabob Bay", "Oyster Bay C1", "Oyster Bay  C2")
plot.temps <- c("6","6","6","6","10","10","10","10")

pdf(file="Figures/gonad-stage-by-cohort", height = 5.75, width = 7.6)
par(mfrow=c(2,4), oma=c(5,5,0,2), mar=c(0,3,5,0), mgp=c(2.6,0.6,0))
for (i in 1:8) {
  barplot(t(prop.table(CT.domsex.stage.pop[,,plot.cohort[i], plot.temps[i]], 1)), xlab=F, las=1, col=c("#f7f7f7", "#cccccc", "#636363", "#252525",  "#969696"), cex.lab=1.4, cex.axis = 1.2, col.axis = "gray30", col.lab = "gray30", legend.text = F)
  title(plot.names[i], line = 1, cex.main=1.5, col.main = "gray30", font.main = 1)
  
}
mtext(side=1,text=(expression(paste(pCO[2], " treatment"))), outer=T,line=3.5, col="gray30", font=1, cex=1.1)
mtext(side=2,text="Proportion Sampled", outer=T,line=0, col="gray30", font=1, cex=1, at=0.2)
mtext(side=2,text="Proportion Sampled", outer=T,line=0, col="gray30", font=1, cex=1, at=0.7)
mtext(side=2,text="6°C Treatment", outer=T,line=2, col="gray30", font=3, cex=1.2, at=0.7)
mtext(side=2,text="10°C Treatment", outer=T,line=2, col="gray30", font=3, cex=1.2, at=0.2)
mtext(side=3,outer=T,line=-2, col="gray30", font=3, cex=1.2, text=expression(paste("Gonad stage by temperature, ", pCO[2], ", and cohort")))
dev.off()

# Gonad sex for each cohort 

pdf(file="Figures/gonad-sex-by-cohort", height = 5.75, width = 7.6)
par(mfrow=c(2,4), oma=c(5,5,0,2), mar=c(0,3,5,0), mgp=c(2.6,0.6,0))
for (i in 1:8) {
  barplot(t(prop.table(CT.sex.pop.plots[,,plot.cohort[i], plot.temps[i]], 1)), xlab=F, las=1, col=c("#f7f7f7", "#67a9cf", "#d1e5f0","gray85", "#fddbc7","#ef8a62" ), cex.lab=1.4, cex.axis = 1.2, col.axis = "gray30", col.lab = "gray30", legend.text = F)
  title(plot.names[i], line = 1, cex.main=1.5, col.main = "gray30", font.main = 1)
}
mtext(side=1,text=(expression(paste(pCO[2], " treatment"))), outer=T,line=3.5, col="gray30", font=1, cex=1.1)
mtext(side=2,text="Proportion Sampled", outer=T,line=0, col="gray30", font=1, cex=1, at=0.2)
mtext(side=2,text="Proportion Sampled", outer=T,line=0, col="gray30", font=1, cex=1, at=0.7)
mtext(side=2,text="6°C Treatment", outer=T,line=2, col="gray30", font=3, cex=1.2, at=0.7)
mtext(side=2,text="10°C Treatment", outer=T,line=2, col="gray30", font=3, cex=1.2, at=0.2)
mtext(side=3,outer=T,line=-2, col="gray30", font=3, cex=1.2, text=expression(paste("Gonad sex by temperature, ", pCO[2], ", and cohort")))
dev.off()

# -------  Male gonad stage by each cohort
plot.cohort <- c("NF","HL", "SN", "K", "NF","HL", "SN", "K")
plot.names <- c("Fidalgo Bay","Dabob Bay", "Oyster Bay C1", "Oyster Bay  C2", "Fidalgo Bay","Dabob Bay", "Oyster Bay C1", "Oyster Bay  C2")
plot.temps <- c("6","6","6","6","10","10","10","10")

# check out colors for stage plots

pdf(file="Figures/male-gonad-stage-by-cohort", height = 5.75, width = 7.6)
par(mfrow=c(2,4), oma=c(5,5,0,2), mar=c(0,3,5,0), mgp=c(2.6,0.6,0))
for (i in 1:8) {
  barplot(t(prop.table(CT.malestage.pop[,,plot.cohort[i], plot.temps[i]], 1)), xlab=F, las=1, col=c("#f7f7f7", "#cccccc", "#636363", "#252525",  "#969696"), cex.lab=1.4, cex.axis = 1.2, col.axis = "gray30", col.lab = "gray30", legend.text = F)
  title(plot.names[i], line = 1, cex.main=1.5, col.main = "gray30", font.main = 1)
  
}
mtext(side=1,text=(expression(paste(pCO[2], " treatment"))), outer=T,line=3.5, col="gray30", font=1, cex=1.1)
mtext(side=2,text="Proportion Sampled", outer=T,line=0, col="gray30", font=1, cex=1, at=0.2)
mtext(side=2,text="Proportion Sampled", outer=T,line=0, col="gray30", font=1, cex=1, at=0.7)
mtext(side=2,text="6°C Treatment", outer=T,line=2, col="gray30", font=3, cex=1.2, at=0.7)
mtext(side=2,text="10°C Treatment", outer=T,line=2, col="gray30", font=3, cex=1.2, at=0.2)
mtext(side=3,outer=T,line=-2, col="gray30", font=3, cex=1.2, text=expression(paste("Male gamete stage by temperature, ", pCO[2], ", and cohort")))
dev.off()

pdf(file="Figures/female-gonad-stage-by-cohort", height = 5.75, width = 7.6)
par(mfrow=c(2,4), oma=c(5,5,0,2), mar=c(0,3,5,0), mgp=c(2.6,0.6,0))
for (i in 1:8) {
  barplot(t(prop.table(CT.femstage.pop[,,plot.cohort[i], plot.temps[i]], 1)), xlab=F, las=1, col=c("#f7f7f7", "#cccccc", "#636363", "#252525",  "#969696"), cex.lab=1.4, cex.axis = 1.2, col.axis = "gray30", col.lab = "gray30", legend.text = F)
  title(plot.names[i], line = 1, cex.main=1.5, col.main = "gray30", font.main = 1)
  
}
mtext(side=1,text=(expression(paste(pCO[2], " treatment"))), outer=T,line=3.5, col="gray30", font=1, cex=1.1)
mtext(side=2,text="Proportion Sampled", outer=T,line=0, col="gray30", font=1, cex=1, at=0.2)
mtext(side=2,text="Proportion Sampled", outer=T,line=0, col="gray30", font=1, cex=1, at=0.7)
mtext(side=2,text="6°C Treatment", outer=T,line=2, col="gray30", font=3, cex=1.2, at=0.7)
mtext(side=2,text="10°C Treatment", outer=T,line=2, col="gray30", font=3, cex=1.2, at=0.2)
mtext(side=3,outer=T,line=-2, col="gray30", font=3, cex=1.2, text=expression(paste("Female gamete stage by temperature, ", pCO[2], ", and cohort")))
dev.off()

