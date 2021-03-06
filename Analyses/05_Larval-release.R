## Larval release plots and statistics, adapted from Katherine Silliman R code 

# Import data, convert groups to date, numeric and factors 
larvae <- read.csv("Data/larval-release-data.csv", header = TRUE, na.strings = "n/a", stringsAsFactors = F)
larvae$Date <- as.Date(larvae$Date, "%m/%d/%y")
larvae[c("Vol.counted", "Vol.total", "CountA", "CountB", "CountC", "Larvae.mL", "Tot.Larvae")] <- as.numeric(as.character(unlist(larvae[c("Vol.counted", "Vol.total", "CountA", "CountB", "CountC", "Larvae.mL", "Tot.Larvae")])))
larvae[c("Group", "Spawning.Group", "Population", "Treatment", "Temperature", "pH", "Bucket")] <- lapply(larvae[c("Group", "Spawning.Group", "Population", "Treatment", "Temperature", "pH", "Bucket")], factor)
larvae <- arrange(larvae, Date) %>% mutate(CalDay = format(Date,"%j"))  # Add column with calendar day 

# Re-calculate total larvae released to confirm spreadsheet accuracy  
larvae$total.released <- round(((rowMeans(larvae[,c("CountA", "CountB", "CountC")], na.rm = TRUE, dims = 1))/larvae$Vol.counted)*larvae$Vol.total, digits = -1)
print(larvae$error <- larvae$total.released - larvae$Tot.Larvae) #looks good, just rounding error

# --- Normalize # larvae produced by the number of broodstock per spawning group (perbrood). # oysters: 
# NF-10 Ambient A = 15 
# NF-10 Ambient B = 14 
# NF-10 Low A = 14 
# NF-10 Low B = 14 
# NF-6 Ambient A = 15 
# NF-6 Ambient B = 14 
# NF-6 Low A = 14 
# NF-6 Low B = 15 
# SN-10 Ambient A = 17 
# SN-10 Ambient B = 17 
# SN-10 Low A = 15 
# SN-10 Low B = 15 
# SN-6 Ambient A = 15 
# SN-6 Ambient B = 16 
# SN-6 Low A = 17 
# SN-6 Low B = 17 
# HL-10 Ambient = 9 
# HL-10 Low = 16 
# HL-6 Ambient = 14 
# HL-6 Low = 15 
# K-10 Ambient = 115  (K groups were SN-F2's produced by Katherine, younger/smaller and likely inbred)
# K-10 Low = 111  
# K-6 Ambient" = 117
# K-6 Low" = 126

# add number of broodstock in each spawning group for normalization 
larvae$broodstock <- larvae$Spawning.Group
larvae$broodstock <- gsub("NF-10 Ambient A|NF-6 Ambient A|NF-6 Low B|SN-10 Low A|SN-10 Low B|SN-6 Ambient A|HL-6 Low", 15, larvae$broodstock)
larvae$broodstock <- gsub("NF-10 Ambient B|NF-10 Low A|NF-10 Low B|NF-6 Ambient B|NF-6 Low A|HL-6 Ambient", 14, larvae$broodstock)
larvae$broodstock <- gsub("SN-10 Ambient A|SN-10 Ambient B|SN-6 Low A|SN-6 low A|SN-6 Low B", 17, larvae$broodstock)
larvae$broodstock <- gsub("HL-10 Ambient", 9, larvae$broodstock)
larvae$broodstock <- gsub("SN-6 Ambient B|HL-10 Low", 16, larvae$broodstock)
larvae$broodstock <- gsub("K-10 Ambient", 115, larvae$broodstock)
larvae$broodstock <- gsub("K-10 Low", 111, larvae$broodstock)
larvae$broodstock <- gsub("K-6 Ambient", 117, larvae$broodstock)
larvae$broodstock <- gsub("K-6 Low", 126, larvae$broodstock)
larvae$broodstock <- as.numeric(larvae$broodstock)

#Add mean length for each population for normalization 
larvae$mean.length <- larvae$Population
larvae$mean.length <- gsub("NF|SN", 3.6, larvae$mean.length)
larvae$mean.length <- gsub("HL", 3.0, larvae$mean.length)
larvae$mean.length <- gsub("K", 2.2, larvae$mean.length)
larvae$mean.length <- as.numeric(larvae$mean.length)

# Normalize # larvae spawned daily by #broodstock*mean length - result is # larvae per broodstock centimeter 
larvae$larvae.per.broodcm <- larvae$total.released/(larvae$broodstock*larvae$mean.length)

# Summary statistics 
summarise(larvae, total.by.date = sum(total.released))                                              #total larvae released 
summarise(larvae, total.by.date = mean(total.released))                                              #mean larvae released 
max(larvae$Tot.Larvae, na.rm=T)
aggregate(Tot.Larvae ~ pH + Temperature, larvae, sum, na.rm=TRUE)/180000                                  #By pH & Temperature
aggregate(Tot.Larvae ~ pH + Temperature + Spawning.Group, larvae, sum, na.rm=TRUE)                      #By population
aggregate(Tot.Larvae ~ Spawning.Group, larvae, sum, na.rm=TRUE)                                     #By population & treatment
aggregate(Tot.Larvae ~ pH + Temperature, larvae, mean, na.rm=TRUE)                                     #Overal daily mean release by population & treatment
aggregate(Tot.Larvae ~ pH + Temperature, larvae, sd, na.rm=TRUE)                                     #Overal daily sd release by population & treatment
aggregate(larvae.per.broodcm ~  pH + Temperature, larvae, sum, na.rm=TRUE)                             #total larvae released by # broodstock
aggregate(larvae.per.broodcm ~ pH + Temperature + Spawning.Group, larvae, sum, na.rm=TRUE)                 #By population & treatment
nrow(subset(larvae, total.released >= 10000))                                                       #Number of times >10k larvae were collected (all grps)
sum(aggregate(broodstock ~ Spawning.Group, subset(larvae), mean)$broodstock)                        #Total # broodstock 

# ----- Statistics 

# summarize data for each spawning bucket 
spawning_group_total <- group_by(larvae, Spawning.Group, Population, Treatment, pH, Temperature) %>% mutate(cum.total=cumsum(total.released),cum.percap = cumsum(larvae.per.broodcm),CalDay = as.numeric(format(Date,"%j"))) %>% arrange(Date) %>% dplyr::select(Date,CalDay,Spawning.Group,Population,Treatment,pH,Temperature,total.released,larvae.per.broodcm,cum.total,cum.percap)

# Summarize data for each treatment treatment, pulling out key dates and summing/averaging larvae 
spawning_group_sum <- spawning_group_total %>% group_by(Population, pH, Temperature, Spawning.Group) %>% dplyr::summarize(overall_Total = sum(total.released, na.rm = T), mean.larvae = mean(total.released,na.rm=T), se.larvae = std.error(total.released,na.rm=T), mean.percap = mean(larvae.per.broodcm,na.rm=T), total.percap = sum(larvae.per.broodcm,na.rm=T), maxday = as.numeric(CalDay[which.max(total.released)]), max = max(total.released), max.percap = max(larvae.per.broodcm), first.big = as.numeric(CalDay[which(total.released > 10000)[1]]), release.days = as.numeric(length(CalDay[total.released > 10000])))

# Assess data distributions 
metrics <- list("total.released"=spawning_group_total$total.released, 
                "overall_Total"=spawning_group_sum$overall_Total, 
                "total.percap"=spawning_group_sum$total.percap, 
                "mean.larvae"=spawning_group_sum$mean.larvae, 
                "cum.total"=spawning_group_total$cum.total, 
                "cum.percap"=spawning_group_total$cum.percap, 
                "first.big"=spawning_group_sum$first.big, 
                "max"=spawning_group_sum$max, 
                "maxday"=spawning_group_sum$maxday, 
                "total.bigdays"=spawning_group_sum$release.days)

# Assess data distributions for metrics (normal, lognormal, and poisson)
for(i in 1:length(metrics)) {
  par(mfrow=c(2,2))
  qqp(metrics[[i]], "norm", main=paste(names(metrics[i]), "-normal"))
  qqp(metrics[[i]], "lnorm", main=paste(names(metrics[i]), "-lognormal")) 
}

# Best fit distributions: 
# spawning_group_total$total.released = daily larvae released/collected (not normalized)                <-- LOG NORMAL 
# spawning_group_sum$overall_Total = sum of larvae released in each bucket                              <-- NORMAL
# spawning_group_sum$total.percap = sum of larvae released in each bucket, normalized by broodstock cm  <-- NORMAL 
# spawning_group_sum$mean.larvae = average # larvae per release event                                   <-- NORMAL 
# spawning_group_total$cum.total = running total # larvae = cum.total                                   <-- LOG NORMAL 
# spawning_group_total$cum.percap = running total # larvae per broodstock cm                            <-- LOG NORMAL 
# spawning_group_sum$first.big = First big larval release (>10k)                                        <-- NORMAL OK
# spawning_group_sum$max = max larvae collected in one day for each spawning bucket                     <-- NORMAL OK
# spawning_group_sum$maxday = date that max larvae were collected                                       <-- NORMAL OK
# spawning_group_sum$release.days = number of days >10k larvae collected                                <-- NORMAL OK


# Compare raw daily collection data 
summary(test1 <- aov(log(total.released+1) ~ Population*Temperature*pH, data=spawning_group_total)) # <-- daily release data NO DIFF 
summary(test2 <- aov(log(total.released+1) ~ Population*Temperature, data=spawning_group_total)) # <-- daily release data NO DIFF 
summary(test3 <- aov(log(total.released+1) ~ Population, data=spawning_group_total)) # <-- daily release data NO DIFF 
anova(test2, test3) #no diff. Use simplest = Population only 
TukeyHSD(total.released.aov <- aov(log(total.released+1) ~ Population, data=spawning_group_total)) # Diff by Pop

summary(aov(log(total.released+1) ~ Temperature*pH, data=subset(spawning_group_total, Population=="NF")))  
summary(aov(log(total.released+1) ~ Temperature*pH, data=subset(spawning_group_total, Population=="HL"))) 
summary(aov(log(total.released+1) ~ Temperature*pH, data=subset(spawning_group_total, Population=="SN"))) 
summary(aov(log(total.released+1) ~ Temperature*pH, data=subset(spawning_group_total, Population=="K"))) 

# Total larval release, normalized by #brood-cm
anova(tot1 <- lm(total.percap ~ Population*pH*Temperature, data=spawning_group_sum)) 
anova(tot2 <- lm(total.percap ~ Population*pH*Temperature-pH, data=spawning_group_sum)) 
anova(tot3 <- lm(total.percap ~ Population*pH*Temperature-pH-Population:pH, data=spawning_group_sum)) 
anova(tot4 <- lm(total.percap ~ Population*pH*Temperature-pH-Population:pH-Population:Temperature, data=spawning_group_sum)) 
anova(tot5 <- lm(total.percap ~ Population+Temperature+pH:Temperature, data=spawning_group_sum)) 
anova(tot6 <- lm(total.percap ~ Population+pH:Temperature, data=spawning_group_sum)) 
anova(tot7 <- lm(total.percap ~ Population, data=spawning_group_sum))
AIC(tot1,tot2,tot3,tot4,tot5,tot6,tot7) #tot1 through tot4 lowest AIC; check model F
summary(tot1) #R2=.7188, p=0.014
summary(tot2)  #R2=.7188, p=0.014
summary(tot3)  #R2=.7188, p=0.014
summary(tot4)  #R2=.7188, p=0.014 <--- most parsimoneous  

# Response: total.percap
#                            Df     Sum Sq   Mean Sq F value   Pr(>F)   
# Population                 3 1334810672 444936891 15.2615 0.001131 ** <---- 
# Temperature                1  129921069 129921069  4.4563 0.067767 . 
# pH:Temperature             2  232659102 116329551  3.9901 0.062809 . 
# Population:pH:Temperature  9  453645892  50405099  1.7289 0.226205   
# Residuals                  8  233233725  29154216                                              
TukeyHSD(aov(total.percap ~ Population*pH*Temperature-pH-Population:pH-Population:Temperature, data=spawning_group_sum)) 

# Test mean larvae per broodstock, fit models 
anova(mean1 <- lm(mean.percap ~Population*pH*Temperature, data=spawning_group_sum))
anova(mean2 <- lm(mean.percap ~Population*pH*Temperature-pH, data=spawning_group_sum))
anova(mean3 <- lm(mean.percap ~Population*pH*Temperature-pH-Population:pH, data=spawning_group_sum))
anova(mean4 <- lm(mean.percap ~Population*pH*Temperature-pH-Population:pH-Population:Temperature, data=spawning_group_sum))
anova(mean5 <- lm(mean.percap ~Population*pH*Temperature-pH-Population:pH-Population:Temperature-Population:pH:Temperature, data=spawning_group_sum))
anova(mean6 <- lm(mean.percap ~Population + pH:Temperature, data=spawning_group_sum))
anova(mean7 <- lm(mean.percap ~Population, data=spawning_group_sum))
anova(mean8 <- lm(mean.percap ~pH, data=spawning_group_sum))
anova(mean9 <- lm(mean.percap ~Temperature, data=spawning_group_sum))
AIC(mean1,mean2,mean3,mean4,mean5,mean6,mean7,mean8,mean9) #1-4 equal. Most parsimonious is mean4.  

# Response: mean.percap
#                            Df   Sum Sq Mean Sq F value    Pr(>F)    
# Population                 3 15129394 5043131 16.3873 0.0008896 *** <---sign.
# Temperature                1   730910  730910  2.3750 0.1618569    
# pH:Temperature             2  3168818 1584409  5.1484 0.0365471 *  <--- sign.
# Population:pH:Temperature  9  4960241  551138  1.7909 0.2119352    
# Residuals                  8  2461968  307746                      

TukeyHSD(mean4 <- aov(mean.percap ~Population*pH*Temperature-pH-Population:pH-Population:Temperature, data=spawning_group_sum))

# Timing- first big release (>10k)
anova(big1 <- lm(first.big ~ Population*Temperature*pH, data=spawning_group_sum))
anova(big2 <- lm(first.big ~ Population*Temperature+Population:pH+Temperature:pH+Population:Temperature+pH:Population:Temperature, data=spawning_group_sum))
anova(big3 <- lm(first.big ~ Population+Temperature+Population:pH+Temperature:pH+pH:Population:Temperature, data=spawning_group_sum))
anova(big4 <- lm(first.big ~ Population+Temperature+Population:pH+pH:Population:Temperature, data=spawning_group_sum))
anova(big5 <- lm(first.big ~ Population+Temperature+Population:pH, data=spawning_group_sum))
anova(big6 <- lm(first.big ~ Population+Temperature, data=spawning_group_sum))
AIC(big1, big2, big3, big4, big5, big6) 
summary(big4) #most parsimoneous

# Response: first.big
#                             Df Sum Sq Mean Sq F value   Pr(>F)   
# Population                 3 606.71 202.236 15.0501 0.001184 ** <--- sign. 
# Temperature                1 160.17 160.167 11.9194 0.008664 ** <--- sign.
# Population:pH              4 185.38  46.344  3.4488 0.064106 . 
# Population:Temperature:pH  7 229.58  32.798  2.4408 0.117285   
# Residuals                  8 107.50  13.437                    

TukeyHSD(big4 <- aov(first.big ~ Population+Temperature+Population:pH+pH:Population:Temperature, data=spawning_group_sum))

aggregate(first.big ~ pH:Temperature:Population, data=spawning_group_sum, sum)
aggregate(first.big ~ pH:Temperature:Population, data=spawning_group_sum, mean)
aggregate(first.big ~ Groups, data=spawning_group_sum, mean)
145.8-135.9

# Day of maximum release
anova(max1 <- lm(maxday ~ Population*Temperature*pH, data=spawning_group_sum)) 
anova(max2 <- lm(maxday ~ Population*Temperature*pH - Population:pH, data=spawning_group_sum)) 
anova(max3 <- lm(maxday ~ Population*Temperature*pH - Population:pH - Population:Temperature:pH, data=spawning_group_sum)) 
anova(max4 <- lm(maxday ~ Population*Temperature*pH - Population:pH - Population:Temperature:pH- Population:Temperature, data=spawning_group_sum)) 
anova(max5 <- lm(maxday ~ Population+Temperature+pH, data=spawning_group_sum)) 
anova(max6 <- lm(maxday ~ Population+Temperature, data=spawning_group_sum)) 
anova(max7 <- lm(maxday ~ Temperature, data=spawning_group_sum)) 

AIC(max1,max2,max3,max4,max5,max6,max7)
anova(max6) #lowest AIC, just Pop + Temp
# Response: maxday
#                Df  Sum Sq Mean Sq F value  Pr(>F)  
# Population      3  417.37  139.12  2.2364 0.11708  
# Temperature     1  416.67  416.67  6.6979 0.01804 * <--- sign. 
# Residuals       19 1181.96   62.21             
summary(max6 <- lm(maxday ~ Population+Temperature, data=spawning_group_sum)) 

# No. release days 
anova(lm(release.days ~ Temperature*pH, data=spawning_group_sum))
anova(lm(release.days ~ Population*Temperature*pH, data=subset(spawning_group_sum, Population!="K")))

anova(pulses1 <- lm(release.days ~ Population*Temperature*pH, data=spawning_group_sum))
anova(pulses2 <- lm(release.days ~ Population*Temperature*pH-Population:Temperature:pH, data=spawning_group_sum))
anova(pulses3 <- lm(release.days ~ Population*Temperature*pH-Population:Temperature:pH-Population:pH, data=spawning_group_sum))
anova(pulses4 <- lm(release.days ~ Population*Temperature*pH-Population:Temperature:pH-Population:pH-pH, data=spawning_group_sum))
anova(pulses5 <- lm(release.days ~ Population*Temperature, data=spawning_group_sum)) # Lowest AIC
anova(pulses6 <- lm(release.days ~ Population+Temperature, data=spawning_group_sum))
AIC(pulses1,pulses2,pulses3,pulses4,pulses5,pulses6)

# Response: release.days
#                         Df Sum Sq Mean Sq F value    Pr(>F)    
# Population              3 97.875  32.625 13.4710 0.0001209 *** <-- sign.
# Temperature             1 24.000  24.000  9.9097 0.0062217 **  <-- sign.
# Population:Temperature  3 17.375   5.792  2.3914 0.1067488    
# Residuals              16 38.750   2.422                      
# ---
TukeyHSD(test1 <- aov(release.days ~ Population*Temperature, data=spawning_group_sum))
TukeyHSD(test1 <- aov(release.days ~ Population*Temperature, data=subset(spawning_group_sum, Population!="K")))

# Identify peak larval release dates for each treatment (cohorts combined)

# Sum # larvae released by treatment each day 
treatment_total <- group_by(larvae, Date, pH, Temperature) %>% dplyr::summarize(daily_total = sum(larvae.per.broodcm, na.rm = T))
hist(treatment_total$daily_total)

# smoothing resource: https://rafalab.github.io/dsbook/smoothing.html

smooth.6.amb <- as.data.frame(subset(as.data.frame(treatment_total), Temperature==6 & pH=="Ambient")) %>%
  ggplot(aes(x=as.Date(Date), y=daily_total)) +
  geom_point() + 
  geom_smooth(color="red",  span = 0.30, method.args = list(degree=1)) +
  scale_x_date(date_breaks = "1 week") + ggtitle("6 ambient pCO2, smoothed")
ggplotly(smooth.6.amb)
# May 15, June 5

smooth.6.low <- as.data.frame(subset(as.data.frame(treatment_total), Temperature==6 & pH=="Low")) %>%
  ggplot(aes(x=as.Date(Date), y=daily_total)) +
  geom_point() + 
  geom_smooth(color="red",  span = 0.30, method.args = list(degree=1)) +
  scale_x_date(date_breaks = "1 week") + ggtitle("6 high pCO2, smoothed")
ggplotly(smooth.6.low)
# May 21, June 15

smooth.10.amb <- as.data.frame(subset(as.data.frame(treatment_total), Temperature==10 & pH=="Ambient")) %>%
  ggplot(aes(x=as.Date(Date), y=daily_total)) +
  geom_point() + 
  geom_smooth(color="red",  span = 0.30, method.args = list(degree=1)) +
  scale_x_date(date_breaks = "1 week") + ggtitle("10 ambient pCO2, smoothed")
ggplotly(smooth.10.amb)
# May 17, June 2

smooth.10.low <- as.data.frame(subset(as.data.frame(treatment_total), Temperature==10 & pH=="Low")) %>%
  ggplot(aes(x=as.Date(Date), y=daily_total)) +
  geom_point() + 
  geom_smooth(color="red",  span = 0.30, method.args = list(degree=1)) +
  scale_x_date(date_breaks = "1 week") + ggtitle("10 high pCO2, smoothed")
ggplotly(smooth.10.low)
# May 24, June 12

#1st peak diff: 
peaks.1 <- c((24-15),(24-17),(21-15),(21-17))
mean(peaks.1)
sd(peaks.1)

peaks.2 <- c((15-5),(15-2),(12-5),(12-2))
mean(peaks.2)
sd(peaks.2)

# Summary statistics 

aggregate(release.days ~ Population, spawning_group_sum, sd)
aggregate(total.percap ~ Temperature+pH, data=test, mean)
aggregate(max ~ Population, data=spawning_group_sum, mean)
aggregate(max ~ Population, data=spawning_group_sum, sd)
aggregate(maxday ~ Temperature, data=spawning_group_sum, mean)
aggregate(maxday ~ Temperature, data=spawning_group_sum, sd)
aggregate(first.big ~ Temperature, data=spawning_group_sum, mean)
aggregate(first.big ~ Temperature, data=spawning_group_sum, sd)
138.2500-143.4167
mean(subset(spawning_group_sum, Population=="K" | Population=="SN")$release.days)
sd(subset(spawning_group_sum, Population=="K" | Population=="SN")$release.days)
mean(subset(spawning_group_sum, Population=="NF" | Population=="HL")$release.days)
sd(subset(spawning_group_sum, Population=="NF" | Population=="HL")$release.days)

# Post-hoc tests on sign. different metrics via Tukey 
TukeyHSD(cum.total.aov) # running total significantly higher in the 10-ambient vs. 6-ambient 
TukeyHSD(cum.percap.aov) # running total per broodstock cm significantly higher in the 10-ambient vs. 6-ambient 
TukeyHSD(maxday.aov) # max larval release earler in the 10C group 
plot(x=spawning_group_sum$Treatment, y=spawning_group_sum$maxday, main="Calendar day at max larval release") 

# How many calendar days later was the maxday in the chilled group?  
aggregate(maxday ~ pH+Temperature, spawning_group_sum, mean) 
152.1667 - 143.8333

# =================
# Figures 

# reorder population factors for North->South 
spawning_group_sum$Population <- factor(spawning_group_sum$Population, levels=c("NF", "HL", "SN", "K"))


# Cumulative larval release line plots 

#Calculate cumulative larvae released through time for each pH/temp treatment (combine replicates)

Groups <- levels(larvae$Group)
plot.names <- c("Dabob Bay", "Oyster Bay C2", "Fidalgo Bay", "Oyster Bay C1")

larvae.0 <- list()
for (i in 1:length(Groups)) {
  larvae.0[[i]] <- as.data.frame(group_by(subset(larvae, Group==Groups[i]), Population, Treatment, Temperature, pH) %>%  complete(Date=min(Date)-1, fill = list(Tot.Larvae=0, total.released=0, larvae.per.broodcm=0)))  
}
larvae.0 <- rbind(bind_rows(larvae.0), larvae)

fecundity.pop <- group_by(larvae.0, Population, Temperature, pH) %>% dplyr::mutate(cum.total=cumsum(total.released),cum.percap = cumsum(larvae.per.broodcm),CalDay = as.numeric(format(Date,"%j"))) %>% dplyr::arrange(Date) %>% dplyr::select(Date,CalDay,pH,Temperature, Population,total.released,larvae.per.broodcm,cum.total,cum.percap) %>% mutate(Date = as.Date(Date))

plot.cohort <- levels(fecundity.pop$Population)

# NEED TO CHANGE THESE PLOTS: INCREASE FONT SIZE, MAKE GRAY SCALE W/ SHAPES
plot_list = list()
for (i in 1:4) {
  p <- ggplot() +
    geom_line(data=subset(fecundity.pop, Population == plot.cohort[i]), aes(x=CalDay-101, y=cum.percap, group=Temperature:pH, color=Temperature:pH), size=1.6) + theme_bw(base_size = 10) + ylab("No. of larvae") + xlab(label="Days in reproductive conditioning") + 
    labs(title=(plot.names[i])) +
    theme(plot.title = element_text(size = 13, hjust=0.08, margin = margin(t = 30, b = 0), colour = "gray30"), 
          axis.title.y = element_blank(), axis.title.x = element_text(colour="gray30"), axis.text.x = element_text(size=10), axis.text.y = element_text(size=10)) +
    scale_x_continuous(limits=c(min=28,max=90), breaks = c(28,42,56,70,86)) +
    scale_y_continuous(label=comma, breaks=scales::pretty_breaks(n = 5)) +
    theme(legend.position = "none", legend.text = element_text(colour="gray30", size=14), legend.title = element_text(colour="gray30", size=14),  panel.grid.minor = element_blank(), 
          panel.grid.major.x = element_blank(), panel.border = element_blank(), axis.text.y = element_text(size=10), axis.line.x = element_line(color="gray30", size = .2), axis.line.y = element_line(color="gray30", size = .2)) +
    scale_color_manual(name="Temperature/pCO2",values=c('#67a9cf', '#2166ac', '#ef8a62', '#b2182b'), label=c("6°C/Amb", "6°C/High", "10°C/Amb", "10°C/High")) + geom_point(data=subset(fecundity.pop, Population == plot.cohort[i]), aes(x=CalDay-101, y=cum.percap, group=Temperature:pH, fill=Temperature:pH), shape=21, size=2, stroke=.2) + scale_fill_manual(values=c("#d1e5f0","#67a9cf","#fddbc7","#ef8a62"))
  plot_list[[i]] <- p
}

# Save plots to pdf. Makes a separate file for each plot.
for (i in 1:4) {
  file_name = paste("Figures/cumulative_larvae_", i, ".pdf", sep="")
  pdf(file_name, width = 6, height = 3)
  print(plot_list[[i]])
  dev.off()
}

# Mean daily release per adult  
pdf(file = "Figures/mean-release.pdf", width = 4.5, height = 5.5)
ggplot(spawning_group_sum, aes(x=Temperature:pH, y=mean.percap, fill=Temperature:pH)) + 
  geom_boxplot(width=0.35, position = position_dodge(width = .55)) +
  scale_fill_manual(values=c('#d1e5f0','#67a9cf','#fddbc7','#ef8a62'), label=c("6°C / Ambient", "6°C / High", "10°C / Ambient","10°C / High"), name="Treatment") + 
  geom_point(size=2.75, color="gray20", aes(shape=Population, group=Temperature:pH), position=position_jitterdodge(jitter.width = 0.75, jitter.height = .1, dodge.width = 0.5)) + 
  scale_shape_manual(values=c(15, 17, 19, 8), labels=c("Fidalgo Bay", "Dabob Bay", "Oyster Bay C1", "Oyster Bay C2")) + 
  theme_bw(base_size = 10) + 
  theme(plot.title = element_text(size = 14, hjust = 0.5, margin = margin(t = 30, b = 0), colour = "gray30"), axis.title.y = element_text(colour="gray30", size=12), axis.title.x = element_text(colour="gray30", size=12), axis.text.x = element_blank(), legend.position = "left", legend.text = element_text(colour="gray30", size=12), legend.title = element_text(colour="gray30", size=12), panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(), axis.ticks = element_blank(), panel.border = element_blank(), axis.line.x = element_line(color="gray30", size = .2), axis.line.y = element_line(color="gray30", size = .2)) + guides(color = guide_legend(override.aes = list(size=5.5)), shape = guide_legend(override.aes = list(size=5))) +
  ggtitle("Mean larvae released\nper oyster, per day") + ylab("No. larvae") + xlab("Treatment") + scale_y_continuous(label=comma, limits=c(min=0, max=4800))
dev.off()

spawning_group_sum$pH <- factor(spawning_group_sum$pH, levels=c("Low", "Ambient"))
spawning_group_sum$Temperature <- factor(spawning_group_sum$Temperature, levels=c(10, 6))

# Peak larval release date 
pdf(file = "Figures/peak-release.pdf", width = 6, height = 3)
ggplot(spawning_group_sum, aes(x=Temperature:pH, y=maxday-100, fill=Temperature:pH)) + 
  geom_boxplot(width=0.35, position = position_dodge(width = .55)) +
  scale_fill_manual(values=c('#ef8a62','#fddbc7','#67a9cf','#d1e5f0'), label=c("6°C / Ambient", "6°C / High", "10°C / Ambient","10°C / High"), name="Treatment") + 
  geom_point(size=2.75, color="gray20", aes(shape=Population, group=Temperature:pH), position=position_jitterdodge(jitter.width = 0.75, jitter.height = .1, dodge.width = 0.5)) + 
  scale_shape_manual(values=c(15, 17, 19, 8), labels=c("Fidalgo Bay", "Dabob Bay", "Oyster Bay C1", "Oyster Bay C2")) +  theme_bw(base_size = 10) + 
  theme(plot.title = element_text(size = 15, hjust = 0.5, margin = margin(t = 30, b = 0), colour = "gray30"), axis.title.y = element_text(colour="gray30", size=11), axis.text.y = element_blank(), axis.title.x = element_text(colour="gray30", size=11), axis.text.x = element_text(size=11), legend.position = "none",panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(), axis.ticks = element_blank(), panel.border = element_blank(), axis.line.x = element_line(color="gray30", size = .2), axis.line.y = element_line(color="gray30", size = .2)) + 
  ggtitle("Peak larval release day") + ylab("No. days in spawning tanks") + xlab("Treatment") + scale_y_continuous(limits=c(min=28,max=90), breaks = c(28,42,56,70,86)) + coord_flip() 
dev.off()


# for plot legend only 
pdf(file = "Figures/release-legend.pdf", width = 4.5, height = 5.5)
ggplot(spawning_group_sum, aes(x=Temperature:pH, y=mean.percap, fill=Temperature:pH)) + 
  geom_point(size=2.75, color="gray20", aes(shape=Population, group=Temperature:pH)) + 
  scale_shape_manual(values=c(15, 17, 19, 8), labels=c("Fidalgo Bay", "Dabob Bay", "Oyster Bay C1", "Oyster Bay C2")) + 
  geom_boxplot() +
  scale_fill_manual(values=c('#d1e5f0','#67a9cf','#fddbc7','#ef8a62'), label=c("6°C / Ambient", "6°C / High", "10°C / Ambient","10°C / High"), name="Treatment") + 
  theme_bw(base_size = 10) + 
  theme(legend.position = "left", legend.text = element_text(colour="gray30", size=12), legend.title = element_text(colour="gray30", size=12)) + guides(color = guide_legend(override.aes = list(size=5.5)), shape = guide_legend(override.aes = list(size=5))) +
  ggtitle("For larval release legend only")
dev.off()


# ===========================
# Supplementary Materials 

# Scatter plot of larval release magnitude; shows patters among cohorts 
pdf(file = "Figures/release-mag-scatter.pdf", width = 7.45, height = 3.75)
ggplot(spawning_group_sum, aes(x=release.days, y=overall_Total)) +
  geom_point(size=3, stroke=2, aes(shape=Population, colour=Temperature:pH), position = position_jitter(w = 0.3, h = 0)) + scale_color_manual(values=c('#67a9cf', '#2166ac', '#ef8a62', '#b2182b'), label=c("6°C / Ambient", "6°C / High", "10°C / Ambient", "10°C / High"), name="Treatment") + scale_shape_manual(values=c(15, 17, 19, 8), labels=c("Fidalgo Bay", "Dabob Bay", "Oyster Bay C1", "Oyster Bay C2")) + 
  ggtitle("Larval release magnitude") + theme_bw(base_size=12) +
  theme(plot.title = element_text(size = 16, hjust = 0, colour = "gray30"),
        legend.position = "right", axis.title.x = element_text(colour="gray30", size=14), axis.text.x = element_text(colour="gray30", size=14), axis.title.y = element_text(colour="gray30", size=14), axis.text.y = element_text(colour="gray30", size=14)) +
  xlab("No. large larval pulses (~families)") + ylab("Cumulative larvae released") +
  scale_x_continuous(limits=c(min=0,max=max(spawning_group_sum$release.days))) +
  scale_y_continuous(limits=c(min=0,max=max(spawning_group_sum$overall_Total)), label=comma) 
dev.off()

# Release Timing

# first.big and maxday are plotted as x-100, since the calendar day that reproductive conditioning was day 100 (april 11th, 2017); this shows the # days in reproductive conditioning. Reproductive patterns among cohorts are evident. 
pdf(file = "Figures/release-timing-scatter.pdf", width = 7.45, height = 3.75)
ggplot(spawning_group_sum, aes(x=first.big-100, y=maxday-100)) +
  geom_point(size=3, stroke=2, aes(shape=Population, colour=Temperature:pH), position = position_jitter(w = 0.8, h = 0.8)) + scale_color_manual(values=c('#67a9cf', '#2166ac', '#ef8a62', '#b2182b'), label=c("6°C / Ambient", "6°C / High", "10°C / Ambient", "10°C / High"), name="Treatment") + scale_shape_manual(values=c(15, 17, 19, 8), labels=c("Fidalgo Bay", "Dabob Bay", "Oyster Bay C1", "Oyster Bay C2")) +  
  ggtitle("Larval release timing") + theme_bw(base_size=12) +
  theme(plot.title = element_text(size = 16, hjust = 0, colour = "gray30"), axis.text.x = element_text(colour="gray30", size=14), axis.title.x = element_text(colour="gray30", size=14), axis.title.y = element_text(colour="gray30", size=14), axis.text.y = element_text(colour="gray30", size=14), legend.text = element_text(colour="gray30", size=14), legend.title = element_text(colour="gray30", size=14)) + xlab("Days to release onset") + ylab("Days to maximum release")
dev.off()
