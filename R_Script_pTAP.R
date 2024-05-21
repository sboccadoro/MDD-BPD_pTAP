
#________________________Import data________________________________________________________________________________________________________________________
library(readxl)
pTAP <- read_excel("C:/Yourpath/R_data_pTAP.xlsx")     #directory of the data and name of the file with the data
View(pTAP)
#________________________Check structure of imported data________________________________________________________________________________________________________________________
str(pTAP)

#________________________Recode variables________________________________________________________________________________________________________________________
#Recode variables into factors, numeric, integer and rename new variables-> important for mixed model!

pTAP$Trial <- as.numeric(pTAP$Trial) 
pTAP$Trial.z <- (pTAP$Trial - mean(pTAP$Trial))/sd(pTAP$Trial)   # z transformation
pTAP$Subject <- factor(pTAP$Subject)
pTAP$Aggrchoice <- as.numeric (pTAP$Choice)
pTAP$Outcome <- factor(pTAP$Outcome)
pTAP$Gender <- factor(pTAP$Gender)
pTAP$Age <- as.numeric (pTAP$Age)
pTAP$Group <- factor(pTAP$Group)
pTAP$Belief <- factor(pTAP$Belief)
pTAP$SCR <- as.numeric (pTAP$SCR)
pTAP$Cluster <- factor(pTAP$Cluster)

#_______________________Models______________________________________________________________________________________________________________

#null model selection

m0 <- lmer(Aggrchoice ~ (1|Subject), data = pTAP)
m01 <- lmer(Aggrchoice ~ (1 + Trial.z|Subject), data = pTAP)
anova(m0, m01)

m02 <- lmer(SCR ~ (1|Subject), data = pTAP)
m03 <- lmer(SCR ~ (1 + Trial.z|Subject), data = pTAP)
anova(m02, m03)

#check and plot residuals of final null models

qqnorm(resid(m01))
qqline(resid(m01))
shapiro.test(residuals(m01))

qqnorm(resid(m03))
qqline(resid(m03))
shapiro.test(residuals(m03))

# Fit robust linear mixed-effects model for Aggrchoice

library(robustlmm)

m1 <- rlmer(Aggrchoice ~ Group + Outcome + Gender + Belief + Trial.z + Group*Outcome + Gender*Outcome
                    + (1 + Trial.z|Subject), data = pTAP, method ="DASvar")
summary(m1)

library(sjPlot)
tab_model(m1, show.se = TRUE, show.stat = "t")

# Fit robust linear mixed-effects model for SCRs

m2 <- rlmer(SCR ~ Aggrchoice + Group + Outcome + Gender + Belief + Trial.z + Group*Aggrchoice + Gender*Aggrchoice +
             Group*Outcome + Gender*Outcome + (1+Trial.z|Subject), data = pTAP, method ="DASvar")
summary(m2)
tab_model(m2, show.se = TRUE, show.stat = "t")

#post-hoc tests for significant interaction effects

library (emmeans)

emtrends(m2, pairwise ~ Group, var = "Aggrchoice")

#plot main effect of Trial.z

library(effects)
library(ggplot2)

ef1 <- effect("Trial.z", m2, KR=T)
y <- as.data.frame(ef1)
g <- ggplot(y, aes(Trial.z, fit)) + geom_line(aes(color = "blue")) + geom_ribbon(aes(ymin=fit-se, ymax=fit+se), fill = "blue", alpha = 0.3)
g1 <- g + theme(axis.text=element_text(size=12), axis.text.x = element_text(face="bold"),
                axis.title=element_text(size=14,face="bold")) + theme(plot.title = element_text(size=15, face = "bold", hjust = 0.5))
suppfig1 <- g1 + labs(x="Trial.z", y="SCR") + ggtitle("Main effect of Trial on SCR") + scale_color_manual(values=c("blue")) + theme(legend.position = "none")
plot(suppfig1)

#plot interaction effect between Aggrchoice and Group

ef2 <- effect(term = "Aggrchoice*Group", m2, multiline = TRUE)
suppfig2 <- as.data.frame(ef2)
ggplot(suppfig2, aes(Aggrchoice, fit, group = Group)) +
  geom_line(aes(color = Group, linetype = Group), alpha = 1, linewidth = 1) +
  geom_ribbon(aes(ymin = fit - se, ymax = fit + se, fill = Group), alpha = 0.3) +
  labs(x = expression(bold("Aggrchoice")), y = expression(bold("SCR")), title = expression(bold("Interaction effect between Aggrchoice and Group on SCR"))) +
  scale_color_manual(values = c("blue", "gold2", "firebrick3"), name = expression(bold("Group")), labels = c("HC", "MDD", "BPD")) +
  scale_fill_manual(values = c("blue", "gold2", "firebrick3"), name = expression(bold("95% CI")), labels = c("HC", "MDD", "BPD")) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"), name = expression(bold("Group")), labels = c("HC", "MDD", "BPD")) +
  theme_minimal() +
  theme(legend.position = "right",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 15, face = "bold"))


#Clusters

# Fit robust linear mixed-effects model for Aggrchoice

m3 <- rlmer(Aggrchoice ~ Cluster + Outcome + Gender + Belief + Trial.z + Cluster*Outcome 
                    + Gender*Outcome + (1+Trial.z|Subject), data=pTAP, method ="DASvar")
summary(m3)
tab_model(m3, show.se = TRUE, show.stat = "t")

#post-hoc tests for significant interaction effects

emmeans(m3, pairwise ~ Outcome|Cluster,  adjust = "tukey")
emmeans(m3, pairwise ~ Cluster|Outcome,  adjust = "tukey")

#plot interaction effect between Cluster and Outcome

(mylist <- list(Choice=seq(1,4,by=0.5),Outcome=c("1(Won)","2(Loss)")))
r <- emmip(m3, Cluster ~Outcome, at=mylist,CIs=TRUE) + xlab("Outcome") + 
  ylab("Aggrchoice") + theme(text = element_text(face = "bold", size = 14), legend.text = element_text(face = "plain"))

r1 <- r + labs(title = "Interaction effect between Outcome and Cluster on Aggrchoice") + theme(plot.title = element_text(size=15, face = "bold", hjust = 0.5))
r2 <- r1 + scale_colour_manual(values = c("#0088cc", "orange"), labels = c("LPA", "HPA"))
fig4 <- r2 + scale_x_discrete(labels = c("Won","Loss"))
plot(fig4)

# Fit robust linear mixed-effects model for SCRs

m4 <- rlmer(SCR ~ Aggrchoice + Cluster + Outcome + Belief + Gender + Trial.z + Cluster*Aggrchoice + 
              Gender*Aggrchoice + Cluster*Outcome + Gender*Outcome +
              (1+Trial.z|Subject), data = pTAP, method ="DASvar")
summary(m4)
tab_model(m4, show.se = TRUE, show.stat = "t")