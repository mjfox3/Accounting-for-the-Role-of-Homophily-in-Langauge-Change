library(GGally)
library(ggplot2)
library(intergraph)
library(devtools)
library(digest)
library(AICcmodavg)
library(effects)
library(bipartite)
library(stringr)
library(dplyr)
library(reshape)
library(lme4)


####Hypotheses (Modeling)####

####(Hypothesis One)####
## 1) Similarity in school co-attendance network, as an estimation of 
#     latent induced homophily and measured by Hamming Distance, 
#     will affect linguistic similarity independent of geographic proximity, 
#     such that similarity in ecology will be positively correlated with 
#     linguistic similarity. Vowel classes undergoing change are expected to 
#     show greater dissimilarity as dyads get further apart in age and as a 
#     function of their involvement in the NCS, and social ecology

####H1 - F2####

##BOT##
H1.F2.BOT <- list()
H1.F2.BOT[[1]] <- lm(MVCD_absF2 ~ 1, data = Blau.df[Blau.df$vowel_class %in% c("BOT"),])

H1.F2.BOT[[2]] <- lm(MVCD_absF2 ~ yob_diff, data = Blau.df[Blau.df$vowel_class %in% c("BOT"),])
H1.F2.BOT[[3]] <- lm(MVCD_absF2 ~ yob_diff + BD_town, data = Blau.df[Blau.df$vowel_class %in% c("BOT"),])
H1.F2.BOT[[4]] <- lm(MVCD_absF2 ~ yob_diff + BD_town + HD_ij, data = Blau.df[Blau.df$vowel_class %in% c("BOT"),])

H1.F2.BOT[[5]] <- lm(MVCD_absF2 ~ yob_diff*HD_ij, data = Blau.df[Blau.df$vowel_class %in% c("BOT"),])
H1.F2.BOT[[6]] <- lm(MVCD_absF2 ~ yob_diff*BD_town, data = Blau.df[Blau.df$vowel_class %in% c("BOT"),])
H1.F2.BOT[[7]] <- lm(MVCD_absF2 ~ yob_diff*BD_town + yob_diff*HD_ij, data = Blau.df[Blau.df$vowel_class %in% c("BOT"),])
H1.F2.BOT[[8]] <- lm(MVCD_absF2 ~ yob_diff*HD_ij*BD_town, data = Blau.df[Blau.df$vowel_class %in% c("BOT"),])

H1.F2.BOT.mod.names <- c("H1NULL", "H1M1F2", "H1M2F2", "H1M3F2", "H1M4F2", "H1M5F1", "H1M6F2", "H1M7F2")
names(H1.F2.BOT) <- H1.F2.BOT.mod.names
H1.F2.BOT.AIC.tab <- aictab(cand.set = H1.F2.BOT, modnames = H1.F2.BOT.mod.names, sort=T)

H1.F2.BOT.inter <- as.data.frame(effect("yob_diff*HD_ij*BD_town", H1.F2.BOT[[8]]))

H1.F2.BOT.N <- ddply(Blau.df[Blau.df$vowel_class %in% c("BOT"),], .(BD_town), summarize, N = length(BD_town))
H1.F2.BOT.N$N <- do.call(paste, list(rep("n=", nrow(H1.F2.BOT.N)), H1.F2.BOT.N$N))
H1.F2.BOT.inter <- merge(H1.F2.BOT.inter, H1.F2.BOT.N, by=c("BD_town"))
H1.F2.BOT.inter$measure <- "F2"

##BUT##
H1.F2.BUT <- list()
H1.F2.BUT[[1]] <- lm(MVCD_absF2 ~ 1, data = Blau.df[Blau.df$vowel_class %in% c("BUT"),])

H1.F2.BUT[[2]] <- lm(MVCD_absF2 ~ yob_diff, data = Blau.df[Blau.df$vowel_class %in% c("BUT"),])
H1.F2.BUT[[3]] <- lm(MVCD_absF2 ~ yob_diff + BD_town, data = Blau.df[Blau.df$vowel_class %in% c("BUT"),])
H1.F2.BUT[[4]] <- lm(MVCD_absF2 ~ yob_diff + BD_town + HD_ij, data = Blau.df[Blau.df$vowel_class %in% c("BUT"),])

H1.F2.BUT[[5]] <- lm(MVCD_absF2 ~ yob_diff*HD_ij, data = Blau.df[Blau.df$vowel_class %in% c("BUT"),])
H1.F2.BUT[[6]] <- lm(MVCD_absF2 ~ yob_diff*BD_town, data = Blau.df[Blau.df$vowel_class %in% c("BUT"),])
H1.F2.BUT[[7]] <- lm(MVCD_absF2 ~ yob_diff*BD_town + yob_diff*HD_ij, data = Blau.df[Blau.df$vowel_class %in% c("BUT"),])
H1.F2.BUT[[8]] <- lm(MVCD_absF2 ~ yob_diff*HD_ij*BD_town, data = Blau.df[Blau.df$vowel_class %in% c("BUT"),])

H1.F2.BUT.mod.names <- c("H1NULL", "H1M1F2", "H1M2F2", "H1M3F2", "H1M4F2", "H1M5F1", "H1M6F2", "H1M7F2")
names(H1.F2.BUT) <- H1.F2.BUT.mod.names
H1.F2.BUT.AIC.tab <- aictab(cand.set = H1.F2.BUT, modnames = H1.F2.BUT.mod.names, sort=T)

H1.F2.BUT.inter <- as.data.frame(effect("yob_diff*HD_ij*BD_town", H1.F2.BUT[[8]]))

H1.F2.BUT.N <- ddply(Blau.df[Blau.df$vowel_class %in% c("BUT"),], .(BD_town), summarize, N = length(BD_town))
H1.F2.BUT.N$N <- do.call(paste, list(rep("n=", nrow(H1.F2.BUT.N)), H1.F2.BUT.N$N))
H1.F2.BUT.inter <- merge(H1.F2.BUT.inter, H1.F2.BUT.N, by=c("BD_town"))
H1.F2.BUT.inter$measure <- "F2"


##Plotting Interactions##
H1.F2.BOT.inter$vowel <- "BOT"
H1.F2.BUT.inter$vowel <- "BUT"
H1.F2.interact <- rbind(H1.F2.BOT.inter, H1.F2.BUT.inter)

H1.F2.interact$BD_town <- revalue(H1.F2.interact$BD_town, c("Same" = "Same (Town)", "Different" = "Different (Town)"))
H1.F2.interact <- transform(H1.F2.interact, BD_town=factor(BD_town, c("Same (Town)", "Different (Town)")))

ggplot(H1.F2.interact[H1.F2.interact$HD_ij %in% c(0, 8, 14),], aes(yob_diff, fit, fill=as.factor(HD_ij), linetype=as.factor(HD_ij), color=as.factor(HD_ij))) +
  geom_line() +
  geom_ribbon(aes(ymin=lower,ymax=upper), alpha=0.4, color=NA) +
  facet_grid(BD_town~vowel) +
  #scale_fill_grey() +
  xlab(expression(abs(YoB[i] - YoB[j]))) +
  ylab(expression("Predicted MVCD"[Z2])) +
  guides(fill=guide_legend(title=expression("HD"[ij])), linetype=guide_legend(title=expression("HD"[ij])), color=guide_legend(title=expression("HD"[ij]))) +
  theme_bw(14) +
  theme(legend.position = "bottom")
ggsave("H1M7Z2_BOT_BUT_conditional_slopes_color.pdf", height=6, width=7)


####H1 - Diagonal####
H1.poop <- lm(MVCD_absD ~ yob_diff*BD_town*HD_ij, data = Blau.df[Blau.df$vowel_class %in% c("BAD"),] )

Blau.df$BD_town <- relevel(Blau.df$BD_town, ref= "Same")

H1.diag <- list()
H1.diag[[1]] <- lmer(MVCD_absD ~ (1|dyad), data = Blau.df[Blau.df$vowel_class %in% c("BAG", "BAN", "BAD", "BAT", "BACK", "BAC"),],  REML = F )

H1.diag[[2]] <- lmer(MVCD_absD ~ vowel_class*yob_diff + (1|dyad), data = Blau.df[Blau.df$vowel_class %in% c("BAG", "BAN", "BAD", "BAT", "BACK", "BAC"),],  REML = F )
H1.diag[[3]] <- lmer(MVCD_absD ~ vowel_class*yob_diff + BD_town + (1|dyad), data = Blau.df[Blau.df$vowel_class %in% c("BAG", "BAN", "BAD", "BAT", "BACK", "BAC"),],  REML = F )
H1.diag[[4]] <- lmer(MVCD_absD ~ vowel_class*yob_diff + BD_town + HD_ij + (1|dyad), data = Blau.df[Blau.df$vowel_class %in% c("BAG", "BAN", "BAD", "BAT", "BACK", "BAC"),],  REML = F )

H1.diag[[5]] <- lmer(MVCD_absD ~ vowel_class*yob_diff*HD_ij + (1|dyad), data = Blau.df[Blau.df$vowel_class %in% c("BAG", "BAN", "BAD", "BAT", "BACK", "BAC"),],  REML = F )
H1.diag[[6]] <- lmer(MVCD_absD ~ vowel_class*yob_diff*BD_town + (1|dyad), data = Blau.df[Blau.df$vowel_class %in% c("BAG", "BAN", "BAD", "BAT", "BACK", "BAC"),],  REML = F )
H1.diag[[7]] <- lmer(MVCD_absD ~ vowel_class*yob_diff*HD_ij + vowel_class*yob_diff*BD_town + (1|dyad), data = Blau.df[Blau.df$vowel_class %in% c("BAG", "BAN", "BAD", "BAT", "BACK", "BAC"),],  REML = F )
H1.diag[[8]] <- lmer(MVCD_absD ~ vowel_class*yob_diff*BD_town*HD_ij + (1|dyad), data = Blau.df[Blau.df$vowel_class %in% c("BAG", "BAN", "BAD", "BAT", "BACK", "BAC"),],  REML = F )

H1.diag.mod.names <- c("H1NULL", "H1M1D", "H1M2D", "H1M3D", "H1M4D", "H1M5D", "H1M6D", "H1M7D")
names(H1.diag) <- H1.diag.mod.names
H1.diag.AIC.tab <- aictab(cand.set = H1.diag, modnames = H1.diag.mod.names, sort=T)

H1.diag.inter <- as.data.frame(effect("vowel_class*yob_diff*BD_town*HD_ij", H1.diag[[8]], KR=F))
H1.diag.inter$BD_town <- revalue(H1.diag.inter$BD_town, c("Same" = "Same (Town)", "Different" = "Different (Town)"))
H1.diag.inter <- transform(H1.diag.inter, vowel_class=factor(vowel_class, c("BAD", "BAT", "BAN", "BAG", "BACK", "BAC")), BD_town=factor(BD_town, c("Same (Town)", "Different (Town)")))

H1.diag.inter.town$term <- "Town[ij]"
H1.diag.inter.HD$term <- "HD[ij]"
H1.diag.inter <- rbind.fill(H1.diag.inter.town, H1.diag.inter.HD[H1.diag.inter.HD$HD_ij %in% c(0, 40, 80),])

H1.diag.N <- ddply(Blau.df[Blau.df$vowel_class %in% c("BAG", "BAN", "BAD", "BAT", "BACK", "BAC"),], .(vowel_class, BD_town), summarize, N = length(vowel_class))
H1.diag.N$N <- do.call(paste, list(rep("n=", nrow(H1.diag.N)), H1.diag.N$N))

H1.diag.inter <- merge(H1.diag.inter, H1.diag.N, by=c("vowel_class", "BD_town"))
H1.diag.inter$town_N <- as.factor(do.call(paste, list(H1.diag.inter$SCH_ij, "\n(", H1.diag.inter$N, ")", sep="")))

H1.diag.min <- ddply(H1.diag.inter, .(vowel_class), summarize, min=min(lower))
H1.diag.N <- merge(H1.diag.N, H1.diag.min, by=c("vowel_class"))

H1.diag.inter$measure <- "diagonal"

H1.diag.inter.HD$HD_ij <- as.factor(H1.diag.inter.HD$HD_ij)
H1.diag.inter$HD_ij <- as.factor(H1.diag.inter$HD_ij)
H1.diag.inter$HD_ij <- as.factor(H1.diag.inter$HD_ij)


H1.diag.inter.HD <- transform(H1.diag.inter.HD, vowel_class=factor(vowel_class, levels=c("BAD", "BAT", "BAN", "BAG", "BACK", "BAC")))

##Conditional Slope Plotting##
##Hamming Distance##
ggplot(H1.diag.inter.HD[H1.diag.inter.HD$vowel_class != "BAC",], aes(x=yob_diff, y=fit, linetype=as.factor(HD_ij), fill=as.factor(HD_ij))) +
  geom_line() +
  geom_ribbon(aes(ymin=lower,ymax=upper), alpha=0.4) +
  #geom_density_2d(data=Blau.df[Blau.df$vowel_class %in% c("BAD", "BAT", "BAG", "BACK", "BAN"),], aes(yob_diff, MVCD_absD, color=vowel_class), inherit.aes = F) +
  facet_wrap(~vowel_class) +
  ylab(expression("Predicted MVCD"[ijD])) +
  xlab("abs(YoB[i] - YoB[j])", parse=T) +
  scale_fill_grey() +
  theme_bw(14) +
  guides(linetype=guide_legend(title=expression("HD"[ij])), fill=guide_legend(title=expression("HD"[ij]))) +
  theme(legend.position = c(0.88,0.15), legend.justification = c(0.88,0.15))
ggsave("H1M6D_AE_HD_slopes.pdf", height=6, width=7)

##TOWN##

H1.diag.inter.town <- transform(H1.diag.inter.town, vowel_class=factor(vowel_class, levels=c("BAD", "BAT", "BAN", "BAG", "BACK", "BAC")))

ggplot(H1.diag.inter.town[H1.diag.inter.town$vowel_class != "BAC",], aes(x=yob_diff, y=fit, linetype=BD_town, fill=BD_town)) +
  geom_line() +
  geom_ribbon(aes(ymin=lower,ymax=upper), alpha=0.4) +
  #stat_summary(data=Blau.df[Blau.df$vowel_class %in% c("BAD", "BAT", "BAG", "BACK", "BAN"),], fun.data = "mean_se", alpha=0.3, aes(y=MVCD_absD, x=yob_diff)) +
  #geom_density_2d(data=Blau.df[Blau.df$vowel_class %in% c("BAD", "BAT", "BAG", "BACK", "BAN"),], aes(yob_diff, MVCD_absD, color=vowel_class), inherit.aes = F) +
  facet_wrap(~vowel_class) +
  ylab(expression("Predicted MVCD"[ijD])) +
  xlab(expression("YoB"[ij])) +
  ylim(0, 0.7) +
  scale_fill_grey() +
  theme_bw(14) +
  guides(linetype=guide_legend(title=expression("Town"[ij])), fill=guide_legend(title=expression("Town"[ij]))) +
  theme(legend.position = c(0.88,0.15), legend.justification = c(0.88,0.15))
ggsave("H1M6D_AE_town_slopes.pdf", height=6, width=7)


ggplot(H1.diag.inter.town[H1.diag.inter.town$vowel_class != "BAC",], aes(x=yob_diff, y=fit, linetype=BD_town)) +
  geom_line() +
  facet_grid(.~vowel_class) +
  theme_bw()


ggplot(H1.diag.inter[H1.diag.inter$vowel_class != "BAC" & H1.diag.inter$HD_ij %in% c(0, 8, 14),], aes(x=yob_diff, y=fit, linetype=as.factor(HD_ij), fill=as.factor(HD_ij), color=as.factor(HD_ij))) +
  geom_line() +
  geom_ribbon(aes(ymin=lower,ymax=upper), alpha=0.4, color=NA) +
  facet_grid(BD_town~vowel_class) +
  ylab(expression("Predicted MVCD"[D])) +
  xlab(expression(abs(YoB[i] - YoB[j]))) +
  theme_bw(14) +
  guides(linetype=guide_legend(title=expression("HD"[ij])), fill=guide_legend(title=expression("HD"[ij])), color=guide_legend(title=expression("HD"[ij]))) +
  theme(legend.position = "bottom")
ggsave("H1M7D_conditional_slopes_color.pdf", heigh=6, width=7)

###Plotting Coefficients###

H1.diag.coeffs <- as.data.frame(summary(H1.diag[[8]])$coefficients)
H1.diag.coeffs$upper <- H1.diag.coeffs$Estimate + H1.diag.coeffs$`Std. Error` * 1.96
H1.diag.coeffs$lower <- H1.diag.coeffs$Estimate - H1.diag.coeffs$`Std. Error` * 1.96
H1.diag.coeffs$Variable <- row.names(H1.diag.coeffs)

H1.diag.coeffs$Order <- as.factor(str_count(H1.diag.coeffs$Variable, ":"))
names(H1.diag.coeffs) <- c("Estimate", "SE", "t", "upper", "lower", "Variable", "Order")

write.csv(H1.diag.coeffs, "H1.diag.coeffs2.csv", row.names = F, quote = F)
H1.diag.coeffs<- read.csv(file.choose(), header=T,sep=",")

ex2 <- parse(text = levels(droplevels(H1.diag.coeffs[H1.diag.coeffs$over_0 == "red" & H1.diag.coeffs$Order %in% c(1,2,3),]$Variable)))

ggplot(H1.diag.coeffs[H1.diag.coeffs$Order == 3,], aes(x=Estimate, y=Variable)) + 
  geom_point() +
  geom_vline(xintercept=0, linetype=2) +
  geom_errorbarh(aes(xmin=lower,xmax=upper), height=0.13) +
  theme_bw() +
  #theme(axis.text.x = element_text(angle = -90, hjust=1)) +
  scale_y_discrete(labels = ex2)
ggsave("H1_diag_coeffs_sub.pdf", height=6, width=7)


####(Hypothesis Two)####
## 2)   Socio-demographic similarity, as an estimation of latent choice homophily 
#       and measured by whether actors i-j are/have the same sex, occupation,
#       and year of birth, will affect linguistic similarity net of latent 
#       induced homophily. Vowel classes undergoing change expected to show 
#       greater dissimilarity as dyads get further apart in age and as a function
#       of their involvement in the NCS, and socio-demographic similarity.

####H2 - F2####

##BOT##
H2.F2.BOT <- list()
H2.F2.BOT[[1]] <- lm(MVCD_absF2 ~ 1, data = Blau.df[Blau.df$vowel_class %in% c("BOT"),])
H2.F2.BOT[[2]] <- lm(MVCD_absF2 ~ BD_town + HD_ij, data = Blau.df[Blau.df$vowel_class %in% c("BOT"),])
H2.F2.BOT[[3]] <- lm(MVCD_absF2 ~ BD_town + HD_ij + yob_diff, data = Blau.df[Blau.df$vowel_class %in% c("BOT"),])
H2.F2.BOT[[4]] <- lm(MVCD_absF2 ~ BD_town + HD_ij + yob_diff + BD_collar, data = Blau.df[Blau.df$vowel_class %in% c("BOT"),])
H2.F2.BOT[[5]] <- lm(MVCD_absF2 ~ BD_town + HD_ij + yob_diff + BD_collar + BD_sex, data = Blau.df[Blau.df$vowel_class %in% c("BOT"),])

H2.F2.BOT[[6]] <- lm(MVCD_absF2 ~ BD_town + HD_ij + yob_diff*BD_collar + BD_sex, data = Blau.df[Blau.df$vowel_class %in% c("BOT"),])
H2.F2.BOT[[7]] <- lm(MVCD_absF2 ~ BD_town + HD_ij + yob_diff*BD_sex + BD_collar, data = Blau.df[Blau.df$vowel_class %in% c("BOT"),])
H2.F2.BOT[[8]] <- lm(MVCD_absF2 ~ BD_town + HD_ij + yob_diff*BD_sex + yob_diff*BD_collar, data = Blau.df[Blau.df$vowel_class %in% c("BOT"),])

H2.F2.BOT[[9]] <- lm(MVCD_absF2 ~ BD_town + HD_ij + yob_diff*BD_sex*BD_collar, data = Blau.df[Blau.df$vowel_class %in% c("BOT"),])

H2.F2.BOT.mod.names <- c("H2NULL", "H2M1Z2", "H2M2Z2", "H2M3Z2", "H2M4Z2", "H2M5Z2", "H2M6Z2", "H2M7Z2", "H2M8Z2")
names(H2.F2.BOT) <- H2.F2.BOT.mod.names 
H2.F2.BOT.AIC.tab <- aictab(cand.set = H2.F2.BOT, modnames = H2.F2.BOT.mod.names, sort=T)

H2.F2.inter <- as.data.frame(effect("vowel_class*collar_ij*yob_diff*SEX_ij", H2.F2[[8]], KR=F, type="response"))
H2.F2.inter <- transform(H2.F2.inter, collar_ij=factor(collar_ij, levels=c("BC-BC", "WCnd-WCnd", "WCd-WCd", "WCnd-BC", "WCd-BC", "WCd-WCnd")))

H2.F2.N <- ddply(Blau.df[Blau.df$vowel_class %in% c("BET", "BUT", "BOT"),], .(vowel_class, SEX_ij, collar_ij), summarize, N = length(vowel_class))
H2.F2.N$SEX_ABV_ij <- ""
H2.F2.N[H2.F2.N$SEX_ij == "Male-Male",]$SEX_ABV_ij <- "M"
H2.F2.N[H2.F2.N$SEX_ij == "Female-Female",]$SEX_ABV_ij <- "F"
H2.F2.N[H2.F2.N$SEX_ij == "Different",]$SEX_ABV_ij <- "D"
H2.F2.N$SEX_ABV_ij <- as.factor(H2.F2.N$SEX_ABV_ij)

H2.F2.min <- ddply(H2.F2.inter, .(vowel_class), summarize, min=min(lower))
H2.F2.N <- merge(H2.F2.N, H2.F2.min, by=c("vowel_class"))
H2.F2.N[H2.F2.N$SEX_ij == "Different",]$min <- H2.F2.N[H2.F2.N$SEX_ij == "Different",]$min + 0.02
H2.F2.N[H2.F2.N$SEX_ij == "Male-Male",]$min <- H2.F2.N[H2.F2.N$SEX_ij == "Male-Male",]$min - 0.02

H2.F2.N$town_N <- do.call(paste, list("n", "[", H2.F2.N$SEX_ABV_ij, "]", "==", as.character(H2.F2.N$N), sep=""))

H2.F2.inter <- merge(H2.F2.inter, H2.F2.N, by=c("vowel_class", "SEX_ij", "collar_ij"))

##BUT##
H2.F2.BUT <- list()
H2.F2.BUT[[1]] <- lm(MVCD_absF2 ~ 1, data = Blau.df[Blau.df$vowel_class %in% c("BUT"),])
H2.F2.BUT[[2]] <- lm(MVCD_absF2 ~ BD_town + HD_ij, data = Blau.df[Blau.df$vowel_class %in% c("BUT"),])
H2.F2.BUT[[3]] <- lm(MVCD_absF2 ~ BD_town + HD_ij + yob_diff, data = Blau.df[Blau.df$vowel_class %in% c("BUT"),])
H2.F2.BUT[[4]] <- lm(MVCD_absF2 ~ BD_town + HD_ij + yob_diff + BD_collar, data = Blau.df[Blau.df$vowel_class %in% c("BUT"),])
H2.F2.BUT[[5]] <- lm(MVCD_absF2 ~ BD_town + HD_ij + yob_diff + BD_collar + BD_sex, data = Blau.df[Blau.df$vowel_class %in% c("BUT"),])

H2.F2.BUT[[6]] <- lm(MVCD_absF2 ~ BD_town + HD_ij + yob_diff*BD_collar + BD_sex, data = Blau.df[Blau.df$vowel_class %in% c("BUT"),])
H2.F2.BUT[[7]] <- lm(MVCD_absF2 ~ BD_town + HD_ij + yob_diff*BD_sex + BD_collar, data = Blau.df[Blau.df$vowel_class %in% c("BUT"),])
H2.F2.BUT[[8]] <- lm(MVCD_absF2 ~ BD_town + HD_ij + yob_diff*BD_sex + yob_diff*BD_collar, data = Blau.df[Blau.df$vowel_class %in% c("BUT"),])

H2.F2.BUT[[9]] <- lm(MVCD_absF2 ~ BD_town + HD_ij + yob_diff*BD_sex*BD_collar, data = Blau.df[Blau.df$vowel_class %in% c("BUT"),])

H2.F2.BUT.mod.names <- c("H2NULL", "H2M1Z2", "H2M2Z2", "H2M3Z2", "H2M4Z2", "H2M5Z2", "H2M6Z2", "H2M7Z2", "H2M8Z2")
names(H2.F2.BUT) <- H2.F2.BUT.mod.names 
H2.F2.BUT.AIC.tab <- aictab(cand.set = H2.F2.BUT, modnames = H2.F2.BUT.mod.names, sort=T)

pred_data <- expand.grid(BD_sex=levels(Blau.df$BD_sex), 
                         BD_collar=levels(Blau.df$BD_collar), 
                         BD_town=levels(Blau.df$BD_town), 
                         HD_ij=seq(0,14, 2),
                         yob_diff=seq(0, 60, 10))

BOT_pred <- modavgPred(H2.F2.BOT, H2.F2.BOT.mod.names, newdata = pred_data)
BOT_pred <- cbind(pred_data, as.data.frame(BOT_pred[1:6]))

H1.F2.BOT.inter$vowel <- "BOT"
H1.F2.BUT.inter$vowel <- "BUT"
H1.F2.interact <- rbind(H1.F2.BOT.inter, H1.F2.BUT.inter)

H1.F2.interact <- transform(H1.F2.interact, BD_town=factor(BD_town, c("Same", "Different")))

ggplot(H1.F2.interact[H1.F2.interact$HD_ij %in% c(0, 8, 14),], aes(yob_diff, fit, fill=as.factor(HD_ij), linetype=as.factor(HD_ij))) +
  geom_line() +
  geom_ribbon(aes(ymin=lower,ymax=upper), alpha=0.4) +
  facet_grid(BD_town~vowel) +
  scale_fill_grey() +
  xlab(expression(YoB[ij])) +
  ylab(expression("Predicted MVCD"[Z2ij])) +
  guides(fill=guide_legend(title=expression("HD"[ij])), linetype=guide_legend(title=expression("HD"[ij]))) +
  theme_bw(14) +
  theme(legend.position = "bottom")
ggsave("H1M7Z2_BOT_BUT_conditional_slopes.pdf", height=6, width=7)


####H2 - Diagonal####
H2.diag <- list()
H2.diag[[1]] <- lmer(MVCD_absD ~ 1 + (1|dyad), data = Blau.df[Blau.df$vowel_class %in% c("BAG", "BAN", "BAD", "BAT", "BACK", "BAC"),],  REML = F)
H2.diag[[2]] <- lmer(MVCD_absD ~ BD_town + HD_ij + (1|dyad), data = Blau.df[Blau.df$vowel_class %in% c("BAG", "BAN", "BAD", "BAT", "BACK", "BAC"),],  REML = F)
H2.diag[[3]] <- lmer(MVCD_absD ~ BD_town + HD_ij + vowel_class*yob_diff + (1|dyad), data = Blau.df[Blau.df$vowel_class %in% c("BAG", "BAN", "BAD", "BAT", "BACK", "BAC"),],  REML = F)
H2.diag[[4]] <- lmer(MVCD_absD ~ BD_town + HD_ij + vowel_class*yob_diff + vowel_class*BD_collar + (1|dyad), data = Blau.df[Blau.df$vowel_class %in% c("BAG", "BAN", "BAD", "BAT", "BACK", "BAC"),],  REML = F)
H2.diag[[5]] <- lmer(MVCD_absD ~ BD_town + HD_ij + vowel_class*yob_diff + vowel_class*BD_collar + vowel_class*BD_sex + (1|dyad), data = Blau.df[Blau.df$vowel_class %in% c("BAG", "BAN", "BAD", "BAT", "BACK", "BAC"),],  REML = F)

H2.diag[[6]] <- lmer(MVCD_absD ~ BD_town + HD_ij + vowel_class*yob_diff*BD_collar + vowel_class*BD_sex + (1|dyad), data = Blau.df[Blau.df$vowel_class %in% c("BAG", "BAN", "BAD", "BAT", "BACK", "BAC"),],  REML = F)
H2.diag[[7]] <- lmer(MVCD_absD ~ BD_town + HD_ij + vowel_class*yob_diff*BD_sex + vowel_class*BD_collar + (1|dyad), data = Blau.df[Blau.df$vowel_class %in% c("BAG", "BAN", "BAD", "BAT", "BACK", "BAC"),],  REML = F)
H2.diag[[8]] <- lmer(MVCD_absD ~ BD_town + HD_ij + vowel_class*yob_diff*BD_sex + vowel_class*yob_diff*BD_collar + (1|dyad), data = Blau.df[Blau.df$vowel_class %in% c("BAG", "BAN", "BAD", "BAT", "BACK", "BAC"),],  REML = F)

H2.diag[[9]] <- lmer(MVCD_absD ~ BD_town + HD_ij + vowel_class*yob_diff*BD_sex*BD_collar + (1|dyad), data = Blau.df[Blau.df$vowel_class %in% c("BAG", "BAN", "BAD", "BAT", "BACK", "BAC"),],  REML = F)

H2.diag.mod.names <- c("H2NULL", "H2M1D", "H2M2D", "H2M3D", "H2M4D", "H2M5D", "H2M6D", "H2M7D", "H2M8D")
names(H2.diag) <- H2.diag.mod.names 
H2.diag.AIC.tab <- aictab(cand.set = H2.diag, modnames = H2.diag.mod.names, sort=T)


H2.diag.inter <- as.data.frame(effect("vowel_class*yob_diff*BD_sex*BD_collar", H2.diag[[9]], KR=F))

H2.diag.N <- ddply(Blau.df[Blau.df$vowel_class %in% c("BAG", "BAN", "BAD", "BAT", "BACK", "BAC", "BIT", "BOUGHT"),], .(vowel_class, SEX_ij, collar_ij), summarize, N = length(vowel_class))
H2.diag.N$SEX_ABV_ij <- ""
H2.diag.N[H2.diag.N$SEX_ij == "Male-Male",]$SEX_ABV_ij <- "M"
H2.diag.N[H2.diag.N$SEX_ij == "Female-Female",]$SEX_ABV_ij <- "F"
H2.diag.N[H2.diag.N$SEX_ij == "Different",]$SEX_ABV_ij <- "D"
H2.diag.N$SEX_ABV_ij <- as.factor(H2.diag.N$SEX_ABV_ij)

H2.diag.min <- ddply(H2.diag.inter, .(vowel_class), summarize, min=min(lower))
H2.diag.N <- merge(H2.diag.N, H2.diag.min, by=c("vowel_class"))
H2.diag.N[H2.diag.N$SEX_ij == "Different",]$min <- H2.diag.N[H2.diag.N$SEX_ij == "Different",]$min + 0.1
H2.diag.N[H2.diag.N$SEX_ij == "Male-Male",]$min <- H2.diag.N[H2.diag.N$SEX_ij == "Male-Male",]$min - 0.1

H2.diag.N$town_N <- do.call(paste, list("n", "[", H2.diag.N$SEX_ABV_ij, "]", "==", as.character(H2.diag.N$N), sep=""))

H2.diag.inter$BD_collar <- revalue(H2.diag.inter$BD_collar, c("Same" = "Same (OCU)", "Different" = "Different (OCU)")) 
H2.diag.inter <- transform(H2.diag.inter, BD_sex=factor(BD_sex, c("Same", "Different")), BD_collar=factor(BD_collar, c("Same (OCU)", "Different (OCU)")), vowel_class=factor(vowel_class, levels=c("BAD", "BAT", "BAN", "BAG", "BACK", "BAC")))

ggplot(H2.diag.inter[H2.diag.inter$vowel_class != "BAC",], aes(x=yob_diff, y=fit, linetype=BD_sex, fill=BD_sex, color=BD_sex)) +
  geom_line() +
  geom_ribbon(aes(ymin=lower,ymax=upper), alpha=0.4, color=NA) +
  facet_grid(BD_collar~vowel_class) +
  ylab(expression("Predicted MVCD"[D])) +
  xlab(expression(abs(YoB[i] - YoB[j]))) +
  #scale_fill_grey() +
  scale_color_brewer(palette="Set2") +
  scale_fill_brewer(palette="Set2") +
  theme_bw(14) +
  guides(linetype=guide_legend(title=expression("SEX"[ij])), fill=guide_legend(title=expression("SEX"[ij])), color=guide_legend(title=expression("SEX"[ij]))) +
  theme(legend.position = "bottom")
ggsave("H2M7D_conditional_slopes_color.pdf", heigh=6, width=7)


##Extracting Coefficients##
H2.diag.coeffs <- as.data.frame(summary(H2.diag[[9]])$coefficients)
H2.diag.coeffs$upper <- H2.diag.coeffs$Estimate + H2.diag.coeffs$`Std. Error` * 1.96
H2.diag.coeffs$lower <- H2.diag.coeffs$Estimate - H2.diag.coeffs$`Std. Error` * 1.96
H2.diag.coeffs$Variable <- row.names(H2.diag.coeffs)

H2.diag.coeffs$Order <- as.factor(str_count(H2.diag.coeffs$Variable, ":"))
names(H2.diag.coeffs) <- c("Estimate", "SE", "t", "upper", "lower", "Variable", "Order")

write.csv(H2.diag.coeffs, "H2.diag.coeffs.csv", row.names = F, quote = F)
