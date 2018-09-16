####(Blau-Space Analysis)####
vowel.means.df <- ddply(.data=droplevels(vowels.df[vowels.df$ethnicity != "Hmong",]), .(speaker, vowel_class, sex, yob, COLLAR, EDU, SCH_district), 
                        summarize, meanF1.norm = mean(F1.50.norm), meanF2.norm = mean(F2.50.norm), mean.diagonal = mean(diagonal.50.norm))

vowel.means.df.new <- droplevels(vowel.means.df[vowel.means.df$vowel_class %in% c("BAG", "BACK", "BAD", "BAT", "BAN", "BAC", "BOT", "BUT"),])

##Dyad Method##
speakers <- levels(vowel.means.df$speaker)
vowels <- levels(vowel.means.df$vowel_class)
Blau.df <- data.frame(row.names = NULL)
used_up <- c()


for(i in 1:length(speakers)){
  for(j in 1:length(speakers)){
    if(i != j & !speakers[j] %in% used_up){
      i.df <- vowel.means.df[vowel.means.df$speaker == speakers[i],]
      j.df <- vowel.means.df[vowel.means.df$speaker == speakers[j],]
      HD_ij <- ec2[i,j]
      temp.frame <- cbind(speakers[i], speakers[j], 
                          paste(speakers[i], "_", speakers[j], collapse="", sep=""),
                          vowels, 
                          HD_ij,
                          as.character(i.df$sex[1]), as.character(j.df$sex[1]),
                          as.character(i.df$yob[1]), as.character(j.df$yob[1]), 
                          as.character(i.df$COLLAR[1]), as.character(j.df$COLLAR[1]),
                          as.character(i.df$EDU[1]), as.character(j.df$EDU[1]),
                          as.character(i.df$SCH_district[1]), as.character(j.df$SCH_district[1]),
                          (i.df$meanF1.norm - j.df$meanF1.norm),
                          (i.df$meanF2.norm - j.df$meanF2.norm),
                          (i.df$mean.diagonal - j.df$mean.diagonal))
      Blau.df <- rbind(Blau.df, temp.frame)
      used_up <- c(used_up, speakers[i])
    }
  }
}

names(Blau.df) <- c("speaker_i", "speaker_j", "dyad", "vowel_class", "HD_ij", "sex_i", "sex_j", "yob_i", "yob_j", "collar_i", "collar_j", "edu_i", "edu_j", "SCHD_i", "SCHD_j", "mean.F1.diff", "mean.F2.diff", "mean.diagonal.diff")
Blau.df$yob_i <- as.numeric(as.character(Blau.df$yob_i))
Blau.df$yob_j <- as.numeric(as.character(Blau.df$yob_j))
Blau.df$mean.F1.diff <- as.numeric(as.character(Blau.df$mean.F1.diff))
Blau.df$mean.F2.diff <- as.numeric(as.character(Blau.df$mean.F2.diff))
Blau.df$mean.diagonal.diff <- as.numeric(as.character(Blau.df$mean.diagonal.diff))
Blau.df$HD_ij <- as.numeric(as.character(Blau.df$HD_ij))


####(Variable Operationalization)####
Blau.df$MVCD_absD <- abs(Blau.df$mean.diagonal.diff)
Blau.df$MVCD_absF2 <- abs(Blau.df$mean.F2.diff)
Blau.df$yob_diff <- abs(Blau.df$yob_i - Blau.df$yob_j)

Blau.df$collar_i <- revalue(Blau.df$collar_i, c("PRO" = "WCd"))
Blau.df$collar_j <- revalue(Blau.df$collar_j, c("PRO" = "WCd"))
Blau.df$collar_ij <- interaction(Blau.df$collar_i, Blau.df$collar_j)
Blau.df$collar_ij <- revalue(Blau.df$collar_ij, c("WCnd.WCd" = "WCd.WCnd", "BC.WCd" = "WCd.BC", "BC.WCnd" = "WCnd.BC"))
Blau.df$collar_ij <- revalue(Blau.df$collar_ij, c("BC.BC" = "BC-BC", "WCd.WCd" = "WCd-WCd", "WCnd.WCnd" = "WCnd-WCnd", "WCd.BC" = "WCd-BC", "WCd.WCnd" = "WCd-WCnd", "WCnd.BC" = "WCnd-BC"))


Blau.df$edu_i <- revalue(Blau.df$edu_i, c("MS" = "PB", "PHD" = "PB", "MBA" = "PB", "AD2" = "SC"))
Blau.df$edu_j <- revalue(Blau.df$edu_j, c("MS" = "PB", "PHD" = "PB", "MBA" = "PB", "AD2" = "SC"))
Blau.df$EDU_ij <- interaction(Blau.df$edu_i, Blau.df$edu_j)
Blau.df$EDU_ij <- revalue(Blau.df$EDU_ij, c("SC.HSG" = "HSG.SC", "SC.PB" = "PB.SC", "SC.BD4" = "BD4.SC", "HSG.PB" = "PB.HSG", "HSG.BD4" = "BD4.HSG", "BD4.PB" = "PB.BD4"))

Blau.df$SCH_ij <- interaction(Blau.df$SCHD_i, Blau.df$SCHD_j)
Blau.df$SCH_ij <- revalue(Blau.df$SCH_ij, c("Chippewa Falls.Eau Claire"="Eau Claire.Chippewa Falls", "Altoona.Chippewa Falls"="Chippewa Falls.Altoona", "Altoona.Eau Claire"="Eau Claire.Altoona"))
Blau.df$SCH_ij <- revalue(Blau.df$SCH_ij, c("Altoona.Altoona" = "AL-AL", 
                                            "Chippewa Falls.Chippewa Falls" = "CF-CF", 
                                            "Eau Claire.Eau Claire" = "EC-EC",
                                            "Eau Claire.Altoona" = "EC-AL",
                                            "Eau Claire.Chippewa Falls" = "EC-CF",
                                            "Chippewa Falls.Altoona" = "CF-AL"))

Blau.df$SEX_ij <- interaction(Blau.df$sex_i, Blau.df$sex_j)
Blau.df$SEX_ij <- revalue(Blau.df$SEX_ij, c("Male.Female" = "Different", "Female.Male" = "Different", "Male.Male" = "Male-Male", "Female.Female" = "Female-Female"))

# 1-4 - Operationalization
Blau.df$edu_num_i <- as.numeric(revalue(Blau.df$edu_i, c("HSG" = "1", "SC" = "2", "BD4" = "3", "PB" = "4")))
Blau.df$edu_num_j <- as.numeric(revalue(Blau.df$edu_j, c("HSG" = "1", "SC" = "2", "BD4" = "3", "PB" = "4")))
Blau.df$edu_num_ij <- abs(Blau.df$edu_num_i - Blau.df$edu_num_j)

Blau.df$collar_num_i <- as.numeric(revalue(Blau.df$collar_i, c("BC" = "1", "WCnd" = "2", "WCd" = "3")))
Blau.df$collar_num_j <- as.numeric(revalue(Blau.df$collar_j, c("BC" = "1", "WCnd" = "2", "WCd" = "3")))
Blau.df$collar_num_ij <- abs(Blau.df$collar_num_i - Blau.df$collar_num_j)

Blau.df$BD_num_ij <- abs()

# 0/1 - Operationalization
Blau.df$BD_sex <- as.factor(Blau.df$sex_i == Blau.df$sex_j)
Blau.df$BD_collar <- as.factor(Blau.df$collar_i == Blau.df$collar_j)
Blau.df$BD_edu <- as.factor(Blau.df$edu_i == Blau.df$edu_j)
Blau.df$BD_town <- as.factor(Blau.df$SCHD_i == Blau.df$SCHD_j)

Blau.df$BD_sex <- revalue(Blau.df$BD_sex, c("TRUE" = "Same", "FALSE" = "Different"))
Blau.df$BD_collar <- revalue(Blau.df$BD_collar, c("TRUE" = "Same", "FALSE" = "Different"))
Blau.df$BD_town <- revalue(Blau.df$BD_town, c("TRUE" = "Same", "FALSE" = "Different"))

Blau.df <- merge(Blau.df, HD_ij, by="dyad")
Blau.df$HD_ij <- as.numeric(as.character(Blau.df$HD_ij))
Blau.df$HD_ij.dist <- as.numeric(as.character(Blau.df$HD_ij.dist))


town.N <- ddply(Blau.df[Blau.df$vowel_class %in% c("BAG", "BAN", "BACK", "BAD", "BAT", "BOT", "BOUGHT", "BET", "BUT", "BIT"),], .(SCH_ij), summarise, N = length(SCH_ij)/10)

Blau.df$HD_ij_binned <- cut(Blau.df$HD_ij, breaks=7, labels=c("0-2", "2-4", "4-6", "6-8", "8-10", "10-12", "12-14"))
Blau.df$city_internal <- revalue(Blau.df$SCH_ij, c("AL-AL" = "Altoona", "CF-CF" = "Chippewa Falls", "EC-EC" = "Eau Claire"))

##Plotting Dyad Data within Town##
ggplot(transform(Blau.df[Blau.df$vowel_class %in% c("BAD", "BACK", "BAT", "BAG", "BAN") & Blau.df$city_internal %in% c("Altoona", "Eau Claire", "Chippewa Falls"),], vowel_class=factor(vowel_class, c("BAD", "BAT", "BAG", "BACK", "BAN"))), aes(x=yob_diff, y=MVCD_absD, color=HD_ij_binned, fill=HD_ij_binned, linetype=HD_ij_binned)) +
  geom_density_2d(aes(x=yob_diff, y=MVCD_absD), inherit.aes = F, color="grey", alpha=0.4) +
  geom_smooth(method="lm") +
  scale_fill_grey() +
  scale_color_grey() +
  xlab(expression(abs(YoB[i] - YoB[j]))) +
  ylab(expression(MVCD[D])) +
  facet_grid(city_internal~vowel_class) +
  guides(fill=guide_legend(title=expression("HD"[ij])), linetype=guide_legend(title=expression("HD"[ij])), color=guide_legend(title=expression("HD"[ij]))) +
  theme_bw(14)
ggsave("City_VC_dyads_raw.pdf", height=6, width=7)


ggplot(Blau.df[Blau.df$vowel_class %in% c("BAG", "BAN", "BAD", "BAT", "BACK", "BAC") & Blau.df$SCH_ij %in% c("Altoona", "Eau Claire", "Chippewa Falls"),], aes(x=yob_diff, y=MVCD_absD, z=HD_ij)) +
  geom_density_2d(aes(fill=..level..), geom="polygon", contour=T) +
  facet_grid(SCH_ij~vowel_class)

ggplot(Blau.df[Blau.df$vowel_class %in% c("BAG", "BAN", "BAD", "BAT", "BACK", "BAC") & Blau.df$SCH_ij %in% c("AL-AL", "EC-EC", "CF-CF"),], aes(x=yob_diff, y=MVCD_absD, z=HD_ij)) +
  geom_contour(aes(fill=..level..)) +
  facet_grid(SCH_ij~vowel_class)
