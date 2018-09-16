library(dplyr)
library(igraph)
library(sna)
library(network)



vowels.df <- read.csv("/Users/mjfox/Google\ Drive/Research/SOC_Masters/Schools_network/vowels.df.csv", na.strings = c("undefined", "--undefined--"))
vowels.means.df <- read.csv("/Users/mjfox/Google\ Drive/Research/SOC_Masters/Schools_network/vowels.means.df.csv", na.strings = c("undefined", "--undefined--"))
Blau.df <- read.csv("/Users/mjfox/Google\ Drive/Research/SOC_Masters/Schools_network/Blau.df.csv", na.strings = c("undefined", "--undefined--"))

Blau.df <- Blau.df[Blau.df$vowel_class %in% c("BAG", "BACK", "BAD", "BAT", "BAN", "BAC", "BOT", "BUT") ,c("dyad", "speaker_i", "speaker_j", "vowel_class", "sex_i", "sex_j", "yob_i", "yob_j", "collar_i", "collar_j", "edu_i", "edu_j", "SCHD_i", "SCHD_j", "mean.F1.diff", "mean.F2.diff", "mean.diagonal.diff")]

####Network Data####
info <- read.csv("/Users/mjfox/Google\ Drive/Research/data_bases/speaker_info.csv", header=T,sep=",")
info.df <- info[info$Group %in% c("ECa0", "ECb0"), c("speaker", "ethnicity", "sex", "yob", "EDU", "F_EDU", "M_EDU",
                                                     "OCU", "SOC", "F_OCU", "M_OCU", "COLLAR", "F_COLLAR", 
                                                     "M_COLLAR","elem_school", "middle_school", "high_school", "all_schools", "SCH_district", "grad_high_school")]
info.df <- info.df[info.df$ethnicity != "Hmong",]
info.df <- droplevels(info.df)
row.names(info.df) <- NULL

two_mode_edges <- expandColumns(info.df[, c("speaker", "elem_school", "middle_school", "high_school")])
two_mode_edges <- table(two_mode_edges)
two_mode_edges <- as.matrix(two_mode_edges)

one_mode_proj <- tcrossprod(two_mode_edges)
diag(one_mode_proj) <- 0



####(Calculate HD_ij)####
two.mode.incidence <- as.matrix(as.data.frame.matrix(two_mode_edges))
ec.names <- c(rownames(two.mode.incidence), colnames(two.mode.incidence))

ec1.dist <- sedist(two.mode.incidence, method="hamming", mode="graph")

dat <- as.sociomatrix.sna(two.mode.incidence, simplify = TRUE)
n <- dim(dat)[2]
m <- 1
d <- array(dim = c(n, n))
d[] <- dat
k=1

ec1 <- array(dim = c(n, n))
for (i in 1:n){
  for (j in 1:n){
    ec1[i, j] <- sum(abs(d[, i] - d[, j]), na.rm = TRUE)
  }
}

rownames(ec1.dist) <- ec.names
colnames(ec1.dist) <- ec.names
ec2.dist <- ec1.dist[1:132, 1:132]

rownames(ec1) <- ec.names
colnames(ec1) <- ec.names
ec2 <- ec1[1:132, 1:132]



###Convert to Merge-able DF###
speakers <- rownames(ec2)
used_up <- c()
HD_ij<- c()
for(i in 1:nrow(ec2)){
  for(j in 1:nrow(ec2)){
    if(i != j & !speakers[j] %in% used_up){
      temp.frame <- cbind(paste(speakers[i], "_", speakers[j], collapse="", sep=""),
                          ec2[i,j], ec1.dist[i,j])
      HD_ij <- rbind(HD_ij, temp.frame)
      used_up <- c(used_up, speakers[i])
    }
  }
}
HD_ij <- as.data.frame(HD_ij)
names(HD_ij) <- c("dyad", "HD_ij", "HD_ij.dist")

####Vowel Data####

####(Load and Clean Data)####
ECa <- read.csv("/Users/mjfox/Google\ Drive/Praat/ECab0_remeasure/singles_ECab0_remeasure.csv",header=T,sep=",", na.strings = c("undefined", "--undefined--"))
names(ECa)[names(ECa) == "mode"] <- 'method'
ECa <- ECa[ECa$group %in% c("ECa", "ECb", "ECb0"),]
ECa$group <- revalue(ECa$group, c(ECb0 = "ECb"))
ECa$speaker <- droplevels(ECa$speaker)

ECa$phone <- as.character("")

ECa[ECa$vowel_class %in% c("BAG", "BACK", "BAT", "BANG", "BAD", "BAN", "BAC"),]$phone <- "AE1"
ECa[ECa$vowel_class == "BOT",]$phone <- "AA1"
ECa[ECa$vowel_class == "BOUGHT",]$phone <- "AO1"
ECa[ECa$vowel_class == "BAIT",]$phone <- "EY1"
ECa[ECa$vowel_class == "BEET",]$phone <- "IY1"
ECa[ECa$vowel_class == "BET",]$phone <- "EH1"
ECa[ECa$vowel_class %in%  c("ABOUT", "BOUT"),]$phone <- "AW1"
ECa[ECa$vowel_class == "BIDE",]$phone <- "AY1"
ECa[ECa$vowel_class == "BIT",]$phone <- "IH1"
ECa[ECa$vowel_class == "BOAT",]$phone <- "OW1"
ECa[ECa$vowel_class == "BOOK",]$phone <- "UH1"
ECa[ECa$vowel_class == "BOOT",]$phone <- "UW1"
ECa[ECa$vowel_class == "BOY",]$phone <- "OY1"
ECa[ECa$vowel_class == "BUT",]$phone <- "AH1"
ECa$word <- as.factor(tolower(as.character(ECa$word)))
ECa$phone_p <- as.factor(toupper(as.character(ECa$phone_p)))
ECa$phone_f <- as.factor(toupper(as.character(ECa$phone_f)))
ECa$phone <- as.factor(ECa$phone)
ECa <- droplevels(ECa)



ECb1 <- read.csv("/Users/mjfox/Google\ Drive/Praat/ECb0_2014_automeasure/singles_ECb0_2014.csv",header=T,sep=",", na.strings = c("undefined", "--undefined--"))
#ECb1$int_pk <- NULL
#ECb1$word_broken <- NULL
#ECb1$stress <- NULL
#ECb1$task.1 <- NULL
ECb1$sex <- as.factor(ECb1$sex)
ECb1$sex <- revalue(ECb1$sex, c(F = "Female", M = "Male"))
ECb1$ethnicity <- revalue(ECb1$ethnicity, c(W = "White", H = "Hmong"))
ECb1$method <- revalue(ECb1$method, c("automeasure" = "Automatic"))
ECb1$group <- revalue(ECb1$group, c(ECb0 = "ECb"))
ECb1$word <- as.factor(tolower(as.character(ECb1$word)))
ECb1$phone_p <- as.factor(toupper(as.character(ECb1$phone_p)))
ECb1$phone_f <- as.factor(toupper(as.character(ECb1$phone_f)))
ECb1$bad_f3 <- as.character("")
ECb1$bad_f3 <- "NO"
ECb1$bad_f3 <- as.factor(ECb1$bad_f3)
ECb1 <- droplevels(ECb1)

ECb2 <- read.csv("/Users/mjfox/Google\ Drive/Praat/ECb0_2016/singles_ECb0_2016.csv",header=T,sep=",", na.strings = c("undefined", "--undefined--"))
names(ECb2)[names(ECb2) == "mode"] <- 'method'
ECb2$method <- revalue(ECb2$method, c("Automatic Measurement" = "Automatic"))

vowels <- c("BAT", "BAG", "BAD", "BAT", "BANG", "BAN", "BACK", "BEET", "BIT", "BAIT", "BET", "BOT", "BOUGHT", "BUT", "BOAT", "BOOT", "BOOK")

skip <- tolower(c("A", "AN", "AND", "ALL", "AT", "BECAUSE", "CALL", "BUT", "COULD", "COULDN'T", 
                  "DID", "DIDN'T", "DON'T", "FOR", "GET", "HAD", "HAS", "HAVE", "HE", "HE'S", "HOW",
                  "I", "I'M", "I'VE", "IN", "ITS", "INTO", "KNOW", "LET'S", "MY", "OF", "ON", "ONTO", 
                  "NOT", "OUT", "SHE", "SHE'S", "THEY", "THIS", "US", "UP", "WHO",
                  "NEARBY", "WATCH", "WHILE", "WAITING", "TO", "THERE", "AROUND", 
                  "ELECTRIC", "AWAY", "WINTER", "WANTED", "BEFORE", "ONE", "IT", "WHEN", 
                  "REALLY", "ALREADY", "RINSE", "WORKING", "WE", "WITH", "THE", "DELEGATE", 
                  "RESPONSIBILITY", "YES", "IS", "YOU", "YOUR", "HARD", "OUR", "AFFILIATE", 
                  "POLITICAL", "VERY", "PLAYER", "SLEEPING", "ALIEN", "POLICE", "QUITE", 
                  "APPROPRIATE", "THROW", "CLEANS", "YOU'LL", "LONDON", "MILWAUKEE", "WAY", "RECORD", 
                  "UMBRELLA", "SORRY", "ENTIRE", "CAR", "WAS", "UW", "SLEEP", "HORSE", "PRESENT", 
                  "START", "POURED", "LIKE", "DOLL", "SCARF", "LOGIC", "PROFIT", "PROJECT", "HARD", "GOSH"))

ECa <- ECa[!ECa$word %in% skip,]
ECb1 <- ECb1[!ECb1$word %in% skip,]
ECb2 <- ECb2[!ECb2$word %in% skip,]

names(ECa)[names(ECa) %in% c("time_finish", "f1_finish", "b1_finish", "amp1_finish", "f2_finish", "b2_finish", "amp2_finish", "f3_finish", "b3_finish", "amp3_finish")] <- c("time_100", "f1_100", "b1_100", "amp1_100", "f2_100", "b2_100", "amp2_100", "f3_100", "b3_100", "amp3_100")
names(ECb2)[names(ECb2) %in% c("max_formants")] <- c("max_freq")

ECab <- rbind.fill(ECa, ECb1, ECb2)
ECab$yob <- NULL
ECab$task.1 <- NULL

ECab$version <- as.factor(as.character(ECab$version))
ECab$method <- as.factor(as.character(ECab$method))
ECab$task_ver <- as.factor(as.character(ECab$task_ver))
ECab$date <- as.factor(as.character(ECab$date))
ECab$filename <- as.factor(as.character(ECab$filename))
ECab$group <- as.factor(as.character(ECab$group))
ECab$speaker <- as.factor(as.character(ECab$speaker))
ECab$sex <- as.factor(as.character(ECab$sex))
ECab$ethnicity <- as.factor(as.character(ECab$ethnicity))
ECab$word <- as.factor(as.character(tolower(ECab$word)))
ECab$phone <- as.factor(as.character(ECab$phone))
ECab$vowel_class <- as.factor(as.character(ECab$vowel_class))
ECab$phone_p <- as.factor(as.character(ECab$phone_p))
ECab$phone_f <- as.factor(as.character(ECab$phone_f))
ECab$place_p <- as.factor(as.character(ECab$place_p))
ECab$manner_p <- as.factor(as.character(ECab$manner_p))
ECab$voice_p <- as.factor(as.character(ECab$voice_p))
ECab$place_f <- as.factor(as.character(ECab$place_f))
ECab$manner_f <- as.factor(as.character(ECab$manner_f))
ECab$voice_f <- as.factor(as.character(ECab$voice_f))
ECab$HNR <- as.numeric(ECab$HNR)
ECab$jitter <- as.numeric(ECab$jitter)
ECab$shimmer <- as.numeric(ECab$shimmer)
ECab$bad_f3 <- as.factor(as.character(ECab$bad_f3))
ECab$speaker <- as.factor(as.character(ECab$speaker))

ECab$stress <- str_sub(ECab$phone, 3,3)
ECab$phone <- str_sub(ECab$phone, 1,2)
ECab$stress <- as.factor(as.character(ECab$stress))
ECab$phone <- as.factor(as.character(ECab$phone))

##Word level corrections##
#ECab[ECab$word == "laugh" & ECab$vowel_class == "BAG",]$vowel_class <- "BAC"
#ECab[ECab$word == "band" & ECab$vowel_class == "BAT",]$vowel_class <- "BAN"
#ECab[ECab$word == "pan" & ECab$vowel_class == "BAT",]$vowel_class <- "BAN"
#ECab[ECab$word == "pant" & ECab$vowel_class == "BAT",]$vowel_class <- "BAN"
#ECab[ECab$word == "last" & ECab$vowel_class == "BAT",]$vowel_class <- "BAC"


ECab[ECab$word %in% c("bog", "cog", "fog", "hog", "jog", "log", "dog", "awful", "chalk", "coffee", "law", "dogs", "law", "caught", "lost", "cost") & ECab$vowel_class == "BOT",]$vowel_class <- "BOUGHT"
ECab[ECab$word %in% c("sot", "got") & ECab$vowel_class == "BOUGHT",]$vowel_class <- "BOT"
ECab[ECab$word %in% c("bog", "cog", "fog", "hog", "jog", "log", "dog", "awful", "chalk", "coffee", "law", "dogs", "law", "caught", "lost", "cost") & ECab$phone == "AA",]$phone <- "AO"
ECab[ECab$word %in% c("sot", "got") & ECab$phone == "AO",]$phone <- "AA"

ECab$place_f <- as.character(ECab$place_f)
ECab[ECab$word == "father",]$place_f <- "dental" 
ECab[ECab$word == "moth",]$place_f <- "dental"
ECab[ECab$word %in%  c("awful", "coffee", "off"),]$place_f <- "labio-dental"
ECab[ECab$word == "forgot",]$place_f <- "alveolar"
ECab$place_f <- as.factor(ECab$place_f)

ECab$place_p <- as.character(ECab$place_p)
ECab[ECab$word %in% c("la", "law", "rock"),]$place_p <- "lateral"
ECab[ECab$word %in% c("father", "faucet", "fawned", "fog", "fond", "forget", "fossil"),]$place_p <- "labio-dental"
ECab[ECab$word %in% c("thought"),]$place_p <- "dental"
ECab[ECab$word %in% c("forgot"),]$place_p <- "velar"
ECab$place_p <- as.factor(ECab$place_p)

ECab[ECab$word == "toms",]$word <- "tom's"

#ECab <- ECab[-42387,]

ECab <- ECab[!ECab$vowel_class %in% c("BOUT", "ABOUT", "BIDE", "BANG", "BOY"),]
ECab <- droplevels(ECab)
ECab$vowel_class <- as.factor(ECab$vowel_class)


vowel_means <- ddply(ECab[ECab$method == "Manual",], .(sex, vowel_class), summarize, F1.mean = mean(f1_50), F1.sd = sd(f1_50), F2.mean = mean(f2_50), F2.sd = sd(f2_50))
vowel_means$F1.upper <- vowel_means$F1.mean + (2*vowel_means$F1.sd)
vowel_means$F1.lower <- vowel_means$F1.mean - (2*vowel_means$F1.sd)
vowel_means$F2.upper <- vowel_means$F2.mean + (2*vowel_means$F2.sd)
vowel_means$F2.lower <- vowel_means$F2.mean - (2*vowel_means$F2.sd)

vowel_means$vowel_class <- factor(vowel_means$vowel_class, levels=levels(ECab$vowel_class))
vowel_means$sex <- droplevels(vowel_means$sex)

##Mark outliers from Model of Handmeasured Tokens##
ECab$outlier.F1 <- as.character("")
ECab$outlier.F2 <- as.character("")

ECab$vowel_class <- as.factor(droplevels(ECab$vowel_class))
vowel_means <- droplevels(vowel_means)


for(vowel in 1:nrow(vowel_means)){
  ECab[ECab$sex == vowel_means$sex[vowel] & ECab$vowel_class == vowel_means$vowel_class[vowel] & (ECab$f1_50 <= vowel_means$F1.upper[vowel] & ECab$f1_50 >= vowel_means$F1.lower[vowel]),]$outlier.F1 <- "INSIDE"
  ECab[ECab$sex == vowel_means$sex[vowel] & ECab$vowel_class == vowel_means$vowel_class[vowel] & (ECab$f1_50  > vowel_means$F1.upper[vowel] | ECab$f1_50  < vowel_means$F1.lower[vowel]),]$outlier.F1 <- "OUTSIDE"
  
  ECab[ECab$sex == vowel_means$sex[vowel] & ECab$vowel_class == vowel_means$vowel_class[vowel] & (ECab$f2_50 <= vowel_means$F2.upper[vowel] & ECab$f2_50 >= vowel_means$F2.lower[vowel]),]$outlier.F2 <- "INSIDE"
  ECab[ECab$sex == vowel_means$sex[vowel] & ECab$vowel_class == vowel_means$vowel_class[vowel] & (ECab$f2_50  > vowel_means$F2.upper[vowel] | ECab$f2_50  < vowel_means$F2.lower[vowel]),]$outlier.F2 <- "OUTSIDE"
}

ECab[ECab$method == "Manual",]$outlier.F1 <- "INSIDE"
ECab[ECab$method == "Manual",]$outlier.F2 <- "INSIDE"

ECab$outlier.F1 <- as.factor(droplevels(as.factor(ECab$outlier.F1)))
ECab$outlier.F2 <- as.factor(droplevels(as.factor(ECab$outlier.F2)))

vowels.df <- ECab[ECab$mahalanobis < 5 & (ECab$outlier.F1 == "INSIDE" & ECab$outlier.F2 == "INSIDE"),]
vowels.df <- vowels.df[!vowels.df$phone %in% c("AW", "AY", "OY"),]
vowels.df <- lobanov.norm(vowels.df)
vowels.df$yob <- NULL
vowels.df <- auto.code(vowels.df, type="speaker", variables=c("EDU", "COLLAR", "SCH_district", "yob"))
#vowels.df <- merge(vowels.df, info.df[,c("speaker", "QuaBiMo", "nestedness")], by.x="speaker", by.y="speaker")
vowels.df$SCH_district <- revalue(vowels.df$SCH_district, c(Regis = "Eau Claire"))
vowels.df$EDU <- revalue(vowels.df$EDU, c(GED = "HSG", EdS = "PHD"))

vowels.df[vowels.df$word == "jaw",]$phone_f <- "none"
vowels.df[vowels.df$word == "jaw",]$place_f <- "none"
vowels.df[vowels.df$word == "jaw",]$manner_f <- "none"
vowels.df[vowels.df$word == "jaw",]$voice_f <- "none"

vowels.df$phone <- droplevels(vowels.df$phone)
vowels.df$phone_p <- droplevels(vowels.df$phone_p)
vowels.df$phone_f <- droplevels(vowels.df$phone_f)
vowels.df$place_p <- droplevels(vowels.df$place_p)
vowels.df$place_f <- droplevels(vowels.df$place_f)
vowels.df$vowel_class <- droplevels(vowels.df$vowel_class)
