library(ggplot2)
library(plyr)

####(Formant Space Analysis)####
label_points <- ddply(.data=vowels.df[vowels.df$vowel_class %in% c("BAD", "BAT", "BAG", "BACK", "BAN", "BOT") & vowels.df$yob >= 1990,], .(vowel_class), summarize, diagonal.m = mean(diagonal.50.norm))
label_points[label_points$vowel_class %in% c("BAD", "BAT"),]$diagonal.m <- label_points[label_points$vowel_class %in% c("BAD", "BAT"),]$diagonal.m + c(0.11, 0.09)
label_points[label_points$vowel_class %in% c("BAG"),]$diagonal.m <- label_points[label_points$vowel_class %in% c("BAG"),]$diagonal.m + 0.05
label_points[label_points$vowel_class %in% c("BOT"),]$diagonal.m <- -0.53


BAT <- vowels.df[vowels.df$vowel_class %in% c("BAD", "BAT", "BAG", "BACK", "BAN"),]
BAT$COLLAR <- revalue(BAT$COLLAR, c("PRO" = "WCd"))
BAT <- transform(BAT, vowel_class=factor(vowel_class, levels=c("BAD", "BAT", "BAN", "BAG", "BACK")))
BAT$town_label <- str_sub(BAT$SCH_district, 1,1)


##Faceted by vowel_class##
ggplot(BAT, aes(yob, diagonal.50.norm, shape=sex, linetype=sex)) +
  #geom_point(alpha=0.4) +
  stat_summary(fun.data = "mean_se", alpha=0.3, aes(group=speaker)) +
  geom_smooth(method="lm", color="black") +
  facet_grid(SCH_district~vowel_class) +
  xlab("Year of Birth") +
  ylab("Z2 - Z1 (Diagonal)") +
  scale_x_continuous(breaks = c(1930, 1960, 1990)) +
  scale_linetype(name="Sex") +
  scale_shape(name="Sex") +
  guides(shape = guide_legend(override.aes = list(size=0.7))) +
  theme_bw(14) +
  theme(legend.position = "bottom")
ggsave(filename="AE_raw_yob_town_stat_sum.pdf", height=7, width=7.5)

ncs_lables <- data.frame(label=c("More-NCS", "Less-NCS", "More-NCS", "Less-NCS"), x=c(1935, 1935, 1935, 1935), y=c(1.8, -1.8, 1.8, -1.8), vowel_class=factor(c("BAD", "BAD", "BAG", "BAG"), levels=c("BAD", "BAT", "BAN", "BAG", "BACK")))
ggplot(BAT, aes(yob, diagonal.50.norm, linetype=SCH_district, shape=SCH_district, color=SCH_district, fill=SCH_district)) +
  #geom_point(alpha=0.4) +
  stat_summary(fun.data = "mean_se", alpha=0.3, aes(group=speaker)) +
  geom_smooth(method="lm") +
  geom_text(data=ncs_lables, aes(x=x, y=y, label=label), inherit.aes = F, fontface=2, nudge_x=6) +
  facet_wrap(~vowel_class) +
  xlab("Year of Birth") +
  ylab("Vowel Position (Diagonal)") +
  scale_x_continuous(breaks = c(1930, 1960, 1990)) +
  scale_linetype(name="Town") +
  scale_shape(name="Town") +
  scale_color_discrete(name="Town") +
  scale_fill_discrete(name="Town") +
  guides(shape = guide_legend(override.aes = list(size=0.7))) +
  theme_bw(14) +
  theme(legend.position = c(1,0), legend.justification = c(1,0))
ggsave(filename = "AE_yob_town_color_summary.pdf", height=6, width=7)

ggplot(BAT[BAT$vowel_class %in% c("BAD", "BAG", "BAN"),], aes(yob, diagonal.50.norm, linetype=SCH_district, shape=SCH_district)) +
  #geom_point(alpha=0.4) +
  stat_summary(fun.data = "mean_se", alpha=0.3, aes(group=speaker)) +
  geom_smooth(method="lm", color="black") +
  facet_grid(sex~vowel_class) +
  xlab("Year of Birth") +
  ylab("Z2 - Z1 (Diagonal)") +
  scale_x_continuous(breaks = c(1930, 1960, 1990)) +
  scale_linetype(name="Town") +
  scale_shape(name="Town") +
  guides(shape = guide_legend(override.aes = list(size=0.7))) +
  theme_bw(14) +
  theme(legend.position = "bottom")
ggsave(filename = "BAD_BAG_SEX_yob_town_summary.pdf", height=5, width=7)

BOT_BUT <- vowels.df[vowels.df$vowel_class %in% c("BOT", "BUT"),]


ggplot(BOT_BUT, aes(yob, F2.50.norm, shape=sex, linetype=sex)) +
  #geom_point(alpha=0.4) +
  stat_summary(fun.data = "mean_se", alpha=0.3, aes(group=speaker)) +
  geom_smooth(method="lm", color="black") +
  facet_grid(SCH_district~vowel_class) +
  xlab("Year of Birth") +
  ylab(expression("Z2")) +
  scale_x_continuous(breaks = c(1930, 1960, 1990)) +
  scale_linetype(name="Sex") +
  scale_shape(name="Sex") +
  guides(shape = guide_legend(override.aes = list(size=0.7))) +
  theme_bw(14) +
  theme(legend.position = "bottom")
ggsave(filename = "BOT_BUT_raw_yob_town_sum_stat.pdf", height=7, width=7)

##BOT and BUT by TOWN##
ncs_lables <- data.frame(label=c("More-NCS", "Less-NCS", "Less-NCS", "More-NCS"), x=c(1935, 1935, 1935, 1935), y=c(-0.15, -0.75, -0.15, -0.75), vowel_class=factor(c("BOT", "BOT", "BUT", "BUT"), levels=c("BOT", "BUT")))

ggplot(BOT_BUT, aes(yob, F2.50.norm, shape=SCH_district, linetype=SCH_district, color=SCH_district, fill=SCH_district)) +
  #geom_point(alpha=0.4) +
  stat_summary(fun.data = "mean_se", alpha=0.3, aes(group=speaker)) +
  geom_smooth(method="lm") +
  geom_text(data=ncs_lables, aes(x=x, y=y, label=label), inherit.aes = F, fontface=2, nudge_x=6) +
  facet_grid(.~vowel_class) +
  xlab("Year of Birth") +
  ylab("Vowel Position (Z2)") +
  scale_x_continuous(breaks = c(1930, 1960, 1990)) +
  scale_linetype(name="Town") +
  scale_shape(name="Town") +
  scale_color_discrete(name="Town") +
  scale_fill_discrete(name="Town") +
  guides(shape = guide_legend(override.aes = list(size=0.7))) +
  theme_bw(14) +
  theme(legend.position = "bottom")
ggsave(filename = "AA_AH1_yob_town_color_summary.pdf", height=6, width=7)

##Simple NCS retraction plots##


##Summarize points##
BAT.p <- ggplot(data=BAT, aes(yob, diagonal.50.norm, linetype=vowel_class, fill=vowel_class, color=vowel_class)) +
  #geom_jitter(alpha=0.2) +
  geom_smooth(method="lm", color="black") +
  stat_summary(fun.data = "mean_se", alpha=0.3) +
  geom_text(data=label_points[label_points$vowel_class != "BOT",], aes(label = vowel_class, x = Inf, y = diagonal.m), hjust = -.1, color="black", inherit.aes = F) +
  annotate(geom="text", label=c("More-NCS", "Less-NCS"), x=c(1935,1935), y=c(1.8, -1.8), fontface=2) +
  ylab("Vowel Position (Diagonal)") +
  theme_bw(14) +
  scale_fill_grey() +
  scale_color_grey() +
  scale_x_continuous(breaks = c(1930, 1940, 1950, 1960, 1970, 1980, 1990)) +
  scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
  theme(legend.position = "none", plot.margin = unit(c(1,3,1,1), "lines"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
BAT.gt <- ggplotGrob(BAT.p)
BAT.gt$layout$clip[BAT.gt$layout$name == "panel"] <- "off"
grid.draw(BAT.gt)
ggsave(filename="AE_raw_yob.pdf", plot=BAT.gt, height=5, width=6)


##BOT and BUT by just YoB##
BOT.p <- ggplot(BOT_BUT[BOT_BUT$vowel_class == "BOT",], aes(yob, F2.50.norm, fill=vowel_class, linetype=vowel_class)) +
  geom_smooth(method="lm", color="black") +
  stat_summary(fun.data = "mean_se", alpha=0.3, aes(group=speaker)) +
  geom_text(data=label_points[label_points$vowel_class == "BOT",], aes(label = vowel_class, x = Inf, y = diagonal.m), hjust = -.1, color="black") +
  annotate(geom="text", label=c("Less-NCS", "More-NCS"), x=c(1935,1935), y=c(-0.7, -0.2), fontface=2) +
  xlab("Year of Birth") +
  ylab("Vowel Position (Z2)") +
  theme_bw(14) +
  scale_color_grey() +
  scale_fill_grey() +
  scale_x_continuous(breaks = c(1930, 1940, 1950, 1960, 1970, 1980, 1990)) +
  theme(legend.position = "none", plot.margin = unit(c(1,3,1,1), "lines"))
BOT.gt <- ggplotGrob(BOT.p)
BOT.gt$layout$clip[BOT.gt$layout$name == "panel"] <- "off"
grid.draw(BOT.gt)
ggsave(filename = "BOT_raw_yob.pdf", height=5, width=6)

arrange.plot(BAT.grid, BOT.grid, nrow=2, ncol=1)

BAT_BOT.grid <- grid.arrange(BAT.gt, BOT.gt)
ggsave(filename = "BAT_BOT_raw_yob.pdf", plot=BAT_BOT.grid, height=7, width=6)

##SEX and OCU. Apparent-time###
BAT.SEX <- BAT
BAT.SEX$top <- "Sex"
BAT.COLLAR <- BAT
BAT.COLLAR$top <- "Occupation"
BAT.double <- rbind(BAT.SEX, BAT.COLLAR)

##SEX TOP HALF##
BAT.SEX <- ggplot(BAT[BAT$vowel_class %in% c("BAD", "BAT"),], aes(yob, diagonal.50.norm, linetype=sex, shape=sex, color=sex, fill=sex)) + 
  stat_summary(fun.data = "mean_se", alpha=0.2, aes(group=speaker)) +
  geom_smooth(method="lm") +
  geom_text(data=ncs_lables[ncs_lables$vowel_class %in% c("BAD", "BAT"),], aes(x=x, y=y, label=label), inherit.aes = F, fontface=2, nudge_x=6) +
  facet_grid(.~vowel_class) +
  xlab("Year of Birth") +
  theme_bw(12) +
  scale_linetype(name="Sex") +
  scale_shape(name="Sex") +
  scale_fill_brewer(palette="Set2", name="Sex") +
  scale_color_brewer(palette="Set2", name="Sex") +
  scale_x_continuous(breaks = c(1930, 1945, 1960, 1975, 1990)) +
  theme(legend.position= c(1,1), 
        legend.justification = c(1,1),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank())

##SEX BOTTOM HALF##

BAT.OCU <- ggplot(BAT[BAT$vowel_class %in% c("BAD", "BAT"),], aes(yob, diagonal.50.norm, linetype=COLLAR, shape=COLLAR, color=COLLAR, fill=COLLAR)) + 
  stat_summary(fun.data = "mean_se", alpha=0.2, aes(group=speaker)) +
  geom_smooth(method="lm") +
  geom_text(data=ncs_lables[ncs_lables$vowel_class %in% c("BAD", "BAT"),], aes(x=x, y=y, label=label), inherit.aes = F, fontface=2, nudge_x=6) +
  facet_grid(.~vowel_class) +
  xlab("Year of Birth") +
  theme_bw(12) +
  scale_linetype(name="Occu.") +
  scale_shape(name="Occu.") +
  scale_fill_discrete(name="Occu.") +
  scale_color_discrete(name="Occu.") +
  scale_x_continuous(breaks = c(1930, 1945, 1960, 1975, 1990)) +
  theme(legend.position= c(1,1), 
        legend.justification = c(1,1), 
        legend.direction = "horizontal",
        legend.background = element_blank(),
        strip.text.x = element_blank(),
        axis.title.y = element_blank())

BAT_SEX_OCU.grid <- grid.arrange(BAT.SEX, BAT.OCU, left="Vowel Position (Diagonal)")
ggsave(filename = "BAT_SEX_OCU_raw_yob.pdf", plot=BAT_SEX_OCU.grid, height=6, width=6)
