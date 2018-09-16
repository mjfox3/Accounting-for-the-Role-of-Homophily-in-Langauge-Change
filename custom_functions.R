# Custom functions written by Michael Joseph Fox
# Copyright the Author 2/4/2013

require(lme4)
require(AICcmodavg)
require(plyr)
require(languageR)
require(ggplot2)
require(gss)
require(plyr)
require(stringr)


computeMerger <- function(df, formants=c(), vowels=c(), vw_class="vowel_class", contexts="phone_f"){
  require(plyr)
  stopifnot(is.data.frame(df) == TRUE)
  stopifnot(length(formants) == 2)
  stopifnot(length(vowels) == 2)
  
  df <- df[df[,vw_class] %in% vowels,]
  df <- droplevels(df)
  
  spk <- levels(df$speaker)
  output.frame <- data.frame(speaker = spk, pillai=0, pillai.p_value=0, euc_dist=0, row.names = NULL)

  for(c in 1:length(spk)){
    s.sub <- df[df$speaker == spk[c],]
    s.sub <- droplevels(s.sub)
    
    deps <- cbind(s.sub[,formants[1]], s.sub[,formants[2]])
    type <- factor(s.sub[,vw_class])
    phon <- factor(s.sub[,contexts])
    fit <- manova(deps ~ type + phon)
    output.frame[output.frame$speaker == spk[c],]$pillai <- summary(fit)$stats[1,2]
    output.frame[output.frame$speaker == spk[c],]$pillai.p_value <- summary(fit)$stats[1,6]
    
    me1 <- apply(s.sub[s.sub[,vw_class] == vowels[1], formants], 2, mean)
    me2 <- apply(s.sub[s.sub[,vw_class] == vowels[2], formants], 2 ,mean)
    output.frame[output.frame$speaker == spk[c],]$euc_dist <- sqrt( (me1[1] - me2[1])^2 + (me1[2] - me2[2])^2 )  
  }
  return(output.frame)
}


calc_ellips <- function(df1, df2){
  require(spatstat)
  df1 <- droplevels(df1)
  df2 <- droplevels(df2)
  #dataEllipse(x$x, x$y, levels=0.05)
  vowel1 <- levels(df1$vowel_class)
  vowel2 <- levels(df2$vowel_class)
  df1 <- data.frame(x=df1$F2.50.norm, y=df1$F1.50.norm, row.names = NULL)
  df2 <- data.frame(x=df2$F2.50.norm, y=df2$F1.50.norm, row.names = NULL)
  me1 <- apply(df1, 2, mean)
  me2 <- apply(df2, 2, mean)
  v1 <- var(df1)
  v2 <- var(df2)
  rad1 <- sqrt(2*qf(0.5, 2, nrow(df1)-1))
  rad2 <- sqrt(2*qf(0.5, 2, nrow(df2)-1))
  z1 <- as.data.frame(car::ellipse(me1, v1, rad1, segments=1001, add=FALSE, draw=FALSE))
  z2 <- as.data.frame(car::ellipse(me2, v2, rad2, segments=1001, add=FALSE, draw=FALSE))
  e1 <- list(x=z1$x, y=z1$y)
  e2 <- list(x=z2$x, y=z2$y)
  dist2center1 <- sqrt(rowSums((t(t(z1)-me1))^2))
  dist2center2 <- sqrt(rowSums((t(t(z2)-me2))^2))
  area1 <- pi*min(dist2center1)*max(dist2center1)
  area2 <- pi*min(dist2center2)*max(dist2center2)
  overlap <- abs(overlap.xypolygon(e1,e2))
  
  out <- data.frame(row.names=NULL)
  out <- cbind(area1, area2, overlap)
  colnames(out) <- c(vowel1, vowel2, "overlap")
  return(out)
}


overlap_2 <- function(df1, df2){
  e1 <- list(x=df1$coordsSDE[,1], y=df1$coordsSDE[,2])
  e2 <- list(x=df2$coordsSDE[,1], y=df2$coordsSDE[,2])
  overlap <- abs(overlap.xypolygon(e1,e2))
  return(overlap)
}

euc_dist <- function(df, vowels=c(), formants=c(), type="mean to mean", speakers="single"){
  stopifnot(is.data.frame(df) == TRUE)
  stopifnot(length(formants) == 2)
  stopifnot(length(vowels) == 2)
  
  if(speakers == "single"){
    df1 <- df[df$vowel_class == vowels[1], formants]
    df2 <- df[df$vowel_class == vowels[2], formants]
    
    me1 <- apply(df1,2,mean)
    me2 <- apply(df2,2,mean)
    if(type == "mean to mean"){
      euc <- sqrt( (me1[1] - me2[1])^2 + (me1[2] - me2[2])^2 )
      return(euc)
    }
  }else if(speakers == "many"){
    output.frame <- data.frame(row.names = NULL)
    spks <- levels(df$speaker)
    for(c in 1:length(spks)){
      s.sub <- df[df$speaker == spks[c], ]
      df1 <- s.sub[s.sub$vowel_class == vowels[1],]
      df2 <- s.sub[s.sub$vowel_class == vowels[2],]
      mean.F1.df1 <- mean(df1[,formants[1]])
      mean.F2.df1 <- mean(df1[,formants[2]])
      mean.F1.df2 <- mean(df2[,formants[1]])
      mean.F2.df2 <- mean(df2[,formants[2]])
      df1$dist.to.other <- sqrt( (df1[,formants[1]] - mean.F1.df2)^2 + (df1[,formants[2]] - mean.F2.df2)^2 )
      df2$dist.to.other <- sqrt( (df2[,formants[1]] - mean.F1.df1)^2 + (df2[,formants[2]] - mean.F2.df1)^2 )
      output.frame <- rbind(output.frame, df1, df2)
    }
    return(output.frame)
  }
}


######################################
# Takes 2 or more columns and expands 
# the 2+ such that any delimiter
# contained within will have one
# entry with 1st row as the ID var.
######################################
expandColumns <- function(columns, map=seq(1, length(columns), 1), delim = ";"){
  if(is.data.frame(columns) == TRUE){
    out.frame <- data.frame(row.names=NULL)
    sources <- levels(droplevels(as.factor(columns[,1])))
    for(c in 2:(length(columns))){
      split_column <- strsplit(as.character(columns[,c]), delim)
      for(i in 1:length(sources)){
        ID.col <- rep(sources[i], length(split_column[[i]]))
        Expanded.col <- split_column[[i]]
        ID.df <- cbind(ID.col, Expanded.col)
        out.frame <- rbind(out.frame, ID.df)
      }
    }
  }else{
    print("Columns is not a dataframe")
  }
  names(out.frame) <- c("Source", "Target")
  return(out.frame)
}


addDiffCols <- function(token){
  #Token is a vector of formant measurements
  vec.len <- length(token)
  token.diff <- abs(diff(token))
  vec.diff <- vec.len - length(token.diff)
  vec.rep <- rep(0, vec.diff)
  token.diff <- c(vec.rep, token.diff)
  return(token.diff)
}

codeSlice <- function(token, tolerance=1.5){
  #token = vector of formant measurements
  #tolerance = the number of SD's around mean to count as outliers
  vec.len <- length(token)
  token.sd <- sd(token)
  mean.token <- mean(token)
  threshold <- mean.token + (tolerance*token.sd)
  out.vec <- c()
  
  for(c in 1:vec.len){
    if(token[c] >= threshold){
      out.vec <- c(out.vec, "BAD")
    }else{
      out.vec <- c(out.vec, "GOOD")
    }  
  }
  return(out.vec)
}


# Function that returns compute Locus Equation coefficients, S.E. and R-sqred.
# Needs updated to different dataframe structure
locuseq <- function(data, type, plot=FALSE, cons=c()){

	data <- data[data$phone_f %in% cons,]

	data <- droplevels(data)

	output.frame <- data.frame(subject=character(0),group=character(0), sex=character(0), yob=numeric(0), 
			type=character(0), phone=character(0), slope=numeric(0), y.inter=numeric(0), 
			r_sqr=numeric(0), Std_Error=numeric(0), slope_SE=numeric(0), y.inter_SE=numeric(0), 
			Mean.Mid=numeric(0), Mean.Off=numeric(0), Var_Coeff.Mid=numeric(0), Var_Coeff.Off=numeric(0), 
			StDev.Mid=numeric(0), StDev.Off=numeric(0), Avg_RoC=numeric(0), N_Tokens=numeric(0), row.names = NULL)

	
	if (is.data.frame(data) == FALSE){
		stop("Data is not in the form of a data frame.  Please convert to data frame and try again")
	}	

	data$phone_p <- as.factor(data$phone_p)
	data$phone_f <- as.factor(data$phone_f)	
	

	if(type== "CV"){
		numcons <- length(levels(data$phone_p))
		consonants <- levels(data$phone_p)
	}else if(type == "VC"){
		numcons <- length(levels(data$phone_f))
		consonants <- levels(data$phone_f)
	}else{
		stop("Specify the type of Locus Equation you wish to compute, e.g. CV or VC.")
	}


	data$speaker <- as.factor(data$speaker)
	numspk <- length(levels(data$speaker))
	spk_list <- levels(data$speaker)

	for(c in 1:numcons){	
		#Subset into consonants
		if(type=="CV"){
			c.sub <- data[data$phone_p == consonants[c],]
		}else if(type=="VC"){
			c.sub <- data[data$phone_f == consonants[c],]
		}	

		CON.frame <- data.frame(subject=character(0), group=character(0), sex=character(0), yob=numeric(0), 
			type=character(0), phone=character(0), place=character(0), slope=numeric(0), y.inter=numeric(0), 
			r_sqr=numeric(0), Std_Error=numeric(0), slope_SE=numeric(0), y.inter_SE=numeric(0), 
			Mean.Mid=numeric(0), Mean.Off=numeric(0), Var_Coeff.Mid=numeric(0), Var_Coeff.Off=numeric(0), 
			StDev.Mid=numeric(0), StDev.Off=numeric(0), Avg_RoC=numeric(0), N_Tokens=numeric(0), row.names=NULL)

		for(i in 1:numspk){				
			#Subset into speaker
			s.sub <- c.sub[c.sub$speaker == spk_list[i], ]

			sex.code = s.sub$sex[1:1]
			yob.code = s.sub$yob[1:1]
			speaker = s.sub$speaker[1:1]
			group = s.sub$group[1:1]

			if(type=="CV"){
				place = s.sub$place_p[1:1]
			}else if(type=="VC"){
				place = s.sub$place_f[1:1]
			}

			
			CON.lm <- lm(s.sub$f2_finish ~ s.sub$f2_50)

			CON.pred <- predict(CON.lm)
			resid.lm <- s.sub$f2_finish - CON.pred
			sq.lm <- resid.lm ^ 2
			SE.lm = sqrt(sum(sq.lm)/(length(s.sub$f2_finish)-2))


			#F2 Locus Equation Model
			F2.Mid.Cof <- coef(CON.lm)["s.sub$f2_50"]
			F2.Off.Cof <- coef(CON.lm)["(Intercept)"]
			F2.Mod.Rsqr <- summary(CON.lm)$r.squared
			Coef.Var.F2.Mid = sd(s.sub$f2_50)/mean(s.sub$f2_50)
			Coef.Var.F2.Off = sd(s.sub$f2_finish)/mean(s.sub$f2_finish)
			F2.Mid.SD = sd(s.sub$f2_50)
			F2.Off.SD = sd(s.sub$f2_finish)
			F2.Std_Error <- coef(summary(CON.lm))[, "Std. Error"]
			
			#This doesn't quite work yet.
			#if(plot == TRUE){
			#	min.X = min(s.sub$f2_50)
			#	max.X = max(s.sub$f2_50)
			#	min.Y = min(s.sub$f2_finish)
			#	max.Y = max(s.sub$f2_finish)
			#	char.label = paste(c("EC", i, gen.code,"-RegMod"),sep="", collapse="") 
			#	file.name = paste(c(char.label, ".png"), sep="", collapse="")
			#	png(file=file.name, family="Times")
			#	plot(s.sub$f2_50, s.sub$f2_finish, pch=17, xlab="F2-Midpoint", ylab="F2-Offset", main=char.label, xlim=c(min.X, max.X), ylim=c(min.Y, max.Y))
			#	abline(CON.lm, lty=2)
			#	dev.off()
			#}

			mean.Mid = mean(s.sub$f2_50)
			mean.Off = mean(s.sub$f2_finish)

			token.count = nrow(s.sub)

			spk.frame <- data.frame(subject = speaker, group=group, sex=sex.code, yob=yob.code, type=type, phone=consonants[c], 
					place=place,slope = F2.Mid.Cof, y.inter = F2.Off.Cof, r_sqr=F2.Mod.Rsqr, Std_Error=SE.lm, slope_SE=F2.Std_Error[2], 
					y.inter_SE=F2.Std_Error[1], Mean.Mid=mean.Mid, Mean.Off=mean.Off, Var_Coeff.Mid=Coef.Var.F2.Mid, 
					Var_Coeff.Off=Coef.Var.F2.Off, StDev.Mid=F2.Mid.SD, StDev.Off=F2.Off.SD, N_Tokens=token.count, row.names = NULL)
			CON.frame <- rbind(CON.frame, spk.frame)
		}
		output.frame <- rbind(output.frame, CON.frame)
	}
	write.table(output.frame, file="LoEq - Output.csv", sep=",", append=F)
	return(output.frame)
}


#FUNCTION FOR PLOTTING SECOND ORDER LOCUS EQUATIONS (SOLE) FROM THE OUTPUT OF THE LOCUS EQUATION FUNCTION ABOVE
#ONLY PLOTS ONE PAIR OF COMPARISONS AT A TIME WHICH NEED TO BE SPECIFIED

SOLE.plot <- function(data, group, wCI = "YES", conf=0.95, point=c(16,1), ID=""){
	#SET UP LISTS
	data_list <- list()
	model_list <- list()
	place_list <- list()
	band_list <- list()
	return_list <- list()

	#SUBSET THE RIGHT DATAFRAME
	SOLE.sub <- data[data$place == group,]
	SOLE.sub <- droplevels(SOLE.sub)
	cons <- levels(SOLE.sub$phone)
	num_cons <- length(cons)

	#FIGURE OUT PLOTTING DIMENSIONS
	xmax = max(SOLE.sub$slope) + 0.25
	ymax = max(SOLE.sub$y.inter) + 25
	xmin = min(SOLE.sub$slope) - 0.25
	ymin = min(SOLE.sub$y.inter) - 25
	midx = median(SOLE.sub$slope)
	midy = median(SOLE.sub$y.inter)


	#RUN SLOPE COMPARISON MODEL PLUS INDIVIDUAL MODELS IN LIST
	inter.mod <- lm(y.inter ~ slope*phone, data=SOLE.sub)

	for(c in 1:num_cons){
		data_list[[c]] <- data[data$phone == cons[c],]
		model_list[[c]] <- lm(y.inter ~ slope, data = data_list[[c]])
		band_list[[c]] <- predict(model_list[[c]], se.fit=T, level=conf, interval='confidence')
	}
	
	#FIGURE OUT LABELS FOR LEGEND	
	phone_1 <- paste("/", data_list[[1]]$phone[1:1], "/", sep="", collapes="")
	phone_2 <- paste("/", data_list[[2]]$phone[1:1], "/", sep="", collapes="")

	#SET UP BLANK BLOT
	plot(midx, midy, xlim=c(xmin, xmax), ylim=c(ymin,ymax), type="n", xlab="Slope", ylab="y-Intercept")

	#PLOT THE SOLEs
	for(x in 1:num_cons){
		points(data_list[[x]]$slope, data_list[[x]]$y.inter, pch=point[x])
		abline(model_list[[x]], lty=x)

		if(wCI == "YES" | wCI == "Y"){
			lines(sort(data_list[[x]]$slope), sort(band_list[[x]]$fit[,2], decreasing=T), lty=x)
			lines(sort(data_list[[x]]$slope), sort(band_list[[x]]$fit[,3], decreasing=T), lty=x)
		}
		if(ID != ""){
			text(x=(xmax-0.1), y=(ymin+50), labels=ID, font=2)
		}
	}
	legend("topright", legend=c(phone_1, phone_2), pch=point, lty=c(1,2), inset=0.1)

	#OUTPUT THE DATA
	return_list[[1]] = summary(inter.mod)
	return_list[[2]] = summary(model_list[[1]])
	return_list[[3]] = summary(model_list[[2]])
	
	return(return_list)
}

# FUNCTION FOR NORMALIZING VOWEL FORMANT DATA USING A MODIFIED VERSION OF LOBANOV'S FORMULA
#
# WORKS ON TWO DIFFERENT TYPES OF DATA FRAMES GENERATED BY THE REMEASURE SCRIPT
#	(1) THE SLICES DATAFRAME, i.e., type = "SLICE" or "SL" 
#	(2) THE SINGLES DATAFRAME i.e., type = "NORMAL" or "NM"
# ARGUMENTS
#	(1) data	:	dataframe containing the values to be normalized
#	(2) type	:	the type of values e.g. either slices, normal, or from Ultrasound scripts
#	(3) f3	:	Normalize f3 or drop it.

lobanov.norm <- function(data, type="NORMAL", f3 = "NO"){
	f.sub <- data.frame(row.names=NULL)

	numspk <- length(levels(as.factor(data$speaker)))
	data$phone <- droplevels(as.factor(data$phone))
	spk_list <- levels(droplevels(as.factor(data$speaker)))

	#Normalization
	if(type == "SLICE" | type == "SL"){
		for(c in 1:numspk){
			s.sub <- data[data$speaker == spk_list[c], ]
      
      if(summary(s.sub$bad_f3)[2] > 0){
  			s.sub[s.sub$bad_f3Di == "YES",]$f3_slice <- NA
      }
      
      print(as.character(unique(s.sub$speaker)))
      flush.console()
      s.sub$f3f2_slice <- s.sub$f3_slice - s.sub$f2_slice
			slice_25 <- s.sub[s.sub$slice_num == 25,]	
			slice_25$phone <- droplevels(slice_25$phone)		

			F1.mean <- mean(tapply(slice_25$f1_slice, slice_25$phone, mean, simplify=TRUE, na.rm=T))
			F2.mean <- mean(tapply(slice_25$f2_slice, slice_25$phone, mean, simplify=TRUE, na.rm=T))

			F1.SD <- sd(tapply(slice_25$f1_slice, slice_25$phone, mean, simplify=TRUE, na.rm=T))
			F2.SD <- sd(tapply(slice_25$f2_slice, slice_25$phone, mean, simplify=TRUE, na.rm=T))

			F1.norm <- (s.sub$f1_slice - F1.mean)/F1.SD
			F2.norm <- ((s.sub$f2_slice - F2.mean)/F2.SD) + 1

			if(f3 == "YES" | f3 == "Y"){
				F3.mean <- mean(tapply(slice_25$f3_slice, slice_25$phone, mean, simplify=TRUE, na.rm=T))
				F3.SD <- sd(tapply(slice_25$f3_slice, slice_25$phone, mean, simplify=TRUE, na.rm=T))
				F3.norm <- ((s.sub$f3_slice - F3.mean)/F3.SD) + 2
        
				F3F2.mean <- mean(tapply(slice_25$f3f2_slice, slice_25$phone, mean, simplify=TRUE, na.rm=T))
				F3F2.SD <- sd(tapply(slice_25$f3f2_slice, slice_25$phone, mean, simplify=TRUE, na.rm=T))
				F3F2.norm <- ((s.sub$f3f2_slice - F3F2.mean)/F3F2.SD)
			}else{
				F3.norm = NA
				F3F2.norm = NA
			}

			d.sub <- cbind(s.sub, F1.norm, F2.norm, F3.norm, F3F2.norm)
			f.sub <- rbind(f.sub, d.sub)
		}
	}else if(type == "NORMAL" | type == "NM"){
		for(c in 1:numspk){
			s.sub <- data[data$speaker == spk_list[c], ]

			print(as.character(unique(s.sub$speaker)))
			flush.console()
      
			F1.mean = mean(tapply(s.sub$f1_50, s.sub$phone, mean, na.rm=T))
			F2.mean = mean(tapply(s.sub$f2_50, s.sub$phone, mean, na.rm=T))
			F1.SD = sd(tapply(s.sub$f1_50, s.sub$phone, mean, na.rm=T))
			F2.SD = sd(tapply(s.sub$f2_50, s.sub$phone, mean, na.rm=T))


			F1.0.norm <- (s.sub$f1_0 - F1.mean)/F1.SD
			F2.0.norm <- (s.sub$f2_0 - F2.mean)/F2.SD
			F1.20.norm <- (s.sub$f1_20 - F1.mean)/F1.SD
			F2.20.norm <- (s.sub$f2_20 - F2.mean)/F2.SD
			F1.25.norm <- (s.sub$f1_25 - F1.mean)/F1.SD
			F2.25.norm <- (s.sub$f2_25 - F2.mean)/F2.SD
			F1.35.norm <- (s.sub$f1_35 - F1.mean)/F1.SD
			F2.35.norm <- (s.sub$f2_35 - F2.mean)/F2.SD
			F1.40.norm <- (s.sub$f1_40 - F1.mean)/F1.SD
			F2.40.norm <- (s.sub$f2_40 - F2.mean)/F2.SD
			F1.50.norm <- (s.sub$f1_50 - F1.mean)/F1.SD
			F2.50.norm <- (s.sub$f2_50 - F2.mean)/F2.SD
			F1.60.norm <- (s.sub$f1_60 - F1.mean)/F1.SD
			F2.60.norm <- (s.sub$f2_60 - F2.mean)/F2.SD
			F1.65.norm <- (s.sub$f1_65 - F1.mean)/F1.SD
			F2.65.norm <- (s.sub$f2_65 - F2.mean)/F2.SD
			F1.75.norm <- (s.sub$f1_75 - F1.mean)/F1.SD
			F2.75.norm <- (s.sub$f2_75 - F2.mean)/F2.SD
			F1.80.norm <- (s.sub$f1_80 - F1.mean)/F1.SD
			F2.80.norm <- (s.sub$f2_80 - F2.mean)/F2.SD
			F1.100.norm <- (s.sub$f1_100 - F1.mean)/F1.SD
			F2.100.norm <- (s.sub$f2_100 - F2.mean)/F2.SD
		
			if(f3 == "YES" | f3 == "Y"){
        #Null out the bad F3 tokens
        if(summary(s.sub$bad_f3)[2] > 0){
          s.sub[s.sub$bad_f3 == "YES",]$f3_start <- NA
          s.sub[s.sub$bad_f3 == "YES",]$f3_20 <- NA
          s.sub[s.sub$bad_f3 == "YES",]$f3_25 <- NA
          s.sub[s.sub$bad_f3 == "YES",]$f3_35 <- NA
          s.sub[s.sub$bad_f3 == "YES",]$f3_40<- NA
          s.sub[s.sub$bad_f3 == "YES",]$f3_50 <- NA
          s.sub[s.sub$bad_f3 == "YES",]$f3_60 <- NA
          s.sub[s.sub$bad_f3 == "YES",]$f3_65 <- NA
          s.sub[s.sub$bad_f3 == "YES",]$f3_75 <- NA
          s.sub[s.sub$bad_f3 == "YES",]$f3_80 <- NA
          s.sub[s.sub$bad_f3 == "YES",]$f3_finish <- NA
        }
        
				F3.mean = mean(tapply(s.sub$f3_50, s.sub$phone, mean, na.rm=T))
				F3.SD = sd(tapply(s.sub$f3_50, s.sub$phone, mean, na.rm=T))
				F3.0.norm <- (s.sub$f3_0 - F3.mean)/F3.SD
				F3.20.norm <- (s.sub$f3_20 - F3.mean)/F3.SD
				F3.25.norm <- (s.sub$f3_25 - F3.mean)/F3.SD
				F3.35.norm <- (s.sub$f3_35 - F3.mean)/F3.SD
				F3.40.norm <- (s.sub$f3_40 - F3.mean)/F3.SD
				F3.50.norm <- (s.sub$f3_50 - F3.mean)/F3.SD
				F3.60.norm <- (s.sub$f3_60 - F3.mean)/F3.SD
				F3.65.norm <- (s.sub$f3_65 - F3.mean)/F3.SD
				F3.75.norm <- (s.sub$f3_75 - F3.mean)/F3.SD
				F3.80.norm <- (s.sub$f3_80 - F3.mean)/F3.SD
				F3.100.norm <- (s.sub$f3_100 - F3.mean)/F3.SD
			}else{
				F3.0.norm <- NA
				F3.20.norm <- NA
				F3.25.norm <- NA
				F3.35.norm <- NA
				F3.40.norm <- NA
				F3.50.norm <- NA
				F3.60.norm <- NA
				F3.65.norm <- NA
				F3.75.norm <- NA
				F3.80.norm <- NA
				F3.100.norm <- NA
			}


			d.sub <- cbind(s.sub, F1.0.norm, F2.0.norm, F3.0.norm, F1.20.norm, F2.20.norm, F3.20.norm,
						    F1.25.norm, F2.25.norm, F3.25.norm, F1.35.norm, F2.35.norm, F3.35.norm, F1.40.norm,
						    F2.40.norm, F3.40.norm, F1.50.norm, F2.50.norm, F3.50.norm, F1.60.norm, F2.60.norm,
						    F3.60.norm, F1.65.norm, F2.65.norm, F3.65.norm, F1.75.norm, F2.75.norm, F3.75.norm,
						    F1.80.norm, F2.80.norm, F3.80.norm, F1.100.norm, F2.100.norm, F3.100.norm)
			f.sub <- rbind(f.sub, d.sub)
		}
		f.sub$diagonal.50.norm <- f.sub$F2.50.norm - f.sub$F1.50.norm
		f.sub$diagonal.0.norm <- f.sub$F2.0.norm - f.sub$F1.0.norm
		f.sub$diagonal.100.norm <- f.sub$F2.100.norm - f.sub$F1.100.norm
		f.sub$n_vsl_1 <- sqrt((f.sub$F2.35.norm - f.sub$F2.20.norm)^2 + (f.sub$F1.35.norm - f.sub$F1.20.norm)^2)
		f.sub$n_vsl_2 <- sqrt((f.sub$F2.50.norm - f.sub$F2.35.norm)^2 + (f.sub$F1.50.norm - f.sub$F1.35.norm)^2)
		f.sub$n_vsl_3 <- sqrt((f.sub$F2.65.norm - f.sub$F2.50.norm)^2 + (f.sub$F1.65.norm - f.sub$F1.50.norm)^2)
		f.sub$n_vsl_4 <- sqrt((f.sub$F2.80.norm - f.sub$F2.65.norm)^2 + (f.sub$F1.80.norm - f.sub$F1.65.norm)^2)

		f.sub$n_VL <- sqrt((f.sub$F2.80.norm - f.sub$F2.20.norm)^2 + (f.sub$F1.80.norm - f.sub$F1.20.norm)^2)
		f.sub$n_TL <- f.sub$n_vsl_1 + f.sub$n_vsl_2 + f.sub$n_vsl_3 + f.sub$n_vsl_4
	
		f.sub$n_vsl_roc_1 <- (f.sub$n_vsl_1 / (f.sub$dur*0.15))
		f.sub$n_vsl_roc_2 <- (f.sub$n_vsl_2 / (f.sub$dur*0.15))
		f.sub$n_vsl_roc_3 <- (f.sub$n_vsl_3 / (f.sub$dur*0.15))
		f.sub$n_vsl_roc_4 <- (f.sub$n_vsl_4 / (f.sub$dur*0.15))

		f.sub$n_tl_roc <- f.sub$n_TL/(0.6*f.sub$dur)
	}else if(type == "ULTRASOUND" | type == "US"){
		for(c in 1:numspk){
			s.sub <- data[data$speaker == spk_list[c], ]
			F1.mean = mean(tapply(s.sub$f1, s.sub$phone, mean))
			F2.mean = mean(tapply(s.sub$f2, s.sub$phone, mean))
			F1.SD = sd(tapply(s.sub$f1, s.sub$phone, mean))
			F2.SD = sd(tapply(s.sub$f2, s.sub$phone, mean))

			F1.norm <- (s.sub$f1 - F1.mean)/F1.SD
			F2.norm <- (s.sub$f2 - F2.mean)/F2.SD

			d.sub <- cbind(s.sub, F1.norm, F2.norm)
			f.sub <- rbind(f.sub, d.sub)
		}
	}
	return(f.sub)
}

#Model selection bootstrap function that compares three different methods
#for selection, BIC, AIC, and F-testing(multiple regression)
#The model used for generating the Monte Carlo data sets must be number 1 in the list
#Variables: data		 = data.frame with original observations
#		gen.model	 = maximally complex model from which simulation data-sets are generated
#		models 	 = list() of predetermined lmer() models
#		R 		 = the number of simulation iterations
model.boot.IC <- function(data, gen.model, models, R=1000){

	output_frame <- data.frame(intercept=numeric(0), row.names=NULL)

	#Start simulation
	for(x in 1:R){
		sim_models <- list()
		sim_AIC <- list()
		sim_BIC <- list()
		beta_star <- list()
		form <- list()
		n <- length(models)
		simdata <- simulate(gen.model, 1)
		#sim_data_frame <- cbind(sim_data_frame, simdata)
		for(c in 1:n){
			sim_models[[c]] <- refit(models[[c]], simdata)
			beta_star[[c]] <- as.data.frame(t(as.data.frame(fixef(sim_models[[c]]))))
			sim_AIC[[c]] <- AIC(sim_models[[c]])
			sim_BIC[[c]] <- BIC(sim_models[[c]])
		}
		modnames <- paste(1:length(sim_models), sep = " ")
		mod_aic <- aictab(cand.set = sim_models, modnames = modnames, sort=F)
		simAIC <- unlist(sim_AIC)
		simBIC <- unlist(sim_BIC)
		index <- seq(x, x, length.out=length(sim_models))
		beta_star_frame <- rbind.fill(beta_star)
		aic_frame <- cbind(index, mod_aic, simAIC, simBIC, beta_star_frame) 
		output_frame <- rbind(output_frame, aic_frame)	
	}
	return(output_frame)
}

model.boot.siglvl <- function(data, gen.model, R=1000){
	index <- c("gen.model", "stepwise", "forward", "backward")
	c = length(data) + 1
	output.frame <- data.frame(inercept=numeric(0), row.names=NULL)

	sim.data <- simulate(gen.model, R)
	data <- cbind(data, sim.data)

	beta.list <- list()
	#p.vals.list <- list()
	system.time(for(x in 1:R){	
		sim.model.1 <- update(gen.model, data[,c] ~ . )
		sim.model.2 <- update(gen.model, data[,c] ~ . - yob - sex - yob*sex - context - duration - F1.on.norm - F1.off.norm - TL - VL - RoC)
		
		sink("auxil.txt")
		mod.stepwise <- step(sim.model.2, scope="~ . + yob + sex + yob*sex + context + duration + F1.on.norm + F1.off.norm + TL + VL + RoC", direction="both", test="F")
		mod.forward <- step(sim.model.2, scope="~ . + yob + sex + yob*sex + context + duration + F1.on.norm + F1.off.norm + TL + VL + RoC", direction="forward", test="F")
		mod.backward <- step(sim.model.1, direction="backward", test="F")
		sink(NULL)
		
		#p.vals.list[[1]] <- as.data.frame(t(as.data.frame(p.values.lmer(sim.model))))[4,]
		#p.vals.list[[2]] <- as.data.frame(t(as.data.frame(p.values.lmer(mod.05))))[4,]
		#p.vals.list[[3]] <- as.data.frame(t(as.data.frame(p.values.lmer(mod.01))))[4,]
		beta.list[[1]] <- as.data.frame(t(as.data.frame(coef(sim.model.1))))
		beta.list[[2]] <- as.data.frame(t(as.data.frame(coef(mod.stepwise))))
		beta.list[[3]] <- as.data.frame(t(as.data.frame(coef(mod.forward))))
		beta.list[[4]] <- as.data.frame(t(as.data.frame(coef(mod.backward))))

		iter <- c(x, x, x, x)
		beta.frame <- rbind.fill(beta.list)
		#pvals.frame <- rbind.fill(p.vals.list)
		#names_vec <- names(pvals.frame)
		#for(c in 1:length(names_vec)){
		#	names_vec[c] <- paste(names_vec[c], "p.val", sep="_")
		#}	
		#names(pvals.frame) <- names_vec
		beta.frame <- cbind(iter, index, beta.frame)
		output.frame <- rbind.fill(output.frame, beta.frame)
		c = c + 1
	})
	return(output.frame)
}

p.values.lmer <- function(x) {
  summary.model <- summary(x)
  data.lmer <- data.frame(model.matrix(x))
  names(data.lmer) <- names(fixef(x))
  names(data.lmer) <- gsub(pattern=":", x=names(data.lmer), replacement=".", fixed=T)
  names(data.lmer) <- ifelse(names(data.lmer)=="(Intercept)", "Intercept", names(data.lmer))
  string.call <- strsplit(x=as.character(x@call), split=" + (", fixed=T)
  var.dep <- unlist(strsplit(x=unlist(string.call)[2], " ~ ", fixed=T))[1]
  vars.fixef <- names(data.lmer)
  formula.ranef <- paste("+ (", string.call[[2]][-1], sep="")
  formula.ranef <- paste(formula.ranef, collapse=" ")
  formula.full <- as.formula(paste(var.dep, "~ -1 +", paste(vars.fixef, collapse=" + "), 
                  formula.ranef))
  data.ranef <- data.frame(x@frame[, 
                which(names(x@frame) %in% names(ranef(x)))])
  names(data.ranef) <- names(ranef(x))
  data.lmer <- data.frame(x@frame[, 1], data.lmer, data.ranef)
  names(data.lmer)[1] <- var.dep
  out.full <- lmer(formula.full, data=data.lmer, REML=F)
  p.value.LRT <- vector(length=length(vars.fixef))
  for(i in 1:length(vars.fixef)) {
    formula.reduced <- as.formula(paste(var.dep, "~ -1 +", paste(vars.fixef[-i], 
                       collapse=" + "), formula.ranef))
    out.reduced <- lmer(formula.reduced, data=data.lmer, REML=F)
    out.LRT <- data.frame(anova(out.full, out.reduced))
    p.value.LRT[i] <- round(out.LRT[2, 7], 3)
  }
  summary.model@coefs <- cbind(summary.model@coefs, p.value.LRT)
}


# Function has.interaction checks whether x is part of a term in terms
# terms is a vector with names of terms from a model
has.interaction <- function(x,terms){
    out <- sapply(terms,function(i){
        sum(1-(strsplit(x,":")[[1]] %in% strsplit(i,":")[[1]]))==0
    })
    return(sum(out)>0)
}

# Function Model.select
# model is the lm object of the full model
# keep is a list of model terms to keep in the model at all times
# sig gives the significance for removal of a variable. Can be 0.1 too (see SPSS)
# verbose=T gives the F-tests, dropped var and resulting model after 
model.select <- function(model,keep,sig=0.05,verbose=F, iter){
      counter=1
	nonsig.vars <- c()
      # check input
      #if(!is(model,"lm")) stop(paste(deparse(substitute(model)),"is not an lm object\n"))
      # calculate scope for drop1 function
      terms <- names(fixef(model))
      if(missing(keep)){ # set scopevars to all terms
          scopevars <- terms
      } else{            # select the scopevars if keep is used
          index <- match(keep,terms)
          # check if all is specified correctly
          if(sum(is.na(index))>0){
              novar <- keep[is.na(index)]
              warning(paste(
                  c(novar,"cannot be found in the model",
                  "\nThese terms are ignored in the model selection."),
                  collapse=" "))
              index <- as.vector(na.omit(index))
          }
          scopevars <- terms[-index]
      }

      # Backward model selection : 

      while(T){
          # extract the test statistics from drop.
          test <- drop1(model,test="Chisq")

          if(verbose){
              cat("-------------STEP ",counter, iter, sig, "-------------\n",
              "The drop statistics : \n")
              print(test)
          }

          pval <- test[,dim(test)[2]]

          names(pval) <- rownames(test)
          pval <- sort(pval,decreasing=T)
		
	    for(c in 1:length(pval)){
	    	if(pval[c] > sig) nonsig.vars[c] <- names(pval[c]) 
	    }

          if(sum(is.na(pval))>0) stop(paste("Model",
              deparse(substitute(model)),"is invalid. Check if all coefficients are estimated."))

          # check if all significant
          if(pval[1]<sig) break # stops the loop if all remaining vars are sign.

          # select var to drop
          i=1
          while(T){
              dropvar <- names(pval)[i]
              check.terms <- terms[-match(dropvar,terms)]
              x <- has.interaction(dropvar,check.terms)
              if(x){i=i+1;next} else {break}              
          } # end while(T) drop var

          if(pval[i]<sig) break # stops the loop if var to remove is significant

          if(verbose){
             cat("\n--------\nTerm dropped in step",counter,":",dropvar,"\n--------\n\n")              
          }

          #update terms, scopevars and model
          scopevars <- scopevars[-match(dropvar,scopevars)]
          terms <- terms[-match(dropvar,terms)]

          formul <- as.formula(paste(".~.-",dropvar))
          model <- update(model,formul)

          if(length(scopevars)==0) {
              warning("All variables are thrown out of the model.\n",
              "No model could be specified.")
              return()
          }
          counter=counter+1
      } # end while(T) main loop
      return(model)
}

#Function for plotting Smoothing Spline ANOVA models for vowel formant data
#Code written by Michael J. Fox
#
# LIST OF FUNCTION VARIABLES
#	data			:	data.frame containing the observations
#	vowels 		:	vector of the vowels you want to compare, e.g. (vowels = c("BOT", "BOUGHT"))
#	num_formants	:	the number of formants you want to plot. Default is 2
#	image_type		:	the type of image file you want to save the plot as. Default is png

formant.smoother <- function(data, vowels, num_formants=2, image_type="png"){
	cur_dir <- getwd()
	system <- .Platform$OS.type
	date_time <- gsub(":", ".", Sys.time())
	user <- Sys.info()['user']
	if(system == "windows"){
		dir_create <- paste(c("C:/", user, "/", "Documents/", "SSANOVA output ", date_time), sep="", collapse="")
	}else{
		dir_create <- paste(c("/Users/", user, "/", "SSANOVA output ", date_time), sep="", collapse="")
	}

	dir.create(dir_create)
	num_spk <- length(levels(as.factor(data$speaker)))
	num_vowels <- length(vowels)

	#if(image_type != "png" | image_type != "eps" | image_type != "jpeg" | image_type != "pdf"){
	#	return("Please specify the image type")
	#}

	for (c in 1:num_spk){
		flush.console()
		spks <- levels(data$speaker)
		s.sub <- subset(data, speaker == spks[c])
		v.sub <- s.sub[s.sub$vowel_class %in% vowels,]
		v.sub <- droplevels(v.sub)
	
		spk.label <- levels(as.factor(v.sub$speaker))[1]
		file.name <- paste(c(spk.label, "_", num_vowels, "_", num_formants, "_SS-ANOVA", ".", image_type), sep="", collapse="")	
	
		len <- length(levels(as.factor(v.sub$slice_num)))
		grid <- as.data.frame(expand.grid(slice_num = seq(1, len, length = len), KEEP.OUT.ATTRS = TRUE, vowel_class = vowels))

		theme_set(theme_bw())

		if(num_formants == 1){
			print(paste(c("Running ONE formant models for speaker: ", spk.label), sep="", collapse=""))
			f1.model <- ssanova(f1_slice ~ vowel_class*slice_num, type=list(slice_num="cubic"), method = "v", data = v.sub)
			grid$F1.Fit <- predict(f1.model,newdata = grid, se = TRUE)$fit
			grid$F1.SE <- predict(f1.model, newdata = grid, se = TRUE)$se.fit
		
			formant.comparison <- ggplot(grid, aes(x = slice_num, colour = vowel_class, group = vowel_class))
			formant.comparison<-formant.comparison + geom_line(aes(y = F1.Fit),alpha = 1, colour = "grey20")
			formant.comparison<-formant.comparison + geom_ribbon(aes(ymin = F1.Fit-(1.96*F1.SE), ymax = F1.Fit+(1.96*F1.SE),fill = vowel_class), alpha = 0.5,colour = "NA")
			formant.comparison<-formant.comparison + ylab("Hz") + labs(title=spk.label)
		}else if(num_formants == 2){	
			print(paste(c("Running TWO formant models for speaker: ", spk.label), sep="", collapse=""))
			f1.model <- ssanova(f1_slice ~ vowel_class*slice_num, type=list(slice_num="cubic"), method = "v", data = v.sub)
			f2.model <- ssanova(f2_slice ~ vowel_class*slice_num, type=list(slice_num="cubic"), method = "v", data = v.sub)
			grid$F1.Fit <- predict(f1.model,newdata = grid, se = TRUE)$fit
			grid$F1.SE <- predict(f1.model, newdata = grid, se = TRUE)$se.fit
			grid$F2.Fit <- predict(f2.model, newdata = grid, se = T)$fit
			grid$F2.SE <- predict(f2.model, newdata = grid, se = T)$se.fit

			formant.comparison <- ggplot(grid, aes(x = slice_num, colour = vowel_class, group = vowel_class))
			formant.comparison<-formant.comparison + geom_line(aes(y = F1.Fit),alpha = 1, colour = "grey20")
			formant.comparison<-formant.comparison + geom_line(aes(y = F2.Fit),alpha = 1, colour = "grey20")
			formant.comparison<-formant.comparison + geom_ribbon(aes(ymin = F1.Fit-(1.96*F1.SE), ymax = F1.Fit+(1.96*F1.SE),fill = vowel_class), alpha = 0.5,colour = "NA")
			formant.comparison<-formant.comparison + geom_ribbon(aes(ymin = F2.Fit-(1.96*F2.SE), ymax = F2.Fit+(1.96*F2.SE),fill = vowel_class), alpha = 0.5,colour = "NA")
			formant.comparison<-formant.comparison + ylab("Hz") + labs(title=spk.label)
		}else if(num_formants == 3){	
			print(paste(c("Running THREE formant models for speaker: ", spk.label), sep="", collapse=""))
			f1.model <- ssanova(f1_slice ~ vowel_class*slice_num, type=list(slice_num="cubic"), method = "v", data = v.sub)
			f2.model <- ssanova(f2_slice ~ vowel_class*slice_num, type=list(slice_num="cubic"), method = "v", data = v.sub)
			f3.model <- ssanove(f3_slice ~ vowel_class*slice_num, type=list(slice_num="cubic"), method = "v", data = v.sub)
			grid$F1.Fit <- predict(f1.model,newdata = grid, se = TRUE)$fit
			grid$F1.SE <- predict(f1.model, newdata = grid, se = TRUE)$se.fit
			grid$F2.Fit <- predict(f2.model, newdata = grid, se = T)$fit
			grid$F2.SE <- predict(f2.model, newdata = grid, se = T)$se.fit
			grid$F3.Fit <- predict(f3.model, newdata = grid, se = T)$fit
			grid$F3.SE <- predict(f3.model, newdata = grid, se = T)$se.fit
			formant.comparison <- ggplot(grid, aes(x = slice_num, colour = vowel_class, group = vowel_class))
			formant.comparison<-formant.comparison + geom_line(aes(y = F1.Fit), alpha = 1, colour = "grey20")
			formant.comparison<-formant.comparison + geom_line(aes(y = F2.Fit), alpha = 1, colour = "grey20")
			formant.comparison<-formant.comparison + geom_line(aes(y = F3.Fit), alpha = 1, colour = "grey20")
			formant.comparison<-formant.comparison + geom_ribbon(aes(ymin = F1.Fit-(1.96*F1.SE), ymax = F1.Fit+(1.96*F1.SE),fill = vowel_class), alpha = 0.5,colour = "NA")
			formant.comparison<-formant.comparison + geom_ribbon(aes(ymin = F2.Fit-(1.96*F2.SE), ymax = F2.Fit+(1.96*F2.SE),fill = vowel_class), alpha = 0.5,colour = "NA")
			formant.comparison<-formant.comparison + geom_ribbon(aes(ymin = F3.Fit-(1.96*F3.SE), ymax = F3.Fit+(1.96*F3.SE),fill = vowel_class), alpha = 0.5,colour = "NA")
			formant.comparison<-formant.comparison + ylab("Hz") + labs(title=spk.label)
		}else if(num_formants > 3){
			return("You specified too many formants")
		}else{
			return("You didn't specify how many formants you want to compare")
		}
		print(paste(c("Saving image file: ", file.name), sep="", collapse=""))
		ggsave(file.name, path=dir_create)
		dev.off()
	}
}

## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
    require(plyr)

    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }

    # This is does the summary; it's not easy to understand...
    datac <- ddply(data, groupvars, .drop=.drop,
                   .fun= function(xx, col, na.rm) {
                           c( N    = length2(xx[,col], na.rm=na.rm),
                              mean = mean   (xx[,col], na.rm=na.rm),
                              sd   = sd     (xx[,col], na.rm=na.rm)
                              )
                          },
                    measurevar,
                    na.rm
             )

    # Rename the "mean" column    
    datac <- rename(datac, c("mean"=measurevar))

    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult

    return(datac)
}


processPVI <- function(PVI.df, SYB.df){
	PVI.sub <- subset(PVI.df, PVI != 2 & PVI != 0)
	PVI.median <- aggregate(PVI ~ speaker, data=PVI.sub, FUN=median)
	PVI.int <- aggregate(int ~ speaker, data=PVI.sub, FUN=max)
	PVI.mean <- aggregate(PVI ~ speaker, data=PVI.sub, FUN=mean)
	syb.median <- aggregate(duration ~ speaker, data=SYB.df, FUN=median)
	syb.mean <- aggregate(duration ~ speaker, data=SYB.df, FUN=mean)

	spk.info <- unique(PVI.sub[1:4])
	spk.info <- spk.info[order(spk.info$speaker),]
	d.frame <- cbind(spk.info, PVI.median[2:2], PVI.mean[2:2], PVI.int[2:2], syb.median[2:2], syb.mean[2:2])
	colnames(d.frame) <- c("speaker","state","sex","ethnicity","PVI.median","PVI.mean","PVI.int","syb.median","syb.mean")
	new.frame <- auto.code(d.frame, "speaker") 
	return(new.frame)
}


#FUNCTION FOR COMBINING MULTIPLE DATAFRAMES INTO ONE WITH AN INDEX IDENTIFYING THEM
# VERSION 1.0

combine.txt <- function(directory="G:\\Ultrasound\\acoustic_data\\ash", separator=",", header=T){
	output.frame <- data.frame(row.names=NULL)
	files <- list.files(path=directory)
	numfiles = length(files)
	
	for(c in 1:numfiles){
		temp.df <- read.csv(paste(directory, "\\", files[c], sep="", collapse=""), header=header, sep=separator)
		df.len <- length(temp.df[,1])
		name.id <- ""
		name.id[1:df.len] <- files[c]
		temp.df <- cbind(name.id, temp.df)
		output.frame <- rbind(output.frame, temp.df)
	}
	return(output.frame)
}

split.spk <- function(data, vowels=c("")){
	speakers <- as.character(levels(data$speaker))
	n_spks <- length(speakers)
	for(c in 1:n_spks){
		if(vowels[1] == ""){
			s.sub <- subset(data, speaker == speakers[c])
		}else{
			s.sub <- subset(data, speaker == speakers[c] & vowel_class %in% vowels)
		}
		f.sub <- cbind(as.character(s.sub$speaker), as.character(s.sub$vowel_class), s.sub$f1_25, s.sub$f2_25, s.sub$f1_50, s.sub$f2_50, s.sub$f1_75, s.sub$f2_75, s.sub$dur, s.sub$tl, s.sub$vl)
		colnames(f.sub) <- c("speaker", "vowel_class", "f1_25", "f2_25", "f1_50", "f2_50", "f1_75", "f2_75", "dur", "tl", "vl")
		write.table(f.sub, paste(speakers[c], ".csv", sep="", collapse=""), sep=",", quote=F, row.names=F)
	}
}

auto.code <- function(data, type, variables=c()){
	if(type == "word"){
		split.df <- t(as.data.frame(strsplit(as.character(data[,2]), "_")))
		split.df[,1] <- sub("1", "", split.df[,1])
		spk <- substring(data$file, 1,2)
		data$vowel <- split.df[,4]
		data$word <- split.df[,1]
		data$speaker <- spk
		v.codes <- read.csv("/Users/mjfox/Google Drive/Research/data_bases/unique_words.csv", header=T,sep=",")
		new.df <- merge(x=data, y=v.codes, by.x="word", by.y="word2") 
	}else if(type == "speaker"){
		s.codes = read.csv("/Users/mjfox/Google Drive/Research/data_bases/speaker_info.csv", header=T, sep=",")
		if(length(variables) > 0){
			variables = c(variables,  "speaker")
			s.codes = s.codes[, variables]
		}
		new.df <- merge(x=data, y=s.codes, by.x="speaker", by.y="speaker")
	}
	return(droplevels(new.df))
}
#"EY1", "IY1", "IH1", "EH1", "AA1", "AO1", "UH1", "UW1"

select.us <- function(orig_directory="", dest_directory="", speaker="", vowel=c("AE1"), seperator="_"){
	output.frame <- data.frame(row.names=NULL)

	if(file.exists(dest_directory) == FALSE){
		dir.create(dest_directory)
	}

	files <- list.files(path=orig_directory)
	files_split <- t(as.data.frame(strsplit(files, split=seperator), row.names=NULL))
	temp_frame <- cbind(files, files_split)
	temp_frame <- subset(temp_frame, temp_frame[,5] %in% vowel)
	temp_frame <- as.data.frame(temp_frame)
	temp_split <- split(temp_frame, temp_frame[,2])

	temp_len <- length(temp_split)
	for(c in 1:temp_len){
		us_len <- length(temp_split[[c]][,1])
		if((us_len %% 2) == 0){
			us_mid = us_len / 2 
		}else{
			us_mid = us_len / 2 - 0.5
		}
		us_select <- as.character(temp_split[[c]][us_mid,1])
		output.frame <- rbind(output.frame, temp_split[[c]][us_mid,])
		file.copy(paste(orig_directory, "/", us_select, sep="", collapse=""), paste(dest_directory, "/", us_select, sep="", collapse=""))	
	}
	output.frame[,6] <- gsub(".jpg", "", output.frame[,6])
	output.frame$speaker <- speaker
	if(file.exists(paste(dest_directory, "/", "time_stamps.csv", sep = "", collapse="")) == TRUE){
		write.table(output.frame, paste(dest_directory, "/", "time_stamps.csv", sep = "", collapse=""), sep=",", col.names=FALSE, row.names=FALSE, append=TRUE)
	}else{
		write.table(output.frame, paste(dest_directory, "/", "time_stamps.csv", sep = "", collapse=""), sep=",", row.names=FALSE, append=FALSE)
	}
}


random.stim <- function(wordlist, number_pauses, number_reps, carrier_phrase="Give me a : again"){
	len <- nrow(data)
	stim_len = round(len / number_pauses)
	extra <- strsplit(as.character(stim_len), "\\.")[[1]][2]
	counter = 1
	group_list <- list()
	ran.phrase = ""
	is.even <- len %% 2 == 0

	pdf(file="word_stim.pdf", onefile=TRUE, width=10)
		for(x in 1:number_reps){
			grp_start = 1
			grp_end = stim_len
		
			#GENERATE RANDOM LIST FROM WORDS GIVEN	
			index <- seq(from = 1, to = len, by = 1)
			index.ran <- sample(index, replace=F)
			words <- data$word
			ran.word <- words[index.ran]

			#MAKE PHRASE
			split_phrase <- strsplit(carrier_phrase, split=":")
			for(h in 1:len){
				#ran.phrase[h] <- as.character(ran.word[h])
				ran.phrase[h] <- paste(as.character(split_phrase[[1]][1]), ran.word[h], as.character(split_phrase[[1]][2]), sep="", collapse="")
			}

			#SPLIT UP LIST INTO PAUSE GROUPS
			for(z in 1:number_pauses){
				group_list[[z]] <- ran.phrase[grp_start:grp_end]
				grp_start = grp_start + stim_len
				grp_end = grp_end + stim_len
	
			}
			if(is.even == FALSE){
				group_list[[number_pauses]][(stim_len +1)] <- ran.phrase[len]
			}
		
			for(c in 1:number_pauses){
				grp_len <- length(group_list[[c]])
				for(m in 1:grp_len){
					if(is.na(group_list[[c]][m]) == F){
						plot(1, 1, type="n", axes=F, xaxt="n", ann=FALSE)
						text(1, 1, labels=group_list[[c]][m], cex=4, xaxt="n")
					}
				}
				phrase = group_list[[c]][m]
				if(c != number_pauses){
					plot(1, 1, type="n", axes=F, xaxt="n", ann=FALSE)
					text(1, 1, labels="PAUSE", col="red", cex=6, xaxt="n")
				}
				write.table(group_list[[c]], paste("word_stim_",x, "_", c, ".txt", sep="", collapse=""), sep="", row.names=FALSE, col.names=FALSE)
			}
			if(x != number_reps){
				plot(1, 1, type="n", axes=F, xaxt="n", ann=FALSE)
				text(1, 1, labels="PAUSE", col="red", cex=6, xaxt="n")
			}
		}
		plot(1, 1, type="n", axes=F, xaxt="n", ann=FALSE)
		text(1, 1, labels="THE END!", col="red", cex=6, xaxt="n")
	dev.off()
}



gen.batch.process <- function(local_files="", trans_files="~/to_align", server_files="/phon/NC_fa"){
	files <- list.files(path=local_files)
	write("#!/bin/bash", "batch_process.sh")
	numfiles = length(files)

	files <- substr(files, 1, (nchar(files)-4))
	split.df <- strsplit(files, "_")
	for(x in 1:length(split.df)){
		split.df[[x]] <- as.data.frame(t(as.data.frame(split.df[[x]])),stringsAsFactors=FALSE)
	}
	split.df <- rbind.fill(split.df)
	endc <- length(split.df[1,])
	split.df[,endc] <- substr(split.df[,endc], 1,1)

	for(c in 1:numfiles){ 
		if(is.na(split.df[c,3]) == TRUE){
			split.df[c,3] <- ""
		}
		if(is.na(split.df[c,4]) == TRUE){
			split.df[c,4] <- ""
		}
		input_wav <- paste(paste(split.df[c,1], split.df[c,2], split.df[c,3], split.df[c,4], sep="_", collapse=""), ".wav", sep="", collapse="")
		transcript <- paste(paste(split.df[c,2], "transcript", split.df[c,3], sep="_", collapse=""), ".txt", sep="", collapse="")
		output_grid <- paste(paste(split.df[c,1], split.df[c,2], split.df[c,3], split.df[c,4], sep="_", collapse=""), ".TextGrid", sep="", collapse="")

		line_print <- paste("python /phon/p2fa/align.py ", server_files, "/", input_wav, " ", trans_files, "/", transcript, " ~/to_align/grids/", output_grid, sep="", collapse="")
		write(line_print, "batch_process.sh", append=T, sep="")
	}
}

## re = object of class ranef.mer
ggCaterpillar <- function(re, QQ=TRUE, likeDotplot=TRUE) {
    require(ggplot2)
    f <- function(x) {
        pv   <- attr(x, "postVar")
        cols <- 1:(dim(pv)[1])
        se   <- unlist(lapply(cols, function(i) sqrt(pv[i, i, ])))
        ord  <- unlist(lapply(x, order)) + rep((0:(ncol(x) - 1)) * nrow(x), each=nrow(x))
        pDf  <- data.frame(y=unlist(x)[ord],
                           ci=1.96*se[ord],
                           nQQ=rep(qnorm(ppoints(nrow(x))), ncol(x)),
                           ID=factor(rep(rownames(x), ncol(x))[ord], levels=rownames(x)[ord]),
                           ind=gl(ncol(x), nrow(x), labels=names(x)))

        if(QQ) {  ## normal QQ-plot
            p <- ggplot(pDf, aes(nQQ, y))
            p <- p + facet_wrap(~ ind, scales="free")
            p <- p + xlab("Standard normal quantiles") + ylab("Random effect quantiles")
        } else {  ## caterpillar dotplot
            p <- ggplot(pDf, aes(ID, y)) + coord_flip()
            if(likeDotplot) {  ## imitate dotplot() -> same scales for random effects
                p <- p + facet_wrap(~ ind)
            } else {           ## different scales for random effects
                p <- p + facet_grid(ind ~ ., scales="free_y")
            }
            p <- p + xlab("Levels") + ylab("Random effects")
        }

        p <- p + theme(legend.position="none")
        p <- p + geom_hline(yintercept=0)
        p <- p + geom_errorbar(aes(ymin=y-ci, ymax=y+ci), width=0, colour="black")
        p <- p + geom_point(aes(size=1.2), colour="blue") 
        return(p)
    }

    lapply(re, f)
}

extract.coef <- function(mod.list, mod.group="1", exclude.list=c()){
	output.frame <- data.frame(Mod.group = character(), Mod.Name = character(), IV = character(), Estimate = numeric(), Std.Err. = numeric(), t = numeric(), CI.lw = numeric(), CI.upr = numeric(), row.names=NULL)
	islist <- is.list(mod.list)
  if(islist == TRUE & length(mod.list) != 12){
		len <- length(mod.list)
		for(c in 1:len){
			if(class(mod.list[[c]])[1] == "lmerMod"){
				cc <- fixef(mod.list[[c]])
			}else if(class(mod.list[[c]])[1] == "lm"){
				cc <- mod.list[[c]]$coef
			}
			ss <- sqrt(diag(vcov(mod.list[[c]])))
			n.coef <- length(cc)
			r.names <- rep(paste("Mod", c, sep="", collapse=""), n.coef)
			est.names <- names(cc)
			t <- cc/ss
			lwr <- cc - 1.96*ss
			upr <- cc + 1.96*ss
			temp <- data.frame(Mod.group = mod.group, Mod.Name = r.names, IV = est.names, Estimate = cc, Std.Err. = ss, t = t, CI.lw = lwr, CI.upr = upr, row.names=NULL)
			output.frame <- rbind(output.frame, temp)
		}
	}else{
	  if(class(mod.list)[1] == "lmerMod"){
	    cc <- fixef(mod.list)
	  }else if(class(mod.list)[1] == "lm"){
	    cc <- mod.list$coef
	  }
	  ss <- sqrt(diag(vcov(mod.list)))
		n.coef <- length(cc)
		r.names <- rep(paste("Mod", c, sep="", collapse=""), n.coef)
		est.names <- names(cc)
		t <- cc/ss
		lwr <- cc - 2*ss
		upr <- cc + 2*ss
		temp <- data.frame(Mod.group = mod.group, Mod.Name = r.names, IV = est.names, Estimate = cc, Std.Err. = ss, t = t, CI.lw = lwr, CI.upr = upr, row.names=NULL)
		output.frame <- rbind(output.frame, temp)
	}
  all.list <- names(cc)
  include.list <- setdiff(all.list, exclude.list)
  output.frame <- output.frame[output.frame$IV %in% include.list,]
	return(output.frame)
}

nest.table <- function(df, stats="se", rnd = 3){
	rank <- data.frame(mod = character(), num = numeric(), row.names=NULL)
	mod.nums <- levels(df$Mod)
	mod.list <- list()
	len <- length(mod.nums)
	var.names <- unique(df$IV)
	for(c in 1:len){
		s.df <- df[df$Mod.Name == mod.nums[c],]
		s.df <- droplevels(s.df)
		temp <- data.frame(mod = paste("Mod", c, sep="", collapse=""), num = length(levels(s.df$IV)), row.names=NULL)
		rank <- rbind(rank,temp)
	}
	rank <- rank[order(-rank$num), ]			
	for(c in 1:len){
		c.df <- df[df$Mod.Name == rank[c,1],]
		names(c.df)[3] <- c.df$Mod.Name[c]
		mod.list[[c]] <- c.df
	}
	#var.names <- mod.list[[1]]$IV
	output.frame <- data.frame(vars=var.names, row.names=NULL)
	for(c in 1:len){
		s.sub <- mod.list[[c]][,2:5]
		s.sub[,2] <- round(s.sub[,2], rnd)
		s.sub[,3] <- round(s.sub[,3], rnd)
		s.sub[,4] <- round(s.sub[,4], rnd)
		s.len <- length(s.sub[,1])
		if(stats == "se" | stats == "SE"){
			for(x in 1:s.len){
				s.sub[x,2] <- paste(s.sub[x,2], " (", s.sub[x,3], ")", sep="", collapse="")
			}
		}else if(stats == "t" | stats == "T"){
			for(x in 1:s.len){
				s.sub[x,2] <- paste(s.sub[x,2], " (", s.sub[x,4], ")", sep="", collapse="")
			}
		}
		s.sub <- s.sub[,1:2]
		output.frame <- merge(x=output.frame, y=s.sub, by.x="vars", by.y="IV", all.x=TRUE) 
	}	
	n.len <- length(output.frame)
	output.frame <- cbind(output.frame$vars, output.frame[,n.len:2])
}


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


require(proto)

StatEllipse <- proto(ggplot2:::Stat,
                     {
                       required_aes <- c("x", "y")
                       default_geom <- function(.) GeomPath
                       objname <- "ellipse"
                       
                       calculate_groups <- function(., data, scales, ...){
                         .super$calculate_groups(., data, scales,...)
                       }
                       calculate <- function(., data, scales, level = 0.75, segments = 51,...){
                         dfn <- 2
                         dfd <- length(data$x) - 1
                         if (dfd < 3){
                           ellipse <- rbind(c(NA,NA))	
                         } else {
                           require(MASS)
                           v <- cov.trob(cbind(data$x, data$y))
                           shape <- v$cov
                           center <- v$center
                           radius <- sqrt(dfn * qf(level, dfn, dfd))
                           angles <- (0:segments) * 2 * pi/segments
                           unit.circle <- cbind(cos(angles), sin(angles))
                           ellipse <- t(center + radius * t(unit.circle %*% chol(shape)))
                         }
                         
                         ellipse <- as.data.frame(ellipse)
                         colnames(ellipse) <- c("x","y")
                         return(ellipse)
                       }
                     }
)

stat_ellipse <- function(mapping=NULL, data=NULL, geom="path", position="identity", ...) {
  StatEllipse$new(mapping=mapping, data=data, geom=geom, position=position, ...)
}


vp.layout <- function(x, y) viewport(layout.pos.row=x, layout.pos.col=y)
arrange.plot <- function(..., nrow=NULL, ncol=NULL, as.table=FALSE) {
  dots <- list(...)
  n <- length(dots)
  if(is.null(nrow) & is.null(ncol)) { nrow = floor(n/2) ; ncol = ceiling(n/nrow)}
  if(is.null(nrow)) { nrow = ceiling(n/ncol)}
  if(is.null(ncol)) { ncol = ceiling(n/nrow)}
  ## NOTE see n2mfrow in grDevices for possible alternative
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(nrow,ncol) ) )
  ii.p <- 1
  for(ii.row in seq(1, nrow)){
    ii.table.row <- ii.row
    if(as.table) {ii.table.row <- nrow - ii.table.row + 1}
    for(ii.col in seq(1, ncol)){
      ii.table <- ii.p
      if(ii.p > n) break
      print(dots[[ii.table]], vp=vp.layout(ii.table.row, ii.col))
      ii.p <- ii.p + 1
    }
  }
}