library(dplyr)
library(magrittr)
library(gridExtra)
library(ggplot2)
library(data.table)

## combine MOtors
wd <- c("C:/Users/Shayl/Desktop/My projects/Electricity/motor 20-22")
multmerge = function(wd){
  filenames=list.files(path=wd, full.names=TRUE)
  rbindlist(lapply(filenames, fread))
}

df = multmerge(wd)
## AMP <- df %>% select(contains("AMP"))
AMP <- df[,c(
             "P1_2405_01A_AMP_VAL0","P1_2405_01B_AMP_VAL0","P1_2405_01C_AMP_VAL0","P1_2405_01D_AMP_VAL0",
             "P2_3501_01A_AMP_VAL0","P2_3501_01B_AMP_VAL0","P2_3501_01C_AMP_VAL0","P2_3501_01D_AMP_VAL0","P2_3501_01E_AMP_VAL0",
             "E3_3502_01A_AMP_VAL0","E3_3502_01B_AMP_VAL0","E3_3502_01C_AMP_VAL0","E3_3502_01D_AMP_VAL0","E3_3502_01E_AMP_VAL0")]

AMP <- as.data.frame(apply(AMP, 2, function(x) ifelse(x <= 1,0,x)))
AMP <- as.data.frame(apply(AMP, 2, function(x) x*400*cos(0.014)))
AMP <- AMP %>% mutate(TimeStamp = df$TimeStamp)


## combine analogs
wd <- c("C:/Users/Shayl/Desktop/My projects/Electricity/analogs 20-22")
df = multmerge(wd)
Flow <- df[,c(
              "FIT_2405_01A_FL_VAL0","FIT_2405_01B_FL_VAL0","FIT_2405_01C_FL_VAL0","FIT_2405_01D_FL_VAL0",
              "FIT_3502_01A_FL_VAL0","FIT_3502_01B_FL_VAL0","FIT_3502_01C_FL_VAL0","FIT_3502_01D_FL_VAL0","FIT_3502_01E_FL_VAL0",
              "FIT_3502_03A_FL_VAL0","FIT_3502_03B_FL_VAL0","FIT_3502_03C_FL_VAL0","FIT_3502_03D_FL_VAL0","FIT_3502_03E_FL_VAL0")]
Flow <- as.data.frame(apply(Flow, 2, function(x) ifelse(x<=1,NA,x)))
Flow <- Flow %>% mutate(TimeStamp = df$TimeStamp)

## merge
df <- merge(AMP, Flow, by.x = "TimeStamp", 
                   by.y = "TimeStamp", all.x = FALSE, all.y = FALSE)

NormPumpPower = with(df, data.frame(
  power_UFA = P1_2405_01A_AMP_VAL0/FIT_2405_01A_FL_VAL0,
  power_UFB = P1_2405_01B_AMP_VAL0/FIT_2405_01B_FL_VAL0,
  power_UFC = P1_2405_01C_AMP_VAL0/FIT_2405_01C_FL_VAL0,
  power_UFD = P1_2405_01D_AMP_VAL0/FIT_2405_01D_FL_VAL0,
  power_HPA = P2_3501_01A_AMP_VAL0/FIT_3502_01A_FL_VAL0,
  power_HPB = P2_3501_01B_AMP_VAL0/FIT_3502_01B_FL_VAL0,
  power_HPC = P2_3501_01C_AMP_VAL0/FIT_3502_01C_FL_VAL0,
  power_HPD = P2_3501_01D_AMP_VAL0/FIT_3502_01D_FL_VAL0,
  power_HPE = P2_3501_01E_AMP_VAL0/FIT_3502_01E_FL_VAL0,
  power_turboA = E3_3502_01A_AMP_VAL0/FIT_3502_03A_FL_VAL0,
  power_turboB = E3_3502_01B_AMP_VAL0/FIT_3502_03B_FL_VAL0,
  power_turboC = E3_3502_01C_AMP_VAL0/FIT_3502_03C_FL_VAL0,
  power_turboD = E3_3502_01D_AMP_VAL0/FIT_3502_03D_FL_VAL0,
  power_turboE = E3_3502_01E_AMP_VAL0/FIT_3502_03E_FL_VAL0
))


#### Remove outliers
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
NormPumpPower = as.data.frame(apply(NormPumpPower, 2, remove_outliers ))
NormPumpPower <- NormPumpPower %>% mutate(TimeStamp = df$TimeStamp)

## Creating graphs and save them

NormPumpPower <- sample_frac(NormPumpPower, size = .1)
wd <- c("C:/Users/Shayl/Desktop/My projects/Electricity/images")
setwd(wd)


for (i in colnames(NormPumpPower)) {
   P1 <- ggplot(NormPumpPower, aes(x = TimeStamp, y = !!sym(i))) +
      geom_point(size = .1) +
      xlab("Time") +
      ylab("Power [KWh/m3]") +
      ggtitle(paste(i))
   
   P2 <- ggplot(NormPumpPower, aes(x = TimeStamp, y = !!sym(i))) +
     geom_smooth(colour = "red") +
     xlab("Time") +
     ylab("Power [KWh/m3]") +
     ggtitle(paste(i))
   P = grid.arrange(P1, P2, ncol=1)
  ggsave(P, file=paste0(i,".png"))
}


