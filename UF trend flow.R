library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

## import and bind csv files
wd <- c("C:/Users/Bazan_Lab/Desktop/My projects/UF_flow/24.10.2021")
setwd(wd)
file_names <- dir(wd)
dataset <- do.call(rbind, lapply(file_names, read.csv, header = TRUE, sep = ",", quote = "'"))

#### fix data  ####
dataset$TimeStamp <- as.POSIXct(dataset$TimeStamp)
dataset <- dataset[,2:22]


## remove 15% from low data points
dataset <- na.omit(dataset)
keep <- lapply(dataset, function(x) x >= quantile(x, .2))



datasetA <- dataset[keep$FIT_2405_01A_FL_VAL0,]
datasetB <- dataset[keep$FIT_2405_01B_FL_VAL0,]
datasetC <- dataset[keep$FIT_2405_01C_FL_VAL0,]
datasetD <- dataset[keep$FIT_2405_01D_FL_VAL0,]


## sample 50% of data
datasetA <- sample_frac(datasetA, .5)
datasetB <- sample_frac(datasetB, .5)
datasetC <- sample_frac(datasetC, .5)
datasetD <- sample_frac(datasetD, .5)

#### end ####

## UF flow graphs

#### smooth ####
colors <- c("UF A" = "Green", "UF B" = "Red", "UF C" = "Blue", "UF D" = "Orange")

UF_trend <- ggplot(datasetA, aes(x = TimeStamp)) +
  geom_smooth(data = datasetA, aes(y = FIT_2405_01A_FL_VAL0, colour = "UF A"), size = 1.5 ) + 
  geom_smooth(data = datasetB, aes(y = FIT_2405_01B_FL_VAL0, colour = "UF B"), size = 1.5) +
  geom_smooth(data = datasetC, aes(y = FIT_2405_01C_FL_VAL0, colour = "UF C"), size = 1.5) +
  geom_smooth(data = datasetD, aes(y = FIT_2405_01D_FL_VAL0, colour = "UF D"), size = 1.5) +
  labs(x="Time",
       y="Flow [m^3/h]",
       color = "Legend") +
  scale_color_manual(values = colors) +
  ggtitle("UF trend") +
  theme(plot.title = element_text(hjust=0.5))

#### end ####

#### grid arrange ####
UFA <- ggplot(datasetA, aes(x = TimeStamp)) +
  geom_point(data = datasetA, aes(y = FIT_2405_01A_FL_VAL0), size = 1.5, colour = "green" ) +
  labs(x="Time",
       y="Flow [m^3/h]") +
  ggtitle("UF A") +
  theme(plot.title = element_text(hjust=0.5), legend.position = "none")

UFB <- ggplot(datasetB, aes(x = TimeStamp)) +
  geom_point(data = datasetB, aes(y = FIT_2405_01B_FL_VAL0), size = 1.5, colour = "red" ) +
  labs(x="Time",
       y="Flow [m^3/h]") +
  ggtitle("UF B") +
  theme(plot.title = element_text(hjust=0.5), legend.position = "none")

UFC <- ggplot(datasetC, aes(x = TimeStamp)) +
  geom_point(data = datasetC, aes(y = FIT_2405_01C_FL_VAL0), size = 1.5 , colour = "blue" ) +
  labs(x="Time",
       y="Flow [m^3/h]") +
  ggtitle("UF C") +
  theme(plot.title = element_text(hjust=0.5), legend.position = "none")

UFD <- ggplot(datasetD, aes(x = TimeStamp)) +
  geom_point(data = datasetD, aes(y = FIT_2405_01D_FL_VAL0), size = 1.5, colour = "orange" ) +
  labs(x="Time",
       y="Flow [m^3/h]") +
  ggtitle("UF D") +
  theme(plot.title = element_text(hjust=0.5), legend.position = "none")

grid.arrange(UFA, UFB, UFC, UFD, UF_trend, nrow = 3)

#### end ####
