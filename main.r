\documentclass{article}
\usepackage{flexisym}
\usepackage{titlesec}
\usepackage{subfig}
\usepackage{lipsum}
\usepackage[margin=1in]{geometry}
\renewcommand{\thesubsection}{\arabic{subsection}}

\titleformat{\section}{\centering\large}{}{0em}{}[\titlerule]
\titleformat{\subsection}{}{}{0em}{}

<<echo=FALSE>>=
suppressMessages(library(faraway))
suppressMessages(library(MASS))
suppressMessages(library(lattice))
suppressMessages(library(ggplot2))
suppressMessages(library(quantreg))
suppressMessages(library(robustbase))
@

<<echo=FALSE>>=
  mayFinalsData <- read.csv("D:\\C\\ThisShitisFuckingAnnoying\\R data\\Honnold Data\\rawWifi2016\\mayData\\may2016Finals_Clean.csv",header=TRUE)

finals <- mayFinalsData
  
finals$Campus<-sub("claremontmckenna.edu", "cmc", finals$Campus);
finals$Campus<-sub("CMC.edu", "cmc", finals$Campus);
finals$Campus<-sub("pomona.edu", "pomona", finals$Campus);
finals$Campus<-gsub("POm | Pom | pOM | campus.pomona.edu" , "pomona", finals$Campus);
finals$Campus<-sub("pitzer.edu", "pitzer", finals$Campus);
finals$Campus<-gsub("PTZ|ptz|PIt|Pit|pIT|piT", "pitzer", finals$Campus);
finals$Campus<-gsub("Cgu|CGU.edu", "cgu", finals$Campus);
finals$Campus<-sub('cgu"', 'cgu', finals$Campus);
finals$Campus<-sub("hmc.edu", "hmc", finals$Campus);

mayFinalsData <- finals


data_201605 <- mayFinalsData  
data_201605$Folder<-sub("Top > CUC > Honold Mudd Library > First Floor", "1F", data_201605$Folder)
data_201605$Folder<-sub("Top > CUC > Honold Mudd Library > Second Floor", "2F", data_201605$Folder)
data_201605$Folder<-sub("Top > CUC > Honold Mudd Library > Third Floor", "3F", data_201605$Folder)
data_201605$Folder<-sub("Top > CUC > Honold Mudd Library > Fourth Floor", "4F", data_201605$Folder)
data_201605$Folder<-sub("Top > CUC > Honold Mudd Library > Cafe", "Cafe", data_201605$Folder)
mayFinalsData <- data_201605

  
#Calculating the means of Avg_Usage on each Folder and Time Frame
Avg_Usage_F1_morning   <- mean(data_201605[data_201605$Connect_Hour == 8:11 & data_201605$Folder == "1F", ]$Avg_Usage, na.rm = TRUE)

Avg_Usage_F1_afternoon <- mean(data_201605[data_201605$Connect_Hour == 12:17 & data_201605$Folder == "1F", ]$Avg_Usage, na.rm = TRUE)
Avg_Usage_F1_evening   <- mean(data_201605[data_201605$Connect_Hour == 18:24 & data_201605$Folder == "1F", ]$Avg_Usage, na.rm = TRUE)
Avg_Usage_F1_nightowl  <- mean(data_201605[data_201605$Connect_Hour == 1:7 & data_201605$Folder == "1F", ]$Avg_Usage, na.rm = TRUE)

Avg_Usage_F1 <- c(Avg_Usage_F1_morning, Avg_Usage_F1_afternoon, Avg_Usage_F1_evening, Avg_Usage_F1_nightowl)

Avg_Usage_F2_morning   <- mean(data_201605[data_201605$Connect_Hour == 8:11 & data_201605$Folder == "2F", ]$Avg_Usage, na.rm = TRUE)
Avg_Usage_F2_afternoon <- mean(data_201605[data_201605$Connect_Hour == 12:17 & data_201605$Folder == "2F", ]$Avg_Usage, na.rm = TRUE)
Avg_Usage_F2_evening   <- mean(data_201605[data_201605$Connect_Hour == 18:24 & data_201605$Folder == "2F", ]$Avg_Usage, na.rm = TRUE)
Avg_Usage_F2_nightowl  <- mean(data_201605[data_201605$Connect_Hour == 1:7 & data_201605$Folder == "2F", ]$Avg_Usage, na.rm = TRUE)
Avg_Usage_F2 <- c(Avg_Usage_F2_morning, Avg_Usage_F2_afternoon, Avg_Usage_F2_evening, Avg_Usage_F2_nightowl)

Avg_Usage_F3_morning   <- mean(data_201605[data_201605$Connect_Hour == 8:11 & data_201605$Folder == "3F", ]$Avg_Usage, na.rm = TRUE)
Avg_Usage_F3_afternoon <- mean(data_201605[data_201605$Connect_Hour == 12:17 & data_201605$Folder == "3F", ]$Avg_Usage, na.rm = TRUE)
Avg_Usage_F3_evening   <- mean(data_201605[data_201605$Connect_Hour == 18:24 & data_201605$Folder == "3F", ]$Avg_Usage, na.rm = TRUE)
Avg_Usage_F3_nightowl  <- mean(data_201605[data_201605$Connect_Hour == 1:7 & data_201605$Folder == "3F", ]$Avg_Usage, na.rm = TRUE)
Avg_Usage_F3 <- c(Avg_Usage_F3_morning, Avg_Usage_F3_afternoon, Avg_Usage_F3_evening, Avg_Usage_F3_nightowl)

Avg_Usage_F4_morning   <- mean(data_201605[data_201605$Connect_Hour == 8:11 & data_201605$Folder == "4F", ]$Avg_Usage, na.rm = TRUE)
Avg_Usage_F4_afternoon <- mean(data_201605[data_201605$Connect_Hour == 12:17 & data_201605$Folder == "4F", ]$Avg_Usage, na.rm = TRUE)
Avg_Usage_F4_evening   <- mean(data_201605[data_201605$Connect_Hour == 18:24 & data_201605$Folder == "4F", ]$Avg_Usage, na.rm = TRUE)
Avg_Usage_F4_nightowl  <- mean(data_201605[data_201605$Connect_Hour == 1:7 & data_201605$Folder == "4F", ]$Avg_Usage, na.rm = TRUE)
Avg_Usage_F4 <- c(Avg_Usage_F4_morning, Avg_Usage_F4_afternoon, Avg_Usage_F4_evening, Avg_Usage_F4_nightowl)

Avg_Usage_Cafe_morning   <- mean(data_201605[data_201605$Connect_Hour == 8:11 & data_201605$Folder == "Cafe", ]$Avg_Usage, na.rm = TRUE)
Avg_Usage_Cafe_afternoon <- mean(data_201605[data_201605$Connect_Hour == 12:17 & data_201605$Folder == "Cafe", ]$Avg_Usage, na.rm = TRUE)
Avg_Usage_Cafe_evening   <- mean(data_201605[data_201605$Connect_Hour == 18:24 & data_201605$Folder == "Cafe", ]$Avg_Usage, na.rm = TRUE)
Avg_Usage_Cafe_nightowl  <- mean(data_201605[data_201605$Connect_Hour == 1:7 & data_201605$Folder == "Cafe", ]$Avg_Usage, na.rm = TRUE)
Avg_Usage_Cafe <- c(Avg_Usage_Cafe_morning, Avg_Usage_Cafe_afternoon, Avg_Usage_Cafe_evening, Avg_Usage_Cafe_nightowl)


locationTime <- data.frame("F1" = Avg_Usage_F1, "F2" = Avg_Usage_F2, "F3" = Avg_Usage_F3,
                           "F4" = Avg_Usage_F4, "Cafe" = Avg_Usage_Cafe)
@

\begin{document}

\title{Honnold Project}
\author{Harry Choi, John Kim, Alex Lee, Lauren Su}
\date{Dec 1, 2016}
\maketitle