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



%-------------------------INTRODUCTION-----------------------------

\section{1. Introduction}

\subsection{\textbf{1.1 Motivation}}

Claremont Colleges Library (CCL hereinafter) serves the Claremont Colleges, a consortium composed of five undergraduate and two graduate schools of higher education. Ranking third among the private institutions in California, behind only Stanford and USC, CCL is clearly a noteworthy example of institutional cooperation. At the same time, it is definitely larger than any of the seven schools could afford to own on its own. Therefore optimizing library management under resource constraints is a high priority for CCL.\\

When our team was asked to provide insights drawn from data analysis, we began by focusing on final examination period during academic year. Some advantages of looking into exam period are:
\begin{itemize}
  \item Finals week is a specific time period that occurs twice every academic year (fall and spring semesters), so data pattern recognition is possible.
  \item CCL operates 24 hours as opposed to regular hours throughout a semester. (17 hours Mon-Thur; 14 hours Fri; 11 hours Sat; 16 hours Sun)
  \item Finals week is presumably time of the year when the library resources are most utilized by students, so analytical insights can lead to effective library management.
  \item Finals week is a part of academic calendar that is planned out years in advance, so data prediction is possible.
\end{itemize}

Our research interest lies in the following questions:

\begin{itemize}
\item What kind of students study during the night hours? Where do they study? 
\item What behaviors do students indicate during finals week? What factors do they take into account?
\item Can we predict the study habits and library resource usage of a single student? 
\end{itemize}

To address these questions, the report employs various statistical techniques to assess and interpret the given dataset $Data_{201605}$. First, the report provides a brief description of the data and conducts an exploratory analysis. Basic plots such as boxplot and scatter plot are included to see the appropriateness of the fitted linear model. Second, the report introduces various statistical models and tests that are used in the actual analysis of the dataset. Third, the report conducts a thorough statistical anlysis. In particular, regression diagnostics comes in useful as the resulting statistics give an overall characterization of the measurements and evaluate feasibility of the fitted model. Finally, the report concludes by tying back the analytical results to the aforementioned questions.

\pagebreak