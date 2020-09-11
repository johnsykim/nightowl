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

%-------------------------DATA-------------------------------------

\section{2. Data}

\subsection{\textbf{2.1 Data Description}}

The $Data_{201605}$ that we are using is a data that describes the wifi usage at the Honnold-Mudd Library across the entire month of May. The data measures each connection to the wifi router and featuers all these various aspect of the single connection described in Figure~\ref{fig:Variables}:

\begin{figure}[ht!]
\begin{center}
	\includegraphics[width=11cm]{variables.JPG}
    \caption{Names of variables in data_201605 dataset}
	\label{fig:Variables}
\end{center}
\end{figure}

The variables above can be categorized into several different types. Namely,

\begin{itemize}
\item \textbf{Categorical}: Campus, Role, Device_Name, Group, Folder, Device_Location, Vendor, Connection_Mode, Device_Type, Manufacturer, Model, OS
\item \textbf{Numeric}
  \begin{itemize}
    \item \textbf{Integer}: Connect_Year, Connect_Month, Connect_Day, Connect_Hour, Connect_Minute, Connect_Second, Connect_DW, Connect_DY, TT, TT_In, TT_Out, Avg_Usage, Avg_Signal, Avg_Signal_Quality
    \item \textbf{Double}: Connect_DST, Disconnect_Time, Duration
  \end{itemize}
\end{itemize}

In our study, we decide to study the efficiency of opening up the library 24/7 during Finals, which is known as the \textit{Night Owl} program. Therefore, of the May data, we parse out the days that correlate the Finals period, namely May $4^{th}$ to $12^{th}$. Of this time, too, we decide to focus on the Night Owl hours of finals week, since that is the unique pattern that sets the finals week aside from the \\.

This means that we will make heavy use of the Connect_Hour variable, which signifies the start time of a connection. We can look at how many students there are per connection hour, but we can also use it convineintly divide the data into regular hours and nightowl hour to determine the difference in other parameters such as the location of the connection. \\

In our study, we mainly use the \textit{Campus} variable -- the school afilliation -- \textit{Device_Location}, the location of connection, \textit{Avg_Usage}, the mean of the traffic of a connection, and \textit{Duration}, the duration of a single connection. 


\subsection{\textbf{2.2 Exploratory Data Analysis}}

First we look at a simple histogram to see how much people use the library at night owl hours during the finals week: 

<<echo = FALSE, out.width='4in', fig.align='center', fig.show='hold'>>=

hist(mayFinalsData$Connect_Hour, main = "Connect Hour for Finals Week", xlab = "Hour")
@

From a histogram, we see that the connection peaks around midday, and after 1am, when the night owls program starts, the connection frequency drops by a lot. This justifies our motivation to study the finals week: clearly, the library usage during the special hours are extremely low and we want to see if we can optimize the library resources; usage is low, so perhaps we could leave the library open in only certain locations? If a certain campus uses more night owl hours, is that indicative of poor resources at their home insitution? 

<<echo = FALSE, out.width='3in', fig.align='center', fig.show='hold'>>=
nightO <- read.csv("D:\\C\\ThisShitisFuckingAnnoying\\R data\\Honnold Data\\rawWifi2016\\mayData\\foo.csv",
                    header=TRUE)

day<-mayFinalsData[ !(mayFinalsData %in% nightO), ]


pom <- sum(with(day, Campus == "pomona"))
cmc <- sum(with(day, Campus == "cmc"))
scr <- sum(with(day, Campus == "scripps"))
hmc <- sum(with(day, Campus == "hmc"))
pit <- sum(with(day, Campus == "pitzer"))
cgu <- sum(with(day, Campus == "cgu"))
kgi <- sum(with(day, Campus == "kgi"))
cuc <- sum(with(day, Campus == "cuc"))


lbls <- c("Pomona", "CMC", "Scripps", "HMC", "Pitzer", "CGU", "KGI", "CUC")
slices<- c(pom, cmc, scr, hmc, pit, cgu, kgi, cuc)


pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Library network entries by institution")


#night owl
pom <- sum(with(nightO, Campus == "pomona"))
cmc <- sum(with(nightO, Campus == "cmc"))
scr <- sum(with(nightO, Campus == "scripps"))
hmc <- sum(with(nightO, Campus == "hmc"))
pit <- sum(with(nightO, Campus == "pitzer"))
cgu <- sum(with(nightO, Campus == "cgu"))
kgi <- sum(with(nightO, Campus == "kgi"))
cuc <- sum(with(nightO, Campus == "cuc"))


lbls <- c("Pomona", "CMC", "Scripps", "HMC", "Pitzer", "CGU", "KGI", "CUC")
slices<- c(pom, cmc, scr, hmc, pit, cgu, kgi, cuc)


pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Library network entries by institution (Night owl)")
@

We can also look at locations used in the library depending on normal hours and night owl data:

<<echo = FALSE, out.width='3in', fig.align='center', fig.show='hold'>>=

F1 <- sum(with(mayFinalsData, Folder == "1F"))
F2 <- sum(with(mayFinalsData, Folder == "2F"))
F3 <- sum(with(mayFinalsData, Folder == "3F"))
F4 <- sum(with(mayFinalsData, Folder == "4F"))
Cafe <- sum(with(mayFinalsData, Folder == "Cafe"))

lbls <- c("1F", "2F", "3F", "4F", "Cafe")
slices <- c(F1, F2, F3, F4, Cafe)

pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Library network entries by location")

  nightO <- read.csv("D:\\C\\ThisShitisFuckingAnnoying\\R data\\Honnold Data\\rawWifi2016\\mayData\\foo.csv",
                     header=TRUE)  
  day<-mayFinalsData[ !(mayFinalsData %in% nightO), ]

  F1 <- sum(with(nightO, Folder == "1F"))
  F2 <- sum(with(nightO, Folder == "2F"))
  F3 <- sum(with(nightO, Folder == "3F"))
  F4 <- sum(with(nightO, Folder == "4F"))
  Cafe <- sum(with(nightO, Folder == "Cafe"))
  
  lbls <- c("1F", "2F", "3F", "4F", "Cafe")
  slices <- c(F1, F2, F3, F4, Cafe)
  
  pct <- round(slices/sum(slices)*100)
  lbls <- paste(lbls, pct) # add percents to labels 
  lbls <- paste(lbls,"%",sep="") # ad % to labels 
  pie(slices,labels = lbls, col=rainbow(length(lbls)),
      main="Library network entries by location (Night Owls)")
@

%Lauren
To explore this data, we constructed pie charts to examine the individuals using the library internet broken down by campus during finals season. We were interested in comparing if these results would be different during the typical library hours, from 8 AM to 1 AM, and Night Owl hours that are open solely during finals season from 1 AM to 8 AM. During regular library hours, we found that Scripps students had the highest percentage of connections, 23$\%$, followed by Pitzer, Claremont McKenna and Claremont Graduate University each at 18$\%$. Pomona students accounted for 12$\%$, followed by CUC, KGI, and Harvey Mudd. When we compared these values to those from Night Owl hours, we did not see any significant changes, and overall trends were maintained. \\

We next broke down the individuals occupying each floor during finals week by campus to determine whether some floors tended to be used by certain schools. 

<<echo = FALSE, out.width='3in', fig.align='center', fig.show='hold'>>=
#{r floor 1 by hr}
day$Folder<-sub("Top > CUC > Honold Mudd Library > First Floor", "1F", day$Folder)
day$Folder<-sub("Top > CUC > Honold Mudd Library > Second Floor", "2F", day$Folder)
day$Folder<-sub("Top > CUC > Honold Mudd Library > Third Floor", "3F", day$Folder)
day$Folder<-sub("Top > CUC > Honold Mudd Library > Fourth Floor", "4F", day$Folder)


nightO$Folder<-sub("Top > CUC > Honold Mudd Library > First Floor", "1F", nightO$Folder)
nightO$Folder<-sub("Top > CUC > Honold Mudd Library > Second Floor", "2F", nightO$Folder)
nightO$Folder<-sub("Top > CUC > Honold Mudd Library > Third Floor", "3F", nightO$Folder)
nightO$Folder<-sub("Top > CUC > Honold Mudd Library > Fourth Floor", "4F", nightO$Folder)


pom<-sum(with(day,Campus=="pomona"&Folder=="1F"))
cmc<-sum(with(day,Campus=="cmc"&Folder=="1F"))
scr<-sum(with(day,Campus=="scripps"&Folder=="1F"))
hmc<-sum(with(day,Campus=="hmc"&Folder=="1F"))
pit<-sum(with(day,Campus=="pitzer"&Folder=="1F"))
cgu<-sum(with(day,Campus=="cgu"&Folder=="1F"))
kgi<-sum(with(day,Campus=="kgi"&Folder=="1F"))
cuc<-sum(with(day,Campus=="cuc"&Folder=="1F"))


lbls <- c("Pomona", "CMC", "Scripps", "HMC", "Pitzer", "CGU", "KGI", "CUC")
slices<- c(pom, cmc, scr, hmc, pit, cgu, kgi, cuc)


pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Floor 1 by institution day")


pom<-sum(with(nightO,Campus=="pomona"&Folder=="1F"))
cmc<-sum(with(nightO,Campus=="cmc"&Folder=="1F"))
scr<-sum(with(nightO,Campus=="scripps"&Folder=="1F"))
hmc<-sum(with(nightO,Campus=="hmc"&Folder=="1F"))
pit<-sum(with(nightO,Campus=="pitzer"&Folder=="1F"))
cgu<-sum(with(nightO,Campus=="cgu"&Folder=="1F"))
kgi<-sum(with(nightO,Campus=="kgi"&Folder=="1F"))
cuc<-sum(with(nightO,Campus=="cuc"&Folder=="1F"))


lbls <- c("Pomona", "CMC", "Scripps", "HMC", "Pitzer", "CGU", "KGI", "CUC")
slices<- c(pom, cmc, scr, hmc, pit, cgu, kgi, cuc)


pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)), main="Floor 1 by institution night")


#{r floor 2 by hr}
pom<-sum(with(day,Campus=="pomona"&Folder=="2F"))
cmc<-sum(with(day,Campus=="cmc"&Folder=="2F"))
scr<-sum(with(day,Campus=="scripps"&Folder=="2F"))
hmc<-sum(with(day,Campus=="hmc"&Folder=="2F"))
pit<-sum(with(day,Campus=="pitzer"&Folder=="2F"))
cgu<-sum(with(day,Campus=="cgu"&Folder=="2F"))
kgi<-sum(with(day,Campus=="kgi"&Folder=="2F"))
cuc<-sum(with(day,Campus=="cuc"&Folder=="2F"))


lbls <- c("Pomona", "CMC", "Scripps", "HMC", "Pitzer", "CGU", "KGI", "CUC")
slices<- c(pom, cmc, scr, hmc, pit, cgu, kgi, cuc)


pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Floor 2 by institution day")


pom<-sum(with(nightO,Campus=="pomona"&Folder=="2F"))
cmc<-sum(with(nightO,Campus=="cmc"&Folder=="2F"))
scr<-sum(with(nightO,Campus=="scripps"&Folder=="2F"))
hmc<-sum(with(nightO,Campus=="hmc"&Folder=="2F"))
pit<-sum(with(nightO,Campus=="pitzer"&Folder=="2F"))
cgu<-sum(with(nightO,Campus=="cgu"&Folder=="2F"))
kgi<-sum(with(nightO,Campus=="kgi"&Folder=="2F"))
cuc<-sum(with(nightO,Campus=="cuc"&Folder=="2F"))


lbls <- c("Pomona", "CMC", "Scripps", "HMC", "Pitzer", "CGU", "KGI", "CUC")
slices<- c(pom, cmc, scr, hmc, pit, cgu, kgi, cuc)


pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Floor 2 by institution night")