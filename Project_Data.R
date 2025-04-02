
library(rvest)
library(dplyr)
library(tidyverse)

# Scrape NIL contributions
url2 <- "https://nil-ncaa.com/"
page2 <- read_html(url2)

# Extract data
NCAA_Public_Universities <- page2 %>%
  html_elements("#tablepress-S-1 .column-1") %>%
  html_text()

Contributions <- page2 %>%
  html_elements("#tablepress-S-1 .column-5") %>%
  html_text()


College_personal <- page2 %>%
  html_elements("#tablepress-S-1 .column-8") %>%
  html_text()

AD_Revenue<- page2 %>%
  html_elements("#tablepress-S-1 .column-2") %>%
  html_text()

Ticket<- page2 %>%
  html_elements("#tablepress-S-1 .column-4") %>%
  html_text()
Rights<- page2 %>%
  html_elements("#tablepress-S-1 .column-6") %>%
  html_text()
Student_fees<- page2 %>%
  html_elements("#tablepress-S-1 .column-7") %>%
  html_text()
Other<- page2 %>%
  html_elements("#tablepress-S-1 .column-9") %>%
  html_text()


School2 <- page2 %>%
  html_elements("#tablepress-1-N2 .column-1") %>%
  html_text()

Conference <- page2 %>%
  html_elements("#tablepress-1-N2 .column-2") %>%
  html_text()


# Build data frames
Football <- data.frame( 
  School = NCAA_Public_Universities,
  Contributions = as.numeric(gsub(",", "", Contributions)),
  College_personal= as.numeric(gsub(",", "", College_personal)),
  AD_Revenue= as.numeric(gsub(",", "", AD_Revenue)),
  Ticket= as.numeric(gsub(",", "", Ticket)),
  Rights= as.numeric(gsub(",", "", Rights)),
  Student_fees=as.numeric(gsub(",", "", Student_fees)),
  Other = as.numeric(gsub(",", "", Other)),
  
  
  stringsAsFactors = FALSE
)

Football2 <- data.frame(
  School = School2,
  Conference = Conference,
  stringsAsFactors = FALSE
)

# Join the two data frames on School
Football_Combined2<- Football %>%
  inner_join(Football2, by = "School")

Football_Combined2 <- Football_Combined2 %>%
  mutate(across(everything(), ~replace_na(.x, 0)))
Football_Combined2
sorted_data <- Football_Combined2 %>%
  arrange(desc(.[[1]]))
sorted_data

money <- Football_Combined2 %>%
  mutate(School = recode(School, 
                         "Western Michigan" = "Western Mich.",
                         "Western Kentucky" = "Western Ky.",
                         "Washington State" = "Washington St.",
                         "Teaxs-San Antonio" = "UTSA",
                         "Utah State" = "Utah St.",
                         "Texas-El Paso" = "UTEP",
                         "Nevada-Las Vegas" = "UNLV",
                         "Lousiana-Monroe" = "ULM",
                         "Connecticut" = "UConn",
                         "Central Florida" = "UFC",
                         "Alabama at Birmingham" = "UAB",
                         "Texas State" = "Texas St.",
                         "Texas Christian" = "TCU",
                         "Southern Mississippi" = "Southern Miss.",
                         "South Florida" = "South Fla.",
                         "San Jose State" = "San Jose St.",
                         "San Diego State" = "San Diego St.",
                         "Penn State" = "Penn St.",
                         "Oregon State" = "Oregon St.",
                         "Mississippi" = "Ole Miss",
                         "Oklahoma State" = "Ohlahoma St.",
                         "Ohio State" = "Ohio St.",
                         "New Mexico State" = " New Mexico St.",
                         "Northern Illinois" = "NIU",
                         "North Carolina State" = "NC State",
                         "Mississippi State" = "Mississippi St.",
                         "Middle Tennessee" = "Middle Tenn.",
                         "LSU" = "LSU",
                         "Kent State" = "Kent St.",
                         "Kansas State" = "Kansas St.",
                         "Jacksonville State" = "Jacksonville St.",
                         "Iowa State" = "Iowa St.",
                         "Georgia State" = "Georgia St.",
                         "Fresno State" = "Fresno St.",
                         "Florida State" = "Florida St.",
                         "Florida Atlantic" = "Fla. Atlantic",
                         "Florida International" = "FIU",
                         "Eastern Michigan" = "Eastern Mich.",
                         "Colorado State" = "Colorado St.",
                         "Boise State" = "Boise St.",
                         "Ball State" = "Ball St.",
                         "Arkansas State" = "Arkansas St.",
                         "Arizona State" = "Arizona St.",
                         "Connecticut" = "UConn",
                         "Appalachian State" = "App State"
  ))

sorted_data <- money %>%
  arrange(desc(.[[1]]))
sorted_data

library(rvest)
library(dplyr)

url3 <- "https://www.ncaa.com/stats/football/fbs/current/team/926"
page3 <- read_html(url3)

School <- page3 %>%
  html_elements(".school") %>%
  html_text()
DT <- page3 %>%
  html_elements("td:nth-child(6)") %>%
  html_text()
DT
# Scrape NIL contributions

url4 <- "https://www.ncaa.com/stats/football/fbs/current/team/926/p2"
page4 <- read_html(url4)

School2 <- page4 %>%
  html_elements(".school") %>%
  html_text()
DT2 <- page4 %>%
  html_elements("td:nth-child(6)") %>%
  html_text()

url5 <- "https://www.ncaa.com/stats/football/fbs/current/team/926/p3"
page5 <- read_html(url5)

School3 <- page5 %>%
  html_elements(".school") %>%
  html_text()
DT3 <- page5 %>%
  html_elements("td:nth-child(6)") %>%
  html_text()

Defense_touchdown1 <- data.frame(School = School, DT = DT)
Defense_touchdown2 <- data.frame(School = School2, DT = DT2)
Defense_touchdown3 <- data.frame(School = School3, DT = DT3)

Defense_touchdowns <- bind_rows(Defense_touchdown1, Defense_touchdown2, Defense_touchdown3)

offense <- "https://www.ncaa.com/stats/football/fbs/current/team/27"
read1 <- read_html(offense)
School <- read1 %>%
  html_elements("td:nth-child(2)") %>%
  html_text()
TD<-read1 %>%
  html_elements("td:nth-child(4)") %>%
  html_text()
PPG<-read1 %>%
  html_elements("td:nth-child(11)") %>%
  html_text()
Total_points<-read1 %>%
  html_elements("td:nth-child(10)") %>%
  html_text()
Offense <- data.frame(School = School, TD=TD, Offense_Total_points = Total_points, PPG=PPG)
Offense
sorted_data <- Offense %>%
  arrange(desc(.[[1]]))
sorted_data
#page2
offense1 <- "https://www.ncaa.com/stats/football/fbs/current/team/27/p2"
read2 <- read_html(offense1)
School2 <- read2 %>%
  html_elements("td:nth-child(2)") %>%
  html_text()
TD2<-read2 %>%
  html_elements("td:nth-child(4)") %>%
  html_text()
PPG2<-read2 %>%
  html_elements("td:nth-child(11)") %>%
  html_text()
Total_points2<-read2 %>%
  html_elements("td:nth-child(10)") %>%
  html_text()
Offense2 <- data.frame(School = School2, TD=TD2, Offense_Total_points = Total_points2, PPG=PPG2)
Offense2

#page2
offense2 <- "https://www.ncaa.com/stats/football/fbs/current/team/27/p3"
read3 <- read_html(offense2)
School3 <- read3 %>%
  html_elements("td:nth-child(2)") %>%
  html_text()
TD3<-read3 %>%
  html_elements("td:nth-child(4)") %>%
  html_text()
PPG3<-read3 %>%
  html_elements("td:nth-child(11)") %>%
  html_text()
Total_points3<-read3 %>%
  html_elements("td:nth-child(10)") %>%
  html_text()

Offense4 <- data.frame(School = School3, TD=TD3, Offense_Total_points = Total_points3, PPG=PPG3)
Offense4

Total_Offense<-bind_rows(Offense4,Offense2,Offense)
Total_Offense
Total_Offense %>% count(School) %>% filter(n > 1)

sorted_data <- Total_Offense %>%
  arrange(desc(.[[1]]))
sorted_data

Offense10<-"https://www.ncaa.com/stats/football/fbs/current/team/21"
read10 <- read_html(Offense10)
School10<-read10 %>%
  html_elements("td:nth-child(2)") %>%
  html_text()
OYPG10<-read10 %>%
  html_elements("td:nth-child(8)") %>%
  html_text()
Offense9<-"https://www.ncaa.com/stats/football/fbs/current/team/21/p2"
read9 <- read_html(Offense9)
School9<-read9 %>%
  html_elements("td:nth-child(2)") %>%
  html_text()
OYPG9<-read9%>%
  html_elements("td:nth-child(8)") %>%
  html_text()
Offense8<-"https://www.ncaa.com/stats/football/fbs/current/team/21/p3"
read8 <- read_html(Offense8)
School8<-read8 %>%
  html_elements("td:nth-child(2)") %>%
  html_text()
OYPG8<-read8%>%
  html_elements("td:nth-child(8)") %>%
  html_text()
Offense10 <- data.frame(School = School10, OYPG= OYPG10)
Offense9 <- data.frame(School = School9, OYPG= OYPG9)
Offense8 <- data.frame(School = School8, OYPG =OYPG8)
Total_YPG<-bind_rows(Offense10,Offense9,Offense8)
Total_Offense<-Total_Offense %>% 
  inner_join(Total_YPG, by = "School")



Defense <- "https://www.ncaa.com/stats/football/fbs/current/team/22"
def <- read_html(Defense)
School <- def %>%
  html_elements("td:nth-child(2)") %>%
  html_text()
Defense_YDS_play<-def %>%
  html_elements("td:nth-child(6)") %>%
  html_text()
Defense_YPG<-def %>%
  html_elements("td:nth-child(9)") %>%
  html_text()
OPP_TDS<-def %>%
  html_elements("td:nth-child(8)") %>%
  html_text()
Defense_1<-data.frame(School=School,Defense_YDS_play=Defense_YDS_play, Defense_YPG=Defense_YPG, OPP_TDS=OPP_TDS )
Defense_1
Defense1 <- "https://www.ncaa.com/stats/football/fbs/current/team/22/p2"
def2 <- read_html(Defense1)
School1 <- def2 %>%
  html_elements("td:nth-child(2)") %>%
  html_text()
Defense_YDS_play1<-def2 %>%
  html_elements("td:nth-child(6)") %>%
  html_text()
Defense_YPG1<-def2 %>%
  html_elements("td:nth-child(9)") %>%
  html_text()
OPP_TDS1<-def2 %>%
  html_elements("td:nth-child(8)") %>%
  html_text()
Defense_2<-data.frame(School=School1,Defense_YDS_play=Defense_YDS_play1, Defense_YPG=Defense_YPG1, OPP_TDS=OPP_TDS1 )
Defense_2

Defense2 <- "https://www.ncaa.com/stats/football/fbs/current/team/22/p3"
def3 <- read_html(Defense2)
School2 <- def3%>%
  html_elements("td:nth-child(2)") %>%
  html_text()
Defense_YDS_play2<-def3 %>%
  html_elements("td:nth-child(6)") %>%
  html_text()
Defense_YPG2<-def3 %>%
  html_elements("td:nth-child(9)") %>%
  html_text()
OPP_TDS2<-def3 %>%
  html_elements("td:nth-child(8)") %>%
  html_text()
Defense_3<-data.frame(School=School2,Defense_YDS_play=Defense_YDS_play2, Defense_YPG=Defense_YPG2, OPP_TDS=OPP_TDS2 )
Total_Defense<-bind_rows(Defense_1,Defense_2,Defense_3)
Total_Defense

Total<- Total_Offense %>% 
  inner_join(Total_Defense, by = "School")
sorted_data <- Total %>%
  arrange(desc(.[[1]]))
sorted_data


Win <- "https://www.ncaa.com/standings/football/fbs"
winners <- read_html(Win)
Wins <- winners %>%
  html_elements(".standings-team") %>%
  html_text()
WPY<- winners %>%
  html_elements("td:nth-child(4)") %>%
  html_text()

Wins_Per_Year <- data.frame(School= Wins,WPY )
Wins_Per_Year


Total1<- Total %>% 
  inner_join(Wins_Per_Year, by = "School")
Total1
sorted_data <- Total1 %>%
  arrange(desc(.[[1]]))
sorted_data



Total_stat<-
  CFB<-"https://www.ncaa.com/news/football/article/2025-01-20/teams-most-college-football-playoff-wins-and-appearances"
winners1 <- read_html(CFB)
Wins2 <- winners1 %>%
  html_elements("table~ table td:nth-child(1)") %>%
  html_text()
CFB_Apperances<- winners1 %>%
  html_elements("table~ table td:nth-child(2)") %>%
  html_text()
CFB_totals <- data.frame(School=Wins2,CFB_Apperances=CFB_Apperances )
CFB_totals
CFP_T <- CFB_totals %>%
  mutate(School = recode(School, 
                         "Boise State" = "Boise St.",
                         "Penn State"= "Penn St.",
                         "Michigan State"="Michigan St.",
                         "Florida State"="Florida St.",
                         "Ohio State"="Ohio St.",
                         "Arizona State"="Arizona St."))
CFP_T              

Total4<- Total1 %>%
  full_join(money, by = "School")
Total4
Total4 %>% count(School) %>% filter(n > 1)
Total5<- Total4 %>%
  full_join(CFP_T, by = "School")
Total5



clean_Final <- Total5 %>%
  filter(!is.na(AD_Revenue))%>%
  filter(!is.na(TD))
power4 <- c("Big Ten", "Big 12", "ACC", "SEC")

clean_Final$Power4 <- ifelse(clean_Final$Conference %in% power4, 1, 0)
clean_Final
clean_Final$CFB_Apperances[is.na(clean_Final$CFB_Apperances)] <- 0
clean_Final$CFP <- ifelse(clean_Final$CFB_Apperances > 0, 1, 0)

clean_Final$WPY <- as.numeric(as.character(clean_Final$WPY))
clean_Final$CFB_Apperances <- as.numeric(as.character(clean_Final$CFB_Apperances))
clean_Final$Other <- as.numeric(as.character(clean_Final$Other))
clean_Final$Rights <- as.numeric(as.character(clean_Final$Rights))
clean_Final$Ticket <- as.numeric(as.character(clean_Final$Ticket))
clean_Final$AD_Revenue <- as.numeric(as.character(clean_Final$AD_Revenue))
clean_Final$College_personal <- as.numeric(as.character(clean_Final$College_personal))
clean_Final$Contributions <- as.numeric(as.character(clean_Final$Contributions))
clean_Final$OPP_TDS <- as.numeric(as.character(clean_Final$OPP_TDS))
clean_Final$Defense_YPG <- as.numeric(as.character(clean_Final$Defense_YPG))
clean_Final$Defense_YDS_play <- as.numeric(as.character(clean_Final$Defense_YDS_play))
clean_Final$PPG <- as.numeric(as.character(clean_Final$PPG))
clean_Final$Offense_Total_points <- as.numeric(as.character(clean_Final$Offense_Total_points))
clean_Final$TD <- as.numeric(as.character(clean_Final$TD))
clean_Final$OYPG <- as.numeric(as.character(clean_Final$OYPG))
money$Contributions <- as.numeric(as.character(money$Contributions))
SEC <- clean_Final %>%
  filter(str_detect(Conference, "SEC"))
SEC
ACC<-clean_Final %>%
  filter(str_detect(Conference, "ACC"))
ACC
Big_10<-clean_Final %>%
  filter(str_detect(Conference, "Big Ten"))
Big_10
Big_12<-clean_Final %>%
  filter(str_detect(Conference, "Big 12"))
Big_12
Out_Power_4<-clean_Final %>%
  filter(!str_detect(Conference, "Big Ten"))%>%
  filter(!str_detect(Conference, "SEC"))%>%
  filter(!str_detect(Conference, "ACC"))%>%
  filter(!str_detect(Conference, "Big 12"))
Out_Power_4
Big_12<-clean_Final %>%
  filter(str_detect(Conference, "Big 12"))
Power4 <- clean_Final %>%
  filter(str_detect(Conference, "SEC|Big Ten|ACC|Big 12"))

College_Data <- clean_Final %>%
  filter(!is.na(AD_Revenue))%>%
  filter(!is.na(TD))
College_Data







