#The libraries are loaded
library(tidyverse)
library(DT)


# Import data
deliveries= read.csv("deliveries.csv", stringsAsFactors = F)
matches= read.csv("matches.csv", stringsAsFactors = F)

#Teams with most wins(TOP 10)
most_wins= matches %>%
  group_by(winner) %>%
  dplyr::summarise(Total_wins=n()) %>%
  top_n(10) %>%
  arrange(desc(Total_wins))
datatable(most_wins)

#We can now plot a corresponding bar graph for it
ggplot(most_wins,aes(reorder(winner,Total_wins),Total_wins,fill=winner,label=Total_wins))+
  geom_bar(stat = "identity", width = 0.5, alpha = 0.5)+
  geom_point(size=9, color="black",alpha=0.07)+
  scale_x_discrete(labels = most_wins[order(most_wins$Total_wins),]$winner) +
  theme_classic()+
  geom_text(aes(winner,Total_wins,label =Total_wins))+
  labs(x = "Team", y = "Number of win", title = "Highest match win by Team", subtitle = "")+
  coord_flip()+
  theme(axis.text = element_text(size = 12, face = "bold"), title  =  element_text(size = 16))+
  theme(legend.title = element_text(size=10),legend.text = element_text(size=10),legend.position="bottom")


#From the plot we can conclude that Mumbai Indians is the most successful team in terms of number of wins.


#In most matches, the team wins by batting first. (TOP 10) 

most_win_by_runs=matches %>%
  filter(win_by_runs!=0)%>%
  group_by(winner) %>%
  dplyr::summarise(Total_win=n()) %>%
  arrange(desc(Total_win))%>%
  top_n(10)
datatable(most_win_by_runs)  

ggplot(most_win_by_runs,aes(reorder(winner,Total_win),Total_win,fill=winner,label=Total_win))+
  geom_bar(stat = "identity", width = 0.5, alpha = 0.5)+
  geom_point(size=9, color="black",alpha=0.07)+
  scale_x_discrete(labels = most_win_by_runs[order(most_win_by_runs$Total_win),]$winner) +
  theme_classic()+
  geom_text(aes(winner,Total_win, label = Total_win))+
  labs(x = "Team", y = "Win", title = "Highest match win by run", subtitle = "")+
  coord_flip()+
  theme(axis.text = element_text(size = 14, face = "bold"),title  =  element_text(size = 16))+
  theme(legend.title = element_text(size=10),legend.text = element_text(size=10),legend.position="bottom")

#From the above plot, we can see that Mumbai Indians have won the most matches when batting first.

#Teams winning by highest margin of runs

highest_win_margin = matches %>%
  select(winner, season,win_by_runs) %>%
  arrange(desc(win_by_runs)) %>%
  top_n(10) %>%
  ungroup() %>% 
  mutate(Rank = as.factor(1:10))
datatable(highest_win_margin)

ggplot(highest_win_margin)+
  aes(reorder(Rank, win_by_runs), win_by_runs,fill=winner,label=win_by_runs)+
  geom_bar(stat = "identity", width = 0.3, alpha = 0.5)+
  geom_point(size=9, color="black",alpha=0.07)+
  scale_x_discrete(labels = highest_win_margin[order(highest_win_margin$win_by_runs),]$winner) +
  theme_classic()+
  geom_text(aes(Rank, win_by_runs, label = win_by_runs))+
  labs(x = "", y = "", title = "Highest win by runs", subtitle = "")+
  coord_flip()+
  theme(axis.text = element_text(size = 12, face = "bold"), title  =  element_text(size = 16))+
  theme(legend.title = element_text(size=10),legend.text = element_text(size=10),legend.position="bottom")

#From the above plot, we can see that Mumbai Indians have won by the biggest margin. 

#Narrowest win margin (by 1 run)

win_by_one_runs=matches %>%
  select(team1,team2,venue,city,season,winner,win_by_runs)%>%
  filter(win_by_runs==1)%>%
  top_n(10)%>%
  arrange(season)
datatable(win_by_one_runs)




#In most matches, the team wins by bowling first.
most_win_by_Wickets=matches %>%
  filter(win_by_wickets!=0)%>%
  group_by(winner) %>%
  dplyr::summarise(Total_win=n()) %>%
  arrange(desc(Total_win))%>%
  top_n(10)
datatable(most_win_by_Wickets)  

ggplot(most_win_by_Wickets,aes(reorder(winner,Total_win),Total_win,fill=winner))+
  geom_bar(stat = "identity", width = 0.5, alpha = 0.5)+
  geom_point(size=9, color="black",alpha=0.07)+
  scale_x_discrete(labels = most_win_by_Wickets[order(most_win_by_Wickets$Total_win),]$winner) +
  theme_classic()+
  geom_text(aes(winner,Total_win, label = Total_win))+
  labs(x = "Team", y = "Win", title = "Highest match win by run", subtitle = "")+
  coord_flip()+
  theme(axis.text = element_text(size = 12, face = "bold"), title  =  element_text(size = 16))+
  theme(legend.title = element_text(size=10),legend.text = element_text(size=10),legend.position="bottom")

#From the above plot, we can see that "Kolkata Knight Riders" have won the most matches bowling first.

#Teams with most win by wickets margin(TOP 10)

highest_win_margin_by_wicket = matches %>%
  select(winner,season,win_by_wickets) %>%
  arrange(desc(win_by_wickets)) %>%
  head(10) %>%
  ungroup() %>%
  mutate(Rank = as.factor(1:10))
datatable(highest_win_margin_by_wicket)

ggplot(highest_win_margin_by_wicket)+
  aes(reorder(Rank, win_by_wickets), win_by_wickets,fill=winner)+
  geom_bar(stat = "identity", width = 0.3, alpha = 0.5)+
  geom_point(size=9, color="black",alpha=0.07)+
  scale_x_discrete(labels = highest_win_margin_by_wicket[order(highest_win_margin_by_wicket$win_by_wickets),]$winner) +
  theme_classic()+
  geom_text(aes(Rank, win_by_wickets, label = win_by_wickets))+
  labs(x = "", y = "", title = "Highest win margin by wicket", subtitle = "")+
  coord_flip()+
  theme(axis.text = element_text(size = 12, face = "bold"), title  =  element_text(size = 16))+
  theme(legend.title = element_text(size=10),legend.text = element_text(size=10),legend.position="bottom")

#From above plot ,we can see all team has won by largest wicket margin. 

#Narrowest win margins (by wickets)
lowest_win_margin_by_wicket= matches%>%
  select(team1,team2,venue,city,winner,season,win_by_wickets)%>%
  filter(win_by_wickets!=0)%>%
  arrange(win_by_wickets) %>%
  head(10)
datatable(lowest_win_margin_by_wicket)


#Teams scoring highest runs in a match

Teams_scoring_highest_runs = deliveries %>%
  group_by(batting_team,match_id)%>%
  summarise(Total_run=sum(total_runs))%>%
  arrange(desc(Total_run))%>%
  select(batting_team,Total_run)%>%
  head(10)
datatable(Teams_scoring_highest_runs)


### Batsman Record #####

#Most successful batsman runs scored(TOP 10)
batsman_total_run=deliveries%>%
  group_by(batsman)%>%
  dplyr::summarise(Run=sum(batsman_runs))%>%
  arrange(desc(Run))%>%
  top_n(10)
datatable(batsman_total_run)

ggplot(batsman_total_run)+
  aes(reorder(batsman,Run), Run,fill=batsman)+
  geom_bar(stat = "identity", width = 0.3, alpha = 0.5)+
  geom_point(size=9, color="black",alpha=0.07)+
  scale_x_discrete(labels = batsman_total_run[order(batsman_total_run$Run),]$batsman) +
  theme_classic()+
  geom_text(aes(batsman, Run, label = Run))+
  labs(x = "Batsman Name", y = "", title = "Highest Runs by batsman in IPL", subtitle = "")+
  coord_flip()+
  theme(axis.text = element_text(size = 12, face = "bold"), title  =  element_text(size = 16))+
  theme(legend.title = element_text(size=10),legend.text = element_text(size=10),legend.position="bottom")

#From the above plot, we can see the highest runs scored by V Kohli in IPL.

#Highest score by a batsman in a match
Highest_individual_score =deliveries %>%
  group_by(batsman,match_id) %>%
  dplyr::summarise(Run=sum(batsman_runs))%>%
  arrange(-Run)%>%
  select(batsman,Run)%>%
  head(10)%>%
  ungroup()%>%
  mutate(Rank=as.factor(1:10))

datatable(Highest_individual_score)

ggplot(Highest_individual_score)+
  aes(reorder(Rank,Run), Run,fill=batsman,label=Run)+
  geom_bar(stat = "identity", width = 0.3,alpha=0.5)+
  geom_point(size=9, color="black",alpha=0.07)+
  scale_x_discrete(labels = Highest_individual_score[order(Highest_individual_score$Run),]$batsman) +
  theme_classic()+
  geom_text(aes(Rank, Run, label = Run))+
  labs(x = "Batsman Name", y = "Scores", title = "Highest Runs score by batsman in one matche IPL", subtitle = "")+
  coord_flip()+
  theme(axis.text = element_text(size = 12, face = "bold"), title  =  element_text(size = 16))+
  theme(legend.title = element_text(size=10),legend.text = element_text(size=10),legend.position="bottom")

#From the above plot, we can see the highest runs scored by CH Gayle in Match. 

#Highest run scorer of a team
Highest_run_in_team= deliveries %>%
  group_by(batting_team,batsman)%>%
  summarise(Run=sum(batsman_runs))%>%
  arrange(desc(Run))%>%
  head(10)%>%
  ungroup()%>%
  mutate(Rank=as.factor(1:10))

datatable(Highest_run_in_team)
ggplot(Highest_run_in_team)+
  aes(reorder(Rank,Run), Run,fill=batting_team)+
  geom_bar(stat = "identity", width = 0.5,alpha=0.5)+
  geom_point(size=9, color="black",alpha=0.07)+
  scale_x_discrete(labels = Highest_run_in_team[order(Highest_run_in_team$Run),]$batsman) +
  theme_classic()+
  geom_text(aes(Rank,Run,label = Run))+
  labs(x = "Batsman Name", y = "Scores", title = "Highest Runs score by batsman in one team", subtitle = "")+
  coord_flip()+
  theme(axis.text = element_text(size = 12, face = "bold"), title  =  element_text(size = 16))+
  theme(legend.title = element_text(size=10),legend.text = element_text(size=10),legend.position="bottom")

#From the above plot, we can see the most runs scored by V Kohli in "Royal Challengers Bangalore Team".

#Batsman with Top Strike Rate(min.1000 Run faced)

Batsman_with_Top_Strike= deliveries %>%
  group_by(batsman)%>%
  filter(length(batsman_runs)>1000)%>%
  summarise(strikerate=mean(batsman_runs)*100)%>%
  arrange(desc(strikerate))%>%
  head(10)%>%
  ungroup()%>%
  mutate(Rank=as.factor(1:10))


datatable(Batsman_with_Top_Strike)

ggplot(Batsman_with_Top_Strike)+
  aes(reorder(Rank,strikerate), strikerate,fill=batsman)+
  geom_bar(stat = "identity", width = 0.5,alpha=0.5)+
  geom_point(size=9, color="black",alpha=0.07)+
  scale_x_discrete(labels = Batsman_with_Top_Strike[order(Batsman_with_Top_Strike$strikerate),]$batsman) +
  theme_classic()+
  geom_text(aes(Rank,strikerate,label = strikerate))+
  labs(x = "Batsman Name", y = "Strike Rate", title = "Batsman with Top Strike Rate in IPL", subtitle = "")+
  coord_flip()+
  theme(axis.text = element_text(size = 12, face = "bold"), title  =  element_text(size = 16))+
  theme(legend.title = element_text(size=10),legend.text = element_text(size=10),legend.position="bottom")

#From the above plot, we can see the batsman "RR Pant" with the top strike rate in IPL. 

### Bowler Record ###

#Most successful bowler wickets taken in IPL

successful_bowler= deliveries %>%
  filter(dismissal_kind != "run out",player_dismissed !="") %>%
  group_by(bowler) %>%
  summarise(Total_Wickets=sum(table(dismissal_kind))) %>%
  arrange(desc(Total_Wickets))%>%
  top_n(10)

datatable(successful_bowler)

ggplot(successful_bowler)+
  aes(reorder(bowler,Total_Wickets), Total_Wickets,fill=bowler)+
  geom_bar(stat = "identity", width = 0.5,alpha=0.5)+
  geom_point(size=9, color="black",alpha=0.07)+
  scale_x_discrete(labels = successful_bowler[order(successful_bowler$Total_Wickets),]$bowler) +
  theme_classic()+
  geom_text(aes(bowler,Total_Wickets,label = Total_Wickets))+
  labs(x = "Batsman Name", y = "Strike Rate", title = "Most successful bowler wickets taken in IPL")+
  coord_flip()+
  theme(axis.text = element_text(size = 12, face = "bold"), title  =  element_text(size = 16))+
  theme(legend.title = element_text(size=10),legend.text = element_text(size=10),legend.position="bottom")

#From the above plot, we can see most wickets taken by "SL Malinga" in IPL.


#Bowlers with 5 or more wickets

highest_Wickets_by_bowler= deliveries %>%
  filter(dismissal_kind != "run out",player_dismissed !="") %>%
  group_by(bowler,match_id) %>%
  summarise(Total_Wickets=sum(table(dismissal_kind))) %>%
  arrange(desc(Total_Wickets))%>%
  filter(Total_Wickets >= 5) %>% 
  select(bowler,Total_Wickets) %>%
  head(10)
datatable(highest_Wickets_by_bowler)


ggplot(highest_Wickets_by_bowler)+
  aes(reorder(bowler,Total_Wickets), Total_Wickets,fill=bowler)+
  geom_bar(stat = "identity", width = 0.5,alpha=0.5)+
  geom_point(size=9, color="black",alpha=0.07)+
  scale_x_discrete(labels = highest_Wickets_by_bowler[order(highest_Wickets_by_bowler$Total_Wickets),]$bowler) +
  theme_classic()+
  geom_text(aes(bowler,Total_Wickets,label = Total_Wickets))+
  labs(x = "Batsman Name", y = "", title = "Most successful bowler wickets taken in IPL")+
  coord_flip()+
  theme(axis.text = element_text(size = 12, face = "bold"), title  =  element_text(size = 16))+
  theme(legend.title = element_text(size=10),legend.text = element_text(size=10),legend.position="bottom")

#From the above plot, we can see "5 and more than 5 wickets", taken by a lot of bowlers. 


#Most Maiden Overs in IPL 2008-19
Maiden_Overs= deliveries %>%
  group_by(bowler,match_id,over)%>%
  summarise(Run=sum(total_runs))%>%
  filter(Run==0)%>%
  group_by(bowler)%>%
  summarise(maiden_over=sum(table(over)))%>%
  arrange(desc(maiden_over))%>%
  head((10))

datatable(Maiden_Overs)

ggplot(Maiden_Overs)+
  aes(reorder(bowler,maiden_over), maiden_over,fill=bowler)+
  geom_bar(stat = "identity", width = 0.5,alpha=0.5)+
  geom_point(size=9, color="black",alpha=0.07)+
  scale_x_discrete(labels = Maiden_Overs[order(Maiden_Overs$maiden_over),]$bowler) +
  theme_classic()+
  geom_text(aes(bowler,maiden_over,label = maiden_over))+
  labs(x = "Batsman Name", y = "", title = "Most successful bowler wickets taken in IPL")+
  coord_flip()+
  theme(axis.text = element_text(size = 12, face = "bold"), title  =  element_text(size = 16))+
  theme(legend.title = element_text(size=10),legend.text = element_text(size=10),legend.position="bottom")

#From the above plot, we can see most of the maiden overs done by "P Kumar". 


#Highest Wicket Taker of a team

Highest_Wicket_Taker_of_a_team=deliveries %>%
  filter(dismissal_kind != "run out",player_dismissed !="") %>%
  group_by(bowling_team, bowler) %>% 
  summarise(Total_wickets = sum(table(dismissal_kind))) %>%
  group_by(bowling_team) %>% 
  slice(which.max(Total_wickets))%>%
  arrange(desc(Total_wickets)) %>% 
  head(10)

datatable(Highest_Wicket_Taker_of_a_team)

ggplot(Highest_Wicket_Taker_of_a_team)+
  aes(reorder(bowler,Total_wickets), Total_wickets,fill=bowling_team)+
  geom_bar(stat = "identity", width = 0.5,alpha=0.5)+
  geom_point(size=9, color="black",alpha=0.07)+
  scale_x_discrete(labels = Highest_Wicket_Taker_of_a_team[order(Highest_Wicket_Taker_of_a_team$Total_wickets),]$bowler) +
  theme_classic()+
  geom_text(aes(bowler,Total_wickets,label = Total_wickets))+
  labs(x = "Batsman Name", y = "", title = "Most successful bowler wickets taken in IPL")+
  coord_flip()+
  theme(axis.text = element_text(size = 12, face = "bold"), title  =  element_text(size = 16))+
  theme(legend.title = element_text(size=10),legend.text = element_text(size=10),legend.position="bottom")

#From the above plot, we can see the bowler "SL Malinga" taking the most wickets in a team "Mumbai Indians". 


#Highest individual wickets against a team (aggregate)
Highest_individual_wickets_against_a_team=deliveries %>%
  filter(dismissal_kind != "run out",player_dismissed !="") %>%
  group_by(batting_team, bowler) %>% 
  summarise(Total_wickets= sum(table(dismissal_kind))) %>%
  group_by(batting_team) %>%
  slice(which.max(Total_wickets)) %>%
  arrange(desc(Total_wickets)) %>% 
  head(20)

datatable(Highest_individual_wickets_against_a_team)


### Batsman vs Bowler Record ###


#Most dismissed batsman by a bowler

Most_dismissed_batsman_by_a_bowler=deliveries%>%
  filter(dismissal_kind != "run out",player_dismissed !="") %>%
  group_by(bowler, batsman) %>% 
  summarise(Total_Wickets = sum(table(dismissal_kind))) %>%
  arrange(desc(Total_Wickets)) %>% 
  head(20)
datatable(Most_dismissed_batsman_by_a_bowler)

#Batsman with most Runs against a single bowler

Batsman_with_most_Runs_against_a_single_bowler= deliveries %>%
  group_by(batsman,bowler)%>%
  summarise(Total_runs=sum(table(batsman_runs)))%>%
  arrange(desc(Total_runs))%>%
  head(10)

datatable(Batsman_with_most_Runs_against_a_single_bowler)  
  

# Most number of Man of the Match Awards


Man_of_the_Match_Awards= matches%>%
  group_by(player_of_match)%>%
  summarise(Total=n())%>%
  arrange(desc(Total))%>%
  head(10)
datatable(Man_of_the_Match_Awards)

ggplot(Man_of_the_Match_Awards)+
  aes(reorder(player_of_match,Total), Total,fill=player_of_match)+
  geom_bar(stat = "identity", width = 0.5,alpha=0.5)+
  geom_point(size=9, color="black",alpha=0.07)+
  scale_x_discrete(labels = Man_of_the_Match_Awards[order(Man_of_the_Match_Awards$Total),]$player_of_match) +
  theme_classic()+
  geom_text(aes(player_of_match,Total,label = Total))+
  labs(x = "Batsman Name", y = "", title = "Most successful bowler wickets taken in IPL")+
  coord_flip()+
  theme(axis.text = element_text(size = 12, face = "bold"), title  =  element_text(size = 16))+
  theme(legend.title = element_text(size=10),legend.text = element_text(size=10),legend.position="bottom")

#From the above plot, we can see most of the Man of the Match awards taken by "CH Gayle". 


#Decision at Toss
Toss= matches[matches$toss_decision=="bat",]
bat_first_wins=0
bat_first_loss=0
i=0
for (i in seq(1,nrow(Toss))) {
  if(as.character(Toss$toss_winner[i]==as.character(Toss$winner[i])))
    {
    bat_first_wins=bat_first_wins+1 #Bat First Win
  } else
    {
    bat_first_loss=bat_first_loss+1 #Bat First loss
  }
}



Toss1= matches[matches$toss_decision=="field",]
field_first_wins=0
field_first_loss=0
j=0
for (j in seq(1,nrow(Toss1))) {
  if(as.character(Toss1$toss_winner[j]==as.character(Toss1$winner[j])))
  {
    field_first_wins=field_first_wins+1 # Field First Win
  } else 
    {
    field_first_loss=field_first_loss+1 # Field first loss 
  }
}

Toss_Decision = data.frame("Bat First or Second"=c("Batting First","Fielding First"),"Count"=c(bat_first_wins,field_first_wins))

datatable(Toss_Decision)


ggplot(Toss_Decision,aes(Bat.First.or.Second,Count,fill=Bat.First.or.Second,label=Count))+
  geom_bar(stat = "identity", width = 0.5,alpha=0.5)+
  geom_point(size=9, color="black",alpha=0.07)+
  scale_x_discrete(labels = Toss_Decision[order(Toss_Decision$Count),]$Bat.First.or.Second) +
  theme_classic()+
  geom_text(aes(Bat.First.or.Second,Count,label = Count))+
  labs(x = "Toss Decision", y = "Wins", title = "Most successful Toss Decision taken in IPL",size=0.03)+labs(fill="Toss Decision")+
  theme(axis.text = element_text(size = 12, face = "bold"), title  =  element_text(size = 16))+
  theme(legend.title = element_text(size=10),legend.text = element_text(size=10),legend.position="bottom")

#IPL Winners List from 2008-2019
IPL_Winners=matches %>%
  select(season, id, winner) %>%
  group_by(season) %>%
  slice(which.max(id)) %>%
  select(season, winner)

datatable(IPL_Winners)
