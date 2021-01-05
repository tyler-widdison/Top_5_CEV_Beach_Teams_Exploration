library(fivbvis)
library(tidyverse)

# ---------------------------------
# Get tournaments for 'type' & 'gender' 
# ---------------------------------
tournaments <- 
  v_get_beach_tournament_list(fields = c('Type', 'Season', 'No', 'Name', 'Gender')) %>% 
  select(-Version) %>% 
  mutate(Gender = ifelse(Gender == 0, 'M', 'W'),
         No = as.integer(No))

# ---------------------------------
# Get tournaments type name and replace tournament type
# ---------------------------------
TournTypes <- 
  tibble(
    No = c('0','1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20','21','22','23','24','25','26','27','28','29','30','31','32','33','34','35','36','37','38','39','40','41','42','43','44','45','46','47','48','49','50'),
    Type = c('GrandSlam','Open','Challenger','WorldSeries','WorldChamp','OlympicGames','Satellite','ContinentalChamp','OtherContinental','Other','Masters','ContinentalCup','ContinentalTour','JuniorWorldChamp','YouthWorldChamp','NationalTour','NationalTourU23','NationalTourU21','NationalTourU19','NationalTourU20','NationalTourU17','NationalTourU15','ContinentalChampU22','ContinentalChampU20','ContinentalChampU18','WorldChampU23','WorldChampU21','WorldChampU19','NationalTourU14','NationalTourU16','NationalTourU18','WorldChampU17','MajorSeries','WorldTourFinals','ZonalTour','Test','SnowVolleyball','ContinentalCupFinal','WorldTour5Star','WorldTour4Star','WorldTour3Star','WorldTour2Star','WorldTour1Star','YouthOlympicGames','MultiSports','NationalSnow','NationalTourU22','ContinentalChampU21','ContinentalChampU19','OlympicGamesQualification','KingOfTheCourt')
        )
tournaments$TypeName <- TournTypes$Type[match(tournaments$Type, TournTypes$No)]


# ---------------------------------
# Get beach teams and specific fields
# ---------------------------------
teams <- 
  v_get_beach_team_list(fields =  c('ConfederationCode', 'No', 'Name', 'NoPlayer1', 'Player1TeamName', 'NoPlayer2', 'Player2TeamName', 'NoTournament', 'TournamentTitle', 'Player1FirstName', 'Player2FirstName', 'Player1LastName', 'Player2LastName', 'Player1FederationCode', 'Player2FederationCode', 'Rank')) %>% 
  select(-Version)

# ---------------------------------
# Get unique player numbers and player names for teams
# ---------------------------------
teams <- teams %>% 
  pivot_longer(c('NoPlayer1', 'NoPlayer2'), values_to = 'NoPlayer') %>% 
  mutate(PlayerName = ifelse(name == 'NoPlayer1', Player1TeamName, Player2TeamName),
         PlayerFirstName = ifelse(name == 'NoPlayer1', Player1FirstName, Player2FirstName),
         PlayerLastName = ifelse(name == 'NoPlayer1', Player1LastName, Player2LastName),
         No = as.integer(No),
         NoTournament = as.integer(NoTournament),
         Rank = as.integer(Rank),
         NoPlayer = as.integer(NoPlayer)))
         
# ---------------------------------
# Get beach matches and wrangle fields
# ---------------------------------
beach_matches <- 
  v_get_beach_match_list(fields = c('NoTournament', 'LocalDate', 'NoPlayerA1', 'NoPlayerA2', 'NoPlayerB1', 'NoPlayerB2', 'RoundName', 'TournamentCode',
                                    'PointsTeamASet1', 'PointsTeamASet2', 'PointsTeamASet3', 'MatchPointsA',
                                    'PointsTeamBSet1', 'PointsTeamBSet2', 'PointsTeamBSet3', 'MatchPointsB', 'NoTeamA', 'NoTeamB')) %>% 
  mutate(Year = substr(LocalDate,1,4),
         PointsTeamASet3 = ifelse(PointsTeamASet3 == '', '0', PointsTeamASet3),
         PointsTeamBSet3 = ifelse(PointsTeamBSet3 == '', '0', PointsTeamBSet3),
         NoTournament = as.integer(NoTournament),
         LocalDate = as.Date(LocalDate),
         NoPlayerA1 = as.integer(NoPlayerA1),
         NoPlayerA2 = as.integer(NoPlayerA2),
         NoPlayerB1 = as.integer(NoPlayerB1),
         NoPlayerB2 = as.integer(NoPlayerB2)) %>% 
  
  # reject the fields with missing information
  filter(!is.na(NoPlayerA1), !is.na(NoPlayerA2), !is.na(NoPlayerB1), !is.na(NoPlayerB2), PointsTeamASet1 != '' , PointsTeamASet2 != '') %>% 
  
  # add new player fields to match player names properly 
  # for beach, not doing this will duplicate teams beacuse of the team name organization. For example you might see 'Gibb/Rosenthal' & 'Rosenthal/Gibb' in the same season
  mutate(NoPlyA1 = ifelse(NoPlayerA1 > NoPlayerA2, NoPlayerA1, NoPlayerA2),
         NoPlyA2 = ifelse(NoPlayerA1 == NoPlyA1, NoPlayerA2, NoPlayerA1),
         
         NoPlyB1 = ifelse(NoPlayerB1 > NoPlayerB2, NoPlayerB1, NoPlayerB2),
         NoPlyB2 = ifelse(NoPlayerB1 == NoPlyB1, NoPlayerB2, NoPlayerB1)) %>% 
  
  left_join(select(tournaments, No, Name, Gender, TypeName), tournaments, by = c('NoTournament' = 'No'))
         
# ---------------------------------
# Match up the different dataframes variables to add new variables to 'beach_matches'
# ---------------------------------
beach_matches$PlayerA1TeamName <- teams$PlayerName[match(beach_matches$NoPlyA1, teams$NoPlayer)]
beach_matches$PlayerA2TeamName <- teams$PlayerName[match(beach_matches$NoPlyA2, teams$NoPlayer)]
beach_matches$PlayerB2TeamName <- teams$PlayerName[match(beach_matches$NoPlyB1, teams$NoPlayer)]
beach_matches$PlayerB1TeamName <- teams$PlayerName[match(beach_matches$NoPlyB2, teams$NoPlayer)]
beach_matches$TeamAConfederation <- teams$ConfederationCode[match(beach_matches$NoPlyA1, teams$NoPlayer)]
beach_matches$TeamAFederationCode <- teams$Player1FederationCode[match(beach_matches$NoPlyA1, teams$NoPlayer)]
beach_matches$TeamBConfederation <- teams$ConfederationCode[match(beach_matches$NoPlyB1, teams$NoPlayer)]
beach_matches$TeamBFederationCode <- teams$Player1FederationCode[match(beach_matches$NoPlyB1, teams$NoPlayer)]
beach_matches$TeamAFinish <- teams$Rank[match(beach_matches$NoTeamA, teams$No)]
beach_matches$TeamBFinish <- teams$Rank[match(beach_matches$NoTeamB, teams$No)]

# ---------------------------------
# A bit more wrangling to add a team name
# ---------------------------------
beach_matches <- beach_matches %>% 
  mutate(TeamAName = paste0(PlayerA1TeamName, '/', PlayerA2TeamName),
         TeamBName = paste0(PlayerB2TeamName, '/', PlayerB1TeamName),
         LocalDate = as.Date(LocalDate),
         Year = substr(LocalDate,1,4)) %>% 
  filter(!LocalDate %in% c('1520-12-17', '0918-03-03')) %>% 
  select(-c(NoPlyA1:NoPlyB2, NoPlayerA1:NoPlayerB2, 'Version', PlayerA1TeamName:PlayerB1TeamName, 'NoTournament'))
         
         
         
# ---------------------------------
# Shiny 
# ---------------------------------
library(shiny)
library(shinyWidgets)


ui <- fluidPage(
    fluidRow(
        column(4,
               pickerInput('SelectGender', 'Gender', sort(unique(df$Gender)),
                           options = list('actions-box' = T, size = 12), multiple = T, selected = sort(unique(df$Gender)))),
        column(4,
               pickerInput('SelectTourn', 'Tournament Type', sort(unique(df$TypeName)),
                           options = list('actions-box' = T, size = 12), multiple = T, selected = sort(unique(df$TypeName)))),
        column(4,
               pickerInput('SelectYear', 'Year', sort(unique(df$Year)),
                           options = list('actions-box' = T, size = 12), multiple = T, selected = sort(unique(df$Year))))),
        DT::dataTableOutput('RecordTable'))


server <- function(input, output) {
    
    output$RecordTable <- DT::renderDataTable({
        DT::datatable(
            df %>% 
                filter(Gender %in% input$SelectGender,TeamAConfederation == 'CEV' | TeamBConfederation == 'CEV', Year %in% input$SelectYear, TypeName %in% input$SelectTourn) %>%
                pivot_longer(c('TeamAName', 'TeamBName'), values_to = 'TeamName') %>% 
                mutate(TeamFederation = ifelse(name == 'TeamAName', TeamAFederationCode, TeamBFederationCode),
                       TeamConfederation = ifelse(name == 'TeamAName', TeamAConfederation, TeamBConfederation),
                       TeamFinish = ifelse(name == 'TeamAName', TeamAFinish, TeamBFinish),
                       OpponentFinish = ifelse(name == 'TeamAName', TeamBFinish, TeamAFinish),
                       Result = ifelse(name == 'TeamAName' & MatchPointsA == '2', 'W',
                                       ifelse(name == 'TeamBName' & MatchPointsB == '2', 'W', 'L'))) %>% 
                filter(TeamConfederation == 'CEV') %>% 
                group_by(TeamName, TeamFederation) %>% 
                summarise(played = n(),
                          Won = sum(Result == 'W'),
                          Lost = sum(Result == 'L'),
                          WinPct = round(Won/played,2),
                          AvgFinish = round(mean(TeamFinish, na.rm = T),2),
                          OppAvgFinish = round(mean(OpponentFinish, na.rm = T),2),
                          .groups = 'drop') %>% 
                ungroup(),
            rownames = FALSE,
            class = 'disply nowrap compact',
            filter = "top",
            options = list(
                scrollX = 500,
                pageLength = 20,
                lengthMenu = c(5, 10, 15, 20),
                scroller = T,
                columnDefs = list(list(className = 'dt-center', targets ="_all"))
                )
            ) 
    })
    }


shinyApp(ui, server)
