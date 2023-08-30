#A

library(dplyr)
library(tibble)

setwd("C:/Users/menac/Desktop/R Directory") 



course_num <- 543722
semester <- 'sp23_'
df_name <- 'NBAdata'
file_type <- '.csv'

course_info <- c(course_num, semester, df_name, file_type)
course_labels <- c('Course number', 'Semester', 'File name', 'File type')

names(course_info) <- course_labels

print(course_info)




file_name <- ""


for (string in course_info) {
  
  file_name <- paste0(file_name, string)
}

print(file_name)


file_name_2 <- "543722sp23_NBAdata.csv"

print(file_name == file_name_2)


df <- as_tibble(read.csv(file_name))


df

perform <- c('Year','Player','Pos','Age','Tm','G','FG','FGA','FT','FTA','PTS','AST')

perform_labels <- c('Year','Player','Position','Age','Team','# of Games',
                     '# of Field Goals', '# of Field Throws','# of Free Goals', 
                     '# of Free Throws', 'Points', 'Assists')

names(perform) <- perform_labels



print(perform)

print(length(perform))

#B

#1

new_df <- select(df, one_of(perform))

new_df

print(nrow(new_df))

print(paste("This dataframe has",nrow(new_df),"observations"))


print(paste("The name of the",random_var <- sample(1:ncol(new_df),1), 
            "variable is", colnames(new_df)[random_var]))


#2

knicks_players <- unique(df$Player[df$Tm == "NYK"])

print(knicks_players)

length(knicks_players)


random_player <- knicks_players[sample(1:length(knicks_players),1)]
print(random_player)

player_stats <- df %>%
  filter(Player == random_player)

player_stats

head(player_stats, n = 4)

print(paste("The player", random_player, "has played", nrow(player_stats),
            " seasonsin the NBA"))

#3

new_player_stats <- player_stats[, 6:ncol(player_stats)]
new_player_stats

player_matrix <- as.matrix(new_player_stats)


player_matrix
year_vector <- as.character(player_stats$Year)
col_vector <- colnames(new_player_stats)

year_vector

rownames(player_matrix) <- year_vector


print(player_matrix)

mean(player_stats$PTS)
max(player_stats$FGA)
min(player_stats$FT)
median(player_stats$AST)

#4


vectors_list <- list()

for (col_name in colnames(player_matrix)) {

  vector_name <- paste0(col_name,"_stats")
  vector_data <- c( mean(as.numeric(player_matrix[,col_name])),
                    sum(as.numeric(player_matrix[,col_name])) )
  
  assign(vector_name, vector_data )
  
  vectors_list[[vector_name]] <- vector_data 
  
  
}


vectors_list

statistics_matrix <- do.call(cbind, vectors_list)
rownames(statistics_matrix) <- c("Mean","Sum")

print(statistics_matrix)


combined_matrix <- rbind(statistics_matrix,player_matrix)

print(combined_matrix)

#C

#1


new_df <- cbind(new_df, "TOV" = df$TOV)

# # Removing the 'Age' and 'Gender' columns
# df <- subset(df, select = -c(Age, Gender))


#Changed the column name using this:


new_df

new_df$TOV


counter <- 1
while (is.na(new_df$TOV[counter])== TRUE) {
  
  counter <- counter + 1
  
}

print(paste("The first year that recorded turnovers was",new_df$Year[counter]))


#"% of field goals out of total field throws"

FG_of_FGA <- new_df$FG/new_df$FGA*100

new_df <- cbind(new_df, "Field goals % of total field throws" = FG_of_FGA)

FT_of_FTA <- new_df$FT/new_df$FTA*100

new_df <- cbind(new_df, "Penalty goals % of total penalty throws" = FT_of_FTA)

new_df

# 3( צרו עמודה שתייצג באופן מילולי את גיל השחקן. לשם כך, ראשית הוסיפו את העמודה Age
#    מתוך בסיס הנתונים הכללי. לאחר מכן, העניקו את הערך ‘Young ‘אם גיל השחקן קטן מהערך
#    ,25 את הערך ‘Medium ‘אם גיל השחקן גדול או שווה ל 25- אך קטן מ32- ואת הערך ‘Veteran‘
#    אחרת.

new_df$Age_rating <- c()

new_df$Age_rating[1]
nrow(new_df)

num <- 1
new_df$Age

while(num < nrow(new_df)) {

  if(is.na(new_df$Age[num]) == TRUE){
    num <- num + 1
    } else {
    
    
        if(new_df$Age[num] < 25){
          
          new_df$Age_rating[num] <- "Young"
          
        }else if(new_df$Age[num] < 32) {
          
          new_df$Age_rating[num] <- "Medium"
          
        }else{
          
          new_df$Age_rating[num] <- "Veteran"
          
        }
        
        num <- num + 1 } 
}
  

data2021 <- new_df %>%
              filter(Year == 2001)

data2021$Name_length <- c()

num <- 1
while(num <= nrow(data2021)){

data2021$Name_length[num] <- nchar(data2021$Player[num])

num <- num + 1

}




players_more_than_one_team <- character()

# Loop through each unique player name
for (player_name in unique(data2021$Player)) {
  # Get the unique teams for the current player
  unique_teams <- unique(data2021[data2021$Player == player_name, "Tm"])
  
  # Check if the player played in more than one team
  if (length(unique_teams) > 1) {
    players_more_than_one_team <- c(players_more_than_one_team, player_name)
  }
}

# Print the names of players who played in more than one team
print(players_more_than_one_team)


#D

library(ggplot2)

df2 <- as.tibble(read.csv("543722sp23_NBAdata.csv"))

df2

df2 <- select(df2, one_of(perform))

filtered <- df2 %>%
  filter(Year >= 1980 & Year <= 2000, PTS > 850, Age >= 25 & Age <= 32) %>%
  na.exclude(filtered) %>%
  arrange(desc(AST)) 


print(filtered)


Avg_points <- df2 %>%
  filter(Year >= 1950 & Year <= 2017) %>%
  group_by(Year) %>%
  summarise(players_num = length(unique(Player)), mean_points = mean(PTS))

print(n = 100, Avg_points)

Max_ast_ft <- df2 %>%
  group_by(Year, Tm) %>%
  summarise(max_ast = max(AST), max_ft = max(FT))


print(n = 100, Max_ast_ft)


Avg_age <- df2 %>%
  group_by(Year) %>%
  summarise(avg_age = mean(Age, na.rm = TRUE))


ggplot(Avg_age, aes(Year, avg_age)) + geom_point() +
  ggtitle("Scatter plot: meanAge and Year")


Max_pts <- df2 %>%
  filter(Tm == c('NYK', 'LAL', 'BOS')) %>%
  group_by(Year, Tm) %>%
  summarize(max_pts = max(PTS, na.rm = TRUE))

print(n = 300, Max_pts)

ggplot(Max_pts, aes(Year, max_pts, color = Tm, size = max_pts)) +
  geom_point() +
  facet_wrap(~Tm) +
  ggtitle("Scatter plot: Max Points by Team")


NYK_pos <- df2%>%
  filter(Year == 2017, Tm == "NYK") %>%
  group_by(Pos) %>%
  summarize(player_num = length(Player))

ggplot(NYK_pos, aes(Pos, player_num)) + geom_col() + ggtitle("# of Players by Position - NYK 2017")          


print(n = 100, df2 %>% filter(Year == 2017, Tm == "NYK") %>% arrange(Pos))
