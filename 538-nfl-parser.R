library(stringr)
library(lubridate)
library(XML)

# All of the 538 NFL results are included on a single page, so you only need to read in a single file
setwd('/home/larry/Github-Public/538-NFL-Predictions-2017/raw-data/538-game-predictions/')
x <- readHTMLTable('2017-10-09-2313-538-nfl-game-predictions.html.gz')
names(x) <- sprintf("Week %d Results", length(x):1)
filenames <- str_c(str_replace_all(names(x), " ", "-"), ".csv")

# Parse the 538 results page
for (i in 1:length(x)) {
  colnames(x[[i]]) <- c('Score.V', 'Team.V', 'Team.H', 'Forecast', 'Score.H', 'P.Team', 'My.Team', 'My.Forecast')
  x[[i]] <- x[[i]][ , c(2, 3, 1, 5, 6, 4, 7, 8)]
  for (j in 1:2) {
    teams <- x[[i]][ , j]
    x[[i]][ , j] <- str_extract(str_sub(teams, start = str_length(teams) - 2), '[A-Z]+')
  }
  score <- str_extract_all(x[[i]]$Score.V, '[0-9]+')
  x[[i]]$Score.V <- sapply(score, function(x) as.numeric(x[1]))
  x[[i]]$Score.H <- sapply(score, function(x) as.numeric(x[2]))
  x[[i]]$P.Team <- str_extract(x[[i]]$Forecast, '[A-Z]+')
  forecast <- str_extract(x[[i]]$Forecast, '[0-9]+')
  x[[i]]$Forecast <- as.numeric(forecast) / 100
}

# Add your personal forecasts
setwd('/home/larry/Github-Public/538-NFL-Predictions-2017/raw-data/my-predictions/')
files <- rev(list.files())
for (i in 1:length(files)) {
  print(files[i])
  week <- read.csv(files[i], header = TRUE)
  x[[i]]$My.Team <- week$Team
  x[[i]]$My.Forecast <- week$Forecast
}

# Accumlate the weekly results into a single data.frame
acc <- data.frame()
for (i in 1:length(x)) acc <- rbind(acc, x[[i]])
acc$Week <- unlist(mapply(function(m, n) rep(m, each = n), length(x):1, sapply(x, nrow)))
acc <- acc[ , c(ncol(acc), 1:(ncol(acc) - 1))]

# Additional columns... 
acc$Winner <- ifelse(acc$Score.V > acc$Score.H, acc$Team.V, acc$Team.H)
acc$Correct.538 <- acc$Winner == acc$P.Team
acc$Correct.Me <- acc$Winner == acc$My.Team
acc$Agree <- acc$P.Team == acc$My.Team
if (any(acc$Score.V == acc$Score.H)) warning(sprintf("Tie game", i))

head(acc, 16)
setwd('/home/larry/Github-Public/538-NFL-Predictions-2017/parsed-data/')
write.csv(acc, file = "538-NFL-predictions.csv", row.names = F)
