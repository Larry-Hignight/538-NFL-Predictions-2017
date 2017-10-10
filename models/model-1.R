library(stringr)
library(lubridate)

calc_payout <- function(ml, bet = 100) {
  payout_neg_ml <- function(ml, bet) (bet / ml) * (ml + 100)
  payout_pos_ml <- function(ml, bet) (bet / 100) * (ml + 100)
  if (ml < 0) payout_neg_ml(abs(ml), bet)
  else payout_pos_ml(ml, bet)
}

calc_ev <- function(p, ml, bet = 100) (p * (calc_payout(ml, bet) - bet)) + ((1 - p) * -bet)


# Import the data
setwd('/home/larry/Github-Public/538-NFL-Predictions-2017/parsed-data/')
predictions <- read.csv('538-NFL-predictions.csv', header = TRUE)
lines <- read.csv('oddsshark-nfl-lines.csv', header = TRUE)

# There is no line data for weeks 1-4;  Remove them from the predictions file
predictions <- predictions[!predictions$Week %in% 1:4, ]

# Reformat the prediction data frame w/ each time as a separate row
preds <- rbind(predictions, predictions)
idx <- (nrow(predictions) + 1):nrow(preds)
preds$Team.V[idx] <- preds$Team.H[idx]
colnames(preds)[2] <- 'Team'
preds <- preds[ , c('Week', 'Team', 'Forecast', 'Winner')]
colnames(preds)[3] <- 'P'

idx <- as.character(preds$Team) != as.character(preds$P.Team)
preds$Forecast[idx] <- 1 - preds$Forecast[idx]
preds$Winner <- as.character(preds$Team) == as.character(preds$Winner)
head(preds, 12)

# Use the line info that is closest to game time
lines.split <- split(lines, f = list(lines$Week, lines$Team))
sapply(lines.split, nrow)
has.line <- sapply(lines.split, function(x) nrow(x) > 0)
lines <- do.call(rbind, lapply(lines.split[has.line], function(x) x[nrow(x), ]))
lines <- lines[order(lines$Week), ]
head(lines)

lines <- lines[ , c('Week', 'Date.game', 'Team', 'Bovada.lv', 'Caesars', 'Westgate', 'Station', 'Best')]
lines <- merge(x = lines, y = preds, by = c('Week', 'Team'))
lines$EV <- mapply(calc_ev, lines$P, lines$Best)
head(lines)


## ----------

# Bet Selection Functions
select_by_ev_cutoff <- function(ev.cutoff, start = 0, randomized = FALSE) {
  bets <- cbind(lines[lines$EV >= ev.cutoff, ], data.frame(start, bet = NA, won = NA, end = NA))
  if (randomized) bets[sample(nrow(bets)), ] else bets
}

# Betting Strategy Functions
constant_bet <- function(amt) function(bets) amt

percentage_bet <- function(perc, max.bet = Inf) 
  function(bets) min(bets$start[nrow(bets)] * perc / 100, max.bet)

kelly_bet <- function(max.bet = Inf, kelly.coef = 1) {
  calc_b = function(ml) calc_payout(ml, 1) - 1
  kelly_fraction = function(b, p) (p * (b + 1) - 1) / b
  function(bets) {
    x <- bets[nrow(bets), ]
    b <- calc_b(x$Best)
    bet.amt <- x$start * kelly_fraction(b, x$P) * kelly.coef
    if (bet.amt < 0) bet.amt <- 0
    min(bet.amt, max.bet)
  }
}


# Used to calculate the results for the 'bets' data frame
# Requires the following columns:  Winner, Best, start, bet, won, end
# The betting_amount function is one of the betting strategy functions above (eg kelly_betting)
# The betting_amount function is allowed to access bets from 1:i
calc_bets <- function(bets, betting_amount) {
  for (i in 1:nrow(bets)) {
    if (i > 1) bets$start[i] <- bets$end[i - 1]
    bet <- betting_amount(bets[1:i, ])
    bets$bet[i] <- bet
    bets$won[i] <- ifelse(bets$Winner[i], calc_payout(ml = bets$Best[i], bet), -bet)
    bets$end[i] <- bets$start[i] + bets$won[i]
  }
  bets
}

## ---------------

start_amount <- 200
num_games <- 14
num_sims <- 30
max_bet <- 200
bets <- lapply(1:num_sims, function(n) select_by_ev_cutoff(ev.cutoff = 3, start = start_amount, randomized = TRUE)[1:num_games, ])

results <- list("constant_10" = lapply(bets, function(x) calc_bets(x, constant_bet(amt = 10))),
                "constant_50" = lapply(bets, function(x) calc_bets(x, constant_bet(amt = 50))),
                "percent_5" = lapply(bets, function(x) calc_bets(x, percentage_bet(perc = 5, max.bet = max_bet))),
                "percent_20" = lapply(bets, function(x) calc_bets(x, percentage_bet(perc = 20, max.bet = max_bet))),
                "kelly_p5" = lapply(bets, function(x) calc_bets(x, kelly_bet(kelly.coef = .5, max.bet = max_bet))),
                "kelly_1" = lapply(bets, function(x) calc_bets(x, kelly_bet(kelly.coef = 1, max.bet = max_bet))),
                "kelly_1p5" = lapply(bets, function(x) calc_bets(x, kelly_bet(kelly.coef = 1.5, max.bet = max_bet))))


for (i in 1:length(bets)) {
  max.y <- max(sapply(results, function(x) max(x[[i]]$end)))
  min.y <- min(0, sapply(results, function(x) min(x[[i]]$end)))
  
  div <- ifelse(max.y < 100000, 1, 1000)
  plot(rep(start_amount, nrow(bets[[i]])), type = 'l', col = 'black', main = sprintf("Sim #%d", i),
       xlab = 'Bet #', ylab = ifelse(div == 1000, '$ (K)', '$'), 
       xlim = c(1, nrow(bets[[i]])), ylim = c(min.y %/% div, max.y %/% div))

  line_types <- c(rep(1:2, 3), 3)
  colors <- rep(c('blue', 'green', 'black', 'red'), each = 2)
  for (j in 1:length(results)) lines(results[[j]][[i]]$end %/% div, type = 'l', col = colors[j], lty = line_types[j])
}



for (i in 1:length(results)) {
  print(names(results)[i])
  x <- results[[i]]
  print(summary(sapply(x, function(xx) xx$end[nrow(xx)])))
}

