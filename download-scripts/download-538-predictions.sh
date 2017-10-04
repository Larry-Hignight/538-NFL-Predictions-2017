DIR=raw-data/538-game-predictions
mkdir -p $DIR
wget https://projects.fivethirtyeight.com/nfl-predictions-game/results/ \
     -O `date +$DIR/%Y-%m-%d-%H%M-538-nfl-game-predictions.html.gz`

