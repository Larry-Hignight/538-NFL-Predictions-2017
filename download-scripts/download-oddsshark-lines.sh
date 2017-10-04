DIR=raw-data/oddsshark-lines
mkdir -p $DIR
wget http://www.oddsshark.com/nfl/odds \
     -O `date +$DIR/%Y-%m-%d-%H%M-nfl-lines.html`
