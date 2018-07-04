# Packages "rtweet", "ggplot2", "maps" und "mapproj" installieren, falls nicht vorhanden
if(!"rtweet" %in% rownames(installed.packages())) install.packages("rtweet")
if(!"ggplot2" %in% rownames(installed.packages())) install.packages("ggplot2")
if(!"maps" %in% rownames(installed.packages())) install.packages("maps")
if(!"mapproj" %in% rownames(installed.packages())) install.packages("mapproj")
if(!"dplyr" %in% rownames(installed.packages())) install.packages("dplyr")

# Package "rtweet" laden
library("rtweet")

# Twitter-Token erstellen
twitter_token <- create_token(
    app             = "", # ein Name für den Token
    consumer_key    = "", # API-Key eintragen
    consumer_secret = ""  # API-Secret eintragen
  )

don10 <- get_timeline(    # lade die Timeline (=Tweets)
    "realdonaldtrump",    # des Users @realdonaldtrump
    n = 10,               # und zwar 10 Tweets
    token = twitter_token # mit dem Token im Objekt twiter_token
  )

# Das Resultat ist nun wie eine Tabelle aufbereitet
str(don10)



##############################
### WAS KANN MAN DAMIT MACHEN?

ht_hslu <- search_tweets( # suche nach Tweets
    q = "HSLU",           # mit dem Text "HSLU"
    n = 18000,            # max. 18k Tweets
    include_rts = TRUE,   # inkl. Retweets abrufen
    token = twitter_token
  )

ht_hslu

View(ht_hslu)

# Informationen über die User abrufen, die "HSLU" in ihren (Re)Tweets hatten:
htu_hslu <- users_data(ht_hslu)

View(htu_hslu)

# Zeitreihenplot der HSLU-Tweets (Häufigkeit pro Tag)
ts_plot(ht_hslu)

# Zeitreihe nach Tweet/Retweet getrennt:
ht_hslu %>%                       # nimm die ht_hslu Daten
  dplyr::group_by(is_retweet) %>% # gruppiere sie nach der Variable "is_retweet"
    ts_plot("days")               # und mache einen tageweisen Zeitreihenplot


# Nach max. 18k Tweets (ohne Retweets) mit dem Hashtag #Rstats suchen
rsth <- search_tweets(
    q           = "#rstats",
    n           = 18000,
    include_rts = FALSE,
    token       = twitter_token
  )

# Stündliche Häufigkeiten als Zeitreihe plotten
ts_plot(rsth, "hours")


# API-Limits abrufen
rate_limit()


## ACHTUNG: Läuft >15 Minuten weil man auf mindestens ein Limit-Reset warten muss!
## 25k Tweets auf Deutsch aus dem EU-Raum abrufen und bei Limitüberschreitungen auf Resets warten
#eu_tweet <- search_tweets(
#    q                = "lang:de",
#    geocode          = lookup_coords("eu"),
#    n                = 25000,
#    retryonratelimit = TRUE,   # wartet bis beim Limit wieder Kontingente frei werden
#    token            = twitter_token
#)
#
#eu_tweet
#
## Datensatz mit den deutschen EU-Tweets um Koordinaten anreichern
#eu_tweet_geo <- lat_lng(eu_tweet)


# da das zu lange dauert laden wir ein Datenobjekt mit den gespeicherten Tweets:

# zuerst setzen wir den Pfad in dem der Datensatz und die Skripte sind:
setwd(choose.dir()) # geht nur mit Windows

# Manuell Windows
#setwd("C:/Users/Davide Di Ronza/Google Drive/Dokumente DR/Hakuna Madata/HSLU")
# Manuell Mac
#setwd("/Users/lisa/Documents/Programs/HSLU-R")

# dann laden wir das Objekt mit den Tweets und den Koordinaten
load("hslu-eu_tweet_geo.RData")
eu_tweet_geo



### Daten auf einer Europakarte einzeichnen
# Zeichenbereich vorbereiten und Landkarte zeichnen
par(mar = c(0, 0, 0, 0))
maps::map("world", lwd = 0.25, resolution = 0,
    bg = "gray", fill=TRUE, col="white",
    xlim = c(-5, 25), ylim = c(40, 60)
  )

# Punkte hinzufügen
with(eu_tweet_geo,
  points(x = lng, y = lat, pch = 20, cex = 2, col = rgb(1, 0, 0, .125))
)
