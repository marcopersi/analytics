# Package "httr" installieren, falls es nicht vorhanden ist
if(!"httr" %in% rownames(installed.packages())) install.packages("httr")
if(!"httpuv" %in% rownames(installed.packages())) install.packages("httpuv")

# Package "httr" laden
library("httr")

# Check, ob die Callback-URL auch "http://127.0.0.1:1410" ist
oauth_callback()

# Erstellen eines Objektes mit den Endpunkten
# Diese wäre für Twitter auch vordefiniert in oauth_endpoints("twitter")
twitter_endpoint <- oauth_endpoint(
    request   = "https://api.twitter.com/oauth/request_token",
    authorize = "https://api.twitter.com/oauth/authorize",
    access    = "https://api.twitter.com/oauth/access_token"
  )

# Erstellen eines Objekts mit API Key, Secret und Callback-URL
twitter_app <- oauth_app(
    appname      = "", # hier einen beliebigen Namen für die App eintragen
    key          = "", # hier den API-Key eintragen
    secret       = "", # hier das API-Secret eintragen
    redirect_uri = "http://127.0.0.1:1410" # oder oauth_callback()
  )

# Erstellung eines Tokens
twitter_token <- oauth1.0_token(
    endpoint = twitter_endpoint,
    app      = twitter_app
  )

# Ein
req <- GET(
    url   = "https://api.twitter.com/1.1/statuses/user_timeline.json", # Endpoint um Tweets eines Users zu laden
    query = list("screen_name" = "realdonaldtrump", "count" = "10"),   # 10 Tweets von @realdonaldtrump laden
    config(token = twitter_token)                                      # gibt den Token zum API-Zugriff an
  )
req # das "rohe" Resultat
stop_for_status(req) # wandelt etwaige Fehlermeldungen des Servers in R-Fehler um

ten_tweets <- content(req)     # liest den Inhalt der Serverantwort aus
str(ten_tweets, max.level = 2) # Zusammenfassung der Struktur des Resultats
