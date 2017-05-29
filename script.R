setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

tweets <- searchTwitter("RHCJO",n=10000)
tweets <- searchTwitter('TrumpinSaudi ', since='2017-05-24', until='2017-05-27', n=10000)

df <- twListToDF(tweets)



df <- df[, order(names(df))]
df$created <- strftime(df$created, '%Y-%m-%d')
