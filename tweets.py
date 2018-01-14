
#para instalar tweetpy: $pip install tweepy

import tweepy 
import csv

access_token = ""
access_token_secret = ""
consumer_key = ""
consumer_secret = ""


def get_all_tweets(screen_name):
	#Twitter only allows access to a users most recent 3240 tweets with this method
	
	#authorize twitter, initialize tweepy
	auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
	auth.set_access_token(access_token, access_token_secret)
	api = tweepy.API(auth)
	
	#initialize a list to hold all the tweepy Tweets
	alltweets = []	
	
	#make initial request for most recent tweets (200 is the maximum allowed count)
	new_tweets = api.user_timeline(screen_name = screen_name,count=100, tweet_mode='extended')
	
	#save most recent tweets
	alltweets.extend(new_tweets)
		
	#transform the tweepy tweets into a 2D array that will populate the csv	
	outtweets = [[tweet.id_str, tweet.created_at, tweet.full_text] for tweet in alltweets]
	for i in alltweets:
		gg = i.full_text
		if gg.find("coin") != -1:
			print gg
	#ss = alltweets[0]
	#print outtweets[:,2]
	#print ss.full_text
	#write the csv	
	#with open('%s_tweets.csv' % screen_name, 'wb') as f:
	#	writer = csv.writer(f)
	#	writer.writerow(["id","created_at","text"])
	#	writer.writerows(outtweets)
	
	pass


if __name__ == '__main__':
	#pass in the username of the account you want to download
	get_all_tweets("officialmcafee")

