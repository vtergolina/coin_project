#para instalar tweetpy: $pip install tweepy
import requests
from urllib2 import urlopen
from bs4 import BeautifulSoup
import urllib2
import tweepy 
import smtplib
import re
from email.mime.image import MIMEImage
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText

#Variables that contains the user credentials to access Twitter API 
access_token = "951117734338879488-xC6yB9oPshrP5JCiAlc9cMUKMjYKDlJ"
access_token_secret = "pDaYwYIdYZBLB5cjMjwKKdrvQAolahzdy94idAuh28NUP"
consumer_key = "QqqaHqVryTahq3r3ygQstGOwJ"
consumer_secret = "wK0sacXoU7VAuF30P6lYGab86nqRrt8TBG96ESlX86FIywvOvX"

def url_finder(string):

	urls = re.findall('http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\(\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+', string)
	return urls[0];

def send_nudes(tweet):

	fromaddr = "coin.tergrillo@gmail.com"
	recipients = ['vtergolina@hotmail.com' , 'bruno.grillo@ufrgs.br']
	msg = MIMEMultipart()
	msg['From'] = fromaddr
	msg['To'] = ", ".join(recipients)
	msg['Subject'] = "MCAFEE ALERT"

	body = tweet
	msg.attach(MIMEText(body, 'plain'))

	server = smtplib.SMTP('smtp.gmail.com', 587)

	server.ehlo()
	server.starttls()
	server.ehlo()
	server.login("coin.tergrillo", "bikedosbrothers")
	text = msg.as_string()
	server.sendmail(fromaddr, recipients, text)
	return;

def print_image(url):

	soup = BeautifulSoup(urlopen(url).read())

	for element in soup.findAll('img'):
		vv = element.get('src')
		if vv != None:
			if vv.find("media") != -1:
				r = requests.get(vv, allow_redirects=True)
				open('tweet_img.jpg', 'wb').write(r.content)
				break



def get_tweets(screen_name):
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
		
	for i in alltweets:
		gg = i.full_text
		bb = gg
		gg = gg.lower()
		#if (gg.find("coin") != -1) or (gg.find("coi") != -1) or (gg.find("day") != -1) or (gg.find("week") != -1) or (gg.find("wee") != -1) or (gg.find("of the") != -1):
		if (gg.find("coin of the week") != -1) or (gg.find("coin of the day") != -1):
			send_nudes(bb)
			if gg[:1] != '@':
				open('tweet.txt', 'wb').write(bb.encode("utf-8"))
				url = url_finder(bb)
				print_image(url)
	
	pass


if __name__ == '__main__':
	#pass in the username of the account you want to download
	get_tweets("vtergolina2")

