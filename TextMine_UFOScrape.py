# -*- coding: utf-8 -*-
#"""
#Created on Tue Apr 3 14:23:16 2018
#
#@author: Kim Lowell
#"""
# Data 902 -- Text Mining
#%%
# Assignment 13. Use BeautifulSoup to scrape a web page to get sea kayaking
import requests
from bs4 import BeautifulSoup
import re

#outfile = open('C:\Analytics\DATA900\Python_III\PythIII_Assignments\Detainees.csv', 'w+') 
# The followng is the correct page.
ufobase="https://ufo.net/category/ufo-sightings/page/"
pageno='1'
lastpage=False
filename='1'
testpath='C:/Analytics/DATA900/Python_III/PythIII_Assignments/20news-bydate/20news-bydate-test/talk.ufo/'
trainpath='C:/Analytics/DATA900/Python_III/PythIII_Assignments/20news-bydate/20news-bydate-train/talk.ufo/'
# Start looping through the pages of the UFO site.
while not lastpage:
    ufopage=ufobase+pageno
    print('Scraping UFO page number: ',pageno)
    try:
        response=requests.get(ufopage)
    except:
        lastpage=True
        continue
    #response=requests.get("http://www.showmeboone.com/sherrif/jailresidents/")
    # This is what I am searching for.https://ufo.net/category/ufo-sightings/page/3/
    ufoblog=BeautifulSoup(response.text,"lxml")
    for i,element in enumerate(ufoblog.findAll({'article':{'div class':'pd-rating'}})):
        blogpost=element.get_text()
# the text following "Description of Sighting" is what we want (without date,
# name of blogger, etc.)
        messagestart=blogpost.find('Description of Sighting:')
# If "Message" not in text, get next blog.
        if messagestart == -1:
            continue
        message=blogpost[messagestart+25:]
        message=message.strip()
# Open the output file, write message, and close file. Write every third file
# to the test folder.
        if i%3 == 0:
            outfile = open(testpath+filename, 'w+')
        else:
            outfile = open(trainpath+filename, 'w+')
        try:
            outfile.write(message)
            filename=str(int(filename)+1)
            outfile.close()
        except:
            continue
# Get next page. Get a maximum of 200 pages (about 700 blogs)
    pageno=str(int(pageno)+1)
    if int(pageno) > 200:
        lastpage=True
