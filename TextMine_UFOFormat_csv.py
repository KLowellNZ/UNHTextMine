# -*- coding: utf-8 -*-
"""
Created on Thu Apr  5 09:08:00 2018

@author: Kim Earl Lowell
"""

#%%
# This code will read in all the ufo files pulled down previously
# and convert them to a csv file.  The output csv file will have two
# fields -- an index ('blog') and a text field ('text').
# First read the name of all the files in the directory.
###################### remove CR/LF #############################
# This function removes the carriage return/line feed from a string.
def rmCRLF(string):
    string=string.replace('\n',' ')
    string=string.replace('\r',' ')
    string=string.replace(',',' ')
    return string
##################  MAIN BODY OF PROGRAM ########################s
from os import listdir
import pandas as pd
pathtest='C:/Analytics/DATA900/Python_III/PythIII_Assignments/20news-bydate/20news-bydate-test/talk.ufo/'
pathtrain='C:/Analytics/DATA900/Python_III/PythIII_Assignments/20news-bydate/20news-bydate-train/talk.ufo/'
outpath='C:/Analytics/DATA902/DATA902_TextMining_Phani/TextMine_Assignments/'
outfile='ufo_blogs.csv'
trainfiles=listdir(pathtrain)
#print(trainfiles)
# Set up outputs.
blog=1
cols=['blog','text']
dfout=pd.DataFrame(columns=cols)
for i,file in enumerate(trainfiles):
    blog=int(blog)
    textobj=open(pathtrain+trainfiles[i],'r')
    text=textobj.read()
    text=rmCRLF(text)
#    print(i,'\n',text)
# Create temp dataframe for appending.
    dfnew=pd.DataFrame([[int(blog),text]],columns=cols)
    dfout=dfout.append(dfnew)
    blog=blog+1
#    if i > 9:
#        break
# Now add test files.
testfiles=listdir(pathtest)
for i,file in enumerate(testfiles):
    blog=int(blog)
    textobj=open(pathtest+testfiles[i],'r')
    text=textobj.read()
    text=rmCRLF(text)
#    print(i,'\n',text)
# Create temp dataframe for appending.
    dfnew=pd.DataFrame([[int(blog),text]],columns=cols)
    dfout=dfout.append(dfnew)
    blog=blog+1
dfout['blog']=dfout['blog'].astype(int)
dfout.to_csv(path_or_buf=outpath+outfile, index=False)
