# -*- coding: utf-8 -*-
"""
Created on Fri Jun  1 16:58:22 2018

@author: pqw6
"""
###################Reddit Word2vec#############################################
#Load in Comment and Submission data and create master DF for both
import os
import pandas as pd
import gensim
from datetime import datetime
import string
import re
import matplotlib.pyplot as plt
from gensim.models import KeyedVectors
# go to line 367 for fasttext embeddings

#NLP Packages
from nltk.stem.porter import PorterStemmer
from nltk.stem.snowball import SnowballStemmer
from nltk.stem.lancaster import LancasterStemmer
from nltk.stem import WordNetLemmatizer
from nltk.corpus import stopwords
from nltk import sent_tokenize

##Load list of subreddits
#os.chdir('\\\\cdc.gov\\locker\\ONDIEH_TW\\Papers-And-Projects\\Reddit Drugs')
#
#with open('subreddits.txt') as f:
#    subreddits = [line.rstrip() for line in f]
#
#os.chdir('\\\\cdc.gov\locker\ONDIEH_TW\Papers-And-Projects\Reddit Drugs\Data\Submissions')
#sub_files = os.listdir()
#
#subs = [x.replace('Submissions_', '') for x in sub_files]
#subs = [x.replace('.plk', '') for x in subs]
#missing = list(set(subreddits) - set(subs))
#
##Load subs into df
#subdf = pd.DataFrame()
#
#for file in sub_files:
#      subdf = subdf.append(pd.read_pickle(file))
#
#
#
#os.chdir('\\\\cdc.gov\locker\ONDIEH_TW\Papers-And-Projects\Reddit Drugs\Data\Comments')
#com_files = os.listdir()
#
#comdf = pd.DataFrame()
#
#for file in com_files:
#      comdf = comdf.append(pd.read_pickle(file))


##############################################################################
#Save to pickle
#comdf.to_pickle('Comments.plk')
#subdf.to_pickle('Submissions.plk')
os.chdir('\\\\cdc.gov\locker\ONDIEH_TW\Papers-And-Projects\Reddit Drugs\Data\Comments')
#
comdf = pd.read_pickle('Comments.plk')
subdf = pd.read_pickle('Submissions.plk')
#plk file similar to .R files or .sas7dat

###############################################################################
#make analysis data set
comdf.columns
comdf.id

comdf = comdf[['id', 'author', 'body', 'created_utc', 'parent_id','subreddit']]

comdf.drop_duplicates('id')

subdf.columns
subdf.domain

subdf = subdf[['id', 'author', 'domain', 'title', 'over_18', 'created_utc',
               'selftext', 'subreddit']]
subdf['alltext'] = subdf['title'] + subdf['selftext']
scheck=subdf['alltext'][25]

#x = subdf.drop_duplicates('id')
#crospost_list = x.crosspost_parent
#crospost_list.isna().value_counts()
#columns_list = list(x.columns)

comdf['created_at'] = comdf.created_utc.apply(lambda x: datetime.fromtimestamp(int(x)))
comdf['created_at'][0:200]

subdf['created_at'] = subdf.created_utc.apply(lambda x: datetime.fromtimestamp(int(x)))
subdf['created_at'][0:200]

df1 = subdf[['alltext', 'created_at']]
df1.columns = ['text', 'datetime']
df2 = comdf[['body', 'created_at']]
df2.columns = ['text', 'datetime']

df = pd.concat([df1, df2])

######################################################
#Count number of posts per month per subreddit
#x =  subdf[['subreddit', 'created_at']]
#y = comdf[['subreddit', 'created_at']]
#z = pd.concat([x, y])
#
#
#subreddit_monthly_counts = z.groupby([z.created_at.dt.year, z.created_at.dt.month, 'subreddit']).agg('count')
#subreddit_monthly_counts['year'] = subreddit_monthly_counts.index
#subreddit_monthly_counts[['year', 'month', 'subreddit']] = subreddit_monthly_counts['year'].apply(pd.Series)
#subreddit_monthly_counts.to_csv('postsubreddit.csv')

###############################################################################

df.text.sample(200)

df['text'] = df.text.apply(lambda x: str(x))


def clean_text(text, stemmer = "porter", lem =False,
                remove_stopwords = True,
                stop_words = stopwords.words("english")):
            
      #Remove punct      
      translator = str.maketrans(string.punctuation, ' '*len(string.punctuation))
      text = text.translate(translator)
      
      #remove URL
      text = re.sub(r'http\S+', '', text)
      
      #Make text lowercase
      text = str.lower(text).split()
      #preference to upper or lowercase. 

      stop = set(stop_words)

      #Can add additional stopwords explicitly 
    
      #Remove stop words
      if remove_stopwords == True:
          text = [w for w in text if not w in stop]
      else:
          pass
      
       #Stem words options
      if stemmer == 'None':
          text = text
      if stemmer == "porter":
        porter_stemmer = PorterStemmer()
        text = [porter_stemmer.stem(word) for word in text]
      elif stemmer == "snowball":
        snowball_stemmer = SnowballStemmer("english")
        text = [snowball_stemmer.stem(word) for word in text]
      elif stemmer == "lancaster":    
        lancaster_stemmer = LancasterStemmer()
        text = [lancaster_stemmer.stem(word) for word in text]
      if lem == True:    
        wordnet_lem = WordNetLemmatizer()
        text = [wordnet_lem.lemmatize(word) for word in text]
        
       #Rejoin words into one string
      text = ' '.join(text)    
      
      return text

df['cleantext'] = df.text.apply(lambda x: clean_text(x, stemmer = 'None'))

import nltk
def tokenize_only(text):
    tokens = [word.lower() for sent in sent_tokenize(text) for word in nltk.word_tokenize(sent)]
    return tokens

df['tokens'] = df.cleantext.apply(lambda x: tokenize_only(x))


### this code generates and saves the word embeddings from word2vec
#model = gensim.models.Word2Vec(
#            df['tokens'],
#            size = 300,
#            window = 5,
#            sg = 1,
#            min_count = 5,
#            workers = 10,
#            iter = 5)

#model.wv.save_word2vec_format('reddit_wordvectors.txt')

###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
#LOAD W2V MODEL - CODE FOR PAPER STARTS HERE
os.chdir('\\\\cdc.gov\locker\ONDIEH_TW\Papers-And-Projects\Reddit Drugs\Data')
model = gensim.models.keyedvectors.KeyedVectors.load_word2vec_format('reddit_wordvectors.txt') #file containing words from submissions/comments from all included.

#Check Length of Vocab
len(model.wv.vocab) # ~867700 words

#TABLE 1 -Identify the top 20 words similar to fentanyl and obtain frequency
fentterms = model.wv.most_similar('fentanyl', topn = 500)
fentterms_words = [term[0] for term in fentterms]
fentterms_cosine = [term[1] for term in fentterms]

allfentterms = fentterms_words[0:20] + ['fentanyl', 'tramadol', 'ultram', 'tramal', 'meperidine', 'demerol']


#Selecting 20 for the four categories of opiates of intrest
#Natural/Semi Opioids
#natopi = ['oxycodone', 'hydrocodone', 'hydromorphine', 'oxymorphine', 'morphine',
#          'codeine']
#
#natopi_list = []
#
#for opi in natopi:
#      similar = model.wv.most_similar(opi, topn=20)
#      similar = [x[0] for x in similar]
#      natopi_list.append(similar)

#natopi_list.append(natopi)
#
#natopi_list = [x for subl in natopi_list for x in subl]
#natopi_list = list(set(natopi_list))
#
##Fent
#fent_similar = model.wv.most_similar('fentanyl', topn = 30)
#fent_similar = [x[0] for x in fent_similar]

##methadone
#methadone = model.wv.most_similar('methadone', topn=500)
#methadone_terms = ['methadone', 'mmt', 'methodone', 'mdone', 'methdone',
#                   'methadon', 'methadose', 'methedone', 'metaahdone', 'dolophine', 
#                   'methadon', 'methadones', 'dones']
#
#
##Heroin
#heroin = model.wv.most_similar('heroin', topn=500)
#heroin_terms = ['heroin', 'heroine', 'herion', 'h', 'heroini', 'speedball', '8speed',
#               'heorin']


###############################################################################
#count term occurrence
#load term lists
 
#natopi = terms['Natural/Semi Opioids'].tolist()
#fent = terms['Fentanyl'].tolist()
#fent = [x for x in fent if str(x) != 'nan']
#methadone = terms['Methadone'].tolist()
#methadone = [x for x in methadone if str(x) != 'nan']
#heroin = terms['Heroin'].tolist()
#heroin = [x for x in heroin if str(x) != 'nan']
#


def count_each_opi(string, list_terms):
      count_list = []
      for drug in list_terms:
            count = 0
            if drug in string:
                  count = count + 1
            else:
                  pass
            count_list.append(count)
      return count_list


fent_count = df.tokens.apply(lambda x: count_each_opi(x, allfentterms))
fent_count = [sum(x) for x in zip(*fent_count)]
fenttermfreqs = pd.DataFrame(allfentterms)
fenttermfreqs['freqs'] = fent_count


#df['fent_count'] = df.tokens.apply(lambda x: count_opi(x, fent))
#df['fent_count'].value_counts()
#
#df['natopi'] = df.tokens.apply(lambda x: count_opi(x, natopi))
#df['natopi'].value_counts()
#
#df['heroin'] = df.tokens.apply(lambda x: count_opi(x, heroin))
#df['heroin'].value_counts()
#
#df['methadone'] = df.tokens.apply(lambda x: count_opi(x, methadone))
#df['methadone'].value_counts()

###############################################################################
###############################################################################
###############################################################################
###############################################################################
#FIG 1 - COUNT NUMBER OF POSTS THAT CONTAIN A WORD RELATED TO FENT



###############################################################################
###############################################################################
###############################################################################
###############################################################################
#agg by month
#monthly_counts = pd.DataFrame()
#monthly_counts['post'] = df['datetime'].groupby([df.datetime.dt.year, df.datetime.dt.month]).agg('count')
#monthly_counts['fent'] = df['fent_count'].groupby([df.datetime.dt.year, df.datetime.dt.month]).agg('sum')
#monthly_counts['natopi'] = df['natopi'].groupby([df.datetime.dt.year, df.datetime.dt.month]).agg('sum')
#monthly_counts['heroin'] = df['heroin'].groupby([df.datetime.dt.year, df.datetime.dt.month]).agg('sum')
#monthly_counts['methadone'] = df['methadone'].groupby([df.datetime.dt.year, df.datetime.dt.month]).agg('sum')
#
#monthly_counts.plot(y = ['fent', 'natopi', 'heroin', 'methadone'], use_index = True, figsize = (12, 12))

#monthly_counts['fentnorm'] = (monthly_counts['fent']/monthly_counts['post']) * 10000 
#monthly_counts['natopinorm'] = (monthly_counts['natopi']/monthly_counts['post']) * 10000 
#monthly_counts['heroinnorm'] = (monthly_counts['heroin']/monthly_counts['post']) * 10000 
#monthly_counts['methadonenorm'] = (monthly_counts['methadone']/monthly_counts['post']) * 10000 

#monthly_counts.plot(y = ['fentnorm', 'natopinorm', 'heroinnorm', 'methadonenorm'], use_index = True, figsize = (12, 12))

#monthly_counts.to_csv('Reddit_monthly_counts.csv')
#
#
#monthly_counts = monthly_counts[(2011, 1):]

##############################################################################
#Sensitivity Analysis
#Count each word in the four opiate categories and rerun analysis using most 
#frequent term

#natopi_indcount = df.tokens.apply(lambda x: count_each_opi(x, natopi))
#natopi_indcount2 = [sum(x) for x in zip(*natopi_indcount)]
#sum(natopi_indcount2)
#natopi_sensterm = natopi[natopi_indcount2.index(max(natopi_indcount2))]
#
#fent_indcount = df.tokens.apply(lambda x: count_each_opi(x, fent))
#fent_indcount2 = [sum(x) for x in zip(*fent_indcount)]
#sum(fent_indcount2)
#fent_sensterm = fent[fent_indcount2.index(max(fent_indcount2))]
#
#heroin_indcount = df.tokens.apply(lambda x: count_each_opi(x, heroin))
#heroin_indcount2 = [sum(x) for x in zip(*heroin_indcount)]
#sum(heroin_indcount2)
#heroin_sensterm = heroin[heroin_indcount2.index(max(heroin_indcount2))]
#
#meth_indcount = df.tokens.apply(lambda x: count_each_opi(x, methadone))
#meth_indcount2 = [sum(x) for x in zip(*meth_indcount)]
#sum(meth_indcount2)
#meth_sensterm = methadone[meth_indcount2.index(max(meth_indcount2))]
#
#
#def count_sens_opi(string, drug):
#      if drug in string:
#            return 1
#      else:
#            return 0
#
#df['sens_fent_count'] = df.tokens.apply(lambda x: count_sens_opi(x, fent_sensterm))
#df['sens_natopi_count'] = df.tokens.apply(lambda x: count_sens_opi(x, natopi_sensterm))
#df['sens_heroin_count'] = df.tokens.apply(lambda x: count_sens_opi(x, heroin_sensterm))
#df['sens_methadone_count'] = df.tokens.apply(lambda x: count_sens_opi(x, meth_sensterm))
#
#monthly_counts['fent_sens'] = df['sens_fent_count'].groupby([df.datetime.dt.year, df.datetime.dt.month]).agg('sum')
#monthly_counts['natopi_sens'] = df['sens_natopi_count'].groupby([df.datetime.dt.year, df.datetime.dt.month]).agg('sum')
#monthly_counts['heroin_sens'] = df['sens_heroin_count'].groupby([df.datetime.dt.year, df.datetime.dt.month]).agg('sum')
#monthly_counts['methadone_sens'] = df['sens_methadone_count'].groupby([df.datetime.dt.year, df.datetime.dt.month]).agg('sum')
#
#
#monthly_counts.to_csv('Reddit_monthly_counts_sens.csv')

###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
#FASTTEXT 
os.chdir('\\\\cdc.gov\locker\ONDIEH_TW\Papers-And-Projects\Reddit Drugs\Data')
ft_model = KeyedVectors.load('reddit_wordvectors_fasttext')
### this code generates and saves the word embeddings from fasttext

#ft_model = gensim.models.FastText(
#            df['tokens'],
#            size = 300,
#            window = 5,
#            sg = 1,
#            min_count = 5,
#            workers = 10,
#            iter = 5)
#
#ft_model.wv.save('reddit_wordvectors_fasttext')

##Repeat identifying top 20 similar words to fent as above to compre W2V and FT##
fentterms_FT = ft_model.wv.most_similar('fentanyl', topn = 50)
fenttermFT_words = [term[0] for term in fentterms_FT]


allfentterms_FT = fenttermFT_words[0:20] + ['fentanyl', 'tramadol', 'ultram', 'tramal']

###############################################################################
#Load final fentanyl word list - combination of FT and W2V
allfentterms = pd.read_csv('fent_terms_FIG1.csv')
allfentterms = list(allfentterms['fentterms'])
allfentterms = allfentterms + ['meperidine', 'demerol']
allfentterms = allfentterms[0:34] # alternate analysis just selecting the fent terms


def count_opi(string, list_terms):
      if any(drug in string for drug in list_terms):
            return 1
      else:
            return 0

df['fent_count'] = df.tokens.apply(lambda x: count_opi(x, allfentterms))
df['fent_count'].value_counts()

monthly_counts = pd.DataFrame()
monthly_counts['post'] = df['datetime'].groupby([df.datetime.dt.year, df.datetime.dt.month]).agg('count')
monthly_counts['fent'] = df['fent_count'].groupby([df.datetime.dt.year, df.datetime.dt.month]).agg('sum')
monthly_counts['fentnorm'] = (monthly_counts['fent']/monthly_counts['post']) * 10000 

#monthly_counts.to_csv('Monthly_Counts_Paper_demerol.csv')
monthly_counts.to_csv('Monthly_Counts_Paper_fent_only.csv') # for alternate analysis just selecting the fent terms


#######Obtain Fent Analogues Terms


fent_analogs = ['carfentanil',
                'ocfentanyl',
                'butyrfentanyl',
                'sufentanil',
                'acrylfentanyl',
                'acetylfentanyl',
                'furanylfentanyl',
                'lofentanil'
                ]

# do this to figure out if there are additional terms for each analogue
ana = fent_analogs[4]
ana_w2v = [term[0] for term in model.wv.most_similar(ana, topn = 20)]
ana_ft = [term[0] for term in ft_model.wv.most_similar(ana, topn = 20)]




#Collect frequencies of top 200 terms associated with Fentanyl
#fent_ft_terms = [term[0] for term in x][0:200]
os.chdir('\\\\cdc.gov\locker\ONDIEH_TW\Papers-And-Projects\Reddit Drugs\Data\For Paper')

fent_ft_terms = pd.DataFrame.from_csv('Analog_Terms_v2.csv', index_col = None)
fent_ft_terms = list(fent_ft_terms['Analogues'])


df_fent_ft = pd.DataFrame()
df_fent_ft['Datetime'] = df['datetime']

def count_sens_opi(string, drug):
      if drug in string:
            return 1
      else:
            return 0

for drug in fent_ft_terms:
      print(drug)
      df_fent_ft[drug] = df.tokens.apply(lambda x: count_sens_opi(x, drug))

view1 = df_fent_ft[0:100]

fent_ft_terms_monthly = pd.DataFrame()
for drug in fent_ft_terms:
      print(drug)
      fent_ft_terms_monthly[drug] = df_fent_ft[drug].groupby([df_fent_ft.Datetime.dt.year, df_fent_ft.Datetime.dt.month]).agg('sum')
      
fent_ft_terms_monthly.to_csv('Analog_terms_monthly_paper_v2.csv')

###############################################################################
###############################################################################
###############################################################################
#Check using a control term

cntrl_term = allfentterms

def count_cntrterm(string, term):
      if term in string:
            return 1
      else:
            return 0

df['cntrl_count'] = df.tokens.apply(lambda x: countcntrlterms(x, cntrl_term))
df['cntrl_count'].value_counts()

cntrl_year_counts = pd.DataFrame()
cntrl_year_counts['post'] = df['datetime'].groupby(df.datetime.dt.year).agg('count')
cntrl_year_counts['term'] = df['cntrl_count'].groupby(df.datetime.dt.year).agg('sum')
cntrl_year_counts['norm'] = (cntrl_year_counts['term']/cntrl_year_counts['post']) * 100000 
cntrl_year_counts['year'] = cntrl_year_counts.index 
cntrl_year_counts = cntrl_year_counts[(cntrl_year_counts['year'] >= 2010) & (cntrl_year_counts['year'] <= 2017)]

cntrl_year_counts.plot(x = 'year', y = 'norm')
###############################################################################
###############################################################################
###############################################################################
# checking when the different subreddits started

subdf_sort = subdf.sort_values(['created_at', 'subreddit'], 
                               ascending = [True, False]).groupby('subreddit')
sub_samp = subdf_sort.head(1)

###############################################################################
###############################################################################
###############################################################################
# checking the rate of synthetic opioid posts among just 'opiates' subreddit posts
subdf.columns
opi_subs = subdf[subdf['subreddit'] == 'opiates'] 
opi_coms = comdf[comdf['subreddit'] == 'opiates']


opi_df1 = opi_subs[['alltext', 'created_at']]
opi_df1.columns = ['text', 'datetime']
opi_df2 = opi_coms[['body', 'created_at']]
opi_df2.columns = ['text', 'datetime']

opi_df = pd.concat([opi_df1, opi_df2])
df.text.sample(200)

opi_df['text'] = opi_df.text.apply(lambda x: str(x))

opi_df['cleantext'] = opi_df.text.apply(lambda x: clean_text(x, stemmer = 'None'))

opi_df['tokens'] = opi_df.cleantext.apply(lambda x: tokenize_only(x))
####################################################################
cntrl_term = allfentterms
cntrl_term = ['oxy','oxycodone']
cntrl_term = ['carfentanil','carfent']

def countcntrlterms(string, list_terms):
      if any(term in string for term in list_terms):
            return 1
      else:
            return 0


opi_df['cntrl_count'] = opi_df.tokens.apply(lambda x:  countcntrlterms(x, cntrl_term))
opi_df['cntrl_count'].value_counts()

cntrl_year_counts = pd.DataFrame()
cntrl_year_counts['post'] = opi_df['datetime'].groupby(opi_df.datetime.dt.year).agg('count')
cntrl_year_counts['term'] = opi_df['cntrl_count'].groupby(opi_df.datetime.dt.year).agg('sum')
cntrl_year_counts['norm'] = (cntrl_year_counts['term']/cntrl_year_counts['post']) * 100000 
cntrl_year_counts['year'] = cntrl_year_counts.index 
cntrl_year_counts = cntrl_year_counts[(cntrl_year_counts['year'] >= 2010) & (cntrl_year_counts['year'] <= 2017)]


cntrl_year_counts.plot(x = 'year', y = 'norm')
###############################################################################
###############################################################################
###############################################################################
df1 = subdf[['alltext', 'created_at', 'subreddit']]
df1.columns = ['text', 'datetime', 'subreddit']
df2 = comdf[['body', 'created_at','subreddit']]
df2.columns = ['text', 'datetime', 'subreddit']

df = pd.concat([df1, df2])
df.text.sample(200)

df['text'] = df.text.apply(lambda x: str(x))
df['cleantext'] = df.text.apply(lambda x: clean_text(x, stemmer = 'None'))
df['tokens'] = df.cleantext.apply(lambda x: tokenize_only(x))


df['fent_count'] = df.tokens.apply(lambda x: count_opi(x, allfentterms))
df['fent_count'].value_counts()

subreddit_monthly_counts = pd.DataFrame()
subreddit_monthly_counts['post'] = df['datetime'].groupby([df.datetime.dt.year, df.subreddit]).agg('count')
subreddit_monthly_counts['fent'] = df['fent_count'].groupby([df.datetime.dt.year, df.subreddit]).agg('sum')
subreddit_monthly_counts['fentnorm'] = (subreddit_monthly_counts['fent']/subreddit_monthly_counts['post']) * 100000 

subreddit_monthly_counts.reset_index(inplace = True)

subreddit_monthly_counts.to_csv('subreddit_monthly_counts.csv')

subredtotals = subreddit_monthly_counts.groupby(subreddit_monthly_counts.subreddit).agg('sum')
subredtotals = subredtotals[['post', 'fent']]
sum(subredtotals.fent)

subredtotals.to_csv('syntheticopioid_posts_by_subreddit.csv')
###############################################################################
##############################################################################
##############################################################################
#Normalized fent terms by oxy and oxycodone
oxy_term = ['oxy','oxycodone']
df['fent_count'] = df.tokens.apply(lambda x: count_opi(x, allfentterms))
df['fent_count'].value_counts()

df['oxy_count'] = df.tokens.apply(lambda x: count_opi(x, oxy_term))
df['oxy_count'].value_counts()

monthly_counts = pd.DataFrame()
monthly_counts['post'] = df['datetime'].groupby([df.datetime.dt.year, df.datetime.dt.month]).agg('count')
monthly_counts['fent'] = df['fent_count'].groupby([df.datetime.dt.year,  df.datetime.dt.month]).agg('sum')
monthly_counts['oxy'] = df['oxy_count'].groupby([df.datetime.dt.year,  df.datetime.dt.month]).agg('sum')


#monthly_counts.to_csv('oxy_month_counts.csv')
monthly_counts.to_csv('oxy_month_counts2.csv')

###############################################################################
###############################################################################
###############################################################################
###############################################################################
##############################################################################
##############################################################################
#Normalized fent terms by xanax
list_term = ['xanax','alprazolam']
df['fent_count'] = df.tokens.apply(lambda x: count_opi(x, allfentterms))
df['fent_count'].value_counts()

df['xanax_count'] = df.tokens.apply(lambda x: count_opi(x, list_term))
df['xanax_count'].value_counts()

monthly_counts = pd.DataFrame()
monthly_counts['post'] = df['datetime'].groupby([df.datetime.dt.year, df.datetime.dt.month]).agg('count')
monthly_counts['fent'] = df['fent_count'].groupby([df.datetime.dt.year,  df.datetime.dt.month]).agg('sum')
monthly_counts['xanax'] = df['xanax_count'].groupby([df.datetime.dt.year,  df.datetime.dt.month]).agg('sum')


monthly_counts.to_csv('xanax_month_counts.csv')

###############################################################################
###############################################################################
###############################################################################
###############################################################################
##############################################################################
##############################################################################
#Normalized fent terms by adderall
list_term = ['adderall','adderal','aderall','aderal']
df['fent_count'] = df.tokens.apply(lambda x: count_opi(x, allfentterms))
df['fent_count'].value_counts()

df['adder_count'] = df.tokens.apply(lambda x: count_opi(x, list_term))
df['adder_count'].value_counts()

monthly_counts = pd.DataFrame()
monthly_counts['post'] = df['datetime'].groupby([df.datetime.dt.year, df.datetime.dt.month]).agg('count')
monthly_counts['fent'] = df['fent_count'].groupby([df.datetime.dt.year,  df.datetime.dt.month]).agg('sum')
monthly_counts['adder'] = df['adder_count'].groupby([df.datetime.dt.year,  df.datetime.dt.month]).agg('sum')


monthly_counts.to_csv('adder_month_counts.csv')


###############################################################################
###############################################################################
###############################################################################


#Create plot of fent, oxy, and heroin by mentions of marijuana
#Use w2v and fast text to identify top 10 (5 from each) terms related to marijuan

#W2V
os.chdir('\\\\cdc.gov\locker\ONDIEH_TW\Papers-And-Projects\Reddit Drugs\Data')
model = gensim.models.keyedvectors.KeyedVectors.load_word2vec_format('reddit_wordvectors.txt')

mariterms = model.wv.most_similar('marijuana', topn = 20)
mariterms = [term[0] for term in mariterms]

#FastText
os.chdir('\\\\cdc.gov\locker\ONDIEH_TW\Papers-And-Projects\Reddit Drugs\Data')
ft_model = KeyedVectors.load('reddit_wordvectors_fasttext')

ft_mariterms = ft_model.wv.most_similar('marijuana', topn = 20)
ft_mariterms = [term[0] for term in ft_mariterms]

#Final List of Marijuana Terms

mari_term = mariterms[0:7] + [mariterms[8]] + [mariterms[10]] + ['marijuana']


#Count monthly count of marijuana posts
df['mari_count'] = df.tokens.apply(lambda x: count_opi(x, mari_term))
df['mari_count'].value_counts()

#Count montly count of fent, oxy, and herion post
fent_term = allfentterms
df['fent_count'] = df.tokens.apply(lambda x: count_opi(x, fent_term))
df['fent_count'].value_counts()

oxy_term = ['oxy','oxycodone']
df['oxy_count'] = df.tokens.apply(lambda x: count_opi(x, oxy_term))
df['oxy_count'].value_counts()

heroin_term = ['heroin']
df['heroin_count'] = df.tokens.apply(lambda x: count_opi(x, heroin_term))
df['heroin_count'].value_counts()


mari_monthly_counts = pd.DataFrame()
mari_monthly_counts['post'] = df['datetime'].groupby([df.datetime.dt.year, df.datetime.dt.month]).agg('count')
mari_monthly_counts['fent'] = df['fent_count'].groupby([df.datetime.dt.year,  df.datetime.dt.month]).agg('sum')
mari_monthly_counts['oxy'] = df['oxy_count'].groupby([df.datetime.dt.year,  df.datetime.dt.month]).agg('sum')
mari_monthly_counts['heroin'] = df['heroin_count'].groupby([df.datetime.dt.year,  df.datetime.dt.month]).agg('sum')
mari_monthly_counts['marijuana'] = df['mari_count'].groupby([df.datetime.dt.year,  df.datetime.dt.month]).agg('sum')

mari_monthly_counts.to_csv('marijuana_denom_counts_allfentterms.csv')


#Total monthly posts

#Final dataset to plot in R







































































#Subset comments and subs that use carfent


#######Obtain Fent Analogues
#Fent_ana = model.wv.most_similar('fentanyl', topn = 50)
#Fent_anaterms = [term[0] for term in Fent_ana]
#
#FT_ana = ft_model.wv.most_similar('fentanyl', topn = 50)
#FT_anaterms = [term[0] for term in FT_ana]
#
#fent_analogs = ['carfentanil',
#                'butyrfentanyl',
#                'furanylfentanyl',
#                'acetylfentanyl',
#                'methylfentanyl',
#                'sufentanil',
#                'lofentanil',
#                'ohmefentanyl',
#                'thiofentanyl',
#                'fluorofentanyl',
#                'acrylfentanyl '
#                ]
#
#fent_top10 = ft_model.wv.most_similar(fent_analogs[10], topn =500)
#fent_top10 = [x[0] for x in fent_top10]
#
#
#y = model.wv.most_similar('fentanyl', topn =500)

##Collect frequencies of top 200 terms associated with Fentanyl
##fent_ft_terms = [term[0] for term in x][0:200]
#fent_ft_terms = pd.DataFrame.from_csv('Analog_Terms_v2.csv', index_col = None)
#fent_ft_terms = list(fent_ft_terms['Analogues'])
#
#
#df_fent_ft = pd.DataFrame()
#df_fent_ft['Datetime'] = df['datetime']
#
#def count_sens_opi(string, drug):
#      if drug in string:
#            return 1
#      else:
#            return 0
#
#for drug in fent_ft_terms:
#      print(drug)
#      df_fent_ft[drug] = df.tokens.apply(lambda x: count_sens_opi(x, drug))
#
#fent_ft_terms_monthly = pd.DataFrame()
#for drug in fent_ft_terms:
#      print(drug)
#      fent_ft_terms_monthly[drug] = df_fent_ft[drug].groupby([df_fent_ft.Datetime.dt.year, df_fent_ft.Datetime.dt.month]).agg('sum')
#      
#fent_ft_terms_monthly.to_csv('Analog_terms_monthly_paper_v2.csv')


##############################################################################



#Elbow
#distortions = []
#K = range(1,50)
#
#for k in K:
#      print(k)
#      kmeansModel = KMeans(n_clusters = k).fit(X)
#      kmeansModel.fit(X)
#      distortions.append(sum(np.min(cdist(X, kmeansModel.cluster_centers_,'euclidean'), axis =1)) /X.shape[0])
#
#plt.figure(figsize = (20,20))
#plt.plot(K, distortions, 'bx-')
#plt.xlabel('k')
#plt.ylabel('Distortion')
#plt.title('The Elbow Method showing the optimal k')
#plt.show()

###############################################################################
###############################################################################
#kclusterer = KMeansClusterer(clusters, distance=nltk.cluster.util.cosine_distance)
#assigned_clusters = kclusterer.cluster(X, assign_clusters = True)
#
#
#
#words = list(vocab2)
#for i, word in enumerate(words):  
#    assigned_clusters[i]
#
#clusterdf = pd.DataFrame()
#clusterdf['word'] = words
#clusterdf['cluster'] = assigned_clusters
#
#clusterdict = dict(zip(words, assigned_clusters))

###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################
##TSNE
#from sklearn.manifold import TSNE
#
#vocabulary = model.wv.vocab
#vocabulary = dict((k, vocabulary[k]) for k in closeterms2)
#
#wv = model[vocabulary]
#
#tsne = TSNE(n_components=2, learning_rate = 500, random_state=0, verbose = 2)
#np.set_printoptions(suppress=True)
#Y = tsne.fit_transform(wv)
#
#plt.figure(figsize = (100,100))
#plt.scatter(Y[:, 0], Y[:, 1])
#for label, x, y in zip(vocabulary, Y[:, 0], Y[:, 1]):
#   plt.annotate(label, xy=(x, y), xytext=(0, 0), textcoords='offset points')
#plt.show()
#
################################################################################
################################################################################
################################################################################
#
## running kmeans clustering on word embeddings using scikit
#
#from sklearn.cluster import KMeans
#from sklearn import metrics
#from scipy.spatial.distance import cdist
#
#X = model[vocabulary]
#X
#
#kmeansModel = KMeans(n_clusters = 100).fit(X)
#kmeansModel.labels_
#    
#
#
#clusterdf = pd.DataFrame()
#clusterdf['word'] = list(vocabulary)
#clusterdf['cluster'] = list(kmeansModel.labels_)
#
#clusterdict = dict(zip(words, assigned_clusters))
#
#
## running kmeans clustering on word embeddings using nltk
#from nltk.cluster import KMeansClusterer
#kclusterer = KMeansClusterer(50, distance=nltk.cluster.util.cosine_distance)
#assigned_clusters = kclusterer.cluster(X, assign_clusters = True)
#
#clusterdf = pd.DataFrame()
#clusterdf['word'] = list(vocabulary)
#clusterdf['cluster'] = assigned_clusters
#
#clusterdf['cluster'].value_counts()
#
#clusterdict = dict(zip(vocabulary, assigned_clusters))
#
