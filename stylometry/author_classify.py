#!/usr/bin/env python

from __future__ import division
import preprocess
#from naive_bayes import *
#import numpy as np
import math
from math import sqrt
from math import tanh
import re
#from pysqlite2 import dbapi2 as sqlite
from optparse import OptionParser
import os
import sys
#import bpnn

class Cmd_Options:
  force_retrain = True

def pca(X, to_dim):
  # Principal Component Analysis
  # input: X, matrix with training data as flattened arrays in rows
  # return: projection matrix (with important dimensions first),
  # variance and mean

  #print 'received matrix:', X

  #get dimensions
  print 'processing shape:', X.shape
  num_data,dim = X.shape

  #center data
  mean_X = X.mean(axis=0)
  for i in range(num_data):
      X[i] -= mean_X

  #if dim>100:
  if True:
      print 'PCA - compact trick used'
      M = np.dot(X,X.T) #covariance matrix
      e,EV = np.linalg.eigh(M) #eigenvalues and eigenvectors
      tmp = np.dot(X.T,EV).T #this is the compact trick
      V = tmp[::-1] #reverse since last eigenvectors are the ones we want
      S = np.sqrt(e)[::-1] #reverse since eigenvalues are in increasing order
  else:
      print 'PCA - SVD used'
      U,S,V = np.linalg.svd(X)
      V = V[:num_data] #only makes sense to return the first num_data

  #return the projection matrix, the variance and the mean
  return [[b[0,:to_dim].tolist()[0] for b in redc] for redc in V],S,mean_X

def dec2binstr(n):
  '''convert denary integer n to binary string bStr'''
  bStr = ''
  if n < 0:  raise ValueError, "must be a positive integer"
  if n == 0: return '0'
  while n > 0:
      bStr = str(n % 2) + bStr
      n = n >> 1
  return bStr

def NET_TEST():
  Profile.stop_words = range(2)
  return Neural_Net('test_NN.db', ['0', '1'])

#based on Chap 4: Collective Intelligence
class Neural_Net:
  """This network will be constructed as a multilayer perceptron network
     with the initial input layer, one hidden layer, and the output
     (classification) layer.  For each author, a dictionary has been
     created keying each stop word used in all their works to the frequency
     it appears.  These frequencies are fed into the input layer (currently
     using 119 stop words) and the output layer will generate a binary value
     that will select the author.  So the network will be roughly 119x119x5."""
  def __init__(self, database_path, authors):

    if os.path.exists(database_path) == False:
      Cmd_Options.force_retrain = True

    if Cmd_Options.force_retrain:
      try:
        os.remove(database_path)
      except OSError:
        pass

    self.db_conn = sqlite.connect(database_path)
    self.table_freq_to_hidden  = 'freq_to_hidden'
    self.table_hidden_to_class = 'hidden_to_class'
    self.num_output_nodes      = len(authors)
    self.author_to_layer_dict = dict(zip(authors, map(lambda x: map(lambda v: int(v),dec2binstr(x).rjust(self.num_output_nodes, '0')),
                                                   [1 << x for x in xrange(len(authors))])))
    self.layer_to_author_dict  = dict([(tuple(v),k) for k,v in self.author_to_layer_dict.iteritems()])

    if (os.path.getsize(database_path) == 0):
      print 'Initializing NN table...'
      self.init_tables()

    #now read the network into memory
    self.init_network()

  def update_database(self):
    for i in range(len(Profile.stop_words)):
      for j in range(len(Profile.stop_words)):
        self.setstrength(i,j,0,self.weight_in[i][j])

    for i in range(len(Profile.stop_words)):
      for j in range(self.num_output_nodes):
        self.setstrength(i,j,1,self.weight_out[i][j])

    self.db_conn.commit()

  def init_network(self):
    """read strengths out of the database and initialize node outputs."""
    self.out_freq_layer   = [1.0] * len(Profile.stop_words)
    self.out_hidden_layer = [1.0] * len(Profile.stop_words)
    self.out_class_layer  = [1.0] * self.num_output_nodes

    #extract strengths
    self.weight_in  = []
    for i in range(len(Profile.stop_words)):
      self.weight_in.append([self.getstrength(i,j,0) for j in range(len(Profile.stop_words))])

    self.weight_out = []
    for i in range(len(Profile.stop_words)):
      self.weight_out.append([self.getstrength(i,j,1) for j in range(self.num_output_nodes)])

  def train_association(self, freqs, author, N=0.5):
    self.feedforward(freqs)
    self.back_propagate(self.author_to_layer_dict[author], N)

  def back_propagate(self, target_output, N):
    """run in tandem with feedforward to associate inputs and outputs."""
    def dtanh(x): return 1.0 - x*x

    output_deltas = [0] * self.num_output_nodes
    for k in range(self.num_output_nodes):
      error = target_output[k] - self.out_class_layer[k]
      output_deltas[k] = dtanh(self.out_class_layer[k]) * error

    hidden_deltas = [0] * len(Profile.stop_words)
    for j in range(len(Profile.stop_words)):
      error = 0
      for k in range(self.num_output_nodes):
        error += output_deltas[k] * self.weight_out[j][k]

      hidden_deltas[j] = dtanh(self.out_hidden_layer[j]) * error

    for j in range(len(Profile.stop_words)):
      for k in range(self.num_output_nodes):
        change = output_deltas[k] * self.out_hidden_layer[j]
        self.weight_out[j][k] += N*change

    for i in range(len(Profile.stop_words)):
      for j in range(len(Profile.stop_words)):
        change = hidden_deltas[j] * self.out_freq_layer[i]
        self.weight_in[i][j] += N*change

  def net_classify(self, out_freq):
    def dist_fn(vec1, vec2):
      sum = 0
      for i in range(len(vec1)):
        sum += (vec1[i] - vec2[i])**2

      return sqrt(sum)

    ret = self.feedforward(out_freq)

    min_val = float('infinity')
    for auth,layer in self.author_to_layer_dict.iteritems():
      dist_val = dist_fn(ret, layer)
      if dist_val < min_val:
        selected_author = auth
        min_val = dist_val

    return selected_author


  def feedforward(self, out_freq):
    """passed in a list of the frequencies of stop words in a work (in the order from stop_words.csv)
       and runs feedforward to generate a classification result."""
    self.out_freq_layer = out_freq

    for j in range(len(Profile.stop_words)):
      sum = 0
      for i in range(len(Profile.stop_words)):
        sum += self.out_freq_layer[i] * self.weight_in[i][j]

      self.out_hidden_layer[j] = tanh(sum)

    for k in range(self.num_output_nodes):
      sum = 0
      for j in range(len(Profile.stop_words)):
        sum += self.out_hidden_layer[j] * self.weight_out[j][k]

      self.out_class_layer[k] = tanh(sum)

    return self.out_class_layer

  def init_tables(self):
    """maintains two tables: one containing first to second layer and strength,
       other second to third layer and strength.  Will also initialize all
       connections to equal edge strengths."""
    self.db_conn.execute('create table %s(fromid,toid,strength)' % (self.table_freq_to_hidden))
    self.db_conn.execute('create table %s(fromid,toid,strength)' % (self.table_hidden_to_class))

    #setup default strengths
    for i in xrange(len(Profile.stop_words)):
      for j in xrange(len(Profile.stop_words)):
        self.setstrength(i,j,0,0.1)

    for i in range(len(Profile.stop_words)):
      for j in range(self.num_output_nodes):
        self.setstrength(i,j,1,0.1)

    self.db_conn.commit()

  def getstrength(self, fromid, toid, layer):
    """grabs the edge strength between the specified nodes."""
    if layer == 0:
      table = self.table_freq_to_hidden
    else:
      table = self.table_hidden_to_class

    strength = self.db_conn.execute('select strength from %s where fromid=%d and toid=%d' %
                                    (table,fromid,toid)).fetchone()

    if strength == None:
      if layer == 0: return -0.2 #FIXME: should a negative weight be used if nonexistant?
      if layer == 1: return +0.0

    return strength[0]

  def setstrength(self, fromid, toid, layer, strength):
    if layer == 0:
      table = self.table_freq_to_hidden
    else:
      table = self.table_hidden_to_class

    ret = self.db_conn.execute('select rowid from %s where fromid=%d and toid=%d' %
                               (table,fromid,toid)).fetchone()

    if ret == None:
      self.db_conn.execute('insert into %s (fromid,toid,strength) values (%d,%d,%f)' %
                           (table,fromid,toid,strength))
    else:
      rowid = ret[0]
      self.db_conn.execute('update %s set strength=%f where rowid=%d' %
                           (table,strength,rowid))

  def __del__(self):
    """auto close the database on destruction"""
    self.db_conn.close()

class Node:
  def __init__(self, stopword):
    self.stopword = stopword
    self.last_pos = 0

    self.weights_dict = {}
    for word in Profile.stop_words:
      self.weights_dict[word] = 0

  def KL_calc(self, other_node):
    sum = 0
    for word in self.weights_dict:
      sum += self.weights_dict[word] * \
          math.log10(self.weights_dict[word] / other_node.weights_dict[word])

    return sum

def KL_graph_diff(graph1, graph2):
  kl_sum = 0
  for word in Profile.stop_words:
    kl_sum += (graph1.nodes_dict[word].KL_calc(graph2.nodes_dict[word]) + \
               graph2.nodes_dict[word].KL_calc(graph1.nodes_dict[word])) / 2

  return kl_sum

class Graph:
  def __init__(self, profile):
    self.associated_name = profile.associated_name
    self.nodes_dict = {}

    self.min_weight = 0 #for normalization
    self.max_weigth = 0 #for normalization

    for word in Profile.stop_words:
      self.nodes_dict[word] = Node(word)

    for i in range(len(profile.all_words)):
      curr_word = profile.all_words[i]
      try:
        self.nodes_dict[curr_word]
      except KeyError:
        continue

      #current word is a stop word
      curr_node = self.nodes_dict[curr_word]
      curr_node.last_pos = i + 1
      for word in Profile.stop_words:
        other_node = self.nodes_dict[word]
        if other_node.last_pos != 0:
          weight = curr_node.weights_dict[word] + math.e ** \
              (-abs(other_node.last_pos - curr_node.last_pos))
          curr_node.weights_dict[word] = other_node.weights_dict[curr_word] = weight

    min_arr = []
    max_arr = []
    for word in self.nodes_dict:
      min_arr.append(min([val for key,val in self.nodes_dict[word].weights_dict.iteritems()]))
      max_arr.append(max([val for key,val in self.nodes_dict[word].weights_dict.iteritems()]))

    self.min_weight = min(min_arr)
    self.max_weight = max(max_arr)

    epsilon = 1e-30

    #normalize
    for word in self.nodes_dict:
      for w in self.nodes_dict[word].weights_dict:
        if self.nodes_dict[word].weights_dict[w] <= epsilon:
          self.nodes_dict[word].weights_dict[w] = epsilon
        else:
          self.nodes_dict[word].weights_dict[w] /= self.max_weight

def partitioner(lst, size):
  i = 0
  while i < len(lst):
    yield lst[i:i+size]
    i += size

def calc_common_ngrams(text, n, L):
  """Calculates the most common ngrams and their normalized frequencies
     of occurrences.  Returns a list of 2-tuples."""

  def decrease_sort((k1, v1), (k2,v2)):
    """sort them in reverse order"""
    if   v1 >  v2: return -1
    elif v1 == v2: return  0
    else:          return  1

  #ngrams = map(lambda tup: ''.join(tup), zip(*[text[idx:] for idx in range(n)])[::n])
  ngrams = list(partitioner(text, n))
  ngram_dict = {}
  for ngram in ngrams:
    ngram_dict.setdefault(ngram, 0)
    ngram_dict[ngram] += 1

  ngram_lst = []

  for k,v in ngram_dict.iteritems():
    ngram_lst.append((k, ngram_dict[k] / len(ngrams)))

  return dict(sorted(ngram_lst, cmp=decrease_sort)[:L])

class Profile:

  stop_words     = open('stop_words.csv').read().strip().split(',')
  #good_stopwords = ['any', 'from', 'an', 'may', 'upon', 'can', 'every', 'his', 'do', 'there', 'on']
  good_stopwords =  ['his', 'do', 'there', 'on']

  def __init__(self, text_tree, associated_name):
    self.associated_name         = associated_name #NOTE: this will be the author for known works or the work name otherwise
    self.unproc_work_cat         = "" #will be filled up in GetAllWords()
    self.all_words               = self.GetAllWords(text_tree) #nice to have to mesh all works together
    self.unique_words            = list(set(self.all_words)) #NOTE: not ordered
    self.unique_words_per_work   = self.UniqueWordsPerWork(text_tree) #NOTE: not ordered
    self.works_list              = [work.title for work in text_tree]

    self.avg_words_per_sentence  = self.AverageWordsPerSentence(text_tree)
    self.avg_words_per_paragraph = self.AverageWordsPerParagraph(text_tree)
    self.avg_word_length         = self.AverageWordLength(text_tree)

    self.word_freq_dict          = self.GetWordFreq()

    self.all_word_freq_dict      = self.GetAllWordFreq()

    print 'Generating word length frequencies: %s' % (self.associated_name)
    self.word_length_frequency   = self.GetWordLengthFrequency()

    print 'Finding top keywords: %s' % (self.associated_name)
    self.top_keywords            = self.GetTopKeywords(50)

    print 'Generating ngram profile: %s' % (self.associated_name)
    self.ngram_profile           = calc_common_ngrams(self.unproc_work_cat, n=4, L=5000)

    self.stopword_freq           = self.GetStopwordFreqs(Profile.stop_words, self.all_words)
    self.good_stopword_freq      = self.GetStopwordFreqs(Profile.good_stopwords, self.all_words)

    #break works up into word_chunk pieces
    words_chunk = 1000
    self.words_partition         = filter(lambda chunk: len(chunk) == words_chunk, partitioner(self.all_words, words_chunk))

    #print self.associated_name, self.top_keywords

  def GetMostFrequentWords(self, num_words, text_chunk):
    """returns a list of the num_words most frequent words of text_chunk (from most to least)."""
    ret = self.GetStopwordFreqs(text_chunk, text_chunk)
    return [b for (a,b) in list(reversed(sorted(list(set(ret)))))][:num_words]

  def GetStopwordFreqs(self, key_words, text_chunk):
    """returns a list of the frequency of words from key_words from the text chunk."""
    if key_words == text_chunk:
      all_stop_words = text_chunk
    else:
      all_stop_words = filter(lambda word: word in key_words, text_chunk)

    freq_dict = dict([(word, 0) for word in key_words])
    for word in all_stop_words:
      freq_dict[word] += 1

    for k,v in freq_dict.iteritems():
      freq_dict[k] /= len(text_chunk)

    return [(freq_dict[word], word) for word in key_words]

  def ngram_dist_fn(self, author_ngram, corpus_norm_ngram):
    """This function will only be called by the unknown works
       against all authors."""

    def dict_get(dict, key, default):
      "returns the value at the key or the default if key is not in dict."
      try:
        return dict[key]
      except KeyError:
        return default

    sum = 0
    for g,freq in self.ngram_profile.iteritems():
      fxg  = self.ngram_profile[g]
      ftag = dict_get(author_ngram, g, default=0)
      fng  = dict_get(corpus_norm_ngram, g, default=0)

      sum += ((2*(fxg - ftag)) / (fxg + ftag))**2 * ((2*(fxg - fng)) / (fxg + fng))**2

    return sum

  def decrease_sort(self, (k1, v1), (k2,v2)):
    """sort them in reverse order"""
    if   v1 >  v2: return -1
    elif v1 == v2: return  0
    else:          return  1

  def GetTopKeywords(self, num_keywords):
    """Gets the top keywords for a particular author (only using stop words)."""
    pairs = [(key,val) for key,val in self.all_word_freq_dict.iteritems()]
    pairs = sorted(pairs, cmp=self.decrease_sort)

    #return keywords that aren't too common; those that aren't stop words
    return filter(lambda word: word not in Profile.stop_words, map(lambda (k,v): k, pairs))[:num_keywords]

  def ShowWLFHistPlot(self):
    import matplotlib.pyplot as plt
    import matplotlib.mlab as mlab
    print 'open wlf plot for %s' % (self.associated_name)
    fig = plt.figure()
    ax  = fig.add_subplot(111)
    xs = sorted([key for key in self.word_length_frequency])
    ys = [self.word_length_frequency[x] for x in xs]
    rects = ax.bar(xs, ys, 0.50, color='g')

    ax.set_xlabel('Word Length')
    ax.set_ylabel('Frequencies')
    ax.set_title('Word Length Frequencies: %s' % (self.associated_name))

    ax.set_xbound(0, 15)

    plt.show()

  def KeywordCompare(self, other_profile):
    intersect = set(self.top_keywords).intersection(set(other_profile.top_keywords))

    #return the inverse so lower values correspond to closer 'distances'
    return 1 / len(intersect) if len(intersect) != 0 else float('infinity')


  def WLFDistance(self, other_profile):
    indices = set([key for key in self.word_length_frequency]).intersection( \
                              [key for key in other_profile.word_length_frequency])

    return sum([abs(self.word_length_frequency[idx] - other_profile.word_length_frequency[idx]) for idx in indices])

  def GetWordLengthFrequency(self):
    """returns a dictionary that maps a word length to its frequency"""
    length_freq_dict = {}

    for word in self.all_words:
      length_freq_dict.setdefault(len(word), 0)
      length_freq_dict[len(word)] += 1

    for key in length_freq_dict:
      length_freq_dict[key] /= len(self.all_words)

    return length_freq_dict

  def UniqueWordsPerWork(self, text_tree):
    lst = []

    for work in text_tree:
      words = []
      for paragraph in work.paragraph_objs:
        for sentence in paragraph.sentence_objs:
          words += sentence.words

      words = list(set(words))
      lst.append(words)

    return lst

  def GetWordFreq(self):
    """returns a dictionary that maps each word used by the author to the number
       of documents it appears in."""
    freq = {}
    for i in range(len(self.unique_words_per_work)):
      for word in self.unique_words_per_work[i]:
        freq.setdefault(word, 0)
        freq[word] += 1

    return freq

  def GetAllWordFreq(self):
    """returns a dictionary that maps each word used by the author to the number
       of times it appears in all works."""
    freq = {}
    for word in self.all_words:
      freq.setdefault(word, 0)
      freq[word] += 1

    return freq

  def Distance(self, other_profile):
    sum_of_squares = 0
    #sum_of_squares += (self.avg_words_per_sentence - other_profile.avg_words_per_sentence) ** 2
    #sum_of_squares += (self.avg_words_per_paragraph - other_profile.avg_words_per_paragraph) ** 2
    sum_of_squares += (self.avg_word_length - other_profile.avg_word_length) ** 2

    return sqrt(sum_of_squares)

  def GetAllWords(self, text_tree):
    words = []

    for work in text_tree:
      self.unproc_work_cat += work.unproc_work
      for paragraph in work.paragraph_objs:
        for sentence in paragraph.sentence_objs:
          words += sentence.words

    return words

  def NumWords(self, text_tree):
    return len(self.all_words)

  def AverageWordsPerSentence(self, text_tree):
    sentences = 0

    for work in text_tree:
      for paragraph in work.paragraph_objs:
        sentences += len(paragraph.sentence_objs)

    return self.NumWords(text_tree) / sentences

  def AverageWordsPerParagraph(self, text_tree):
    paragraphs = 0

    for work in text_tree:
      paragraphs += len(work.paragraph_objs)

    return self.NumWords(text_tree) / paragraphs

  def AverageWordLength(self, text_tree):
    words = self.all_words
    num_chars = 0

    for word in words:
      num_chars += len(word)

    return num_chars / len(words)

def graph_author_classify(author_graph_dict, unknown_graph):
  min = float('infinity')
  selected_author = "undecided"
  for author,graph in author_graph_dict.iteritems():
    diff = KL_graph_diff(graph, unknown_graph)
    if diff < min:
      min = diff
      selected_author = author

  return selected_author

def ngram_classify(unknown_profile, author_profile_dict, corpus_norm_profile):
  min_val = float('infinity')
  classified_author = 'error'
  for author, author_profile in author_profile_dict.iteritems():
    dist_val = unknown_profile.ngram_dist_fn(author_profile.ngram_profile, corpus_norm_profile)
    if dist_val < min_val:
      min_val = dist_val
      classified_author = author

  return classified_author

class StatCollector:
  def __init__(self):
    self.correct_pairs = [tuple(map(lambda x: x.strip(), line.split(':'))) for line in open('unknown_author_pairs.txt')]
    self.classifier_data = {}

  def Disp(self, disp):
    """diplays the string and collects the data by parsing it."""
    print disp

    type,work,author = filter(lambda x: x != '', map(lambda word: word.strip(), re.split(r'[:=>]', disp)))
    try:
      self.classifier_data[type].append((work,author))
    except KeyError:
      self.classifier_data[type] = [(work,author)]

  def Disp_Ultimate_Classification(self):
    """does a majority vote on the different classifier and makes the final decision."""
    work_to_authors_dict = {}
    for type,lst in self.classifier_data.iteritems():
      for work,auth in lst:
        work_to_authors_dict.setdefault(work, {})
        work_to_authors_dict[work].setdefault(auth, 0)
        work_to_authors_dict[work][auth] += 1

    ult_pair_dict = {}
    for work,auths in work_to_authors_dict.iteritems():
      ult_pair_dict[work] = max(list(auths.iteritems()), key=lambda (x,y): y)[0]
      print '%s => %s' % (work, ult_pair_dict[work])

    cp_dict = dict(self.correct_pairs)
    cnt = 0
    for k in ult_pair_dict:
      if cp_dict[k] == ult_pair_dict[k]:
        cnt += 1

    print
    print '%d/%d, %.2f%% accurate' % (cnt, len(ult_pair_dict), (cnt / len(ult_pair_dict)) * 100)


  def PrintStats(self):
    correct_cnt = {}
    for correct_pair in self.correct_pairs:
      for type in self.classifier_data:
        correct_cnt.setdefault(type, 0)
        if correct_pair in self.classifier_data[type]:
          correct_cnt[type] += 1

    for type in correct_cnt:
      print '%s => %d/%d, %.2f%% accurate' % (type, correct_cnt[type], len(self.correct_pairs), \
                                             (correct_cnt[type] / len(self.correct_pairs)) * 100)

def k_nearest_neighbor(points_dict, incoming_point, k=1):
  def dist_fn(vec1, vec2):
    sum = 0
    for i in range(len(vec1)):
      sum += (vec1[i] - vec2[i])**2

    return sqrt(sum)

  def cmp_fn((name1,point1), (name2,point2)):
    d1 = dist_fn(point1,incoming_point)
    d2 = dist_fn(point2,incoming_point)

    if   d1  > d2: return  1
    elif d1 == d2: return  0
    else:          return -1

  t_lst = []
  for auth,points in points_dict.iteritems():
    t_lst += zip([auth]*len(points_dict[auth]), points_dict[auth])

  t_lst = sorted(t_lst, cmp=cmp_fn)[:k]

  dicts = {}
  for (k,v) in t_lst:
    dicts.setdefault(k,0)
    dicts[k] += 1

  return max(list(dicts.iteritems()), key=lambda (x,y): y)[0]

def mean_lst_calc(lst):
  sum = [0]*len(lst[0])
  for v in lst:
    for i in range(len(v)):
      sum[i] += v[i]

  return [s/len(lst) for s in sum]

def main():
  parser = OptionParser(usage=
    """Usage: %prog [options]

       Notes: The program is run by calling 'python %prog' on the command line.
       This will then run through the text preprocessing and classification
       and ultimately print out the results for each classifier and their
       combined final result.

       The key external files are the directories "sample_works", "unknown_works",
       stop_words.csv and unknown_author_pairs.txt.

       sample_works/ contains subdirectories of each author which contains a few
       of their selected works to train on.  unknown_works/ is a collection of
       works unknown to the program written by one of the authors.

       unknown_author_pairs.txt contains lines of correctly labelled work,author
       pairs to be used in printing out the results at the end to determine
       classification accuracy.  The program reads in the files from the directories
       at startup so additional works and authors could be added to see how the
       program fairs on those.
    """)
  #parser.add_option("-f", "--force", dest="force",
  #                  default=False, action="store_true",
  #                  help="forces retraining of the neural network")
  parser.add_option("-r", "--run", dest="run",
                    default=False, action="store_true",
                    help="runs the program (prints help by default)")

  (options, args) = parser.parse_args()

  if options.run == False:
    parser.print_help()
    sys.exit(0)

  options.force = False
  Cmd_Options.force_retrain = options.force

  ############################################################################

  print 'Preprocessing sample works...'
  authors_dict = preprocess.PreprocessWorks('sample_works/')

  print 'Preprocessing unknown works...'
  works_dict   = preprocess.PreprocessUnknown('unknown_works/')

  stats = StatCollector()

  author_profile_dict       = {}
  unknown_work_profile_dict = {}

  all_words_in_all_works = []
  for author,text_tree in authors_dict.iteritems():
    author_profile_dict[author] = Profile(text_tree, author)
    all_words_in_all_works += author_profile_dict[author].all_words

  most_common_words_in_all_works = author_profile_dict[author_profile_dict.keys()[0]].GetMostFrequentWords(10, all_words_in_all_works)
  del all_words_in_all_works

  kNN_dict = {}
  for author,_ in authors_dict.iteritems():
    curr = author_profile_dict[author]
    curr.word_partition_freqs = map(lambda chunk: [a for (a,b) in curr.GetStopwordFreqs(most_common_words_in_all_works, chunk)],
                                    curr.words_partition)
    #print author, curr.word_partition_freqs
    kNN_dict[author] = curr.word_partition_freqs

  for unknown_work,text_tree in works_dict.iteritems():
    unknown_work_profile_dict[unknown_work] = Profile(text_tree, unknown_work)
    curr = unknown_work_profile_dict[unknown_work]
    curr.word_partition_freqs = map(lambda chunk: [a for (a,b) in curr.GetStopwordFreqs(most_common_words_in_all_works, chunk)],
                                    curr.words_partition)
    #print unknown_work, curr.word_partition_freqs
    stats.Disp('kNN: %s => %s' % (unknown_work, k_nearest_neighbor(kNN_dict, mean_lst_calc(curr.word_partition_freqs),k=3)))

  #for author,_ in author_profile_dict.iteritems():
  #  print
  #  curr = author_profile_dict[author]
  #  print 'Running PCA...'
  #  print author
  #  P,_,_ = pca(np.matrix(curr.word_partition_freqs), 8)
  #  for point in P:
  #    for dim in point[0]:
  #      print '%.3f ' % (dim),
  #    print

  #for unknown_work,_ in unknown_work_profile_dict.iteritems():
  #  print
  #  curr = unknown_work_profile_dict[unknown_work]
  #  print 'Running PCA...'
  #  print unknown_work
  #  P,_,_ = pca(np.matrix(curr.word_partition_freqs), 8)
  #  for point in P:
  #    for dim in point[0]:
  #      print '%.3f ' % (dim),
  #    print

  #bayes_classifier = NaiveBayes(author_profile_dict)

  print 'Generating stopword graphs...'
  author_graph_dict  = dict([(author,Graph(profile)) for author,profile in author_profile_dict.iteritems()])
  unknown_graph_dict = dict([(work,Graph(profile)) for work,profile in unknown_work_profile_dict.iteritems()])

  #calculate corpus norm profile
  print 'Calculating corpus norm ngrams profile...'
  corpus_norm_profile = calc_common_ngrams( \
        ' '.join([prof.unproc_work_cat for author,prof in author_profile_dict.iteritems()]), n=4, L=5000)

  #print 'Building Neural Network...'
  #neural_net = Neural_Net('author_NN.db', authors_dict.keys())

  if Cmd_Options.force_retrain:
    pass
    #print 'Training Network...'
    #for i in range(1000):
    #  for auth, prof in author_profile_dict.iteritems():
    #    neural_net.train_association([f for f,_ in prof.stopword_freq], auth)

    #print 'Updating database...'
    #neural_net.update_database()

  #print 'Running bpnn lib...'
  #test_up = [map(lambda z: int(z), y) for y in [dec2binstr(x).rjust(8,'0') for x in range(2**8)]]
  #bpnn_Net = bpnn.NN(len(Profile.good_stopwords), len(Profile.good_stopwords), neural_net.num_output_nodes)
  #bpnn_Net = bpnn.NN(int(math.log(len(test_up),2)), int(math.log(len(test_up),2)), int(math.log(len(test_up),2)))
  #training_data = []
  #for auth,prof in author_profile_dict.iteritems():
    #for chunk in prof.words_partition:
    #  training_data.append([map(lambda word: 1 if word in chunk else 0, Profile.good_stopwords),
    #                        list(neural_net.author_to_layer_dict[auth])])
    #training_data.append([map(lambda (v,_): 1000*v, prof.good_stopword_freq), neural_net.author_to_layer_dict[prof.associated_name]])

  #training_data = zip(test_up, test_up)

  #print training_data

  #print 'sflkjslfkj ----->', len(training_data) #2108
  #bpnn_Net.train(training_data, iterations=5000, N=0.5)
  #bpnn_Net.test(training_data)

  print
  print 'Classifications:'

  #run through all metrics to see how well they do separately
  #classifier_dict = {'Distance' : 'Desci', 'WLFDistance' : 'WLF  ', 'KeywordCompare' : 'KW   '}
  classifier_dict = {'WLFDistance' : 'WLF  ', 'KeywordCompare' : 'KW   '}

  for unknown_work, unknown_profile in unknown_work_profile_dict.iteritems():

    stats.Disp('Graph: {work} => {author}'.format(work=unknown_work, author=graph_author_classify( \
                                                                author_graph_dict, unknown_graph_dict[unknown_work])))
    stats.Disp('ngram: {work} => {author}'.format(work=unknown_work, author=ngram_classify( \
                                                                unknown_profile, author_profile_dict, corpus_norm_profile)))

    #stats.Disp('NN   : {work} => {author}'.format(work=unknown_work,
    #                                              author=neural_net.net_classify([f for f,_ in unknown_profile.stopword_freq])))

    #Naive Bayes Decision
    #print 'Bayes: {work} => {author}'.format(work=unknown_work, author=bayes_classifier.classify_work(unknown_profile))

    for func, name in classifier_dict.iteritems():
      dist = float('infinity')
      for author, author_profile in author_profile_dict.iteritems():
        curr_dist = eval('author_profile.%s(unknown_profile)' % (func))
        #curr_dist = author_profile.Distance(unknown_profile)
        if curr_dist < dist:
          dist = curr_dist
          best_match_author = author

      stats.Disp('{type}: {unknown_work} => {author}'.format(type=name, unknown_work=unknown_work, author=best_match_author))

  print

  stats.PrintStats()

  print

  print 'Results:'
  print
  stats.Disp_Ultimate_Classification()
  print

  if preprocess.g_LogVerbosity > 0:

    print 'words per sentence (wps), words per paragraph (wpp), average word length (wl)'
    print

    for unknown_work, profile in unknown_work_profile_dict.iteritems():
      print '%s: wps: %.2f, wpp: %.2f, wl: %.2f' % (unknown_work, profile.avg_words_per_sentence,
                                                    profile.avg_words_per_paragraph,
                                                    profile.avg_word_length)


    print

    for author,profile in author_profile_dict.iteritems():
      print '%s: wps: %.2f, wpp: %.2f, wl: %.2f' % (author, profile.avg_words_per_sentence,
                                                    profile.avg_words_per_paragraph,
                                                    profile.avg_word_length)

if __name__ == '__main__':
  main()

