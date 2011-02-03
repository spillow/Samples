#!/usr/bin/env python
import re
import os

g_LogVerbosity = 1

class Work:
  class Sentence:
    class Word:
      def __init__(self, sentence):
        self.isQuoted = '"' in sentence
        self.numCommas = len(filter(lambda x: x == ',', sentence))
        self.numSemis  = len(filter(lambda x: x == ';', sentence))
        self.punct = sentence[-2] if sentence[-1] == '"' else sentence[-1]
        self.words = self.GetWords(sentence)

      def GetWords(self, sentence):
        """returns a list of all words forced to lower case for easier comparison."""
        return map(lambda x: x.lower(), filter(lambda x: x != '', re.split(r"""\s+|[";.?!,:()]|--|""", sentence)))

    def __init__(self, paragraph):
      self.sentence_objs = self.GetSentences(paragraph)

    def GetSentences(self, paragraph):
      punct = '?.!'
      sentences = filter(lambda x: x, re.split(r'([%s]")|([%s])' % (punct, punct),paragraph))
      sentences = [re.sub(r'\n', ' ', sentence) for sentence in sentences]
      sentences = [sentences[i]+sentences[i+1] if sentences[i+1][0] in punct else
                   sentences[i] for i in range(len(sentences)-1) if sentences[i][0] not in punct]

      sentence_lst = []
      for sentence in sentences:
        sentence_lst.append(self.Word(sentence))

      return sentence_lst

  def __init__(self, txt, title, author='unknown'):
    self.paragraph_objs = self.GetParagraphs(txt)
    self.title = title
    self.author = author
    self.unproc_work = txt

  def GetParagraphs(self, txt):
    """Takes the text itself and segments it into paragraphs
       and returns a list of paragraph objects that have further
       segmenting into sentences and words."""
    paragraph_lst = []
    paragraphs = filter(lambda x: x != '' and x != None, re.split(r'\r\r|\n\n', txt))

    for paragraph in paragraphs:
      paragraph_lst.append(self.Sentence(paragraph))

    return paragraph_lst

def PreprocessUnknown(unknown_works_dir):
  works = filter(lambda x: x.endswith('.txt'), os.listdir(unknown_works_dir))
  sanitized_work = [re.sub(r'_', ' ', work).split('.')[0] for work in works]

  works_dict = {}

  for work, title in zip(works, sanitized_work):
    file_handle = open('%s/%s' % (unknown_works_dir, work), 'r')
    works_dict[title] = [Work(file_handle.read(), title)]
    file_handle.close()

  return works_dict

def PreprocessWorks(sample_works_dir):
  authors = os.listdir(sample_works_dir)
  sanitized_names = [re.sub(r'_', ' ', author) for author in authors]

  authors_dict = {}

  for author_dir, author_name in zip(authors, sanitized_names):
    works = filter(lambda x: x.endswith('.txt'), os.listdir('%s/%s/' % (sample_works_dir, author_dir)))
    work_lst = []

    if g_LogVerbosity > 1:
      print 'Training %s:' % (author_name)
      for work in works:
        print '  %s' % (re.sub(r'_', ' ', work).split('.')[0])

    for work in works:
      file_handle = open('%s/%s/%s' % (sample_works_dir, author_dir, work), 'r')
      work_lst.append(Work(file_handle.read(), re.sub(r'_', ' ', work).split('.')[0], author_name))
      file_handle.close()

    authors_dict[author_name] = work_lst

  return authors_dict

def main():
  PreprocessWorks('sample_works/')

if __name__ == '__main__':
  main()

