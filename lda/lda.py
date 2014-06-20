from nltk.corpus import stopwords
from nltk.probability import FreqDist

from nltk.corpus import europarl_raw

from collections import defaultdict
from random import random, randrange, shuffle
from nltk.stem.snowball import EnglishStemmer

from optparse import OptionParser
import pickle
import math
import time
import sys

# Assuming English everything for now, would require a bit
# of refactoring to try this sampler on a corpus in another
# language.
#
stemmer = EnglishStemmer()

# Default stopwords from NLTK
stopwords = stopwords.words('english')
# Add some of our own.
stopwords.extend(['european', 'commission','like','must','also','would',\
  'mr','mrs','go','.',',','?','new','put','way','use', 'policy', 'europe',\
  'need', 'member','preside','state', 'parliament', 'union', 'make', \
  'propose', 'country', 'council', 'report', 'take', 'develop', 'right', \
  'question', 'therefore'])
stopwords = set([stemmer.stem(x) for x in stopwords])

def should_exclude(word, sw=stopwords):
  """
  Should we exclude word?

  :param word: a word
  :param sw: a set of stopwords to exclude

  :return: True iff we want to exclude word.
  """
  return (word.lower() in sw) or (len(word) <= 3)

def lgammln(xx):
  """
  Returns the gamma function of xx.
  Gamma(z) = Integral(0,infinity) of t^(z-1)exp(-t) dt.
  (Adapted from: Numerical Recipies in C.)
  Usage: lgammln(xx)
  Copied from stats.py by strang@nmr.mgh.harvard.edu
  """

  coeff = [76.18009173, -86.50532033, 24.01409822, -1.231739516,
       0.120858003e-2, -0.536382e-5]
  x = xx - 1.0
  tmp = x + 5.5
  tmp = tmp - (x + 0.5) * math.log(tmp)
  ser = 1.0
  for j in range(len(coeff)):
    x = x + 1
    ser = ser + coeff[j] / x
  return -tmp + math.log(2.50662827465 * ser)


def dict_sample(d):
  """
  Sample a key from a dictionary using the values as probabilities (unnormalized)
  """
  cutoff = random()
  normalizer = float(sum(d.values()))
  #print "Normalizer: ", normalizer

  current = 0
  for i in d:
    assert(d[i] > 0)
    current += float(d[i]) / normalizer
    if current >= cutoff:
      #print "Chose", i
      return i
  assert False, "Didn't choose anything: %f %f" % (cutoff, current)

def take(n, enbl):
  """
  Take n things from the given enumerable.
  """
  for x in enbl:
    if n <= 0:
      break
    n -= 1
    yield x


class LdaState:
  """
  Store the assignments of a corpus.
  """

  def _init_hyperparameters(self, alpha, lamda):
    """
    Initialize the hyperparameter distributions.

    :param alpha: Alpha parameter value for all distns
    :param lamda: Lambda parameter value for all distns
    """
    self._alpha = alpha
    self._lambda = lamda

    self._sum_alpha = self._alpha * self._num_topics
    self._sum_lambda = self._lambda * self._vocab_size

  def __init__(self, vocabulary, num_topics, alpha, lamda):
    """
    Create a new state for LDA.

    :param vocabulary: An object that supports the contains method.
    :param num_topics: The number of topics to use.
    :param alpha: The per-document Dirichlet hyperparameter.
    :param lamda: The vocabulary Dirichlet hyperparameter.
    """
    self._vocab = vocabulary
    self._vocab_size = len(self._vocab)
    self._num_topics = num_topics
    
    self._init_hyperparameters(alpha, lamda)

    # Parameters.
    self._T = defaultdict(float)
    self._V = defaultdict(float)

    self._current_topic = {}

    # max document ID
    self._max_doc_id = 0
    self.doc_ids = []

  def num_topics(self):
    """
    Return the number of topics
    """
    return self._num_topics

  def report(self, wordlimit):
    """
    Create a human readable report of topic probabilities, printing for each
    topic the highest probability words.

    :param wordlimit: Number of words per topic.
    
    :return: A list of pairs of the form
      (topic identifier, top N words)
    """
    results = []
    for topic in self.topics():
      # Count words for this topic.
      wordcounts = {}
      for word in self._vocab:
        termid = self._vocab[word]
        wordcounts[word] = self._V[(topic, termid)]

      # Normalize so the counts become probabilities.
      normalizer = float(sum(wordcounts.values()))
      
      for word in wordcounts:
        wordcounts[word] /= normalizer

      top_words = [(word,prob) for (word,prob) in take(wordlimit,
        sorted(wordcounts.items(), key=lambda (word,prob):prob, reverse=True))]

      results.append((topic, top_words))

    return results

  def _get_new_doc_id(self, id):
    """
    Gets an ID for a new document.

    :param id: Preferred integer ID, or None if we don't care.
    """
    if id is None:
      id = self._max_doc_id + 1
    if id > self._max_doc_id:
      self._max_doc_id = id
    if id in self.doc_ids:
      raise ValueError("Document ID already in use: " + id)
    return id

  def get_term_id(self, term):
    """
    Returns an identifier corresponding to the term parameter, which
    is a word type that must be in the vocabulary.
    """
    assert(term in self._vocab)
    return self._vocab[term]

  def add_document(self, doc, id=None):
    """
    Add a new document to the state, excluding words not in the vocabulary, and initializing topics randomly.

    This class is responsible for keeping track of the assignment
    of tokens for this document and the overall count.

    :param doc: A sequence of strings representing words.
    :param id: An identifier for the document; should be unique.
    If it's not provided, automatically assign an id.
    """
    docid = self._get_new_doc_id(id)

    words = doc
    for ii in xrange(len(words)):
      token = words[ii]
      termid = self.get_term_id(token)

      # Choose a topic at random.
      topic = randrange(self._num_topics)

      # Increment our counters.
      self._T[(docid, topic)] += 1
      self._V[(topic, termid)] += 1

      # Set the current
      self._current_topic[(docid, ii)] = topic

    self.doc_ids.append(docid)
    return docid


  def change_topic(self, doc_id, index, term, new_topic):
    """
    Change the topic of a single token.

    :param doc_id: The id of the document containing the token.
    :param index: The index of the token.
    :param term: The type of the token.
    :param new_topic: The new topic for the token (-1 for unassigned), must be less than the number of topics.
    """
    assert(doc_id in self.doc_ids)
    assert(new_topic < self._num_topics)
    
    # Get the old topic.
    old_topic = self._current_topic[(doc_id, index)]
    
    if (old_topic == new_topic):
      return

    tid = self.get_term_id(term)

    # Decrement old counts
    if old_topic != -1:
      self._T[(doc_id, old_topic)] -= 1
      self._V[(old_topic, tid)] -= 1

    # Set new topic.
    self._current_topic[(doc_id, index)] = new_topic

    # Increment new counts.
    if new_topic != -1:
      self._T[(doc_id, new_topic)] += 1
      self._V[(new_topic, tid)] += 1

  def topics(self):
    """
    Enumerates possible topics.
    """
    for topic in xrange(self._num_topics):
      yield topic

  def vocab(self):
    """
    Get the vocabulary
    """
    return self._vocab

  def probability(self, doc_id, term, topic):
    """
    Return the probability of a particular term in a document
    taking on a topic assignment, conditioned on all other
    assignments.

    Corresponds to:
    
    alpha[topic] + T[doc, topic]   lambda[term] + V[topic, term]
    ---------------------------- * -----------------------------
       sum(alpha) + sum(T)             sum(lambda) + sum(V)

    :param doc_id: The document the term appears in
    :param term: The term whose probability you wish to report
    :param topic: The topic
    """
    termid = self.get_term_id(term)
    # Not normalized.
    topic_numerator = self._alpha + self._T[(doc_id, topic)]
    topic_denominator = self._sum_alpha + sum([self._T[(doc_id, topic)] for topic in
        xrange(self._num_topics)])
    topic_mass =  topic_numerator / topic_denominator
    token_numerator = self._lambda + self._V[(topic, termid)] 
    token_denominator = self._sum_lambda + sum([self._V[(topic,termid)] for topic in
        xrange(self._num_topics)]) 
    token_mass = token_numerator / token_denominator
    
    #print "Prob mass for term = ", term, "doc_id=",doc_id, "topic=",topic, "is", \
    #    topic_numerator,"/",topic_denominator, "*",token_numerator,"/", token_denominator

    return topic_mass * token_mass

  def lhood(self):
    """
    Compute and return the log-likelihood of self.
    """
    pval = 0.0

    for topic in self.topics():
      v_total = sum([self._V[(topic,self.get_term_id(word))] \
        for word in self._vocab])
      denom = lgammln(v_total + self._sum_lambda)
      for word in self._vocab:
        termid = self.get_term_id(word)
        pval += lgammln(self._V[(topic,termid)] + self._lambda)
      pval -= denom
        
    for doc_id in self.doc_ids:
      t_total = sum([self._T[(doc_id, topic)] for topic in self.topics()])
      denom = lgammln(t_total + self._sum_alpha)
      for topic in self.topics():
        pval += lgammln(self._T[(doc_id, topic)] + self._alpha)
      pval -= denom
    
    return pval

class LdaSampler:
  """
  A class that does the work of actually doing Gibbs sampling for LDA.
  """

  def __init__(self, corpus, num_topics):
    """
    Create a new LDA sampler, taking a corpus, and using it to initialize
    the vocabulary, and assigning topics to the words in the corpus.

    :param corpus: An iterator over documents.
    :param num_topics: Number of topics to use.
    """
    documents = []
    for chapter in corpus:
      # lowercase the words for consistency and stem them.
      documents.append([stemmer.stem(word.lower()) \
        for sentence in chapter for word in sentence])

    vocab = {}
    maxval = 0
    for document in documents:
      for word in document:
        if should_exclude(word):
          continue
        if word not in vocab:
          vocab[word] = maxval
          maxval = maxval + 1

    # Remove excluded words
    self._docs = []
    for document in documents:
      self._docs.append([word for word in document if word in vocab])

    # Hyper parameter constants
    ALPHA_VAL = 0.1
    LAMBDA_VAL = 60.0 / len(vocab)

    self.state = LdaState(vocab, num_topics, ALPHA_VAL, LAMBDA_VAL)
    doc_id = 0
    for document in self._docs:
      their_id = self.state.add_document(document, id=doc_id)
      assert(their_id == doc_id)
      doc_id = doc_id + 1

  def documents(self):
    """
    Gets the documents.
    """
    for doc in self._docs:
      yield doc

  def num_topics(self):
    """
    Get number of topics
    """
    return self.state.num_topics()

  def corpus_size(self):
    """
    Computes and returns the number of words in the corpus.
    """
    return sum([len(doc) for doc in self._docs])

  def sample_word(self, doc_id, index):
    """
    Sample the topic assignment of a single token. Also 
    changes the topic to the newly sampled topic in the LDA state.

    :param doc_id: The document the word appears in.
    :param index: The index of the token.
    :return: The new topic associated with the token.
    """
    assert(0 <= doc_id and doc_id < len(self._docs))
    doc = self._docs[doc_id]
    assert(0 <= index and index < len(doc))
    term = doc[index]

    # Unset the current topic
    self.state.change_topic(doc_id, index, term, -1)
    
    distribution = {}
    for topic in self.state.topics():
      distribution[topic] = self.state.probability(doc_id, term, topic)
    new_topic = dict_sample(distribution)

    # Set the new topic
    self.state.change_topic(doc_id, index, term, new_topic)
    return new_topic

  def run_sampler(self, iterations = 100, outfile=None):
    """
    Run a number of Gibbs sampling iterations.

    :param iterations: The number of iterations to run the sampler.
    :param outfile: File to write output to. Can be None

    :return: A list of likelihoods obtained during inference.
    """
    if len(self._docs) == 0:
      raise ValueError("Need at least one document in the corpus!")

    corpus_size = self.corpus_size()

    if outfile is not None:
      outfile.write("Iteration,Log-Likelihood,Corpus Size\n")

    for iteration in xrange(iterations):
      for doc_id in xrange(len(self._docs)):
        doc = self._docs[doc_id]
        for index in xrange(len(doc)):
          #term = doc[index]
          #assert(term in self.state.vocab())
          self.sample_word(doc_id, index)

      # Print diagnostic output to screen.
      lhood = self.lhood()
      print "Iteration %d: log lhood: %g / log lhood/word: %g" % \
        (iteration + 1, lhood, lhood / corpus_size)
      
      # Write comma-separated output.
      if outfile is not None:
        outfile.write("%d,%g,%g\n" % \
          (iteration + 1, lhood, corpus_size))

  def lhood(self):
    """
    Compute the likelihood of the current configuration of latent variables.

    :return: The likelihood of the current configuration of topic
    assignments.
    """
    return self.state.lhood()

def parseOptions():
  """
  Parse and return the command line options from the user.
  """
  usage = "usage: %prog [options] (--help for help)"
  opt_parser = OptionParser(usage)
  opt_parser.add_option("-i", "--iterations", action="store", type='int',
    help="Number of iterations to the run the Gibbs sampler for.")
  opt_parser.add_option("-l", "--doc-limit", action="store",
    type='int', help="limit for the number of docs to consider (-1 for none)")
  opt_parser.add_option("--lhood-file", action="store",
    help="File to write the likelihood per iteration values to.")
  opt_parser.add_option("--topic-report", action="store",
    help="File to write the topic report to.")
  opt_parser.add_option("-r","--randomize", action="store_true",
    help="Whether to randomize the order in which the " + \
    "documents are considered.")
  opt_parser.add_option("-t","--num-topics", action="store",
    type='int', help="Number of topics to use, default 5")
  opt_parser.add_option("-p","--sampler-file", action="store",
    help="Path to the saved sampler file")

  opt_parser.set_defaults(iterations=100, doc_limit=-1,
    lhood_file="lhood.csv", topic_report="treport.txt",
    randomize=False, sampler_file=None, num_topics=5)

  options = None
  (options,_) = opt_parser.parse_args()
  return options

if __name__ == "__main__":
  options = parseOptions()

  doc_limit = options.doc_limit
  iterations = options.iterations
  lhood_file = options.lhood_file
  topic_report = options.topic_report
  randomize = options.randomize
  sampler_file = options.sampler_file
  num_topics = options.num_topics

  # Run LDA for the specified number of iterations limiting
  # yourself to the specified number of documents

  sampler = None
  if sampler_file is not None:
    print "Loading pickled sampler file", sampler_file, "..."
    try:
      with open(sampler_file, 'r') as f:
        p = pickle.Unpickler(f)
        sampler = p.load()
    except:
      print "WARNING: Problem opening saved sampler", \
        sampler_file, ", starting from scratch."

  if sampler is None:
    print "Creating fresh sampler from Europarl English corpus..."
    corpus = None

    raw_documents = [x for x in europarl_raw.english.chapters()]
    
    # Shuffle the documents if we want a random order.
    # We don't randomize the word order within the document
    # but maybe we should -- The model is bag-of-words so
    # this would not impact correctness.
    if randomize:
      shuffle(raw_documents)
    
    # Assume no limit for negative/zero
    if doc_limit <= 0:
      doc_limit = len(raw_documents)

    # Limit the size
    corpus = raw_documents[:doc_limit]
    
    # Create the sampler
    sampler = LdaSampler(corpus, num_topics)

  print "Running LDA with", len([x for x in sampler.documents()]), "documents for", \
    iterations, "iterations\nlhood file =", lhood_file, "num topics =", \
    sampler.num_topics(), "vocab size =", len(sampler.state.vocab())

  with open(lhood_file, 'w+') as f:
    sampler.run_sampler(iterations=iterations, outfile=f)

  print ("Writing topic report to file %s:" % topic_report)
  # Report top words per topic
  with open(topic_report, 'w+') as f:
    for row in sampler.state.report(10):
      topicid, top10 = row
      f.write("Topid ID: %2d\n" % topicid)
      for (word, prob) in top10:
        # Write probabilities with scientific notation, else they'll
        # all end up zeroes.
        f.write("\tWord: %30s  Probability: %5.2e\n" % (word,prob))

  pickle_name = 'sampler.pickle'
  print "Saving the sampler to pickle", pickle_name
  with open(pickle_name, 'w+') as f:
    p = pickle.Pickler(f)
    p.dump(sampler)

  print "Done"
