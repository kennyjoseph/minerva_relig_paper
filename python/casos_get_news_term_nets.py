__author__ = 'kjoseph'

import itertools, codecs, sys, glob, string
from multiprocessing import Pool
from casostwitter.general_utils import tab_stringify_newline
from casostwitter.Tokenize import extract_tokens
from casostwitter.DictionaryLookUp import DictionaryLookUp
from casostwitter.data_tweet_utils import run_text_through_dictionaries
from nltk.tokenize import word_tokenize

DATA_DIR = "/usr3/kjoseph/final_minerva_data/split_fil_out/"
#DATA_DIR = "/Users/kjoseph/Desktop/tmp_arab/split_fil_out/"

OUTPUT_DIR = "/usr3/kjoseph/final_minerva_data/"
DICTIONARY_LOCATION = "groups_for_goldstone_paper_2.xlsx"

def gen_term_network(args):
    file_name,  dictionaries = args
    if '/1.txt' in file_name:
        print file_name

    try:
        newspaper_text = [line.strip().translate(string.punctuation)
                        for line in codecs.open(file_name,"r","latin1").readlines()
                            if len(line.strip()) > 0]

        counts = []
        net = set()

        for untokenized_sentence in newspaper_text:
            sentence = extract_tokens(untokenized_sentence, word_tokenize,do_arabic_stemming=False)
            new_terms, one_grams, terms_replaced = run_text_through_dictionaries(sentence,dictionaries,6)
        
            if len(new_terms) == 0:
                continue
            # append counts
            counts += [[new_terms[term_it], terms_replaced[term_it]] for term_it in range(len(new_terms)) ]

            new_terms = set(new_terms)
            #if len(new_terms) < 2:
            #    continue

            # append networks
            #net = net.union(set([ "\t".join(comb) for comb in itertools.combinations(new_terms, 2)]))

        return file_name, counts#, net
    except:
        print 'FAILED', file_name
        return file_name, [], []



count_out = codecs.open(OUTPUT_DIR+"news_term_counts.tsv","w","utf8")
#network_out = codecs.open(OUTPUT_DIR+"news_term_network.tsv","w","utf8")
#x = gen_term_network([ DATA_DIR+ "rev_2011-01-01/3.txt", DICTIONARY_LOCATION])
#sys.exit(-1)


CPU_COUNT = 60
pool = Pool(processes=CPU_COUNT)


dictionaries = [['group_dict',DictionaryLookUp(DICTIONARY_LOCATION, 0, 1, has_header=True)]]
results = pool.map(gen_term_network,
                   itertools.izip(glob.iglob(DATA_DIR+"*/*"),
                                 itertools.repeat(dictionaries)))
for x in results:
    try:
        fn = x[0].replace(DATA_DIR,"").replace("rev_","").replace(".txt","")
        date,article_num = fn.split("/")
        for count_v in x[1]:
            count_out.write(tab_stringify_newline([date,article_num,count_v[0],count_v[1]]))
            
        #for net_v in x[2]:
        #    network_out.write(tab_stringify_newline([date,article_num,net_v]))
    except:
        print 'failed'
        pass
count_out.close()
#network_out.close()
