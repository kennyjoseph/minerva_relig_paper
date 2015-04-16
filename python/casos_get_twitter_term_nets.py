__author__ = 'kjoseph'

import itertools, codecs, sys
from multiprocessing import Pool
from casostwitter.general_utils import tab_stringify_newline
from casostwitter.DictionaryLookUp import DictionaryLookUp
from casostwitter.data_tweet_utils import run_tweet_through_dictionaries
from casostwitter.Tweet import Tweet
from collections import defaultdict, Counter
import ujson as json

top_dir = "/usr3/kjoseph/final_minerva_data/"
#top_dir = "/Users/kjoseph/Desktop/tmp_arab/"

DICTIONARY_LOCATION = "groups_for_goldstone_paper_2.xlsx"

def print_user_counter(user_count_dict,output_file,type_str):
    for country, date_dict in user_count_dict.items():
        for date, user_set in date_dict.items():
            output_file.write(tab_stringify_newline([country,date,len(user_set),type_str]))


def gen_term_network(args):
    i, data_dir, info_dir, output_dir, dictionary_loc, postfix = args

    dictionaries = [['group_dict',DictionaryLookUp(dictionary_loc, 0, 1, has_header=True)]]

    data_file = data_dir + str(i) + ".json"
    info_file = info_dir + str(i) + ".txt"
    output_count_file = codecs.open(output_dir+str(i)+"_termcount_"+postfix+".tsv", "w", "utf8")
    output_user_count_file = codecs.open(output_dir+str(i)+"_usercount_"+postfix+".tsv", "w", "utf8")
    #output_net_file = codecs.open(output_dir+str(i)+"_termnet_"+postfix+".tsv", "w", "utf8")

    tweet_country_map = dict()
    for line in codecs.open(info_file,"r","utf8"):
        l_spl = line.strip().split("\t")
        text_country = [x for x in l_spl[2].split(",") if x != "" ]
        geo_country = [x for x in l_spl[3].split(",") if x != "" ]
        if len(text_country) > 0 or len(geo_country) > 0:
            tweet_country_map[l_spl[0]] = (text_country, geo_country)

    user_count_text = defaultdict(lambda:defaultdict(set))
    user_count_geo = defaultdict(lambda:defaultdict(set))
    user_count_full = defaultdict(lambda:defaultdict(set))

    for line in codecs.open(data_file):
        try:
            tweet_json = json.loads(line)
            tweet = Tweet(tweet_json)
        except:
            print 'failed tweet'
            pass

        if tweet.created_at is None or str(tweet.id) not in tweet_country_map:
            continue

        new_terms, one_grams, terms_replaced = run_tweet_through_dictionaries(tweet,dictionaries,6)

        # get tweet information
        date = tweet.created_at.strftime("%Y-%m-%d")
        month =  tweet.created_at.strftime("%Y-%m")
        text_country, geo_country = tweet_country_map[str(tweet.id)]
        uid = tweet.user['id'] if tweet.user['id'] is not None else tweet.user['screen_name']

        ##update user counts
        for c in text_country:
            user_count_full[c][month].add(uid)
            user_count_text[c][month].add(uid)
        for c in geo_country:
            user_count_full[c][month].add(uid)
            user_count_geo[c][month].add(uid)

        # output term counts
        for term_it in range(len(new_terms)):

            text = tab_stringify_newline([tweet.id, date, uid, new_terms[term_it], terms_replaced[term_it]],
                                         newline=False)
            for c in text_country:
                output_count_file.write(tab_stringify_newline([text,c, "TEXT"]))
            for c in geo_country:
                output_count_file.write(tab_stringify_newline([text,c, "GEO"]))

        new_terms = set(new_terms)
        #if len(new_terms) < 2:
        #    continue

        # output networks
        #for comb in itertools.combinations(new_terms, 2):
        #    text = tab_stringify_newline([tweet.id, uid, date, comb[0], comb[1]],newline=False)
        #    for c in text_country:
        #        output_net_file.write(tab_stringify_newline([text, c, "TEXT"]))
        #    for c in geo_country:
        #        output_net_file.write(tab_stringify_newline([text, c, "GEO"]))

    print_user_counter(user_count_full,output_user_count_file,"full")
    print_user_counter(user_count_geo,output_user_count_file,"geo")
    print_user_counter(user_count_text,output_user_count_file,"text")
    output_user_count_file.close()
    #output_net_file.close()
    output_count_file.close()
    return i





#for i in range(5000):
#    gen_term_network([i, top_dir+"all_good_tweets/",
#                      top_dir+"all_good_tweets_tweetinfo/",
#                      top_dir+"term_net_out/",
#                      DICTIONARY_LOCATION,
#                      "all_good"])
#sys.exit(-1)


CPU_COUNT = 35
pool = Pool(processes=CPU_COUNT)

general_utils.mkdir_no_err(top_dir+"term_data_out/")

results = pool.map(gen_term_network,
                   itertools.izip(range(5000),
                                 itertools.repeat(top_dir+"all_good_tweets/"),
                                 itertools.repeat(top_dir+"all_good_tweets_tweetinfo/"),
                                 itertools.repeat(top_dir+"term_data_out/"),
                                 itertools.repeat(DICTIONARY_LOCATION),
                                 itertools.repeat("all_good")))
for r in results:
    print r

results = pool.map(gen_term_network,
                   itertools.izip(range(5000),
                                 itertools.repeat(top_dir+"tweet_tracker_split/"),
                                 itertools.repeat(top_dir+"tweet_tracker_split_tweetinfo/"),
                                 itertools.repeat(top_dir+"term_data_out/"),
                                 itertools.repeat(DICTIONARY_LOCATION),
                                 itertools.repeat("tweet_tracker")))
for r in results:
    print r
