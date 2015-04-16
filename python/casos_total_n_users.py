__author__ = 'kjoseph'

import itertools, codecs, sys
from multiprocessing import Pool
from casostwitter.general_utils import tab_stringify_newline
from casostwitter.Tweet import Tweet
from collections import defaultdict, Counter
import ujson as json
from datetime import datetime

top_dir = "/usr3/kjoseph/final_minerva_data/"

EARLIEST_DATE = datetime(2012, 01, 31, 23, 59)
LATEST_DATE = datetime(2013, 01, 01, 00, 00)

def gen_term_network(args):
    i, data_dir, output_dir = args

    data_file = data_dir + str(i) + ".json"

    all_users = set()
    user_count = set()

    for line in codecs.open(data_file):
        try:
            tweet_json = json.loads(line)
            tweet = Tweet(tweet_json)
            uid = tweet.user['id'] if tweet.user['id'] is not None else tweet.user['screen_name']
            if tweet.user['id'] is not None:
                all_users.add(uid)
        except:
            print 'failed tweet'
            pass

        if tweet.created_at is None or tweet.created_at < EARLIEST_DATE or tweet.created_at > LATEST_DATE:
            continue

        user_count.add(uid)

    output_file = codecs.open(output_dir + str(i) + "_u.txt","w","utf8")
    for u in all_users:
        output_file.write(tab_stringify_newline([u]))
    output_file.close()

    print i
    return len(user_count)

CPU_COUNT = 80
pool = Pool(processes=CPU_COUNT)

results = pool.map(gen_term_network,
                   itertools.izip(range(5000),
                                 itertools.repeat(top_dir+"all_good_tweets/"),
                                 itertools.repeat(top_dir+"user_out_good/")))

out_fil = codecs.open("full_user_count.tsv","w","utf8")
for r in results:
    out_fil.write(str(r)+"\n")

results = pool.map(gen_term_network,
                   itertools.izip(range(5000),
                                 itertools.repeat(top_dir+"tweet_tracker_split/"),
                                 itertools.repeat(top_dir+"user_out_tt/")))

for r in results:
    out_fil.write(str(r)+"\n")
    
out_fil.close()
