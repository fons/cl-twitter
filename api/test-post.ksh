#!/usr/bin/env ksh

echo "hello"

oauth_consumer_key="the_consumer_key"
oauth_nonce="the_oauth_none"
oauth_signature="the_open_oath_signature"
oauth_ts="the_oauth_timestamp"
oauth_token="the_oauth_token"




oauth_consumer_key="9hOStbD2Zf7x0mUoo7cYBg"
oauth_nonce="d0ad9124c7eac68ba4f72081ee596042"
oauth_signature="bTRCpB5dsdabe6DZZknGqeNKV4I%3D"
oauth_signature_method="HMAC-SHA1"
oauth_timestamp="1427160979"
oauth_token="111735825-ZRQtsXooX93jU14ymzL9FcV5H0nfzkdzyyTsKlcg"

#


oauth_consumer_key="9hOStbD2Zf7x0mUoo7cYBg" # (oauth_consumer_key . 9hOStbD2Zf7x0mUoo7cYBg) 
oauth_nonce="955e77ffeb3474ebe4222313c171600c" #(oauth_nonce  . 184BEL6S292F1B5N4L241L3B65145X41K4K343Z5Q3Q5M1X1L654QM)
oauth_signature="RhKfZc94Q4tAgQ762qP61WthfjU%3D" #(oauth_signature . XYtdNZCGz54QZ1mzv2rLt77XGvA=)
oauth_signature_method="HMAC-SHA1"
oauth_timestamp="1427161539" #(oauth_timestamp . 1427161729)
oauth_token="111735825-ZRQtsXooX93jU14ymzL9FcV5H0nfzkdzyyTsKlcg"   # (oauth_token . 111735825-ZRQtsXooX93jU14ymzL9FcV5H0nfzkdzyyTsKlcg)


oauth_consumer_key="9hOStbD2Zf7x0mUoo7cYBg"
oauth_nonce ="184BEL6S292F1B5N4L241L3B65145X41K4K343Z5Q3Q5M1X1L654QM"
oauth_signature="XYtdNZCGz54QZ1mzv2rLt77XGvA="
oauth_timestamp="1427161729"
oauth_token="111735825-ZRQtsXooX93jU14ymzL9FcV5H0nfzkdzyyTsKlcg"


curl --request 'POST' 'https://api.twitter.com/1.1/statuses/update.json' --data 'status=test+message+post+1' --header 'Authorization: OAuth oauth_consumer_key="'$oauth_consumer_key'", oauth_nonce="'$oauth_nonce'", oauth_signature="'$oauth_signature'", oauth_signature_method="HMAC-SHA1", oauth_timestamp="'$oauth_timestamp'", oauth_token="'$oauth_token'", oauth_version="1.0"' --verbose












