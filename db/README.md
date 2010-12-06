# CL-TWIT-DB : A database store for tweets

## Note
 
The api calls that this component relies on may have changed.
In addition, this component relies on the [elepehant](http://common-lisp.net/project/elephant/index.html) package
and [Berkley DB](http://www.oracle.com/technetwork/database/berkeleydb/documentation/index.html).

Berkely DB needs to be installed before you can even attempt to use this.

I have not reviewed this component in detail.
My intention is to make this work with [elepehant](http://common-lisp.net/project/elephant/index.html) as well as [mongodb](http://www.10gen.com/index) 
through [cl-mongo](https://github.com/fons/cl-mongo).

Fons Haffmans 12/05/2010

## Twitter Database 


Simple elephant-based database for users, tweet and messages.  When
the db is open, it automatically stores objects that are retrieved via
the API and provides a series of simple accessors that can be easily
expanded.

Open and close:
(open-twitter-db &optional elephant-spec)
(close-twitter-db)

Users
(find-twitter-user &key id screen-name name)
(map-twitter-users fn)

Tweets
(get-tweet id)
(map-tweets fn)
(map-user-tweets fn user)
(user-tweets user)

Messages
(get-tweet-msg id)
(map-tweet-msgs fn)
(map-user-msgs fn user)
(user-msgs user)
(map-user-received-messages fn user)

## DB API Status:


The DB interface is highly experimental and has only been partially
tested.  In particular, we currently store copies of embedded 
structures and should use the existing DB id references to save/restore.
We should also hook into the lookup functions in the main API to avoid
reparsing stored objects.

The time index is currently based on strings, so not properly ordered,
we need to cleanup using universal time integers instead of strings
throughout the API.

Ian Eslick
2/26/2009