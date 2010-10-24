# CL-TWITTER

## NOTE

#### 2010-10-24

This is a clone of cl-twitter's darcs repository http://www.common-lisp.net/project/cl-twitter/darcs/cl-twitter.
More information can be found [on cl-twitters cliki page](http://common-lisp.net/project/cl-twitter/).



## Description 
	
This is a simple interface to the Twitter API current as of February 2009:
(Reference: http://apiwiki.twitter.com/REST+API+Documentation)

It also implements the search and trends APIs.
(Reference: http://apiwiki.twitter.com/Search+API+Documentation)

The primary interface to the API is via the twitter-op command:
   (twitter-op :command &rest args)
which calls the API :command and returns one or more 'elements' which
can be primitives, 'twitter-user, 'tweet or 'twitter-message.  This
API uses struct objects for composite values such as users or tweets.

You can view documentation for commands and elements using:
(list-commands)
(command-help :command-name) 
(element-help 'type)
  types: twitter-user, tweet, twitter-message, search-result, search-ref

Loading: :

(asdf:oos 'asdf:load-op :cl-twitter) -> Creates package :twitter and :twit
(asdf:oos 'asdf:load-op :cl-twitter-db) -> Uses elephant to record elements

## Authentication:


Most API commands require an authenticating user "An authenticating"
user (with valid username & password slots) can be passed to a command
using the :user option.  There is a convenience function
authenticate-user which takes a username and password, creates and
populates a user object (if authentication works) and sets a global
variable *twitter-user* which will be used to authenticate calls by
default.  The :user option overrides the default parameter.

## Quick Start and Shortcuts:


There are a small set of quick interactive routines for using the API:

(authenticate-user screen-name password) - will set the default user if valid
(send-tweet text) - will post a tweet / status update for the current user
(latest-tweets) - the list of latest tweets for each user
(print-tweets list) - will print the tweet sender & text

## Example use:


TWITTER> (twitter-op :test)
"ok"

TWITTER> (authenticate-user "ieslick" "password")
#<TWITTER-USER 'ieslick'>

TWITTER> (latest-tweets)
(#<TWEET 'ieslick' id:1219635898> #<TWEET 'ieslick' id:1217269236>
 #<TWEET 'ieslick' id:1215984895> #<TWEET 'ieslick' id:1214353382>)

TWITTER> (print-tweets *)
status: "The lisp twitter api is nearly done!"
by: Ian Eslick  (ieslick) on: Tue Feb 17 00:49:11 +0000 2009

status: "Benefits of working from home; lunchtime stew in Cambridge, MA http://l\
oopt.us/-Hps2Q"
by: Ian Eslick  (ieslick) on: Mon Feb 16 17:49:52 +0000 2009

status: "My lisp can tweet!"
by: Ian Eslick  (ieslick) on: Mon Feb 16 04:52:28 +0000 2009

status: "Family outing! in Cambridge, MA http://loopt.us/zUhMpQ"
by: Ian Eslick  (ieslick) on: Sun Feb 15 21:14:26 +0000 2009

NIL

TWITTER> (send-tweet "cl-twitter is released!")
#<TWEET 'ieslick' id:1219635898>

TWITTER> (describe *)
by: ieslick (Ian Eslick ) created: Tue Feb 17 17:33:33 +0000 2009
msg: cl-twitter is released!
; No value

TWITTER> (twitter-op :user-show)
________________________________________


## Search API:


The search API returns a 'search-result element which contains a set
of 'search-ref elements accessible via (search-result-results elt)

(twitter-search "query string" &rest args) - is a shortcut for
   (twitter-op :search :q "query string" &rest args)
   and returns two values: the list of refs and the 'search-result elt.

(twitter-trends) - returns the top 20 twitter search trends


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


## Error Handling:


Any API call errors throw a twitter-api-condition which provides
the return code and short and long code messages.  It also provides
the failing URI and the specific server message.  The accessors are
not exported to avoid conflicts, but the slotnames are: 
    return-code, short, long, request, uri.

You can play with this, for example, by trying to perform an API
command with invalid user authentication.

## API Status:


This interface was interactively tested, but bugs remain and not all
features have been exercised.  Help tracking down the last of the 
bugs is always appreciated!

I have also not completely documented all the element slots.  This
should be easy to fix and you can also refer to the twitter APIs.

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