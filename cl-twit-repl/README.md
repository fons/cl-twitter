# CL-TWIT-REPL : A common lisp repl client

## repl twitter client

I've setup a twitter app called cl-twit-repl. The consumer key and consumer secret are stored in twitter.lisp.

## Initial setup

To authenticate from the repl run (cl-twitter:repl-authenticate-user).
You should see output like this :

    please authorize : #<PURI:URI http://twitter.com/oauth/authorize?&oauth_token=ZcXVnoHiuhQKDp9hk5sFscj7NP0Nopx235aZFjn5M>
    enter PIN :   

Log into your twitter account and copy the url into the browser bar.

After agreeing to allow cl-twit-repl access, twitter will give you a pin.
Enter the pin at the command prompt, and hit return.

If everything goes well, you should be logged in.
Your access tokens are stored in clear text in a file called access.ht, in the repo's directory.
This is obviously not the most secure solution.
The \*access-file\* variable controls the name and location of that file.

## Subsequent access

Run (cl-twitter:get-authenticated-user <user name> ). This will read the access credentials from acces.ht.


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


