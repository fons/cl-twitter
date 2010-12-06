# CL-TWITTER : A common lisp API for Twitter

## API Status:

This interface was interactively tested, but bugs remain and not all
features have been exercised.  Help tracking down the last of the 
bugs is always appreciated!

I have also not completely documented all the element slots.  This
should be easy to fix and you can also refer to the twitter APIs.

## Search API:

The search API returns a 'search-result element which contains a set
of 'search-ref elements accessible via (search-result-results elt)

(twitter-search "query string" &rest args) - is a shortcut for
   (twitter-op :search :q "query string" &rest args)
   and returns two values: the list of refs and the 'search-result elt.

(twitter-trends) - returns the top 20 twitter search trends


## Error Handling:


Any API call errors throw a twitter-api-condition which provides
the return code and short and long code messages.  It also provides
the failing URI and the specific server message.  The accessors are
not exported to avoid conflicts, but the slotnames are: 
    return-code, short, long, request, uri.

You can play with this, for example, by trying to perform an API
command with invalid user authentication.



