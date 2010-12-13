# CL-TWITTER 

## Introduction

[On 2010-10-24 I cloned](http://cl-twitter.blogspot.com/2010/11/cl-twitter-blog.html) cl-twitter's [darcs repository](http://www.common-lisp.net/project/cl-twitter/darcs/cl-twitter) 
of Ian Eslick's [common lisp twitter package.](http://common-lisp.net/project/cl-twitter/).

The package in the repo no longer compiled. After getting it to compile I found that it did not work with twitter's oauth protocol 
which at that point was the only way to get authorized access to twitter.

It turned out that all the pieces were in place to enable the open authorization protocol. After doing a quick [review](http://cl-twitter.blogspot.com/2010/11/extending-api.html)
I found that about [half](https://spreadsheets.google.com/ccc?key=0AvMxEpw5nMindFRrY0JfNDBCWWhvZ2xmUTNQVXY3ekE&hl=en) 
of the [twitter api](http://apiwiki.twitter.com/w/page/22554679/Twitter-API-Documentation?mode=print) was more-or-less covered.

I split the code into three pieces : 

* an api piece, which enables calls to twitter's rest api.
* a twitter repl client. 
* a database store.

Consult the respective readme's for more information.
## Current status

### api status
 
The rest api is fully covered. 

The streams tweets resources have not been implemented.

### twitter repl client

The twitter repl client is functional. What's missing are good pretty printer outputs for some twitter resources.

### db api
See the README for that module.

