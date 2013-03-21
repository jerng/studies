# Authorize URL	https://api.twitter.com/oauth/authorize
# Access token URL	https://api.twitter.com/oauth/access_token

import base64
import cgi
import datetime
import hashlib
import hmac
import os
import sys
import time
import urllib
import urllib2
import uuid

from xml.etree import ElementTree
# TODO: figure out how this syntax differs from "import xml, xml.dom.minidom()" etc.

from google.appengine.api import users
from google.appengine.ext import db

#
#
#
#

app_url = 'http://killtweets.appspot.com'

# GAE authentication
if users.get_current_user() is None:
  print 'Location: ' + users.create_login_url(app_url)
  sys.exit()
else:
  current_user = str(users.get_current_user())

# Session model, dependent on GAE 'users' module
class Session(db.Expando):
  created = db.DateTimeProperty()

# if no session is found, create one; if an expired one is found, reset it to zero
session  = Session.get_by_key_name(current_user)
if session is None:
    session = Session(key_name = current_user, created = datetime.datetime.utcnow())
    # fugly: how do I rewrite the class so that I just have to write here `session = Session()?

#
#
#
#

twitterAppConsumerKey	= 'MbjCB24mtHCfCEufgr3FQ'
twitterAppConsumerSecret = 'XHwoteKB35gOzv9g7eRQXByGyKSSmxB23rbXbD5QY'

# block 1
# no query string
if not hasattr(session,'request_oauth_token'):
  # hypothesis: no Access Token or Oauth Verifier available
  # ask for an Oauth Verifier
  tokenSecret = ''

  tokenRequestCallbackUrl = urllib.quote(app_url,'')
  tokenRequestUrl = 'https://api.twitter.com/oauth/request_token?'

  tokenRequestNonce = uuid.uuid4()
  tokenRequestTimestamp = int(time.time())
  tokenRequestNormalisedRequestParameters = urllib.quote(\
    'oauth_callback=%s&'\
    'oauth_consumer_key=%s&'\
    'oauth_nonce=%s&'\
    'oauth_signature_method=HMAC-SHA1&'\
    'oauth_timestamp=%s&'\
    'oauth_version=1.0'\
    % ( tokenRequestCallbackUrl,\
        twitterAppConsumerKey,\
        tokenRequestNonce,\
        tokenRequestTimestamp\
        ),'')
  tokenRequestSignatureBaseString = '%s&%s&%s' % ( \
    'POST', \
    urllib.quote(tokenRequestUrl[:tokenRequestUrl.find('?')].lower(),''), \
    tokenRequestNormalisedRequestParameters\
    )
  tokenRequestSignatureHmacKey = urllib.quote(twitterAppConsumerSecret,'') + '&' + tokenSecret
  tokenRequestSignature = urllib.quote(base64.encodestring(hmac.new(\
    tokenRequestSignatureHmacKey,\
    tokenRequestSignatureBaseString,\
    hashlib.sha1
    ).digest()),'')
  tokenRequestHeaders = {'Authorization':'OAuth '\
    'oauth_callback="%s",'\
    'oauth_consumer_key="%s",'\
    'oauth_nonce="%s",'\
    'oauth_signature_method="HMAC-SHA1",'\
    'oauth_timestamp="%s",'\
    'oauth_signature="%s",'\
    'oauth_version="1.0"'\
    % ( tokenRequestCallbackUrl,\
        twitterAppConsumerKey,\
        tokenRequestNonce,\
        tokenRequestTimestamp,\
        tokenRequestSignature
        )\
    }

  try:
    res = urllib2.urlopen(urllib2.Request(tokenRequestUrl,'',tokenRequestHeaders))
    res_string = res.read()

    # save request token components to Session model
    res_dict = {}
    res_list = res_string.split('&')
    for x in res_list:
      xp = x.partition('=')
      res_dict[xp[0]] = xp[2]
    session.request_oauth_token = str(res_dict['oauth_token'])
    session.request_oauth_token_secret = str(res_dict['oauth_token_secret'])
    if hasattr(session, 'access_oauth_token'):
      del session.access_oauth_token
    if hasattr(session, 'access_oauth_token_secret'):
      del session.access_oauth_token_secret
    session.put()
    # probably a shorter way to write the last two lines; list comprehension?

    print 'Location: https://api.twitter.com/oauth/authorize?' + res_string
    sys.exit()
    # goto: new request that will hit block 2
    # note: res.read() should equal "oauth_token=xyz&oauth_token_secret=abc ..."
    # TODO: remove security hazard, and remove query string; get root condition to check Session model instead of for presence of query string
    # TODO: try cgi.parse_qs
  except IOError, e:
    output_html = 'request token request detained! - ' + str(e.info()) + e.read()
    print "Content-Type: text/html\n\n" + '''
      <html>
        <head>
          <title>KillTweets!</title>
        </head>
        <body>
          <h1>KillTweets: now in HTML</h1>
            <pre>%s</pre>
        </body>
      </html>
      ''' % (output_html)
    sys.exit()
    # debug and die

#
#
#
#

# block 2
# query string found
# no access token
if not hasattr(session,'access_oauth_token'):

  query_string = os.environ['QUERY_STRING']
  query_dict = {}
  query_list = query_string.split('&')
  for x in query_list:
    xp = x.partition('=')
    query_dict[xp[0]] = xp[2]
  session.request_oauth_verifier = query_dict['oauth_verifier']

  tokenSecret = session.request_oauth_token_secret

  tokenRequestUrl = 'https://api.twitter.com/oauth/access_token?'

  tokenRequestNonce = uuid.uuid4()
  tokenRequestTimestamp = int(time.time())
  tokenRequestNormalisedRequestParameters = urllib.quote(\
    'oauth_consumer_key=%s&'\
    'oauth_nonce=%s&'\
    'oauth_signature_method=HMAC-SHA1&'\
    'oauth_timestamp=%s&'\
    'oauth_token=%s&'\
    'oauth_verifier=%s&'\
    'oauth_version=1.0'\
    % ( twitterAppConsumerKey,\
        tokenRequestNonce,\
        tokenRequestTimestamp,\
        session.request_oauth_token,\
        session.request_oauth_verifier
        ),'')
  tokenRequestSignatureBaseString = '%s&%s&%s' % ( \
    'POST', \
    urllib.quote(tokenRequestUrl[:tokenRequestUrl.find('?')].lower(),''), \
    tokenRequestNormalisedRequestParameters\
    )
  tokenRequestSignatureHmacKey = urllib.quote(twitterAppConsumerSecret,'') + '&' + tokenSecret
  tokenRequestSignature = urllib.quote(base64.encodestring(hmac.new(\
    tokenRequestSignatureHmacKey,\
    tokenRequestSignatureBaseString,\
    hashlib.sha1
    ).digest()),'')
  tokenRequestHeaders = {'Authorization':'OAuth '\
    'oauth_consumer_key="%s",'\
    'oauth_nonce="%s",'\
    'oauth_signature_method="HMAC-SHA1",'\
    'oauth_timestamp="%s",'\
    'oauth_token="%s",'\
    'oauth_verifier="%s",'\
    'oauth_signature="%s",'\
    'oauth_version="1.0"'\
    % ( twitterAppConsumerKey,\
        tokenRequestNonce,\
        tokenRequestTimestamp,\
        session.request_oauth_token,\
        session.request_oauth_verifier,\
        tokenRequestSignature
        )\
    }

  try:
    res = urllib2.urlopen(urllib2.Request(tokenRequestUrl,'',tokenRequestHeaders))
    res_string = res.read()

    # save request token components to Session model
    res_dict = {}
    res_list = res_string.split('&')
    for x in res_list:
      xp = x.partition('=')
      res_dict[xp[0]] = xp[2]
    session.access_oauth_token = str(res_dict['oauth_token'])
    session.access_oauth_token_secret = str(res_dict['oauth_token_secret'])
    session.put()
    # probably a shorter way to write the last two lines; list comprehension?

    print 'Location: ' + app_url + '?trivial=1'
    sys.exit()
    # goto: new request that will hit block 3
    # note: res.read() should equal "oauth_token=xyz&oauth_token_secret=abc ..."
    # TODO: remove security hazard, and remove query string; get root condition to check Session model instead of for presence of query string
  except IOError, e:
    output_html = 'access token request detained! - ' + str(e.info()) + e.read()
    print "Content-Type: text/html\n\n" + '''
      <html>
        <head>
          <title>KillTweets!</title>
        </head>
        <body>
          <h1>KillTweets: now in HTML</h1>
            <pre>%s</pre>
        </body>
      </html>
      ''' % (output_html)
    sys.exit()
    # debug and die

# block 3
# query string found
# access token found

# try to read authenticating user's tweets
if True:

  tokenSecret = session.access_oauth_token_secret

  tokenRequestUrl = 'http://api.twitter.com/1/statuses/user_timeline.xml?'\
    'include_rts=true&'\
    'trim_user=true&'\
    'include_entities=false&'\
    'count=50'
  # change this variable name to something more meaningful

  tokenRequestNonce = uuid.uuid4()
  tokenRequestTimestamp = int(time.time())
  tokenRequestNormalisedRequestParameters = urllib.quote(\
    'count=50&'
    'include_entities=false&'\
    'include_rts=true&'\
    'oauth_consumer_key=%s&'\
    'oauth_nonce=%s&'\
    'oauth_signature_method=HMAC-SHA1&'\
    'oauth_timestamp=%s&'\
    'oauth_token=%s&'\
    'oauth_version=1.0&'\
    'trim_user=true'\
    % ( twitterAppConsumerKey,\
        tokenRequestNonce,\
        tokenRequestTimestamp,\
        session.access_oauth_token\
        ),'')
  tokenRequestSignatureBaseString = '%s&%s&%s' % ( \
    'GET', \
    urllib.quote(tokenRequestUrl[:tokenRequestUrl.find('?')].lower(),''), \
    tokenRequestNormalisedRequestParameters\
    )
  tokenRequestSignatureHmacKey = urllib.quote(twitterAppConsumerSecret,'') + '&' + tokenSecret
  tokenRequestSignature = urllib.quote(base64.encodestring(hmac.new(\
    tokenRequestSignatureHmacKey,\
    tokenRequestSignatureBaseString,\
    hashlib.sha1
    ).digest()),'')
  tokenRequestHeaders = {'Authorization':'OAuth '\
    'oauth_consumer_key="%s",'\
    'oauth_nonce="%s",'\
    'oauth_signature_method="HMAC-SHA1",'\
    'oauth_timestamp="%s",'\
    'oauth_token="%s",'\
    'oauth_signature="%s",'\
    'oauth_version="1.0"'\
    % ( twitterAppConsumerKey,\
        tokenRequestNonce,\
        tokenRequestTimestamp,\
        session.access_oauth_token,\
        tokenRequestSignature
        )\
    }

  try:
    res = urllib2.urlopen(urllib2.Request(tokenRequestUrl,None,tokenRequestHeaders))

    # scrape out the status ids
    res_string = res.read()
    res_tree = ElementTree.fromstring(res_string)
    for id in res_tree.findall('status/id'):

      # delete the status ids
      tokenSecret = session.access_oauth_token_secret

      tokenRequestUrl = 'http://api.twitter.com/1/statuses/destroy/%s.xml?'\
        'include_entities=false&'\
        'trim_user=true' % (id.text)
      # change this variable name to something more meaningful

      tokenRequestNonce = uuid.uuid4()
      tokenRequestTimestamp = int(time.time())
      tokenRequestNormalisedRequestParameters = urllib.quote(\
        'include_entities=false&'\
        'oauth_consumer_key=%s&'\
        'oauth_nonce=%s&'\
        'oauth_signature_method=HMAC-SHA1&'\
        'oauth_timestamp=%s&'\
        'oauth_token=%s&'\
        'oauth_version=1.0&'\
        'trim_user=true'
        % ( twitterAppConsumerKey,\
            tokenRequestNonce,\
            tokenRequestTimestamp,\
            session.access_oauth_token\
            ),'')
      tokenRequestSignatureBaseString = '%s&%s&%s' % ( \
        'POST', \
        urllib.quote(tokenRequestUrl[:tokenRequestUrl.find('?')].lower(),''), \
        tokenRequestNormalisedRequestParameters\
        )
      tokenRequestSignatureHmacKey = urllib.quote(twitterAppConsumerSecret,'') + '&' + tokenSecret
      tokenRequestSignature = urllib.quote(base64.encodestring(hmac.new(\
        tokenRequestSignatureHmacKey,\
        tokenRequestSignatureBaseString,\
        hashlib.sha1
        ).digest()),'')
      tokenRequestHeaders = {'Authorization':'OAuth '\
        'oauth_consumer_key="%s",'\
        'oauth_nonce="%s",'\
        'oauth_signature_method="HMAC-SHA1",'\
        'oauth_timestamp="%s",'\
        'oauth_token="%s",'\
        'oauth_signature="%s",'\
        'oauth_version="1.0"'\
        % ( twitterAppConsumerKey,\
            tokenRequestNonce,\
            tokenRequestTimestamp,\
            session.access_oauth_token,\
            tokenRequestSignature
            )\
        }
      try:
        del_res = urllib2.urlopen(urllib2.Request(tokenRequestUrl,'',tokenRequestHeaders))
      except IOError,del_e:
        print "\n\n" + "error deleting\n\n" + str(del_e.info()) + del_e.read()
        sys.exit()
    
    print 'Location: ' + app_url
    sys.exit()
    # batch of tweets deleted, send the user back for more... API calls providing...

  except IOError, e:
    output_html = "Failed to read Tweets; if I can\'t read, I can\'t delete.\n\n" + str(e.info()) + e.read()
    print "Content-Type: text/html\n\n" + '''
      <html>
        <head>
          <title>KillTweets!</title>
        </head>
        <body>
          <h1>KillTweets: now in HTML</h1>
            <pre>%s</pre>
        </body>
      </html>
      ''' % (output_html)
    
    # purge
    if hasattr(session, 'request_oauth_token'):
      del session.request_oauth_token
    if hasattr(session, 'request_oauth_token_secret'):
      del session.request_oauth_token_secret
    if hasattr(session, 'request_oauth_verifier'):
      del session.request_oauth_verifier
    if hasattr(session, 'access_oauth_token'):
      del session.access_oauth_token
    if hasattr(session, 'access_oauth_token_secret'):
      del session.access_oauth_token_secret
    session.put()
    
    sys.exit()
    # debug and die



'''
output_string = "Content-Type: text/plain\n\n" +\
'This server is: ' + os.environ['SERVER_SOFTWARE'] + "\n" +\
'We received a query string from Twitter: ' + os.environ['QUERY_STRING']
print output_string
'''

'''
class Session(db.Expando):
  created = db.DateTimeProperty(verbose_name='Created Datetime')
  def __init__(self,key_name):
    super(Session,self).__init__(key_name=key_name)
      # calls the parent constructor
    self.created = datetime.datetime.utcnow()
      # constructor code that is specific to the child
'''
