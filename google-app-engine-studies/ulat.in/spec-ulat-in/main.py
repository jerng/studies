# Source code of web service application 
# to be deployed at http://spec.ulat.in .
#
# Copyright 2012 Yang Jerng HWA.
#
#

import google.appengine.ext.webapp as framework #webapp2 instance

class request_handler(framework.RequestHandler):
    def get(self):
      
      q = self.request.get('q')
      if q:
        q_collapsed = 'true'
        q_header = 'Say something else...'
        r_collapsed = 'false'
        r = '''
<h3><span class="smaller_font">You said: </span>"%(q)s"</h3>
<ul data-role="listview" data-filter="true">
  <li><a target="_blank" href="https://twitter.com/#!/search/%%22%(q)s%%22%%20%%3A)">happy thoughts <span class="smaller_font">on</span> Twitter <span class="smaller_font">(found by Twitter)</span></a></li>
  <li><a target="_blank" href="https://www.google.com/search?q=%(q)s+site:facebook.com">everything <span class="smaller_font">on</span> Facebook <span class="smaller_font">(found by Google)</span></a></li>
  <li><a target="_blank" href="https://www.google.com/search?q=%(q)s+site:linkedin.com">everything <span class="smaller_font">on</span> LinkedIn <span class="smaller_font">(found by Google)</span></a></li>
  <li><a target="_blank" href="https://twitter.com/#!/search/%%22%(q)s%%22%%20%%3A(">sad thoughts <span class="smaller_font">on</span> Twitter <span class="smaller_font">(found by Twitter)</span></a></li>
  <li><a target="_blank" href="https://www.google.com/search?q=%(q)s+inurl:linkedin.com/groups">groups <span class="smaller_font">on</span> LinkedIn <span class="smaller_font">(found by Google)</span></a></li>
  <li><a target="_blank" href="https://www.google.com/search?q=%(q)s+inurl:linkedin.com/in+OR+inurl:linkedin.com/pub">profiles <span class="smaller_font">on</span> LinkedIn <span class="smaller_font">(found by Google)</span></a></li>
  <li><a target="_blank" href="http://www.linkedin.com/pub/dir/?first=%(q)s&last=">first names <span class="smaller_font">on</span> LinkedIn <span class="smaller_font">(found by LinkedIn)</span></a></li>
  <li><a target="_blank" href="http://www.linkedin.com/pub/dir/?first=&last=%(q)s">last names <span class="smaller_font">on</span> LinkedIn <span class="smaller_font">(found by LinkedIn)</span></a></li>
  <li><a target="_blank" href="https://twitter.com/#!/search/%%22%(q)s%%22%%20%%3F">questions <span class="smaller_font">on</span> Twitter <span class="smaller_font">(found by Twitter)</span></a></li>
  <li><a target="_blank" href="https://twitter.com/#!/search/%%22%(q)s%%22%%20%%3A)%%20%%3F">happy questions <span class="smaller_font">on</span> Twitter <span class="smaller_font">(found by Twiter)</span></a></li>
  <li><a target="_blank" href="https://twitter.com/#!/search/%%22%(q)s%%22%%20%%3A(%%20%%3F">sad questions <span class="smaller_font">on</span> Twitter <span class="smaller_font">(found by Twitter)</span></a></li>
  <li><a target="_blank" href="https://twitter.com/#!/search/%%22%(q)s%%22">everything <span class="smaller_font">on</span> Twitter <span class="smaller_font">(found by Twitter)</span></a></li>
  <li><a target="_blank" href="https://www.google.com/search?q=%(q)s+site:twitter.com">everything <span class="smaller_font">on</span> Twitter <span class="smaller_font">(found by Google)</span></a></li>
  <li><a target="_blank" href="https://www.google.com/search?q=status+%(q)s+site:twitter.com">Tweets <span class="smaller_font">on</span> Twitter <span class="smaller_font">(found by Google)</span></a></li>
  <li><a target="_blank" href="https://www.google.com/search?q=lists+%(q)s+site:twitter.com">Lists <span class="smaller_font">on</span> Twitter <span class="smaller_font">(found by Google)</span></a></li>
  <li><a target="_blank" href="https://www.google.com/search?q=%(q)s">everything <span class="smaller_font">(found by Google)</span></a></li>
</ul>
          ''' % locals()
      else:
        q_collapsed = 'false'
        q_header = 'Say something...'
        r_collapsed = 'true'
        r = ''

      self.response.out.write('''
<!DOCTYPE html> 
<html itemscope itemtype="http://schema.org/Product"> 
<head> 

  <meta itemprop="name" content="spec.ulat.in">
  <meta itemprop="description" content="Find stuff, by spec.ulat.in, a search query compiler.">
  <meta name="google-site-verification" content="B8pMD9twt6e6LKE1err-BpGCLGvNYYc8cUZTRbwymNA" />
  <meta name="viewport" content="width=device-width, initial-scale=1"> 

	<title>spec.ulat.in</title> 

  <link rel="stylesheet" href="http://code.jquery.com/mobile/1.1.0/jquery.mobile-1.1.0.min.css" /> 
  <script src="http://code.jquery.com/jquery-1.7.1.min.js"></script> 
  <script src="http://code.jquery.com/mobile/1.1.0/jquery.mobile-1.1.0.min.js"></script> 

  <style type="text/css"> .smaller_font { font-size: 65%%; } </style> 
  <script type="text/javascript"> $.mobile.textinput.prototype.options.clearSearchButtonText = 'Say it another way.' </script>

</head> 
<body> 

<div data-role="page">

	<div data-role="header">
    <a href="/" data-role="button" data-icon="home" data-iconpos="notext"></a>
    <h1>spec.ulat.in</h1>
	</div><!-- /header -->

	<div data-role="content">	
    
    <div data-role="collapsible-set" data-content-theme="c">

      <div data-role="collapsible" data-collapsed="%(q_collapsed)s">
        <h3>%(q_header)s</h3>
        <form action="/" method="get">
          <input type="search" name="q" id="q" />
        </form>	
      </div>
      
      <div data-role="collapsible" data-collapsed="%(r_collapsed)s" data-theme="e" data-content-theme="e">
        %(r)s
      </div>

      <div data-role="collapsible">
        <h3>About</h3>
        <p>
          Thanks for using <a href="#">spec.ulat.in</a>: a search query compiler. You are using Version 1, released on 2012-05-16. This service tries to help you find what you're looking for on the Internet, by compiling search queries targeting real search applications. (If you study the links that are provided as results, you can further learn how to improve your own writing, of advanced search queries on various websites.) Let us know how we can improve your experience with this service, if you have feature suggestions, if you find bugs, if we have infringed upon your rights, etc. Please direct all communications to <a href="mailto:jerng@[EW-EL-AY-TEE].in">jerng</a>. This is currently a recreational project, so please bear with our unfrenzied pace of development. You may be interested in <a target="_blank" href="http://goodrobot.blogspot.com/2012/05/regression-to-frivolty.html">the back-story</a>. Copyright 2012, <a target="_blank" href="https://plus.google.com/u/0/100132436696303209181/about">Hwa Yang Jerng of Malaysia</a>. All rights reserved.
        </p>
      </div>
    
    </div>

    <div>
      <span class='st_facebook_large' displayText='Facebook'></span>
      <span class='st_twitter_large' displayText='Tweet'></span>
      <span class='st_linkedin_large' displayText='LinkedIn'></span>
      <span class='st_googleplus_large' displayText='Google +'></span>
      <span class='st_email_large' displayText='Email'></span>
      <span class='st_sharethis_large' displayText='ShareThis'></span>
    </div>

    <script type="text/javascript"><!--
      google_ad_client = "ca-pub-3853597882718352";
      /* spec-ulat-in */
      google_ad_slot = "2328295796";
      google_ad_width = 320;
      google_ad_height = 50;
      //-->
    </script>
    <script type="text/javascript" src="http://pagead2.googlesyndication.com/pagead/show_ads.js"></script>

	</div><!-- /content -->

	<div data-role="footer" >
	</div><!-- /footer -->

</div><!-- /page -->

<script type="text/javascript">var switchTo5x=true;</script>
<script type="text/javascript" src="http://w.sharethis.com/button/buttons.js"></script>
<script type="text/javascript">stLight.options({publisher: "2abee94d-9df5-4988-957f-92f42450c324"}); </script>

  <script type="text/javascript">
    /*ga*/
    var _gaq = _gaq || [];
    _gaq.push(['_setAccount', 'UA-31814925-1']);
    _gaq.push(['_trackPageview']);
    (function() {
      var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
      ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
      var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
    })();
  </script>

</body>
</html>
      ''' % locals() )

app = framework.WSGIApplication(  [('/', request_handler)], #routes
                                  debug=True)

#
#
# ... server will execute 'app's webapp.WSfind GIApplication.run() in a CGI environment.
