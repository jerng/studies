#EZ-Erlang

A simple MVC web development framework.

Reading tip: if you're new to Erlang, note that 'john' would refer to the atom,
or symbol, john. The single-quotes are optional, but help to differential atoms
from other lowercase text.

##SECTIONS

- SECTIONS
- NAME OF PROJECT
- VERSION/DATE
- LICENSING
- DEPENDENCIES
- INTENTION
- CAVEATS
- QUICK START: STARTING & STOPPING EZ
- QUICK START: DEVELOPING WEBSITES IN EZ 
- QUICK START: HACKING THE FRAMEWORK
- AUTHOR

##NAME OF PROJECT

"EZ", pronounced "ee-zee", in an American fashion. You may also find it 
convenient to call it EZ-E for "easy Erlang", which is the guiding "vision"
that motivates this project.

##VERSION/DATE

v0.01
- initiated on 2012-Aug-01
- released on 2012-Aug-10

v0.02
- coded shortly after v0.01, pushed on 2013-Mar-22
- documentation may be out of date

##LICENSING

None as yet. I won't prosecute you if you steal ideas from the code as it is.

##DEPENDENCIES

Erlang/OTP R15B installed on either Windows or Linux.
Tested with Windows 7 Home Premum 32bit, Ubuntu 11.04 64bit.
There is a good chance it will work on Erlang/OTP on other OS-es also.

##INTENTION

- tries to provide an Model-View-Controller style web development framework
- requiring minimal introduction to the Erlang/OTP package as a whole
- with minimal dependence on libraries outside the Erlang/OTP 'stdlib'
- runs out-of-the-box on Windows and Linux installations of Erlang/OTP
- this is primarily a study/learning project for the author

##CAVEATS

This is not yet production quality code. I am also new to Erlang/OTP, and
remain quite unaware of many of its conventions, as well as too lazy to 
follow many of them for the time being.

##QUICK START: STARTING & STOPPING EZ

1. Unzip the "ez_app" directory, and access it as your current working directory.

2.  Go to your terminal or command prompt.
On Windows: run ".\ez_app\ezboot.bat"
On Linux: run "bash ./ez_app/ezboot.bat"

3. If you see messages ending with "Attempting to start Inets on Port 8000."
then the example controller-view is probably being served at 
  localhost:8000/ez:router/default/default

4. Browse to that location to check. If it is working...

5. Go back to your terminal / command prompt. Hit the "down arrow" key a couple
of times. You should see the Erlang prompt. Try running "ezutils:stop()."

6. If Erlang shut itself down, then you've successfully started, and stopped EZ.
Pat yourself on the back.

QUICK START: DEVELOPING WEBSITES IN EZ

0. Note that every time you update your MVC source code, you will need to 
call 'ezutils':start() in order to recompile, and reload those modules.
Otherwise the server will be serving old code. This is not Erlang/OTP
hot code-loading. It is just a re-run of the entire start-up process.

1. Switch to "ez_app/dev/"

2. "default.c.erl" - this is an example Controller. Its module name is 'default' 
and it contains a function named 'default' which is an example Action. Hitting 
the url 
  localhost:8000/ez:router/default/default
should send the router to this Controller:Action. Actions should return a 
4-tuple of data. This will get passed by 'ez':'response'/1 to View:'interpolate'/1.
An example is provided. For more information on the variable ViewBindings,
refer to the Erlang/OTP documentation for 'erl_eval':'erl_exprs'/2

3. "default.default.v.html" - this is an example View. Its module name is
'default.default.v', and it contains a single function, interpolate/1
which is called by the 'ez':'response'/1

Interpolation of <erl/> script is done by the module 'ezutils'. All the script
within each pair of <erl/> tags is run inside a 'begin-end' expression, and 
therefore returns a single value. The value should be a binary, integer 
character, or string, or deep list of these - an "io_list". Variables can 
be passed to Views through the ViewBindings variable returned by Actions.
Refer to 2. above for more information. (Sorry, docs are incomplete!)

4. MVC is enforced only by developers. Don't call modules ending with '.m' when
writing code in Views!

##QUICK START: HACKING THE FRAMEWORK

0. This is not properly structured as an Erlang/OTP application. It is just a
bunch of scripts, for now.

1.  All the framework's own code is in two files for now.

1. 1. "ez_app/ez/ez.erl" - this contains the router, and rendering interface with 
'inets', the httpd service, and 'mod_esi' from the 'stdlib'. Documentation 
inside includes a diagram of the function-call sequence when URLs are hit.

1. 2. "ez_app/ez/ezutils.erl" - this contains the code that starts the webserver
("ez_app/ezboot.bat" just compiles this, and runs ezutils:start()), compiles 
MVC source code and loads it into RAM. Documentation inside includes a diagram
of the function-call sequence that begins at ezutils:start()
