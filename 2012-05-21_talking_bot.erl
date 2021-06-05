-module(bot).
-export([respond/0, respond/1]).


respond()->speak().
respond(S)->speak("You said, " ++ S).

speak()->speak("I do not know what to say.").
speak(S)->os:cmd("espeak \"" 
  ++  binary_to_list(
        list_to_binary(
          re:replace(S,"\"","\""))) 
  ++  "\"").

%re:replace("say \"what\"","\"","x").
