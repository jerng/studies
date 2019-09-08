# JSF just stands for JavaScript Framework

Currently a study of 2015-2019 JavaScript developments I've not beeng keeping up with.

After reviewing a number of reactive-style web frameworks, I wasn't clear on how each 
one was architectured.

Instead of reading all the code for all of those, I decided to write a brief study in 
the form. The architecture being developed here is loosely based on EventTarget and
Erlang's VM-actor-messages model.

# Progress

Done:
- window.actorRegistry
- Actor superclass
- Postman subclass
- Actors have inboxes
- Postman determines messaging protocol
- - each window.actorRegistry can have one Postman, at most
- - each Postman carries another internal registry of message recipients; this is uniquely keyed by actor.identity 
- - messages are Objects with symbol keys to prevent accidental mucking up
