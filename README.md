# superails
A study of popular computing technologies, that uses Ruby on Rails for glue.

# try it
* Build the Dockerfile, and run the resulting image with:

  `docker run --rm -it -p 3000:3000 TAG-OR-IMAGE-ID`

  This took 6m29s on an AWS EC2 t2.micro instance, SSD (EBS, no provisioned IOPS).

* Or, pull the Docker image from [Docker Hub](https://registry.hub.docker.com/u/jerng/superails/).

  `docker pull jerng/superails`

# lately involving
* Docker
* jRuby
* Ruby on Rails
* PostgreSQL 

# work has stopped at
* add TorqueBox 3 (4 is in alpha)

# roadmap
In no particular order:
* add WebSockets
* add drag-and-drop database modelling and generation
* add Bootstrap, SASS, and LaTeX
* add proper RBAC - both roles and assets in the DB (reconsider?)
* add instant messaging (Faye?)
* add content editor of WordPress standard (CKE with S3-backed media?)
* add URL slugging system
* add caching control panel
* add RaphaelJS, VelocityJS, jQuery
* add some sort of three-way JS binding (Angular? Ember + FastBoot?)
* add Elastisearch
* add Neo4J or other graph-store
* add connector to Julia / Erlang / Haskell / etc.
* add specifications which aid the reimplementation of the glue code of this project, in any other language stack 
* add one of those admin-panel gems (which may ultimately be written)
* add some sort of Hadoop processing example (Kafka, Storm/Spark, etc.? AWS, GCP, or Azure equivalents?)
* add some sort of natural language processing toolkit
* add some sort of workflow modelling and automation toolkit
* add some sort of CUDA toolkit
* add some sort of dynamic tables control panel
* add internationalisation
* add route translation such that less coding is required in `config/routes`
* add continuous delivery methodologies (BDD? CI Server with process manual? Gitflow?)
* add collaborative document editing
* more...

# motivation

Slightly off kilter - but since I finally have time to write code for fun, I'll probably spend some nights working on this. Motivation - frankly, Rails is great in the way Java is great, but I want it to be a bit more like WordPress. 2015-Apr-15
