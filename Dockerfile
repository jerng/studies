# Modified from a blog post: http://rockyj.in/2014/09/14/jruby_docker.html
# This did not contain a license.

FROM debian:7.8 
#FROM phusion/baseimage:0.9.13

# Set correct environment variables.
ENV HOME /root

# Use baseimage-docker's init system.
# CMD ["/sbin/my_init"]

RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get install -y \
      curl git vim \ 
        # utilities
      openjdk-7-jre 

RUN echo 'deb http://apt.postgresql.org/pub/repos/apt/ wheezy-pgdg main' >> \
      /etc/apt/sources.list.d/pgdg.list && \
    curl -O https://www.postgresql.org/media/keys/ACCC4CF8.asc && \
    apt-key add ACCC4CF8.asc && \
    rm ACCC4CF8.asc && \
    apt-get update && \
    apt-get install -y postgresql 
      # 9.4.1 tested

RUN curl https://raw.githubusercontent.com/creationix/nvm/v0.16.1/install.sh | sh && \
    echo 'PATH=$PATH:/root/.nvm' >> ~/.bashrc && \
    /bin/bash -ci 'nvm install 0.10.33' && \
    /bin/bash -ci 'nvm alias default 0.10.33'
  # Install a version of nodejs.
  # (nvm) installation.
  # Set a version of nodejs as default. (Not sure if this persists through logout/login).

ENV JRUBY_VERSION 1.7.15 
  # 1.7.18 works with Rails 4.2.1 but not ActiveRecord
RUN curl http://jruby.org.s3.amazonaws.com/downloads/$JRUBY_VERSION/jruby-bin-$JRUBY_VERSION.tar.gz | tar xz -C /opt
ENV PATH /opt/jruby-$JRUBY_VERSION/bin:$PATH
  # Get JRuby

RUN echo gem: --no-document >> /etc/gemrc
RUN gem update --system
RUN gem install bundler

#Add user
RUN addgroup --gid 9999 app
RUN adduser --uid 9999 --gid 9999 --disabled-password --gecos "Application" app
RUN usermod -L app
RUN mkdir -p /home/app/my_app
# ADD . /home/app/my_app
RUN chown -R app:app /home/app/

# USER app
RUN gem install rails -v 4.1.8 # 4.1.9 failed with 1.7.18
RUN gem install activerecord-jdbcpostgresql-adapter
# RUN gem install jruby-launcher
RUN echo 'service postgresql start' >> ~/.bashrc && \
    echo 'PATH=$PATH:/usr/lib/postgresql/9.4/bin/' >> ~/.bashrc
USER postgres
RUN service postgresql start && \
    psql --command "CREATE USER my_app WITH SUPERUSER PASSWORD 'my_app';"  && \ 
      # make a role (user)
    psql --command "CREATE DATABASE template_unicode WITH TEMPLATE = template0 ENCODING = 'UNICODE';" && \
    psql --command "UPDATE pg_database SET datistemplate = TRUE WHERE datname = 'template_unicode';" && \
    psql --command "\c template_unicode \\ VACUUM FULL" 
      # create a unicode template
USER root
RUN cd /home/app && \
    rails new my_app -d postgresql && \
    sed -r 's/  database: my_app_development/  database: my_app_development\n  template: template_unicode/' \
      -i /home/app/my_app/config/database.yml && \
    sed -r 's/#username/username/' -i /home/app/my_app/config/database.yml && \
    sed -r 's/#password:/password: my_app/' -i /home/app/my_app/config/database.yml

#JRuby options
ENV PATH /opt/jruby-$JRUBY_VERSION/bin:$PATH
RUN echo compat.version=2.0 >> /home/app/.jrubyrc
RUN echo invokedynamic.all=true >> /home/app/.jrubyrc
ENV JRUBY_OPTS -Xcompile.invokedynamic=false -J-XX:+TieredCompilation -J-XX:TieredStopAtLevel=1 -J-noverify -Xcompile.mode=OFF
  # Partial implementation of methods to speed up JRuby start-time, from:
  #   http://making.change.org/post/58250242540/a-journey-to-speed-up-jruby-startup-time
  # Also consider:
  #   https://github.com/jruby/jruby/wiki/Improving-startup-time

RUN apt-get autoremove -y && apt-get clean
  # Cleanup.

#Get Rails running
WORKDIR /home/app/my_app
# ENV RAILS_ENV staging
RUN /bin/bash -ci 'bundle exec rake db:create'
RUN bundle install --deployment --without test development

# AUFS security work-around
RUN mkdir /etc/ssl/private-copy; \
    mv /etc/ssl/private/* /etc/ssl/private-copy/; \
    rm -r /etc/ssl/private; \
    mv /etc/ssl/private-copy /etc/ssl/private; \
    chmod -R 0700 /etc/ssl/private; \
    chown -R postgres /etc/ssl/private


##################################################################
#Remove these lines when using fig since fog will start the server
##################################################################
# RUN bundle exec rake db:reset
EXPOSE 3000
ENTRYPOINT bash -ci 'bundle exec rails server -b 0.0.0.0'
