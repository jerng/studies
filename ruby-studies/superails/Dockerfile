# Modified from a blog post: http://rockyj.in/2014/09/14/jruby_docker.html
# This did not contain a license.

FROM debian:8.0 
# FROM phusion/baseimage:0.9.13

RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get install -y \
      curl git vim \ 
        # utilities
      openjdk-7-jre 

# Install Postgres
RUN echo 'deb http://apt.postgresql.org/pub/repos/apt/ jessie-pgdg main' >> \
      /etc/apt/sources.list.d/pgdg.list && \
    curl -O https://www.postgresql.org/media/keys/ACCC4CF8.asc && \
    apt-key add ACCC4CF8.asc && \
    rm ACCC4CF8.asc && \
    apt-get update && \
    apt-get install -y postgresql 
      # 9.4.1 tested

# Aptitude cleanups.
RUN apt-get autoremove -y && apt-get clean

# AUFS security work-around; run before we need to run Postgres
RUN echo '### begin AUFS workaround' >> /root/.bashrc && \
    echo 'mkdir /etc/ssl/private-copy' >> /root/.bashrc && \
    echo 'mv /etc/ssl/private/* /etc/ssl/private-copy/' >> /root/.bashrc && \
    echo 'rm -r /etc/ssl/private' >> /root/.bashrc && \
    echo 'mv /etc/ssl/private-copy /etc/ssl/private' >> /root/.bashrc && \
    echo 'chmod -R 0700 /etc/ssl/private' >> /root/.bashrc && \
    echo 'chown -R postgres /etc/ssl/private' >> /root/.bashrc && \
    echo '### end AUFS workaround' >> /root/.bashrc && \
    echo 'service postgresql start' >> /root/.bashrc && \
    echo 'export PATH=$PATH:/usr/lib/postgresql/9.4/bin/' >> /root/.bashrc
USER postgres
RUN service postgresql start && \
    psql --command "CREATE USER my_app WITH SUPERUSER PASSWORD 'my_app';"  && \ 
      # make a role (user)
    psql --command "CREATE DATABASE template_unicode WITH TEMPLATE = template0 ENCODING = 'UNICODE';" && \
    psql --command "UPDATE pg_database SET datistemplate = TRUE WHERE datname = 'template_unicode';" && \
    psql --command "\c template_unicode \\ VACUUM FULL" 
      # create a unicode template
#USER app 
USER root 

# Install NodeJS
RUN curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.25.1/install.sh | bash && \ 
    echo 'export PATH=$PATH:/root/.nvm' >> /root/.bashrc && \
    /bin/bash -ci 'nvm install 0.10.33' && \
    /bin/bash -ci 'nvm alias default 0.10.33'
    # Install a version of nodejs.
    # (nvm) installation.
    # Set a version of nodejs as default. (Not sure if this persists through logout/login).

# Install JRuby
ENV JRUBY_VERSION 1.7.15 
    # 1.7.18 works with Rails 4.2.1 but not ActiveRecord
RUN curl http://jruby.org.s3.amazonaws.com/downloads/$JRUBY_VERSION/jruby-bin-$JRUBY_VERSION.tar.gz | tar xz -C /opt
ENV PATH /opt/jruby-$JRUBY_VERSION/bin:$PATH
    # Get JRuby

# JRuby development environment options.
RUN echo 'compat.version=2.0' >> /root/.jrubyrc && \
    echo 'invokedynamic.all=true' >> /root/.jrubyrc
ENV PATH /opt/jruby-$JRUBY_VERSION/bin:$PATH
ENV JRUBY_OPTS -Xcompile.invokedynamic=false -J-XX:+TieredCompilation -J-XX:TieredStopAtLevel=1 -J-noverify -Xcompile.mode=OFF
  # Partial implementation of methods to speed up JRuby start-time, from:
  #   http://making.change.org/post/58250242540/a-journey-to-speed-up-jruby-startup-time
  # Also consider:
  #   https://github.com/jruby/jruby/wiki/Improving-startup-time

# Install Bundler & some gems
RUN echo gem: --no-document >> /etc/gemrc && \
    gem update --system && \
    gem install bundler && \
    gem install rails -v 4.1.8 && \
      # 4.1.9 failed with 1.7.18
    gem install activerecord-jdbcpostgresql-adapter
      # RUN gem install jruby-launcher

# Add & set user
RUN addgroup --gid 9999 app
RUN adduser --uid 9999 --gid 9999 --disabled-password --gecos "Application" app
RUN usermod -L app
# USER app

# Prepare toy app directory.
RUN mkdir -p /home/app/my_app
RUN chown -R app:app /home/app/

# Generate a toy app.
RUN cd /home/app && \
    rails new my_app -d postgresql  
RUN sed -r 's/  encoding: unicode/  encoding: unicode\n  template: template_unicode\n  username: my_app\n  password: my_app/'  \
       -i /home/app/my_app/config/database.yml && \
    echo "gem 'torquebox', '4.0.0.alpha1'" >> /home/app/my_app/Gemfile 
      # Torquebox included here.

#Get Rails running
WORKDIR /home/app/my_app
RUN bundle install --no-deployment  && \
    bundle install --deployment --without test development && \
    /bin/bash -ci 'bundle exec rake db:create'


##################################################################
#Remove these lines when using fig since fog will start the server
##################################################################
# RUN bundle exec rake db:reset
EXPOSE 3000
ENTRYPOINT bash -ci 'bundle exec rails server torquebox -b 0.0.0.0'
