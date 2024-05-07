FROM ruby:3.0
WORKDIR /work
COPY Gemfile .
RUN bundle install
COPY . .
ADD https://github.com/ufoscout/docker-compose-wait/releases/download/2.9.0/wait /wait
RUN chmod +x /wait


CMD /wait && /usr/local/bin/ruby run.rb
