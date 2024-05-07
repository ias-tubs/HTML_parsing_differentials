from debian:bookworm
ENV DEBIAN_FRONTEND=noninteractive
ENV LANG=en_US.UTF-8
ENV LANGUAGE=en_US:en
ENV TZ=Europe/Berlin

RUN  apt-get update \
      && apt-get install -y pkg-config php-cli php-pgsql php-xml php-curl \
      postgresql-client libpq-dev composer \
      && rm -rf /var/lib/apt/lists/*

RUN mkdir "$HOME/.ssh/"
RUN ssh-keyscan github.com >> "$HOME/.ssh/known_hosts"
RUN ssh-keyscan git.ias.cs.tu-bs.de >> "$HOME/.ssh/known_hosts"

WORKDIR /work
ADD https://github.com/ufoscout/docker-compose-wait/releases/download/2.9.0/wait /wait
RUN chmod +x /wait
COPY . .
RUN composer update
RUN which php
CMD /wait && /usr/bin/php /work/run.php --sanitizer=typo3
