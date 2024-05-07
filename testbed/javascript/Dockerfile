FROM node:16-bullseye
RUN  apt-get update \
      && apt-get install -y libnss3 \
 gstreamer1.0-libav \
        gstreamer1.0-plugins-good \
        ibsoup2.4-1 \
        libasound2 \
        libatk1.0-0 \
        libatk-bridge2.0-0 \
        libatspi2.0-0 \
        libcups2 \
        libdbus-1-3 \
        libdbus-glib-1-2 \
        libdrm2 \
        libegl1 \
        libegl1 \
        libenchant-2-2 \
        libepoxy0 \
        libevdev2 \
        libgbm1 \
        libgdk-pixbuf2.0-0 \
        libgles2 \
        libgles2 \
        libglx0 \
        libgstreamer1.0-0 \
        libgstreamer-gl1.0-0 \
        libgstreamer-gl1.0-0 \
        libgstreamer-plugins-bad1.0-0 \
        libgstreamer-plugins-base1.0-0 \
        libgtk-3-0 \
        libgudev-1.0-0 \
        libharfbuzz-icu0 \
        libhyphen0 \
        libmanette-0.2-0 \
        libnotify4 \
        libnspr4 \
        libopengl0 \
        libopus0 \
        libsecret-1-0 \
        libwayland-client0 \
        libwayland-egl1 \
        libwebp6 \
        libwebpdemux2 \
        libwoff1 \
        libx11-xcb1 \
        libxcomposite1 \
        libxcursor1 \
        libxdamage1 \
        libxfixes3 \
        libxkbcommon0 \
        libxrandr2 \
        libxslt1.1 \
       && rm -rf /var/lib/apt/lists/*
WORKDIR /work
ADD https://github.com/ufoscout/docker-compose-wait/releases/download/2.9.0/wait /wait
RUN chmod +x /wait
RUN chown node:node /work
USER node
COPY --chown=node:node . .
RUN npm install
CMD /wait && /usr/local/bin/node -max-old-space-size=16098 --huge-max-old-generation-size run.js
