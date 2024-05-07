# https://hub.docker.com/_/microsoft-dotnet
FROM mcr.microsoft.com/dotnet/sdk:6.0-bullseye-slim-amd64 AS build
WORKDIR /source

ADD https://github.com/ufoscout/docker-compose-wait/releases/download/2.9.0/wait /wait
RUN chmod +x /wait
# copy csproj and restore as distinct layers
COPY *.csproj .
RUN dotnet restore -r linux-x64 --packages /pkgs

# copy and publish app and libraries
COPY . .
#RUN ls -lah /pkgs
#RUN dotnet publish -c Release -o /app -r linux-x64 --self-contained false --packages /pkgs --no-restore #false --no-restore
RUN dotnet build -c Release --packages /pkgs #--no-restore


#COPY --from=build /app .
#RUN ls -lahR /source
CMD /wait && /source/bin/Release/net6.0/linux-x64/mxssy]
