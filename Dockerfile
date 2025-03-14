FROM mcr.microsoft.com/dotnet/sdk:9.0-alpine AS build-env

RUN apk add --no-cache -U \
    nodejs \
    npm

RUN mkdir -p /app
COPY . /app
WORKDIR /app

RUN dotnet publish ./src/StockScreenerReports.Web --self-contained -r linux-musl-x64 -c Release -o /app/out

FROM mcr.microsoft.com/dotnet/aspnet:9.0-alpine

RUN apk add --no-cache -U \
    curl \
    tzdata \
    icu-libs

WORKDIR /app
COPY --from=build-env /app/out /app

ENV ASPNETCORE_URLS=http://*:8080
ENV DOTNET_SYSTEM_GLOBALIZATION_INVARIANT=false

ENTRYPOINT ["dotnet", "StockScreenerReports.Web.App.dll"]