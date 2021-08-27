FROM mcr.microsoft.com/dotnet/runtime:5.0 AS base
WORKDIR /app

FROM mcr.microsoft.com/dotnet/sdk:5.0 AS build
WORKDIR /src
COPY ["XelmishSnake/XelmishSnake.fsproj", "XelmishSnake/"]
RUN dotnet restore "XelmishSnake/XelmishSnake.fsproj"
COPY . .
WORKDIR "/src/XelmishSnake"
RUN dotnet build "XelmishSnake.fsproj" -c Release -o /app/build

FROM build AS publish
RUN dotnet publish "XelmishSnake.fsproj" -c Release -o /app/publish

FROM base AS final
WORKDIR /app
COPY --from=publish /app/publish .
ENTRYPOINT ["dotnet", "XelmishSnake.dll"]
