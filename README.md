## Financial Reports based on Finviz scans

This repo contains code that can be used to run scans on finviz, then grouped/analyzed/stored any way that one needs for further analysis.

This can be used to maintain a list of interesting stocks and observe the overall trends in the market.

## Implementation details

The code is written in F#, and the simplest way to run it is:

1. Pull this repo locally
2. Assuming you have dotnet runtime installed, run `dotnet run` via command line
3. Once finished, open output.html file that's generated

The types of screens that are run is managed in config.json file. There is a default screener in there that shows an example of how to set one up and how to run it.