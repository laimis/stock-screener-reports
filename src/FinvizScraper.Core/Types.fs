namespace FinvizScraper.Core

module StockTicker =

    type T = StockTicker of string

    // wrap
    let create (s:string) =
        StockTicker (s.ToUpper())

    // unwrap
    let value (StockTicker e) = e

module Constants =
    
    [<Literal>] 
    let NewHighsWithSalesScreenerId = 27

    [<Literal>] 
    let NewHighsScreenerId = 28

    [<Literal>] 
    let TopGainerScreenerId = 29

    [<Literal>] 
    let TopLoserScreenerId = 30
    
    [<Literal>] 
    let NewLowsScreenerId = 31

    [<Literal>]
    let LargeCapCrossing200ScreenerId = 313

    [<Literal>]
    let ColorBlue = "#0074D9"
    
    [<Literal>]
    let ColorRed = "#FF4136"


type ScreenerInput = {
    name:string;
    url:string;
    filename:string;
}

type FinvizConfig =
    {
        screeners:list<ScreenerInput>;
        outputPath:string;
        dbConnectionString:string;
    }

    static member dayRange = 61
    static member industryTrendDayRange = 14
    static member sectorTrendDayRange = 14

    static member getBackgroundColorDefault = "#FF6586"
    
    static member getBackgroundColorForScreenerId id =
        match id with
            | Constants.NewHighsWithSalesScreenerId -> "#2C59D8" // new high
            | Constants.NewHighsScreenerId -> "#3590F3" // new high (w/ sales)
            | Constants.TopGainerScreenerId -> "#4DBEF7" // top gainer
            | Constants.TopLoserScreenerId -> "#C54A8B" // top loser
            | Constants.NewLowsScreenerId -> "#90323C" // new low
            | _ -> FinvizConfig.getBackgroundColorDefault // otherwise return default

type ScreenerResult = {
    ticker:StockTicker.T;
    company:string;
    sector:string;
    industry:string;
    country:string;
    marketCap:decimal;
    price:decimal;
    change:decimal;
    volume:int;
}

type Stock = {
    id: int;
    ticker: StockTicker.T;
    company: string;
    sector: string;
    industry: string;
    country: string;
}

type Screener = {
    id: int;
    name: string;
    url: string;
}

type SMABreakdown =
    {
        date: System.DateTime;
        days: int;
        above: int;
        below: int;
    }

    member this.total = this.above + this.below

    member this.percentAbove =
        match this.total with
            | 0 -> 0.0m
            | _ -> (decimal this.above ) * 100.0m / (decimal this.total)

    member this.percentAboveRounded =
        System.Math.Round(this.percentAbove, 0)

type IndustrySMABreakdown = 
    {
        industry: string;
        breakdown: SMABreakdown;
    }

type TrendDirection =
    | Up
    | Down

type IndustryTrend =
    {
        industry: string;
        streak: int;
        direction: TrendDirection;
        change: decimal;
        days: int;
        date: System.DateTime;
    }

type JobStatus =
    | Success
    | Failure

type JobName =
    | ScreenerJob
    | IndustryTrendsJob
    | TestJob
