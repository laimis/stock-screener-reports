namespace StockScreenerReports.Core

module StockTicker =

    type T = StockTicker of string

    // wrap
    let create (s:string) =
        StockTicker (s.ToUpper())

    // unwrap
    let value (StockTicker e) = e

module Constants =

    [<Literal>] 
    let NewHighsScreenerId = 28

    [<Literal>] 
    let TopGainerScreenerId = 29

    [<Literal>] 
    let TopLoserScreenerId = 30
    
    [<Literal>] 
    let NewLowsScreenerId = 31

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

    static member dayRange = 91
    static member industryTrendDayRange = 14
    static member sectorTrendDayRange = 14

    static member getBackgroundColorDefault = "#FF6586"
    
    static member getBackgroundColorForScreenerId id =
        match id with
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

    static member blank industry =
        {
            industry = industry;
            breakdown = {
                date = System.DateTime.Now;
                days = 0;
                above = 0;
                below = 0;
            }
        }

type TrendDirection =
    | Up
    | Down

type Trend =
    {
        streak: int;
        direction: TrendDirection;
        change: decimal;
    }

    static member blank() = { streak = 0; direction = Up; change = 0.0m }
    member this.streakRate =
        match this.streak with
            | 0 -> 0.0m
            | _ -> (decimal this.change ) / (decimal this.streak)
    override this.ToString() =
        let directionStr = 
            match this.direction with
            | Up -> "Up"
            | Down -> "Down"

        $"Trending <b>{directionStr}</b> for <b>{this.streak} days</b>, change of <b>{this.change:N2}</b>"

type IndustryTrend =
    {
        industry: string;
        trend: Trend;
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

type EarningsTime =
    | BeforeMarket
    | AfterMarket
