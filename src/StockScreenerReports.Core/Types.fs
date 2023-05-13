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

    [<Literal>]
    let ColorBlack = "#111111"

    [<Literal>]
    let SMA20 = 20
    
    [<Literal>]
    let SMA200 = 200
    let SMAS = [SMA20; SMA200]

    let mapSmaToColor sma =
        match sma with
        | SMA20 -> ColorRed
        | SMA200 -> ColorBlue
        | _ -> ColorBlack


type ScreenerInput = {
    name:string;
    url:string;
    filename:string;
}

type ReportsConfig =
    {
        screeners:list<ScreenerInput>;
        outputPath:string;
        dbConnectionString:string;
    }

    static member dayRange = 91

    static member dateRange = (
        System.DateTime.Now.AddDays(-1.0 * 91.0),
        System.DateTime.Now
    )

    static member dateRangeAsStrings = 
        let range = ReportsConfig.dateRange
        let startDate = (range |> fst).ToString("yyyy-MM-dd")
        let endDate = (range |> snd).ToString("yyyy-MM-dd")
        (startDate,endDate)
    static member industryTrendDayRange = 14
    static member sectorTrendDayRange = 14

    static member getBackgroundColorDefault = "#FF6586"
    
    static member getBackgroundColorForScreenerId id =
        match id with
            | Constants.NewHighsScreenerId -> "#3590F3" // new high (w/ sales)
            | Constants.TopGainerScreenerId -> "#4DBEF7" // top gainer
            | Constants.TopLoserScreenerId -> "#C54A8B" // top loser
            | Constants.NewLowsScreenerId -> "#90323C" // new low
            | _ -> ReportsConfig.getBackgroundColorDefault // otherwise return default

    static member getTradingHolidays () =
        let holidays = 
            [
                System.DateTime(2023, 1, 2)
                System.DateTime(2023, 1, 16)
                System.DateTime(2023, 2, 20)
                System.DateTime(2023, 4, 7)
                System.DateTime(2023, 5, 29)
                System.DateTime(2023, 6, 19)
                System.DateTime(2023, 7, 4)
                System.DateTime(2023, 9, 4)
                System.DateTime(2023, 12, 25)
                System.DateTime(2024, 1, 1)
                System.DateTime(2024, 1, 15)
                System.DateTime(2024, 2, 19)
                System.DateTime(2024, 3, 29)
                System.DateTime(2024, 5, 27)
                System.DateTime(2024, 6, 19)
                System.DateTime(2024, 7, 4)
                System.DateTime(2024, 9, 2)
            ]
        holidays

    static member isTradingDay (dateTime:System.DateTime) =
        let dayOfWeek = dateTime.DayOfWeek
        let isWeekend = dayOfWeek = System.DayOfWeek.Saturday || dayOfWeek = System.DayOfWeek.Sunday
        let isHoliday = ReportsConfig.getTradingHolidays() |> List.contains dateTime
        not (isWeekend || isHoliday)

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
        value: decimal;
    }

    static member blank() = { streak = 0; direction = Up; change = 0.0m; value = 0.0m; }
    member this.streakRate =
        match this.streak with
            | 0 -> 0.0m
            | _ -> (decimal this.change ) / (decimal this.streak)

    member this.streakRateFormatted =
        System.String.Format("{0:N2}%", this.streakRate)

    member this.changeFormatted =
        System.String.Format("{0:N0}", this.change)

    member this.streakFormatted =
        System.String.Format("{0:N0}", this.streak)
        
type IndustryTrend =
    {
        industry: string;
        trend: Trend;
        above: int;
        below: int;
        days: int;
        date: System.DateTime;
    }

    member this.abovePercentageFormatted() =
        System.String.Format(
            "{0:N2}%",
            ((decimal this.above) / (decimal (this.below + this.above)) * 100m)
        )

type CyclePoint =
    {
        date: System.DateTime;
        value: decimal;
    }

    static member create (smaBreakdown:SMABreakdown) = 
        {
            date = smaBreakdown.date;
            value = smaBreakdown.percentAbove
        }

type MarketCycle =
    {
        lowPoint: CyclePoint;
        highPoint: CyclePoint;
        currentPoint: CyclePoint;
    }

    member this.age =
        this.currentPoint.date - this.lowPoint.date

    member this.ageFormatted =
        System.String.Format("{0:N0} days", this.age.TotalDays)

    member this.highPointAge =
        this.currentPoint.date - this.highPoint.date

    member this.highPointAgeFormatted =
        System.String.Format("{0:N0} days", this.highPointAge.TotalDays)

    member this.lowPointValue = this.lowPoint.value

    member this.highPointValue = this.highPoint.value

type TrendWithCycle =
    {
        trend: Trend;
        cycle: MarketCycle;
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
