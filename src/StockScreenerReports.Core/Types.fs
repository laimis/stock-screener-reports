namespace StockScreenerReports.Core
open System
open Microsoft.FSharp.Reflection

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


open Constants

type SMA =
        | SMA20
        | SMA200
        
        member this.Interval =
            match this with
            | SMA20 -> 20
            | SMA200 -> 200
            
        member this.Color =
            match this with
            | SMA20 -> ColorRed
            | SMA200 -> ColorBlue
            
        static member FromInterval interval =
            match interval with
            | 20 -> SMA20
            | 200 -> SMA200
            | _ -> failwith $"Invalid SMA interval {interval}"
            
        static member All =
            FSharpType.GetUnionCases typeof<SMA> |> Array.map (fun x -> FSharpValue.MakeUnion(x, [||]) :?> SMA) |> Array.toList
        
module TimeFunctions =
    
    let mutable nowFunc = fun() -> DateTime.UtcNow

type ScreenerInput = {
    name:string;
    url:string;
    filename:string;
}

type ReportsConfig =
    {
        dbConnectionString:string;
    }

    static member days = 91
    
    static member now() =
        TimeFunctions.nowFunc()

    static member nowUtcNow() = DateTime.UtcNow

    static member dateRangeWithDays days = (
        ReportsConfig.now().AddDays(-1.0 * float days),
        ReportsConfig.now()
    )

    static member dateRange() = ReportsConfig.days |> ReportsConfig.dateRangeWithDays

    static member formatDateRangeToStrings (dateRange:DateTime*DateTime) =
        let startDate = (dateRange |> fst).ToString("yyyy-MM-dd")
        let endDate = (dateRange |> snd).ToString("yyyy-MM-dd")
        (startDate,endDate)

    static member dateRangeAsStrings() = 
        ReportsConfig.dateRange() |> ReportsConfig.formatDateRangeToStrings
    
    static member industryTrendDayRange = 14
    static member sectorTrendDayRange = 14

    static member getBackgroundColorDefault = "#FF6586"
    
    static member ngtdDomain = "app.nightingaletrading.com"
    
    static member getBackgroundColorForScreenerId id =
        match id with
            | NewHighsScreenerId -> "#3590F3" // new high (w/ sales)
            | TopGainerScreenerId -> "#4DBEF7" // top gainer
            | TopLoserScreenerId -> "#C54A8B" // top loser
            | NewLowsScreenerId -> "#90323C" // new low
            | _ -> ReportsConfig.getBackgroundColorDefault // otherwise return default

    static member getTradingHolidays () =
        let holidays = 
            [
                DateTime(2023, 1, 2)
                DateTime(2023, 1, 16)
                DateTime(2023, 2, 20)
                DateTime(2023, 4, 7)
                DateTime(2023, 5, 29)
                DateTime(2023, 6, 19)
                DateTime(2023, 7, 4)
                DateTime(2023, 9, 4)
                DateTime(2023, 11, 23)
                DateTime(2023, 12, 25)
                DateTime(2024, 1, 1)
                DateTime(2024, 1, 15)
                DateTime(2024, 2, 19)
                DateTime(2024, 3, 29)
                DateTime(2024, 5, 27)
                DateTime(2024, 6, 19)
                DateTime(2024, 7, 4)
                DateTime(2024, 9, 2)
            ]
        holidays

    static member isHoliday (dateTime:DateTime) =
        ReportsConfig.getTradingHolidays() |> List.contains dateTime

    static member isTradingDay (dateTime:DateTime) =
        // ensure that we have holidays configured for future dates, otherwise we might
        // be running with outdated configuration
        let futureDatesConfigured =
            ReportsConfig.getTradingHolidays()
            |> List.exists (fun date -> date > dateTime)
        match futureDatesConfigured with
            | false -> failwith "Trading holidays not configured for future dates. Add them in ReportsConfig.fs"
            | _ -> ()

        let dayOfWeek = dateTime.DayOfWeek
        let isWeekend = dayOfWeek = DayOfWeek.Saturday || dayOfWeek = DayOfWeek.Sunday
        let isHoliday = dateTime |> ReportsConfig.isHoliday
        not (isWeekend || isHoliday)

    static member listOfBusinessDates (startDate:DateTime,endDate:DateTime) = 
            let holidays = ReportsConfig.getTradingHolidays()

            Seq.initInfinite float
            |> Seq.map (fun i -> startDate.AddDays(i))
            |> Seq.takeWhile (fun date -> date <= endDate)
            |> Seq.where( fun date ->
                date.DayOfWeek = DayOfWeek.Saturday |> not &&
                date.DayOfWeek = DayOfWeek.Sunday |> not &&
                holidays |> List.contains date.Date |> not
            )

    static member userCulture = Globalization.CultureInfo.CreateSpecificCulture("en-US")
    
    // countries table sometimes can have test entries, or entries that no longer return any results
    // where data provider no longer has stocks registered for such a country, e.g.
    // we want to filter those out and provide a method that is a single place for such filtering
    static member includeCountryInScans countryName =
        countryName <> "United States" && countryName <> "Costa Rica"

type ScreenerResult = {
    ticker:StockTicker.T;
    company:string;
    sector:string;
    industry:string;
    country:string;
    marketCap:decimal;
    price:decimal;
    change:decimal;
    volume:int64;
}



type ScreenerResultReport =
    {
        screenerid:int;
        name:string;
        url:string;
        date:DateTime;
        count:int
    }
type ScreenerResultReportItem =
    {
        stockid:int;
        ticker:string;
        name:string;
        sector:string;
        industry:string;
        country:string;
        date:DateTime;
        marketCap:decimal;
        price:decimal;
        change:decimal;
        volume:int64;
        screenerid:int;
        screenername:string;
    }

type Stock = {
    id: int;
    ticker: StockTicker.T;
    company: string;
    sector: string;
    industry: string;
    country: string;
    marketCap: decimal option;
    lastUpdate: DateTime option
}

type Screener = {
    id: int;
    name: string;
    url: string;
}

type SMABreakdown =
    {
        date: DateTime;
        days: SMA;
        above: int;
        below: int;
    }

    member this.total = this.above + this.below

    member this.percentAbove =
        match this.total with
            | 0 -> 0.0m
            | _ -> (decimal this.above ) * 100.0m / (decimal this.total)

    member this.percentAboveRounded =
        Math.Round(this.percentAbove, 0)

type IndustrySMABreakdown = 
    {
        industry: string;
        breakdown: SMABreakdown;
    }

    static member blank industry =
        {
            industry = industry;
            breakdown = {
                date = ReportsConfig.now();
                days = SMA20;
                above = 0;
                below = 0;
            }
        }
        
type CountrySMABreakdown =
    {
        country: string
        breakdown: SMABreakdown
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
        String.Format("{0:N2}%", this.streakRate)

    member this.changeFormatted =
        String.Format("{0:N0}", this.change)

    member this.streakFormatted =
        String.Format("{0:N0}", this.streak)
        
type IndustryTrend =
    {
        industry: string;
        trend: Trend;
        above: int;
        below: int;
        days: SMA;
        date: DateTime;
    }
    
    member this.total = this.above + this.below
    
    member this.percentAbove =
        match this.total with
            | 0 -> 0.0m
            | _ -> (decimal this.above ) * 100.0m / (decimal this.total)

    member this.percentAboveFormatted =
        String.Format(
            "{0:N2}%",
            this.percentAbove
        )

type DataPoint =
    {
        date: DateTime;
        value: decimal;
    }

    static member create (smaBreakdown:SMABreakdown) = 
        {
            date = smaBreakdown.date;
            value = smaBreakdown.percentAbove
        }

type MarketCycle =
    {
        startPoint: DataPoint;
        highPoint: DataPoint;
        currentPoint: DataPoint;
    }

    member this.ageInMarketDays =
        (this.startPoint.date, this.currentPoint.date) |> ReportsConfig.listOfBusinessDates |> Seq.length

    member this.age =
        this.currentPoint.date - this.startPoint.date

    member this.ageDays =
        Math.Floor((this.currentPoint.date - this.startPoint.date).TotalDays) |> decimal

    member this.ageFormatted =
        String.Format("{0:N0} days", this.age.TotalDays)

    member this.highPointAge =
        this.currentPoint.date - this.highPoint.date
    member this.highPointAgeFormatted =
        String.Format("{0:N0} days", this.highPointAge.TotalDays)
    member this.highPointValue = this.highPoint.value
    member this.highPointValueFormatted = String.Format("{0:N0}%", this.highPointValue)
    member this.highPointDate = this.highPoint.date
    member this.highPointDateFormatted = this.highPoint.date.ToString("d")

    member this.startPointValue = this.startPoint.value
    member this.startPointDate = this.startPoint.date
    member this.startPointDateFormatted = this.startPoint.date.ToString("d")

    member this.currentPointValue = this.currentPoint.value
    member this.currentPointDateFormatted = this.currentPoint.date.ToString("d")

    member this.change = this.currentPoint.value - this.startPoint.value

    member this.rateOfChange =
        match this.ageDays with
        | 0m -> this.change
        | _ -> this.change / this.ageDays

type TrendWithCycle =
    {
        trend: Trend;
        cycle: MarketCycle;
    }

type IndustryWithCycle = string * MarketCycle

type JobStatus =
    | Success
    | Warning
    | Failure

type JobName =
    | ScreenerJob
    | TrendsJob
    | TestJob
    | EarningsJob
    | CountriesJob
    | AlertsJob
    
type Job = {
    name: JobName;
    status: JobStatus;
    message: string;
    timestamp: DateTime; 
}

type EarningsTime =
    | BeforeMarket
    | AfterMarket
    
type Sentiment = 
    | Positive
    | Negative
    | Neutral

type AlertType =
    | IndustryAlert of string
    | ScreenerAlert of int
    | IndustryScreenerAlert of string * int
    
type Alert =
    {
        date: DateTime
        sentiment: Sentiment
        description: string
        strength: decimal
        alertType: AlertType
        acknowledged: bool
    }
    with
        member this.identifier =
            
            let additionalInfo =
                match this.alertType with
                | IndustryAlert industry -> $"{nameof(IndustryAlert)}_{industry}"
                | IndustryScreenerAlert(industry,screenerId) -> $"{nameof(IndustryScreenerAlert)}_{industry}_{screenerId}"
                | ScreenerAlert screenerId -> $"{nameof(ScreenerAlert)}_{screenerId}"
                
            let datePart = this.date.ToString("yyyy-MM-dd")
            
            $"{datePart}_{additionalInfo}"
            
        static member getIndustry alert =
            match alert.alertType with
            | IndustryAlert industry -> Some industry
            | IndustryScreenerAlert(industry,_) -> Some industry
            | _ -> None
            
        static member getScreenerId alert =
            match alert.alertType with
            | ScreenerAlert id -> Some id
            | IndustryScreenerAlert(_,id) -> Some id
            | _ -> None

type IndustrySequenceType =
    | High
    | Low
    
    static member fromString str =
        match str with
        | "High" -> High
        | "Low" -> Low
        | _ -> failwith $"Invalid IndustrySequenceType {str}"
    
type IndustrySequence = {
        type': IndustrySequenceType
        industry: string
        values: DataPoint list
        open': bool
    }
    with
        member this.start = this.values |> List.last
        member this.end' = this.values.Head
        member this.length = this.values.Length
        member this.age = this.end'.date - this.start.date
        member this.ageInDays = this.age.TotalDays |> Math.Floor |> int