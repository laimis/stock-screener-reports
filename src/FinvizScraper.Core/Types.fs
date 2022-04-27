namespace FinvizScraper.Core

module StockTicker =

    type T = StockTicker of string

    // wrap
    let create (s:string) =
        StockTicker (s.ToUpper())

    // unwrap
    let value (StockTicker e) = e

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
    static member getRunDate() =
        let date = System.DateTime.Now
        date.ToString("yyyy-MM-dd")

    static member dayRange = 21
    
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

type IndustryUpdate = {
    industry: string;
    date: System.DateTime;
    above: int;
    below: int;
}