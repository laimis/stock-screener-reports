namespace FinvizScraper

type ScreenerInput = {
    name:string;
    url:string;
    filename:string;
}

type FinvizConfig =
    {
        screeners:list<ScreenerInput>;
        outputPath:string;
    }
    static member getRunDate() =
        let date = System.DateTime.Now
        date.ToString("yyyy-MM-dd")

type ScreenerResult = {
    ticker:string;
    company:string;
    sector:string;
    industry:string;
    country:string;
    marketCap:string;
    price:decimal;
    change:string;
    volume:string;
}

type ScreenerBreakdown = {
    name:string;
    breakdown:(ScreenerResult) -> string
}