namespace FinvizScraper

type ScreenerInput = {
    name:string;
    url:string;
    filename:string;
}

type FinvizConfig = {
    screeners:list<ScreenerInput>;
    outputPath:string;
}

type ScreenerResult = {
    ticker:string;
    company:string;
    sector:string;
    industry:string;
    country:string;
    marketCap:string;
    price:string;
    change:string;
    volume:string;
}

type ScreenerBreakdown = {
    name:string;
    breakdown:(ScreenerResult) -> string
}