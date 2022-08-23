namespace FinvizScraper.Analysis
open System

type Price = {
    date: DateTime
    _open: Decimal
    close: Decimal
    high: Decimal
    low: Decimal
}

type Position = {
    entry:DateTime;
    exit:DateTime;
    ticker:FinvizScraper.Core.StockTicker.T;
    entryPrice:decimal;
    exitPrice:decimal;
    numberOfShares:int
}

type PositionOutcome = {
    position:Position;
    days:int;
    profit:decimal;
    profitPercent:decimal;
}