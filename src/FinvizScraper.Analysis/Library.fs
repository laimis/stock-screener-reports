namespace FinvizScraper.Analysis

module Simulation =

    let getPrices ticker entryDate exitDate : list<Price> =
        []

    let execute (position:Position) : PositionOutcome =
    
        let prices = getPrices position.ticker position.entry position.exit

        let mutable stopHit = false
        let mutable exitPrice:option<decimal> = None
        let mutable days = 0

        // stop price is 5% below entry price
        let stopPrice = position.entryPrice * 0.95m

        prices
        |> List.iter( fun price ->
            days <- days + 1
            match stopHit with
            | true -> 
                match exitPrice with
                | None -> exitPrice <- Some price.close 
                | Some _ -> ()
            | false ->
                match stopPrice > price.close with
                | true -> stopHit <- true
                | false -> ()
        )

        {
            position = position
            days = 3
            profit = 0m
            profitPercent = 0m
        }
