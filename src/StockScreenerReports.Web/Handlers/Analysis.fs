namespace StockScreenerReports.Web.Handlers

module Analysis =
    open Giraffe
    open StockScreenerReports.Web.Shared.Views
    open Giraffe.ViewEngine
    open StockScreenerReports.Core
    open StockScreenerReports.Storage
    open StockScreenerReports.Web.Shared

    [<CLIMutable>]
    type ScreenerInput =
        {
            tickers: string
        }

    let createTickerAnalysisForm text =

        // form with a textarea for tickers and a submit button
        form [
                _method "POST"
                _action "/analysis/tickers"
            ] [
                div [ _class "field" ] [
                    textarea [
                        _class "textarea";
                        _name "tickers";
                        _rows "5";
                        _cols "50"
                    ] [
                        str text
                    ]
                ]
                button [
                    _class "button is-primary";
                    _type "submit"
                ] [
                    str "Analyze"
                ]
            ]

    let analyzeHandler : HttpHandler =
        fun (next : HttpFunc) (ctx : Microsoft.AspNetCore.Http.HttpContext) ->
            task {
                let! input = ctx.BindFormAsync<ScreenerInput>()
                
                let tickerForm = createTickerAnalysisForm input.tickers

                // tickers come in looking like this: NASDAQ:VRNS,NYSE:ABG,NASDAQ:LULU,NYSE:CLW
                let cleanedTickers =
                    input.tickers.Split([|','|], System.StringSplitOptions.RemoveEmptyEntries)
                    |> Array.map (fun ticker -> ticker.Trim())
                    |> Array.map (fun ticker -> ticker.Split([|':'|], System.StringSplitOptions.RemoveEmptyEntries)[1])
                    |> Array.map (fun ticker -> ticker.ToUpper())
                    |> Array.map (fun ticker -> 
                        (ticker, ticker |> StockTicker.create)
                    )

                let industryMarketCycles =
                    Constants.SMA20
                    |> Storage.getIndustryCycles
                    |> Map.ofList

                let stockData =
                    cleanedTickers
                    |> Array.map (fun (ticker, stock) -> 
                        (ticker, stock |> Storage.getStockByTicker)
                    )
                    |> Array.map (fun (ticker, stockOption) -> 
                        
                        let cols =
                            match stockOption with
                            | Some stock ->
                                let industry = stock.industry
                                let industryMarketCycle = industryMarketCycles[industry]

                                [
                                    LinkNewTabColumn(ticker, stock.ticker |> StockTicker.value |> Links.stockLink)
                                    StringColumn(stock.company)
                                    LinkNewTabColumn(stock.sector, stock.sector |> Links.sectorLink)
                                    LinkNewTabColumn(stock.industry, stock.industry |> Links.industryLink)
                                    LinkNewTabColumn(stock.country, stock.country |> Links.countryLink)
                                    NumberColumn(industryMarketCycle.currentPoint.value)
                                    NumberColumn(industryMarketCycle.age.TotalDays |> decimal)
                                    LinkNewTabColumn("chart", stock.ticker |> StockTicker.value |> Links.tradingViewLink)
                                ]
                            | None -> 
                                [
                                    StringColumn(ticker)
                                    StringColumn("Not Found")
                                    StringColumn("")
                                    StringColumn("")
                                    StringColumn("")
                                    StringColumn("")
                                    StringColumn("")
                                    StringColumn("")
                                ]

                        cols |> toTr
                    )
                    |> Array.toList
                    

                let headers = ["Ticker"; "Company"; "Sector"; "Industry"; "Country"; "Cycle Value"; "Cycle Age"; ""]
                let table = fullWidthTableWithSortableHeaderCells headers stockData

                let content =
                    div [ _class "container" ] [
                        tickerForm   |> toSection "Tickers to analyze"
                        table |> toSection $"Results ({stockData.Length})"
                    ]

                return! ([content] |> mainLayout "Screeners") next ctx
            }
    let handler : HttpHandler =
        fun (next : HttpFunc) (ctx : Microsoft.AspNetCore.Http.HttpContext) ->

            let tickerForm = createTickerAnalysisForm ""

            let content =
                div [ _class "container" ] [
                    tickerForm   |> toSection "Tickers to analyze"
                ]

            ([content] |> mainLayout "Screeners") next ctx