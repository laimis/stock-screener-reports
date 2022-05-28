namespace FinvizScraper.Web.Handlers

module Dashboard =

    open Giraffe.ViewEngine
    open FinvizScraper.Storage.Reports
    open FinvizScraper.Web.Shared
    open FinvizScraper.Core
    open FinvizScraper.Storage.Storage

    let private generateScreenerResultSection (screener:ScreenerResultReport) = 
        
        let screenerDate = screener.date.ToString("yyyy-MM-dd")
        
        div [_class "content"] [
            h2 [] [
                
                Views.generateHrefWithAttr
                    $"{screener.count}"
                    (Links.screenerResultsLink (screener.screenerid) screenerDate)
                    (_class "button is-primary mr-2")

                str screener.name
            ]
        ]

    let private generateJobStatusRow() =
        div [ _class "columns" ] [
            div [ _class "column" ] [ 
                ScreenerJob |> Views.genericJobStatusGet |> str 
            ]
        ]

    let generateTrendsTable title nameCounts industryUpdates =
        let rows =
            nameCounts
            |> List.truncate 10
            |> List.map (fun (name,count) ->

                let industryRankOption = 
                    industryUpdates |> List.tryFindIndex (fun (industry) ->
                        industry.industry = name
                    )

                let industryRank =
                    match industryRankOption with
                    | Some index ->
                        (index + 1).ToString()
                    | None -> "N/A"

                tr [] [
                    td [] [ 
                        Views.generateHref
                            name
                            (Links.industryLink name)
                    ]
                    td [ _class "has-text-right" ] [
                        str industryRank
                    ]
                    td [ _class "has-text-right"] [ str (count.ToString()) ]
                ])

        let header = tr [] [
            th [] [ str title ]
            th [ _class "has-text-right" ] [ str "Industry Rank" ]
            th [ _class "has-text-right" ] [ str "# of stocks" ]
        ]

        header::rows |> Views.fullWidthTable

    let private generateIndustryTrendsRow days =

        let gainers = FinvizConfig.NewHighsScreener |> getTopIndustriesForScreener days |> List.take 5
        let losers = FinvizConfig.NewLowsScreener |> getTopIndustriesForScreener days |> List.take 5

        let industryUpdates = 
            getIndustryUpdatesLatestDate()
            |> FinvizConfig.formatRunDate
            |> getIndustryUpdates 200

        [
            div [_class "columns"] [
                div [ _class "column" ] [
                    generateTrendsTable "Industries Trending Up" gainers industryUpdates
                ]
                div [ _class "column" ] [
                    generateTrendsTable "Industries Trending Down" losers industryUpdates
                ]
            ]
        ]

    let private generateSectorTrendsRow days =

        let gainers = FinvizConfig.NewHighsScreener |> getTopSectorsForScreener days
        let losers = FinvizConfig.NewLowsScreener |> getTopSectorsForScreener days

        [
            div [_class "columns"] [
                div [ _class "column" ] [
                    Views.toNameCountTableWithLinks "Sectors Trending Up" 5 Links.sectorLink gainers
                ]
                div [ _class "column" ] [
                    Views.toNameCountTableWithLinks "Sectors Trending Down" 5 Links.sectorLink losers
                ]
            ]
        ]

    let private createView (screeners:list<ScreenerResultReport>) =
        
        let screenerRows = screeners |> List.map generateScreenerResultSection

        let industryTrendRows = generateIndustryTrendsRow FinvizConfig.industryTrendDayRange
        let sectorTrendRows = generateSectorTrendsRow FinvizConfig.sectorTrendDayRange

        let jobStatusRow = generateJobStatusRow()

        screenerRows @ industryTrendRows @ sectorTrendRows @ [ jobStatusRow ]

    let handler()  = 
        
        // get screeners, render them in HTML
        getLatestScreeners()
        |> createView
        |> Views.mainLayout "Dashboard"