namespace StockScreenerReports.Web.Handlers

open StockScreenerReports.Core

module Countries =
    open Giraffe
    open Giraffe.ViewEngine
    open StockScreenerReports.Web.Shared
    open StockScreenerReports.Storage
    
    type SortAlgo =
        | PercentAbove200
        | PercentAbove20
        
    let sortAlgoToString sortAlgo =
        match sortAlgo with
        | PercentAbove200 -> "percentAbove200"
        | PercentAbove20 -> "percentAbove20"
        
    let stringToSortAlgo sortAlgoString =
        match sortAlgoString with
        | "percentAbove200" -> PercentAbove200
        | "percentAbove20" -> PercentAbove20
        | _ -> PercentAbove200
    
    let private toBreakdownMap breakdowns =
        (breakdowns:CountrySMABreakdown list)
        |> List.map (fun x -> (x.country, x))
        |> Map.ofList
        
    let private toBreakdownColumns countryBreakdownOption =
        match countryBreakdownOption with
        | Some (countryBreakdown:CountrySMABreakdown) ->
            let breakdown = countryBreakdown.breakdown
            let aboveWithTotal = $"{breakdown.above} / {breakdown.total}"
            [
                Views.StringColumn(aboveWithTotal)
                Views.NumberColumn(breakdown.percentAboveRounded)
            ]
        | None -> [Views.StringColumn("N/A"); Views.StringColumn("N/A")]
        
    let private toCountryStatsCells countrySMABreakdowns20Map countrySMABreakdowns200Map (country:string) =
        let countrySMABreakdown20 = countrySMABreakdowns20Map |> Map.tryFind country
        let countrySMABreakdown200 = countrySMABreakdowns200Map |> Map.tryFind country
        
        let columns20 = countrySMABreakdown20 |> toBreakdownColumns
        let columns200 = countrySMABreakdown200 |> toBreakdownColumns
        
        let countryLink = country |> Links.countryLink
            
        [Views.LinkNewTabColumn(country, countryLink)] @ columns20 @ columns200 
        
    let private toCountryChartRow length dailySMABreakdownMap country =
        
        let dailyBreakdowns = dailySMABreakdownMap |> Map.tryFind country
        
        let contents =
            match dailyBreakdowns with
            | None -> Giraffe.ViewEngine.HtmlElements.div [] [ Giraffe.ViewEngine.HtmlElements.str "No data available to chart"]
            | Some dailyBreakdowns ->
                
                let smaInterval = 20
                
                let dataset : Charts.DataSet<decimal> =
                    let series = 
                        dailyBreakdowns
                        |> List.map (fun (u:CountrySMABreakdown) -> System.Math.Round(u.breakdown.percentAbove, 0))
                        
                    {
                        data = series
                        title = $"{smaInterval} SMA Trend"
                        color = smaInterval |> Constants.mapSmaToColor
                    }
                    
                    
                let labels = dailyBreakdowns |> List.map (fun u -> u.breakdown.date.ToString("MMM/dd"))
                
                let chartElements =
                    [dataset] |> Charts.generateChartElements "sma breakdown chart" Charts.ChartType.Line (Some 100) Charts.smallChart labels
                
                Giraffe.ViewEngine.HtmlElements.div [] chartElements
        
        Giraffe.ViewEngine.HtmlElements.tr [] [
            Giraffe.ViewEngine.HtmlElements.td [ Giraffe.ViewEngine.Attributes._colspan (length.ToString())] [
                contents
            ]
        ]

    let private renderCountriesTable countrySMABreakdowns20Map countrySMABreakdowns200Map dailySMABreakdownMap countries sortFunc =
        
        let headers = ["Country"; "20 Above"; "20 % Above"; "200 Above"; "200 % Above"]
        
        countries
        |> List.sortByDescending sortFunc
        |> List.map (fun country ->
            
            let statsCells = toCountryStatsCells countrySMABreakdowns20Map countrySMABreakdowns200Map country
            let chartRow = toCountryChartRow statsCells.Length dailySMABreakdownMap country
            [statsCells |> Views.toTr; chartRow]
        )
        |> List.concat
        |> Views.fullWidthTableWithSortableHeaderCells headers
        
    let private renderSortSection selectedAlgo =
        let sortLink sortAlgo =
            let sortAlgoAsString = sortAlgoToString sortAlgo
            let href = $"?sortParam={sortAlgoAsString}"
            let classes =
                match selectedAlgo = sortAlgo with
                | true -> "button is-primary"
                | false -> "button is-info is-light"
                
            let title =
                match sortAlgo with
                | PercentAbove200 -> "Sort by 200 SMA %"
                | PercentAbove20 -> "Sort by 20 SMA %"
                
            let link = a [ _class classes; _href href ] [ str title ]
            link
            
        let sortLinks =
            [
                sortLink PercentAbove200
                sortLink PercentAbove20
            ]
            
        let sortLinks = sortLinks |> List.map (fun x -> div [_class "level-item"] [x])
        
        let sortSection = div [_class "level"] sortLinks
            
        sortSection
    
    let generateElementsToRender missedJobs industrySMABreakdowns20Map industrySMABreakdowns200Map dailySMABreakdownMap countries sortAlgo sortFunc =
        
        let warningSection = Views.jobAlertSection missedJobs
        
        let sortSection = renderSortSection sortAlgo
        
        let countriesTable = renderCountriesTable industrySMABreakdowns20Map industrySMABreakdowns200Map dailySMABreakdownMap countries sortFunc

        [
            warningSection
            sortSection
            countriesTable
        ]

    let handler : HttpHandler  =
        fun (next : HttpFunc) (ctx : Microsoft.AspNetCore.Http.HttpContext) ->
            
            let sortParam = ctx.GetQueryStringValue("sortParam")
            let sortAlgo =
                match sortParam with
                | Ok value -> value |> stringToSortAlgo
                | Error _ -> PercentAbove200
                
            let missedJobs =
                Storage.getJobs()
                |> List.filter (fun j -> j.name = JobName.CountriesJob)
                |> List.filter Utils.failedJobFilter
                
            let countries = Storage.getCountries()
            
            let latestDate = Reports.getCountrySMABreakdownLatestDate()
            let formattedDate = latestDate |> Utils.convertToDateString
            let dateRange = ReportsConfig.dateRangeAsStrings()
            
            let countrySMABreakdowns20Map = Reports.getCountrySMABreakdowns Constants.SMA20 formattedDate |> toBreakdownMap
            let countrySMABreakdowns200Map = Reports.getCountrySMABreakdowns Constants.SMA200 formattedDate |> toBreakdownMap
            let dailySMABreakdownMap =
                countries
                |> List.map (fun country ->
                    let breakdowns = country |> Reports.getCountrySMABreakdownsForCountry Constants.SMA20 dateRange
                    (country, breakdowns)
                )
                |> Map.ofList
                
            let sortFunc =
                match sortAlgo with
                | PercentAbove20 ->
                    fun country -> countrySMABreakdowns20Map |> Map.tryFind country |> Option.map (fun x -> x.breakdown.percentAbove)
                | PercentAbove200 ->
                    fun country -> countrySMABreakdowns200Map |> Map.tryFind country |> Option.map (fun x -> x.breakdown.percentAbove)

            let elementsToRender =
                generateElementsToRender
                    missedJobs
                    countrySMABreakdowns20Map
                    countrySMABreakdowns200Map
                    dailySMABreakdownMap
                    countries
                    sortAlgo
                    sortFunc

            (elementsToRender |> Views.mainLayout $"Countries") next ctx