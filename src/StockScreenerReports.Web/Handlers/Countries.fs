namespace StockScreenerReports.Web.Handlers

open StockScreenerReports.Core

module Countries =
    open StockScreenerReports.Web.Shared
    open StockScreenerReports.Storage
    
    let toBreakdownMap breakdowns =
        (breakdowns:CountrySMABreakdown list)
        |> List.map (fun x -> (x.country, x))
        |> Map.ofList
        
    let toBreakdownColumns countryBreakdownOption =
        match countryBreakdownOption with
        | Some (countryBreakdown:CountrySMABreakdown) ->
            let breakdown = countryBreakdown.breakdown
            let aboveWithTotal = $"{breakdown.above} / {breakdown.total}"
            [
                Views.StringColumn(aboveWithTotal)
                Views.NumberColumn(breakdown.percentAboveRounded)
            ]
        | None -> [Views.StringColumn("N/A"); Views.StringColumn("N/A")]
        
    let toCountryStatsCells countrySMABreakdowns20Map countrySMABreakdowns200Map (country:string) =
        let countrySMABreakdown20 = countrySMABreakdowns20Map |> Map.tryFind country
        let countrySMABreakdown200 = countrySMABreakdowns200Map |> Map.tryFind country
        
        let columns20 = countrySMABreakdown20 |> toBreakdownColumns
        let columns200 = countrySMABreakdown200 |> toBreakdownColumns
        
        let countryLink = country |> Links.countryLink
            
        [Views.LinkNewTabColumn(country, countryLink)] @ columns20 @ columns200 
        
    let toCountryChartRow length dailySMABreakdownMap country =
        
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

    let renderCountriesTable countrySMABreakdowns20Map countrySMABreakdowns200Map dailySMABreakdownMap countries =
        
        let headers = ["Country"; "20 Above"; "20 % Above"; "200 Above"; "200 % Above"]
        
        countries
        |> List.sortByDescending (fun country -> countrySMABreakdowns20Map |> Map.tryFind country |> Option.map (fun x -> x.breakdown.percentAbove))
        |> List.map (fun country ->
            
            let statsCells = toCountryStatsCells countrySMABreakdowns20Map countrySMABreakdowns200Map country
            let chartRow = toCountryChartRow statsCells.Length dailySMABreakdownMap country
            [statsCells |> Views.toTr; chartRow]
        )
        |> List.concat
        |> Views.fullWidthTableWithSortableHeaderCells headers
    
    let generateElementsToRender missedJobs industrySMABreakdowns20Map industrySMABreakdowns200Map dailySMABreakdownMap countries =
        
        let warningSection = Views.jobAlertSection missedJobs
        
        let countriesTable = renderCountriesTable industrySMABreakdowns20Map industrySMABreakdowns200Map dailySMABreakdownMap countries

        [
            warningSection
            countriesTable
        ]

    let handler()  =
       
        let missedJobs =
            Storage.getJobs()
            |> List.filter (fun j -> j.name = JobName.CountriesJob)
            |> List.filter Utils.failedJobFilter
            
        let countries = Storage.getCountries()
        
        let latestDate = Reports.getCountrySMABreakdownLatestDate()
        let formattedDate = latestDate |> Utils.convertToDateString
        let dateRange = ReportsConfig.dateRangeAsStrings()
        
        let industrySMABreakdowns20Map = Reports.getCountrySMABreakdowns Constants.SMA20 formattedDate |> toBreakdownMap
        let industrySMABreakdowns200Map = Reports.getCountrySMABreakdowns Constants.SMA200 formattedDate |> toBreakdownMap
        let dailySMABreakdownMap =
            countries
            |> List.map (fun country ->
                let breakdowns = country |> Reports.getCountrySMABreakdownsForCountry Constants.SMA20 dateRange
                (country, breakdowns)
            )
            |> Map.ofList

        let elementsToRender = generateElementsToRender missedJobs industrySMABreakdowns20Map industrySMABreakdowns200Map dailySMABreakdownMap countries

        elementsToRender |> Views.mainLayout $"Countries"