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

    let renderCountriesTable countrySMABreakdowns20Map countrySMABreakdowns200Map countries =
        
        let headers = ["Country"; "20 Above"; "20 % Above"; "200 Above"; "200 % Above"]
        
        countries
        |> List.map (fun country ->
            
            let countrySMABreakdown20 = countrySMABreakdowns20Map |> Map.tryFind country
            let countrySMABreakdown200 = countrySMABreakdowns200Map |> Map.tryFind country
            
            let columns20 = countrySMABreakdown20 |> toBreakdownColumns
            let columns200 = countrySMABreakdown200 |> toBreakdownColumns
            
            let countryLink = country |> Links.countryLink
                
            [Views.LinkNewTabColumn(country, countryLink)] @ columns20 @ columns200 |> Views.toTr
        )
        |> Views.fullWidthTableWithSortableHeaderCells headers
    
    let generateElementsToRender missedJobs industrySMABreakdowns20Map industrySMABreakdowns200Map countries =
        
        let warningSection = Views.jobAlertSection missedJobs
        
        let countriesTable = renderCountriesTable industrySMABreakdowns20Map industrySMABreakdowns200Map countries

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

        let industrySMABreakdowns20Map = Reports.getCountrySMABreakdowns Constants.SMA20 formattedDate |> toBreakdownMap
        let industrySMABreakdowns200Map = Reports.getCountrySMABreakdowns Constants.SMA200 formattedDate |> toBreakdownMap

        let elementsToRender = generateElementsToRender missedJobs industrySMABreakdowns20Map industrySMABreakdowns200Map countries

        elementsToRender |> Views.mainLayout $"Countries"