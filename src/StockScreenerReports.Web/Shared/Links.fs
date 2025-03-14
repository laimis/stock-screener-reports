namespace StockScreenerReports.Web.Shared

open StockScreenerReports.Core

module Links =

    // external links
    let tradingViewLink ticker =
        $"https://tradingview.com/chart/kQn4rgoA/?symbol={ticker}"
        
    let ngtdLink ticker = $"https://app.nightingaletrading.com/stocks/{ticker}"

    let bulmaCssLink = "https://cdn.jsdelivr.net/npm/bulma@1.0.0/css/bulma.min.css"

    let chartJsLink = "https://cdn.jsdelivr.net/npm/chart.js"

    let sortingJsLink = "/scripts/sorting.js"
    let hidingJsLink = "/scripts/hiding.js"
    let alertsJsLink = "/scripts/alerts.js"

    let chartJsDatalabelsLink = "https://cdn.jsdelivr.net/npm/chartjs-plugin-datalabels"


    let home = "/"

    let finvizLogoLink = "/finviz_logo.png"

    // app links
    let stockLink (ticker:string) =
        $"/stocks/{ticker}"

    let screenerLink screenerId =
        $"/screeners/{screenerId}"

    let searchLink = "/search"

    let screenerResultsLink screenerId date =
         $"/screeners/{screenerId}/results/{date |> Utils.convertToDateString}"

    let sectorLink sectorName =
        $"/sectors/{sectorName}"

    let industryLink industryName =
        $"/industries/{industryName}"
        
    let industryLinkWithStartAndEndDate startDate endDate industryName =
        let startFormatted = startDate |> Utils.convertToDateString
        let endFormatted = endDate |> Utils.convertToDateString
        
        $"/industries/{industryName}?startDate={startFormatted}&endDate={endFormatted}"

    let industryExportLink industryName =
        $"/industries/{industryName}/export"
        
    let industrySequenceAnalysis = "/industrySequenceAnalysis"
    let industryCorrelations = "/industryCorrelations"
    let industries = "/industries"
    let industriestable = "/industriestable"
    

    let countryLink countryName =
        $"/countries/{countryName}"

    let migrateDateLink = "/screeners/migratedate"
    
    let deleteDateLink = "/screeners/deletedate"
    let renameStockLink = "/screeners/renamestock"
    let deleteStockLink = "/screeners/deletestock"
    let changeStockIndustryLink = "/screeners/changestockindustry"
    
    let screeners = "/screeners"
    let screenersNew = "/screeners/new"
    let screenersDelete id = $"/screeners/{id}/delete"
    let screenersExport id = $"/screeners/{id}/results/export"
    let adhocReportExport = "/reports/adhoc/export"
    
    let dashboard = "/"
    let exportSMA20 = "/export/sma20"
    let exportCycleStarts = "/export/cyclestarts"
    let exportCycleHighs = "/export/cyclehighs"
    
    let trends = "/trends"
    let countries = "/countries"
    let earnings = "/earnings"
    let alerts = "/alerts"
    let cycles = "/cycles"
    let corporateActions = "/corporateActions"
    let corporateActionsBankruptcy = "/corporateActions/bankruptcy"
    let corporateActionsDelisting = "/corporateActions/delisting"

    let jobsScreeners = "/jobs/screeners"
    let jobsEarnings = "/jobs/earnings"
    let jobsTrends = "/jobs/trends"
    let jobsCountries = "/jobs/countries"
    let jobsAlerts = "/jobs/alerts"
    let jobsCorporateActions = "/jobs/corporateActions"

    let industryFinvizLink (industryName:string) =
        let value =
            industryName
            |> StockScreenerReports.Core.Utils.cleanIndustry
        
        $"https://finviz.com/screener.ashx?v=111&f=ind_" + value
        
    let countryFinvizLink (countryName:string) =
        let value =
            countryName
            |> StockScreenerReports.Core.Utils.cleanIndustry
        
        $"https://finviz.com/screener.ashx?v=111&f=geo_" + value

    let ngtdOutcomesReportLink (title:string,tickers,earnings,startDate,endDate) =
        let commaSeparated = tickers |> String.concat ","
        let earningsCommaSeparated = earnings |> String.concat ","
        let titleCleaned = title |> System.Uri.EscapeDataString

        $"https://{ReportsConfig.ngtdDomain}/reports/outcomes?tickers={commaSeparated}&earnings={earningsCommaSeparated}&title={titleCleaned}&endDate={endDate}&startDate={startDate}"
        
        
    let ngtdTradesReportLink (screenerId,title:string,tickers) =
        let commaSeparated = tickers |> String.concat ","
        let titleCleaned = title |> System.Uri.EscapeDataString
        
        $"https://{ReportsConfig.ngtdDomain}/reports/trades?screenerId={screenerId}&tickers={commaSeparated}&title={titleCleaned}"