namespace StockScreenerReports.Web.Shared

open StockScreenerReports.Core

module Links =

    // external links
    let tradingViewLink ticker =
        $"https://tradingview.com/chart/kQn4rgoA/?symbol={ticker}"

    let bulmaCssLink = "https://cdn.jsdelivr.net/npm/bulma@0.9.3/css/bulma.min.css"

    let chartJsLink = "https://cdn.jsdelivr.net/npm/chart.js"

    let sortingJsLink = "/scripts/sorting.js"
    let hidingJsLink = "/scripts/hiding.js"

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
         $"/screeners/{screenerId}/results/{date}"

    let sectorLink sectorName =
        $"/sectors/{sectorName}"

    let industryLink industryName =
        $"/industries/{industryName}"

    let industryExportLink industryName =
        $"/industries/{industryName}/export"

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
    
    let trends = "/trends"
    let industries = "/industries"
    let countries = "/countries"
    let earnings = "/earnings"
    let cycles = "/cycles"

    let jobsScreeners = "/jobs/screeners"
    let jobsEarnings = "/jobs/earnings"
    let jobsTrends = "/jobs/trends"
    let jobsCountries = "/jobs/countries"

    let industryFinvizLink (industryName:string) =
        let value =
            industryName
            |> StockScreenerReports.Core.Utils.cleanIndustry
        
        $"https://finviz.com/screener.ashx?v=111&f=ind_" + value

    let ngtdOutcomesReportLink (title,tickers,earnings,startDate,endDate) =
        let commaSeparated = tickers |> String.concat ","
        let earningsCommaSeparated = earnings |> String.concat ","
        let titleCleaned = title |> System.Uri.EscapeDataString

        $"https://{ReportsConfig.ngtdDomain}/reports/outcomes?tickers={commaSeparated}&earnings={earningsCommaSeparated}&title={titleCleaned}&endDate={endDate}&startDate={startDate}"
        
        
    let ngtdTradesReportLink (screenerId,title,tickers) =
        let commaSeparated = tickers |> String.concat ","
        let titleCleaned = title |> System.Uri.EscapeDataString
        
        $"https://{ReportsConfig.ngtdDomain}/reports/trades?screenerId={screenerId}&tickers={commaSeparated}&title={titleCleaned}"