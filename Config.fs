namespace FinvizScraper

module Config =
    let newHighsWithSalesGrowth = {
        name = "New Highs, 1.5x vol, >$10, sales growth";
        url = "https://finviz.com/screener.ashx?v=111&s=ta_newhigh&f=fa_salesqoq_high,sh_avgvol_o100,sh_opt_optionshort,sh_price_o10,sh_relvol_o1.5,ta_perf_dup&ft=4&o=-volume";
        filename = "output-newhighs-sales.html";
    }
    let newHighs = {
        name = "New Highs, 1.5x volume, >$10";
        url = "https://finviz.com/screener.ashx?v=111&s=ta_newhigh&f=sh_avgvol_o100,sh_opt_optionshort,sh_price_o10,sh_relvol_o1.5,ta_perf_dup&ft=4&o=-volume";
        filename = "output-newhighs.html";
    }
    let topGainers = {
        name = "Top Gainer (above $10, relvol 1+)";
        url = "https://finviz.com/screener.ashx?v=111&s=ta_topgainers&f=sh_price_o10,sh_relvol_o1&ft=4&o=-change";
        filename = "output-topgainers.html";
    }
    let topLosers = {
        name = "Top Losers (above $10, relvol 1+)";
        url = "https://finviz.com/screener.ashx?v=111&s=ta_toplosers&f=sh_price_o10,sh_relvol_o1&ft=4&o=-change";
        filename = "output-toplosers.html";
    }
    let newLows = {
        name = "New Lows, >$10, relvol 1+";
        url = "https://finviz.com/screener.ashx?v=111&s=ta_newlow&f=sh_avgvol_o100,sh_opt_optionshort,sh_price_o10,sh_relvol_o1&ft=4&o=change";
        filename = "output-newlows.html";
    }

    let breakdowns = [
        {
            name = "Sectors";
            breakdown = fun a -> a.sector
        }
        {
            name = "Industries";
            breakdown = fun a -> a.industry
        }
        {
            name = "Countries";
            breakdown = fun a -> a.country
        }
    ]

    let screeners =
        [
            newHighsWithSalesGrowth
            newHighs    
            topGainers
            topLosers
            newLows
        ]