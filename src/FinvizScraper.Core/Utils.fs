namespace FinvizScraper.Core

    module Utils =

        let private nonAlphaRegex = new System.Text.RegularExpressions.Regex("[^a-zA-Z]")

        let cleanIndustry (industry:string) =
            nonAlphaRegex.Replace(industry, "").ToLower()
