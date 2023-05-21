
type BestFitLine = float * float

let calculateLineBasedOnBestFitParams (bestFitParams:BestFitLine) feed =
    let x = feed |> List.map fst
    let m, b = bestFitParams
    x |> List.map (fun x -> m * x + b)

// Define a function to find the best fit line for a set of points
let linearRegressionBasedBestFitLine points =
    let xs = List.map fst points
    let ys = List.map snd points

    let xMean = xs |> List.average
    let yMean = ys |> List.average
        
    // slope of the line
    // m = Σ(x - x̅)(y - y̅) / Σ(x - x̅)^2
    let m = 
        let numerator = List.zip xs ys |> List.sumBy (fun (x, y) -> (x - xMean) * (y - yMean))
        let denominator = xs |> List.sumBy (fun x -> (x - xMean) ** 2.0)
        numerator / denominator

    // y-intercept of the line
    // b = y̅ - m * x̅

    let b = yMean - m * xMean
    // Get the price feed data

    BestFitLine(m, b)


let priceFeed =
    [ (1.0, 100.0)
      (2, 110)
      (3, 120)
      (4, 115)
      (5, 120)
      (6, 125)
      (7, 130)
      (8, 170)
      (9, 180)
      (10, 190) ]

// Find the best fit line for the price feed data
let lineParams =
  priceFeed
  |> linearRegressionBasedBestFitLine

System.Console.WriteLine("Slope: {0}", fst lineParams)
System.Console.WriteLine("Y-intercept: {0}", snd lineParams)

// Plot the price feed data and the best fit line
// plot priceFeed bestFitLine

#r "nuget: Plotly.NET, 4.0.0"

open Plotly.NET

let xs = priceFeed |> List.map fst
let ys = priceFeed |> List.map snd

let priceChart = Chart.Point(xs, ys)
let lineXs = xs
let lineYs = priceFeed |> calculateLineBasedOnBestFitParams lineParams

let lineChart = Chart.Line(lineXs, lineYs)

let combinedChart = Chart.combine([priceChart; lineChart])

let myFirstStyledChart =
    combinedChart
    |> Chart.withTitle "Hello world!"
    |> Chart.withXAxisStyle ("xAxis")
    |> Chart.withYAxisStyle ("yAxis")

myFirstStyledChart |> Chart.show