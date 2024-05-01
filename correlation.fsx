
// add reference to MathNet library

#r "nuget: MathNet.Numerics"

open MathNet.Numerics.Statistics

// Define your data sets as sequences of arrays
let dataSets = [|
    [| 1.0; 2.0; 3.0; 4.0; 5.0 |]
    [| 2.0; 4.0; 6.0; 8.0; 10.0 |]
    [| 1.5; 3.0; 4.5; -6.0; -7.5 |]
|]

// Calculate the correlation matrix
let correlationMatrix = Correlation.PearsonMatrix(dataSets)

// Print the correlation matrix
printfn "Correlation Matrix:"
System.Console.WriteLine(correlationMatrix.ToMatrixString())