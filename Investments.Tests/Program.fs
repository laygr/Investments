open Deedle
open NUnit.Framework
open FsUnit

let toFloatOption (o:obj option) =
    match o with
    | Some v ->
        match v with
        | :? (System.Decimal) as d -> Some (float d)
        | :? int as i -> Some (float i)
        | :? string as s when s = "" -> None
        | :? string as s -> Some (float s)
        | _ -> None
    | _ -> None

let returnsFromFrame (frame:Frame<_,_>) =
    [for c in 1 .. frame.ColumnCount-1 ->
            let col = frame.GetColumnAt(c)
            col
            |> Series.valuesAll
            |> Seq.map toFloatOption
            |> List.ofSeq
        ]

[<Test>]
let ``Optimal Portfolio on BMV``() =
    let returns = 
        Frame.ReadCsv("C:\Users\laygr\Desktop\Investments\Investments.Tests\BMV Returns.csv")
        |> returnsFromFrame

    let riskFreeRate = 0.000180633

    let portfolioStatistics = Portfolio.optimalPortfolio returns riskFreeRate
    portfolioStatistics.ExpectedReturn - 0.000096348593253227393
    |> should be (lessThan 0.0001)

    portfolioStatistics.Variance - 0.0013160787682347139
    |> should be (lessThan 0.0001)