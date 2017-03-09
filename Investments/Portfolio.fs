module Portfolio

open MathNet.Numerics
open MathNet.Numerics.Statistics
open MathNet.Numerics.LinearAlgebra
open System.Collections.Generic

open Accord.Math.Optimization

type ReturnSeries = float option list
type Returns = ReturnSeries list

type PortfolioStatistics = {
    Weights : Matrix<float>
    ExpectedReturn : float
    Variance : float
}

let array2DOfLists listA listB =
    let lists = [listA; listB]
    let n = List.length listA
    let m = Array2D.create n n 0.0
    for list in 0 .. 1 do
        for i in 0 .. n-1 do
            m.[i, list] <- lists.[list].[i]
    m

let array2DOfRow (row':float seq) =
    let row = Array.ofSeq row'
    let n = Seq.length row
    let m = Array2D.create 1 n 0.0
    for c in 0 .. n-1 do
        m.[0,c] <- row.[c]
    m

let array2DOfColumn (row':float seq) =
    let row = Array.ofSeq row'
    let n = Seq.length row
    let m = Array2D.create n 1 0.0
    for r in 0 .. n-1 do
        m.[r,0] <- row.[r]
    m

let matrixOfArray2D (array:float[,]) =
    let rows = Array2D.length1(array)
    let columns = Array2D.length2(array)

    [for r in 0 .. rows-1 ->
        [for c in 0 .. columns-1 ->
            array.[r, c]
    ]]
    |> matrix
    
let dropMissingValues (values: ReturnSeries) =
    let somes = Seq.skipWhile Option.isNone values
    (* This finds if there was a missing value in the middle of the data
    let none = Seq.tryFindIndex Option.isNone somes
    let index = 
        match none with
        | Some i -> i
        | _ -> 0
    *)
    somes
    |> Seq.map Option.get
    |> List.ofSeq

let innerJoin (listA':ReturnSeries) (listB':ReturnSeries) =
    let listA = dropMissingValues listA'
    let listB = dropMissingValues listB'
    let lengthA, lengthB = List.length listA, List.length listB
    let smallestCount : int = min lengthA lengthB
    Seq.skip (lengthA - smallestCount) listA |> List.ofSeq,
    Seq.skip (lengthB - smallestCount) listB |> List.ofSeq

let portfolioVariance omega (weights:Matrix<float>) =
    let v' : Matrix<float> = weights.Transpose() * omega * weights
    v'.[0, 0]

let portfolioVariance' omega (weights':float[]) =
    array2DOfColumn weights'
    |> matrixOfArray2D
    |> portfolioVariance omega

let portfolioReturn (means:Matrix<float>) (weights:Matrix<float>) =
    let r' : Matrix<float> = means * weights.Transpose()
    r'.[0, 0]

let portfolioReturn' means (weights':float[]) =
    array2DOfColumn weights'
    |> matrixOfArray2D
    |> portfolioReturn means

let portfolioStatistics means omega weights =
    let expectedReturn = portfolioReturn means weights
    let variance = portfolioVariance omega weights
    { Weights = weights; ExpectedReturn = expectedReturn; Variance = variance }

let portfolioStatistics' means omega weights' =
    array2DOfColumn weights'
    |> matrixOfArray2D
    |> portfolioStatistics means omega

let means (returns:Returns) =
    let n = returns.Length
    [for r in 0 .. n-1 -> 
        [for c in 0 .. 0 ->
            let dropped = dropMissingValues returns.[r]
            Statistics.Mean(dropped)]]
    |> matrix

let covariance listA listB =
    let n = List.length listA |> float
    let meanA = List.sum listA / n
    let meanB = List.sum listB / n

    List.zip listA listB
    |> List.fold (fun acum (a,b) -> (a - meanA)*(b - meanB) + acum) 0.0
    |> fun sum -> sum / n

let omega (returns:Returns) =
    let n = returns.Length
    let m = Array2D.create n n 0.


    for r in 0 .. n-1 do
        for c in 0 .. n-1 do
            if r > c
            then m.[r, c] <- m.[c, r]  // covariance(a, b) = covariance(b, a)
            else
            let a, b = innerJoin returns.[r] returns.[c]
            m.[r, c] <- covariance a b

    matrixOfArray2D m

let standarizeWeights weights' =
    let sum = Seq.sum weights'
    let stdWeights' = Seq.map (fun w -> w / sum) weights'
    array2DOfColumn stdWeights'
    |> matrixOfArray2D

let weights (omega:Matrix<float>) (iVector:Matrix<float>) =
    (omega.Inverse()*iVector).Transpose().ToRowArrays().[0] // weights (1*n)
    |> standarizeWeights
    
let optimalPortfolio (returns:Returns) (riskFree:float) =
    let n = returns.Length
    let means = means returns
    let omega = omega returns
    let iVector = means - riskFree

    weights omega iVector
    |> portfolioStatistics means omega

let portfolioOptimization (returns:Returns) (riskFree:float) objectiveReturn =
    let n = returns.Length
    let means = means returns
    let omega = omega returns

    let iVector = means - riskFree

    let optimal = omega.Inverse() * iVector

    let obj = NonlinearObjectiveFunction(n, (portfolioVariance' omega))
    let constraints = List<NonlinearConstraint>()
    constraints.Add(NonlinearConstraint(obj, Seq.sum, ConstraintType.EqualTo, 1.0))
    constraints.Add(NonlinearConstraint(obj, (portfolioReturn' means), ConstraintType.EqualTo, objectiveReturn))

    let solver = Cobyla(obj, constraints)
        
    let success = solver.Minimize()

    let value = solver.Value
    let weights = solver.Solution
    let status = solver.Status
    portfolioStatistics' means omega weights