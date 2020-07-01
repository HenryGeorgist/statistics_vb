Public Class CorrelatedRandomNumbers
    Private _choleskydecomp As Double(,)
    Private _n As Normal
    Sub New(ByVal correlationmatrix As Double(,))
        _choleskydecomp = choleskydecomp(correlationmatrix)
        Dim n As New Normal
        _n = n
    End Sub
    ''' <summary>
    ''' This will give the user the opportunity to get an array of correlated Snormally distributed random numbers, the array of random numbers needs to be the same dimension the correlation matrix (i.e. double(n,0)
    ''' </summary>
    ''' <param name="SNormrandyarray">a standard normal array that is of dimension (n,0)</param>
    ''' <returns>one array(n,0) of random numbers that are correlated, and normally distributed</returns>
    ''' <remarks></remarks>
    Public Function GetCorrelatedSNormRandoms(ByRef SNormrandyarray(,) As Double) As Double(,)
        Return Multiply(_choleskydecomp, SNormrandyarray)
    End Function
    ''' <summary>
    ''' This will give the user the opportunity to get an array of correlated uniform random numbers, the array of random numbers needs to be the same dimension the correlation matrix (i.e. double(n,0)
    ''' </summary>
    ''' <param name="SNormrandyarray">a standard normal array that is of dimension (n,0)</param>
    ''' <returns>one array(n,0) of random numbers that are correlated, and uniformly distributed between zero and 1 exclusive</returns>
    ''' <remarks></remarks>
    Public Function GetCorrelatedUniformRandoms(ByRef SNormrandyarray(,) As Double) As Double(,)
        Dim CorrelatedNormArray As Double(,) = Multiply(_choleskydecomp, SNormrandyarray)
        For i = 0 To CorrelatedNormArray.GetUpperBound(0)
            CorrelatedNormArray(i, 0) = _n.GetCDF(CorrelatedNormArray(i, 0))
        Next
        Return CorrelatedNormArray
    End Function
    Public Function GetCorrelatedUniformRandoms(ByVal seed As Integer) As Double()
        Dim r As New Random(seed)
        Dim n As New Normal
        Dim SNormRandyarray(_choleskydecomp.GetUpperBound(0), 0) As Double
        For i = 0 To _choleskydecomp.GetUpperBound(0)
            SNormRandyarray(i, 0) = n.getDistributedVariable(r.NextDouble)
        Next
        Dim CorrelatedNormArray As Double(,) = Multiply(_choleskydecomp, SNormRandyarray)
        Dim output(_choleskydecomp.GetUpperBound(0)) As Double
        For i = 0 To CorrelatedNormArray.GetUpperBound(0)
            output(i) = _n.GetCDF(CorrelatedNormArray(i, 0))
        Next
        Return output
    End Function
End Class
