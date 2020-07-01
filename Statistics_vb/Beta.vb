Public Class Beta
    Inherits ContinuousDistribution
    Private _alpha As Double
    Private _beta As Double
    Private _sampleSize As Integer
    Sub New()
        'empty for reflection
    End Sub
    Sub New(ByVal data() As Double)
        SetParameters(data)
    End Sub
    Sub New(ByVal alpha As Double, ByVal beta As Double, ByVal samplesize As Integer)
        _alpha = alpha
        _beta = beta
        _sampleSize = samplesize
        Validate()
    End Sub
    Public Overrides Sub SetParameters(data() As Double)
        Dim stats As New BasicProductMomentsStats(data)
        Dim x As Double = stats.GetMean
        Dim v As Double = stats.GetSampleVariance
        If v < (x * (1 - x)) Then
            _alpha = x * (((x * (1 - x)) / v) - 1)
            _beta = (1 - x) * (((x * (1 - x)) / v) - 1)
        Else
            Throw New ContinuousDistributionException("Beta Fitting Error: variance is greater than mean*(1-mean), this data is not factorable to a beta distribution")
        End If
        Validate()
    End Sub
    Public Overrides Function Validate() As String
        If _alpha = 0 Then Return "Beta distribution Error: Alpha parameter cannot be equal to zero"
        Return Nothing
    End Function
    Public ReadOnly Property GetAlpha As Double
        Get
            Return _alpha
        End Get
    End Property
    Public ReadOnly Property GetBeta As Double
        Get
            Return _beta
        End Get
    End Property
    Public Overrides Function Clone() As ContinuousDistribution
        Return New Beta(_alpha, _beta, _sampleSize)
    End Function
    Public Overrides Function GetCDF(value As Double) As Double
        'Throw New NotImplementedException("Need to define the incomplete beta function")
        Return RegularizedIncompleteBetaFunction(_alpha, _beta, value)
    End Function
    Public Overrides ReadOnly Property GetCentralTendency As Double
        Get
            Return 1 / (1 + (_beta / _alpha))
        End Get
    End Property
    Public Overrides Function getDistributedVariable(probability As Double) As Double
        ' use bisection since the shape can be bimodal.
        Dim value As Double = 0.5 'midpoint of the beta output range
        Dim testvalue As Double = GetCDF(value)
        Dim inc As Double = 0.5
        Dim n As Integer = 0
        Do
            If testvalue > probability Then
                'incriment a half incriment down
                inc = inc / 2
                value -= inc
                testvalue = GetCDF(value)
            Else
                'incriment a half incriment up
                inc = inc / 2
                value += inc
                testvalue = GetCDF(value)
            End If
            n += 1
        Loop Until (Math.Abs(testvalue - probability) <= 0.000000000000001 Or n = 100)
        Return value
    End Function
    Public Overrides ReadOnly Property GetNumberOfParameters As Short
        Get
            Return 2
        End Get
    End Property
    Public Overrides Function GetPDF(Value As Double) As Double
        Return ((Value ^ (_alpha - 1)) * ((1 - Value) ^ (_beta - 1))) / BetaFunction(_alpha, _beta)
    End Function
    Public Overrides ReadOnly Property GetSampleSize As Integer
        Get
            Return _sampleSize
        End Get
    End Property
End Class
