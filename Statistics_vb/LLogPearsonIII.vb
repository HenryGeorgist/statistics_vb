Public Class LLogPearsonIII
    Inherits ContinuousDistribution
    Private _mu As Double
    Private _sigma As Double
    Private _gamma As Double
    Private _l1 As Double
    Private _alpha As Double
    Private _beta As Double
    Private _xi As Double
    Private _sampleSize As Int32
    Sub New()
        'empty for reflection
    End Sub
    Sub New(ByVal data() As Double)
        SetParameters(data)
    End Sub
    Public Overrides Sub SetParameters(data() As Double)
        Dim L As New LinearMomentsStats(data)
        _l1 = L.GetL1
        Dim l2 As Double = L.GetL2
        Dim z As Double = 0
        Dim a As Double = 0
        Dim abst3 As Double = Math.Abs(L.GetT3)
        If abst3 > 0 And abst3 < (1 / 3) Then
            z = 3 * Math.PI * (L.GetT3 ^ 2)
            a = ((1 + (0.2906 * z)) / (z + (0.1882 * (z ^ 2)) + (0.0442 * (z ^ 3))))
        ElseIf abst3 < 1 Then
            z = 1 - Math.Abs(L.GetT3)
            a = ((0.36067 * z - (0.59567 * (z ^ 2)) + (0.25361 * (z ^ 3))) / (1 - 2.78861 * z + (2.56096 * (z ^ 2)) - (0.77045 * (z ^ 3))))
        Else
            Throw New ArithmeticException("the absolute value of t3 is greater than 1, no solution is available")
        End If
        _gamma = (2 / Math.Sqrt(a)) * Math.Sign(L.GetT3)
        _mu = _l1
        _sigma = (l2 * Math.Sqrt(Math.PI) * Math.Sqrt(a) * Math.Exp(gammaln(a))) / Math.Exp(gammaln(a + 0.5))
        _samplesize = data.Count
        If _gamma <> 0 Then
            _alpha = 4 / (_gamma ^ 2)
            _xi = _mu - ((2 * _sigma) / _gamma)
            _beta = 0.5 * _sigma * Math.Abs(_gamma)
        Else
            'this should be normal
            Throw New ArithmeticException("this data is better approximated by the normal distribution")
        End If
    End Sub
    Public Overrides Function Validate() As String
        Return Nothing    'can the mean be negative?
    End Function

    Public Overrides Function Clone() As ContinuousDistribution
        Dim result As New LLogPearsonIII
        With result
            ._mu = _mu
            ._sigma = _sigma
            ._gamma = _gamma
            ._alpha = _alpha
            ._beta = _beta
            ._xi = _xi
            ._sampleSize = _sampleSize
            ._l1 = _l1
        End With
        Return result
    End Function
    Public Overrides Function GetCDF(value As Double) As Double
        If _gamma < 0 Then
            Return 1 - (incompletegammalower(_alpha, ((_xi - value) / _beta)) / Math.Exp(gammaln(_alpha)))
        Else
            Return (incompletegammalower(_alpha, ((value - _xi) / _beta)) / Math.Exp(gammaln(_alpha)))
        End If
    End Function
    Public Overrides ReadOnly Property GetCentralTendency As Double
        Get
            Return _l1
        End Get
    End Property

    Public Overrides Function getDistributedVariable(probability As Double) As Double
        Dim xn As Double = GetCentralTendency
        Dim testvalue As Double = GetCDF(xn)
        Dim n As Integer = 0
        Do  'random definition of epsilon
            xn = xn - ((testvalue - probability) / GetPDF(xn))
            testvalue = GetCDF(xn)
            n += 1
        Loop Until (Math.Abs(testvalue - probability) <= 0.000000000000001 Or n = 100)
        Return xn
    End Function
    Public Overrides ReadOnly Property GetNumberOfParameters As Short
        Get
            Return 3
        End Get
    End Property
    Public Overrides Function GetPDF(Value As Double) As Double
        If _gamma < 0 Then
            Return ((((_xi - Value) ^ (_alpha - 1)) * (Math.Exp(-(_xi - Value) / _beta))) / ((_beta ^ _alpha) * (Math.Exp(gammaln(_alpha)))))
        Else
            Return ((((Value - _xi) ^ (_alpha - 1)) * (Math.Exp(-(Value - _xi) / _beta))) / ((_beta ^ _alpha) * (Math.Exp(gammaln(_alpha)))))
        End If
    End Function
    Public Overrides ReadOnly Property GetSampleSize As Integer
        Get
            Return _sampleSize
        End Get
    End Property
End Class
