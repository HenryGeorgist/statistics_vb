Public Class LLogNormal
    Inherits ContinuousDistribution
    Private _k As Double
    Private _alpha As Double
    Private _xi As Double
    Private _l1 As Double
    Private _sampleSize As Int32
    Private Const E0 As Double = 2.0466534
    Private Const E1 As Double = -3.6544371
    Private Const E2 As Double = 1.8396733
    Private Const E3 As Double = -0.20360244
    Private Const F1 As Double = -2.0182173
    Private Const F2 As Double = 1.2420401
    Private Const F3 As Double = -0.21741801
    Sub New()
        'empty for reflection
    End Sub
    Sub New(ByVal data() As Double)
        SetParameters(data)
    End Sub
    Public Overrides Sub SetParameters(data() As Double)
        Dim L As New LinearMomentsStats(data)
        _l1 = L.GetL1
        Dim t3 As Double = L.GetT3
        _k = -t3 * ((E0 + (E1 * (t3 ^ 2)) + (E2 * (t3 ^ 4)) + (E3 * (t3 ^ 6))) / (1 + (F1 * (t3 ^ 2)) + (F2 * (t3 ^ 4)) + (F3 * (t3 ^ 6))))
        Dim sn As New Normal()
        Dim phi As Double = sn.GetCDF(_k / Math.Sqrt(2))
        _alpha = ((L.GetL2 * _k * Math.Exp((_k ^ 2) / 2)) / (1 - 2 * phi))
        _xi = L.GetL1 - ((_alpha / _k) * (1 - Math.Exp((_k ^ 2) / 2)))
        _samplesize = data.Count
    End Sub
    Public Overrides Function Clone() As ContinuousDistribution
        Dim result As New LLogNormal
        With result
            ._k = _k
            ._alpha = _alpha
            ._xi = _xi
            ._samplesize = _samplesize
            ._l1 = _l1
        End With
        Return result
    End Function
    Public Overrides Function Validate() As String
        Return Nothing 'can the mean be negative?
    End Function
    Private Function evaluateY(ByVal value As Double) As Double
        If _k <> 0 Then
            Return (-_k ^ -1) * Math.Log(1 - _k * (value - _xi) / _alpha)
        Else
            Return (value - _xi) / _alpha
        End If
    End Function
    Public Overrides Function GetCDF(value As Double) As Double
        Dim sn As New Normal
        Return sn.GetCDF(evaluateY(value))
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
        Dim y As Double = evaluateY(Value)
        Return (Math.Exp(((_k * y) - ((y ^ 2) / 2))) / (_alpha * Math.Sqrt(2 * Math.PI)))
    End Function

    Public Overrides ReadOnly Property GetSampleSize As Integer
        Get
            Return _sampleSize
        End Get
    End Property
End Class
