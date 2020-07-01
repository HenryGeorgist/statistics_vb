Public Class LPareto
    Inherits ContinuousDistribution
    Private _k As Double
    Private _alpha As Double
    Private _xi As Double
    Private _L1 As Double
    Private _sampleSize As Int32
    Sub New()
        'reflection
    End Sub
    Sub New(ByVal data() As Double)
        SetParameters(data)
    End Sub
    Public Overrides Sub SetParameters(data() As Double)
        Dim l As New LinearMomentsStats(data)
        _samplesize = l.GetSampleSize
        _L1 = l.GetL1
        If l.GetL2 = 0 Then Throw New ContinuousDistributionException("Linear Moments Pareto Distribution Fitting Error: L2 cannot be equal to zero.")
        Dim t3 As Double = l.GetL3 / l.GetL2
        _k = (1 - 3 * t3) / (1 + t3)
        _alpha = (1 + _k) * (2 + _k) * l.GetL2
        _xi = l.GetL1 - (2 + _k) * l.GetL2
        Validate()
    End Sub
    Public Overrides Function Validate() As String
        If _k = 0 Then Return "Linear Moments Pareto Distribution Error: Kappa cannot be equal to zero."
        If _alpha = 0 Then Return "Linear Moments Pareto Distribution Error: Alpha cannot be equal to zero."
        Return Nothing
    End Function

    Public Overrides Function getDistributedVariable(probability As Double) As Double
        If _k <> 0 Then
            Return _xi + (_alpha * (1 - (1 - probability) ^ _k) / _k)
        Else
            Return _xi - _alpha * Math.Log(1 - probability)
        End If
    End Function
    Public Overrides Function GetCDF(value As Double) As Double
        Return 1 - Math.Exp(-y(value))
    End Function
    Public Overrides Function GetPDF(Value As Double) As Double
        Return (_alpha ^ -1) * Math.Exp(-(1 - _k) * y(Value))
    End Function
    Private Function y(ByVal value As Double) As Double
        If _k <> 0 Then
            Return (-_k ^ -1) * Math.Log(1 - _k * (value - _xi) / _alpha)
        Else
            Return (value - _xi) / _alpha
        End If
    End Function
    Public Overrides ReadOnly Property GetNumberOfParameters As Short
        Get
            Return 3
        End Get
    End Property
    Public Overrides ReadOnly Property GetCentralTendency As Double
        Get
            Return _L1
        End Get
    End Property
    Public Overrides ReadOnly Property GetSampleSize As Integer
        Get
            Return _sampleSize
        End Get
    End Property
    Public ReadOnly Property GetAlpha As Double
        Get
            Return _alpha
        End Get
    End Property
    Public ReadOnly Property GetXi As Double
        Get
            Return _xi
        End Get
    End Property
    Public ReadOnly Property GetK As Double
        Get
            Return _k
        End Get
    End Property
    Public Overrides Function Clone() As ContinuousDistribution
        Dim result As New LPareto
        With result
            ._alpha = _alpha
            ._k = _k
            ._xi = _xi
            ._L1 = _L1
            ._sampleSize = _sampleSize
            .Validate()
        End With
        Return result
    End Function

    Public Overloads Shared Operator =(left As LPareto, right As LPareto) As Boolean
        ' Check for null arguments. Keep in mind null == null
        If left Is Nothing AndAlso right Is Nothing Then
            Return True
        ElseIf left Is Nothing Then
            Return False
        ElseIf right Is Nothing Then
            Return False
        End If

        If left._alpha = right._alpha AndAlso left._xi = right._xi AndAlso left._L1 = right._L1 AndAlso left._k = right._k AndAlso left._sampleSize = right._sampleSize Then
            Return True
        Else
            Return False
        End If
    End Operator
    Public Overloads Shared Operator <>(left As LPareto, right As LPareto) As Boolean
        Return Not (left = right)
    End Operator
    Public Overrides Function Equals(obj As Object) As Boolean
        If TypeOf obj Is LPareto Then
            Return Me = CType(obj, LPareto)
        Else
            Return False
        End If
    End Function
    Public Overrides Function GetHashCode() As Integer
        Return Hash(_alpha, _xi, _k, _L1, _sampleSize)
    End Function
End Class
