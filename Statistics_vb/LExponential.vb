Public Class LExponential
    Inherits ContinuousDistribution
    Private _alpha As Double
    Private _xi As Double
    Private _L1 As Double
    Private _L2 As Double
    Private _sampleSize As Int32
    Sub New()
        'empty for reflection
    End Sub
    Sub New(data() As Double)
        SetParameters(data)
    End Sub
    Public Overrides Function GetCDF(value As Double) As Double
        Return 1 - Math.Exp(-(value - _xi) / _alpha)
    End Function
    Public Overrides Function GetPDF(Value As Double) As Double
        Return (_alpha ^ -1) * Math.Exp(-(Value - _xi) / _alpha)
    End Function
    Public Overrides Function getDistributedVariable(probability As Double) As Double
        Return _xi - _alpha * Math.Log(1 - probability)
    End Function
    Public Overrides Function Validate() As String
        If _alpha = 0 Then Return "Linear Moments Exponential Distribution Error: Alpha parameter canont equal zero."
        Return Nothing
    End Function

    Public Overrides ReadOnly Property GetNumberOfParameters As Short
        Get
            Return 2
        End Get
    End Property
    Public Overrides ReadOnly Property GetCentralTendency As Double
        Get
            Return _L1
        End Get
    End Property
    Public ReadOnly Property GetL2 As Double
        Get
            Return _L2
        End Get
    End Property
    Public Overrides ReadOnly Property GetSampleSize As Int32
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
    Public Overrides Sub SetParameters(data() As Double)
        Dim l As New LinearMomentsStats(data)
        _L1 = l.GetL1
        _L2 = l.GetL2
        _sampleSize = l.GetSampleSize
        _alpha = 2 * _L2
        _xi = _L1 - _alpha
        Validate()
    End Sub
    Public Overrides Function Clone() As ContinuousDistribution
        Dim result As New LExponential
        With result
            ._alpha = _alpha
            ._xi = _xi
            ._L1 = _L1
            ._sampleSize = _sampleSize
            .Validate()
        End With
        Return result
    End Function

    Public Overloads Shared Operator =(left As LExponential, right As LExponential) As Boolean
        ' Check for null arguments. Keep in mind null == null
        If left Is Nothing AndAlso right Is Nothing Then
            Return True
        ElseIf left Is Nothing Then
            Return False
        ElseIf right Is Nothing Then
            Return False
        End If

        If left._alpha = right._alpha AndAlso left._xi = right._xi AndAlso left._L1 = right._L1 AndAlso left._L2 = right._L2 AndAlso left._sampleSize = right._sampleSize Then
            Return True
        Else
            Return False
        End If
    End Operator
    Public Overloads Shared Operator <>(left As LExponential, right As LExponential) As Boolean
        Return Not (left = right)
    End Operator
    Public Overrides Function Equals(obj As Object) As Boolean
        If TypeOf obj Is LExponential Then
            Return Me = CType(obj, LExponential)
        Else
            Return False
        End If
    End Function
    Public Overrides Function GetHashCode() As Integer
        Return Hash(_alpha, _xi, _L1, _L2, _sampleSize)
    End Function
End Class
