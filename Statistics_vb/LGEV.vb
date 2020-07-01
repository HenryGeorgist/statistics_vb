Public Class LGEV
    Inherits ContinuousDistribution
    Private _xi As Double
    Private _alpha As Double
    Private _k As Double
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
        _L1 = l.GetL1
        Dim L2 As Double = l.GetL2
        Dim t3 As Double = l.GetL3 / L2
        _samplesize = l.GetSampleSize
        'different formulae finding _k for positive and negative t3 - for very low t3 _k is refined through newton-raphson iteration
        If t3 <= 0 Then 'following works for -0.8 to 0
            _k = (0.2837753 + t3 * (-1.21096399 + t3 * (-2.50728214 + t3 * (-1.13455566 + t3 * -0.07138022)))) / (1 + t3 * (2.06189696 + t3 * (1.31912239 + t3 * 0.25077104)))
            If t3 < -0.8 Then 'use above _k as starting point for newton-raphson iteration to converge to answer
                If t3 <= -0.97 Then _k = 1 - Math.Log(1 + t3) / Math.Log(2) '...unless t3 is below -0.97 in which case start from this formula
                Dim t0 As Double = (t3 + 3) / 2
                For i = 1 To 20
                    Dim x2 As Double = 2 ^ (-_k)
                    Dim x3 As Double = 3 ^ (-_k)
                    Dim xx2 As Double = 1 - x2
                    Dim xx3 As Double = 1 - x3
                    Dim deriv As Double = (xx2 * x3 * Math.Log(3) - xx3 * x2 * Math.Log(2)) / (xx2 ^ 2)
                    Dim kold As Double = _k
                    _k = _k - (xx3 / xx2 - t0) / deriv
                    If Math.Abs(_k - kold) <= _k * 0.000001 Then i = 20
                Next
            Else
                'use the above k, without any newton-raphson
            End If
        Else 'positive t3 always uses the below k
            Dim z As Double = 1 - t3
            _k = (-1 + z * (1.59921491 + z * (-0.48832213 + z * 0.01573152))) / (1 + z * (-0.64363929 + z * 0.08985247))
        End If
        'calculate alpha and xi from k, or if k = 0 calculate them in a different way
        If Math.Abs(_k) < 0.000001 Then
            _k = 0
            _alpha = L2 / Math.Log(2)
            _xi = _L1 - _alpha * 0.57721566 'euler's constant
        Else
            Dim gam As Double = Math.Exp(gammaln(1 + _k))
            _alpha = L2 * _k / (gam * (1 - 2 ^ (-_k)))
            _xi = _L1 - _alpha * (1 - gam) / _k
        End If
        Validate()
    End Sub
    Public Overrides Function Validate() As String
        If _k = 0 Then Return "Linear Moments GEV distribution Error: Kappa parameter cannot equal zero"
        If _alpha = 0 Then Return "Linear Moments GEV Distribution Error: Alpha parameter canont equal zero."
        Return Nothing
    End Function

    Public Overrides Function getDistributedVariable(probability As Double) As Double
        If _k <> 0 Then
            Return _xi + ((_alpha / _k) * ((1 - (-Math.Log(probability)) ^ _k)))
        Else
            Return _xi - _alpha * Math.Log(-Math.Log(probability))
        End If
    End Function
    Public Overrides Function GetCDF(value As Double) As Double
        Return Math.Exp(-Math.Exp(-evaluateY(value)))
    End Function
    Public Overrides Function GetPDF(Value As Double) As Double
        Dim y As Double = evaluateY(Value)
        Return (_alpha ^ -1) * Math.Exp(-(1 - _k) * (y - Math.Exp(-y)))
    End Function
    Private Function evaluateY(ByVal value As Double) As Double
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
    Public ReadOnly Property GetK As Double
        Get
            Return _k
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
    Public Overrides Function Clone() As ContinuousDistribution
        Dim result As New LGEV
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


    Public Overloads Shared Operator =(left As LGEV, right As LGEV) As Boolean
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
    Public Overloads Shared Operator <>(left As LGEV, right As LGEV) As Boolean
        Return Not (left = right)
    End Operator
    Public Overrides Function Equals(obj As Object) As Boolean
        If TypeOf obj Is LGEV Then
            Return Me = CType(obj, LGEV)
        Else
            Return False
        End If
    End Function
    Public Overrides Function GetHashCode() As Integer
        Return Hash(_alpha, _xi, _k, _L1, _sampleSize)
    End Function
End Class
