Public Class Uniform
    Inherits ContinuousDistribution
    Private _sampleSize As Int32
    Private _median As Double
    Private _min As Double
    Private _max As Double
    Sub New()
        'reflection
    End Sub
    Sub New(ByVal data() As Double)
        SetParameters(data)
    End Sub
    Sub New(ByVal min As Double, ByVal max As Double)
        _samplesize = 0
        _max = max
        _min = min
        'Validate()
        _median = (_max + _min) / 2
    End Sub
    Public Overrides Sub SetParameters(data() As Double)
        Dim p As New BasicProductMomentsStats(data)
        _min = p.GetMean - Math.Sqrt(3 * p.GetSampleVariance)
        _max = p.GetMean + Math.Sqrt(3 * p.GetSampleVariance)
        _sampleSize = data.Count
        Validate()
        _median = (_max + _min) / 2
    End Sub
    Public Overrides Function Validate() As String
        If _min > _max Then Return "Uniform distribution error: The min value is greater than the max value"
        Return Nothing
    End Function

    Public ReadOnly Property GetMin As Double
        Get
            Return _min
        End Get
    End Property
    Public ReadOnly Property GetMax As Double
        Get
            Return _max
        End Get
    End Property
    Public Overrides ReadOnly Property GetCentralTendency As Double
        Get
            Return _median
        End Get
    End Property
    Public Overrides Function GetCDF(value As Double) As Double
        If value < _min Then
            Return 0
        ElseIf value <= _max Then
            Return (value - _min) / (_max - _min) '_min - _max)
        Else
            Return 1
        End If
    End Function
    Public Overrides Function GetPDF(Value As Double) As Double
        If Value < _min Then
            Return 0
        ElseIf Value <= _max Then
            Return 1 / (_max - _min) '_min - _max)
        Else
            Return 0
        End If
    End Function
    Public Overrides Function getDistributedVariable(probability As Double) As Double
        Return _min + ((_max - _min) * probability)
    End Function
    Public Overrides ReadOnly Property GetNumberOfParameters As Short
        Get
            Return 2
        End Get
    End Property
    Public Overrides ReadOnly Property GetSampleSize As Integer
        Get
            Return _sampleSize
        End Get
    End Property
    Public Overrides Function Clone() As ContinuousDistribution
        Dim result As New Uniform
        result._median = _median
        result._min = _min
        result._max = _max
        result._sampleSize = _sampleSize
        Return result
    End Function

    Public Overloads Shared Operator =(left As Uniform, right As Uniform) As Boolean
        ' Check for null arguments. Keep in mind null == null
        If left Is Nothing AndAlso right Is Nothing Then
            Return True
        ElseIf left Is Nothing Then
            Return False
        ElseIf right Is Nothing Then
            Return False
        End If

        If left._min = right._min AndAlso left._max = right._max Then
            Return True
        Else
            Return False
        End If
    End Operator
    Public Overloads Shared Operator <>(left As Uniform, right As Uniform) As Boolean
        Return Not (left = right)
    End Operator
    Public Overrides Function Equals(obj As Object) As Boolean
        If TypeOf obj Is Uniform Then
            Return Me = CType(obj, Uniform)
        Else
            Return False
        End If
    End Function
    Public Overrides Function GetHashCode() As Integer
        Return Hash(_min, _max)
    End Function
End Class
