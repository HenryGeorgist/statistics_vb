Public Class LTriangular
    Inherits ContinuousDistribution
    Private _mostlikely As Double 'the mean of the input data
    Private _sampleSize As Int32 'the number of records in the input data
    Private _min As Double
    Private _max As Double
    ''' <summary>
    ''' This constructor is not intended for use, it is available for reflection purposes only
    ''' </summary>
    ''' <remarks></remarks>
    Public Sub New()
        'an empty constructor for reflection
    End Sub
    Sub New(ByVal data() As Double)
        SetParameters(data)
    End Sub
    Public Overrides Sub SetParameters(data() As Double)
        Dim s As New LinearMomentsStats(data)
        'from Ben Chaon
        If s.GetL2 <= 0 Then
            _min = Double.NaN
            _mostlikely = Double.NaN
            _max = Double.NaN
        Else
            Dim sqrt3 As Double = Math.Sqrt(3)
            Dim a3 As Double
            Dim b3 As Double
            Dim angle As Double
            Dim aa As Double
            Dim bb As Double
            Dim qterm As Double = Math.Min(Math.Max(-7 * s.GetT3, -1), 1)
            Dim q2term As Double = qterm ^ 2
            Dim q4term As Double = q2term ^ 2
            a3 = qterm * (q2term + 999)
            b3 = Math.Sqrt(970299 - 968598 * q2term - 1701 * q4term)
            '//find a+ib = cubic root of a3+ib3
            angle = Math.Atan2(a3, b3)
            aa = Math.Cos(angle / 3.0)
            bb = Math.Sin(angle / 3.0)
            Dim p As Double = (9 - qterm + Math.Sqrt(q2term + 99) * (aa - sqrt3 * bb)) / 18
            Dim ss As Double = 15 * s.GetL2() / (p * p - p + 2.0)
            _min = (s.GetL1() - ss * (1 + p) / 3.0)
            _mostlikely = (s.GetL1() - ss * (1 - 2 * p) / 3)
            _max = (s.GetL1() + ss * (2 - p) / 3.0)
        End If

        _sampleSize = s.GetSampleSize
        Validate()
    End Sub
    Public Overrides Function Validate() As String
        If _min > _max Then Return "Linear Moments Triangular distribution error: The min value is greater than the max value"
        If _min > _mostlikely Then Return "Linear Moments Triangular distribution error: The min value is greater than the most likely value"
        If _max < _mostlikely Then Return "Linear Moments Triangular distribution error: The max value is less than the most likely value"
        Return Nothing
    End Function

    ''' <summary>
    ''' A contstuctor to directly define paramaterization of a triangular distribution
    ''' </summary>
    ''' <param name="min">the minimum value that can be returned</param>
    ''' <param name="max">the maximum value that can be returned</param>
    ''' <param name="mostlikely">the most likely value to be returned, usually this is the mode of a dataset</param>
    ''' <remarks></remarks>
    Sub New(ByVal min As Double, ByVal max As Double, ByVal mostlikely As Double)
        _sampleSize = 0
        _min = min
        _max = max
        _mostlikely = mostlikely
        Validate()
    End Sub
    Public Overrides ReadOnly Property GetCentralTendency As Double
        Get
            Return _mostlikely
        End Get
    End Property
    Public ReadOnly Property getMin As Double
        Get
            Return _min
        End Get
    End Property
    Public ReadOnly Property getMax As Double
        Get
            Return _max
        End Get
    End Property
    Public ReadOnly Property getMostlikely As Double
        Get
            Return _mostlikely
        End Get
    End Property
    Public Overrides ReadOnly Property GetSampleSize() As Int32
        Get
            Return _sampleSize
        End Get
    End Property
    ''' <summary>
    ''' returns a triangularly distributed variable based on the min max and most likely variables provided upon the objects creation
    ''' </summary>
    ''' <param name="probability">a random number between zero and 1 exclusive</param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Overrides Function getDistributedVariable(probability As Double) As Double
        Dim a As Double = _mostlikely - _min
        Dim b As Double = _max - _mostlikely
        If probability <= 0.0 Then
            Return _min
        ElseIf probability < (a) / (_max - _min) Then
            Return _min + Math.Sqrt(probability * (_max - _min) * (a))
        ElseIf probability < 1.0 Then
            Return _max - Math.Sqrt((1 - probability) * (_max - _min) * (b))
        Else
            Return _max
        End If
    End Function
    Public Overrides Function GetCDF(value As Double) As Double
        If value < _min Then
            Return 0
        ElseIf value >= _min And value <= _mostlikely Then
            Return (((value - _min) ^ 2) / ((_max - _min) * (_mostlikely - _min)))
        ElseIf value > _mostlikely And value <= _max Then
            Return (((_max - value) ^ 2) / ((_max - _min) * (_max - _mostlikely)))
        Else
            Return 1
        End If
    End Function
    Public Overrides Function GetPDF(Value As Double) As Double
        If Value < _min Then
            Return 0
        ElseIf Value >= _min And Value <= _mostlikely Then
            Return (2 * (Value - _min) / ((_max - _min) * (_mostlikely - _min)))
        ElseIf Value > _mostlikely And Value <= _max Then
            Return (2 * (_max - Value) / ((_max - _min) * (_max - _mostlikely)))
        Else
            Return 0
        End If
    End Function
    Public Overrides ReadOnly Property GetNumberOfParameters As Short
        Get
            Return 3
        End Get
    End Property
    Public Overrides Function Clone() As ContinuousDistribution
        Dim result As New LTriangular
        result._mostlikely = _mostlikely
        result._min = _min
        result._max = _max
        result._sampleSize = _sampleSize
        result.Validate()
        Return result
    End Function

    Public Overloads Shared Operator =(left As LTriangular, right As LTriangular) As Boolean
        ' Check for null arguments. Keep in mind null == null
        If left Is Nothing AndAlso right Is Nothing Then
            Return True
        ElseIf left Is Nothing Then
            Return False
        ElseIf right Is Nothing Then
            Return False
        End If

        If left._min = right._min AndAlso left._max = right._max AndAlso left._mostlikely = right._mostlikely Then
            Return True
        Else
            Return False
        End If
    End Operator
    Public Overloads Shared Operator <>(left As LTriangular, right As LTriangular) As Boolean
        Return Not (left = right)
    End Operator
    Public Overrides Function Equals(obj As Object) As Boolean
        If TypeOf obj Is LTriangular Then
            Return Me = CType(obj, LTriangular)
        Else
            Return False
        End If
    End Function
    Public Overrides Function GetHashCode() As Integer
        Return Hash(_min, _max, _mostlikely)
    End Function
End Class
