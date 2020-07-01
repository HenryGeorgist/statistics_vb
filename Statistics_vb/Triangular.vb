''' <summary>
''' The triangular distribution
''' </summary>
''' <remarks></remarks>
Public Class Triangular
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
        'Validate()
    End Sub
    Public Overrides Sub SetParameters(data() As Double)
        Dim s As New ProductMomentsStats(data)
        'from Ben Chaon
        Dim sqrt2 As Double = Math.Sqrt(2)
        Dim sqrt3 As Double = Math.Sqrt(3)
        Dim a3 As Double = -5 * s.GetSkew
        Dim b3 As Double
        Dim angle As Double
        Dim aa As Double
        Dim bb As Double
        If 8 - a3 * a3 < 0 Then
            a3 = Math.Sign(a3) * 2 * sqrt2
            b3 = 0
        Else
            b3 = Math.Sqrt(8 - a3 * a3)
        End If
        angle = Math.Atan2(b3, a3)
        aa = Math.Cos(angle / 3.0)
        bb = Math.Sin(angle / 3.0)
        _min = (s.GetMean() + sqrt2 * s.GetSampleStDev() * (aa - sqrt3 * bb))
        _mostlikely = (s.GetMean() - 2 * sqrt2 * s.GetSampleStDev() * (aa))
        _max = (s.GetMean() + sqrt2 * s.GetSampleStDev() * (aa + sqrt3 * bb))
        Validate()
        _sampleSize = CInt(s.GetSampleSize)
    End Sub
    Public Overrides Function validate() As String
        If _min > _max Then Return "Triangular distribution error: The min value is greater than the max value"
        If _min > _mostlikely Then Return "Triangular distribution error: The min value is greater than the most likely value"
        If _max < _mostlikely Then Return "Triangular distribution error: The max value is less than the most likely value"
        Return Nothing
    End Function
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
            Return 1 - (((_max - value) ^ 2) / ((_max - _min) * (_max - _mostlikely)))
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
        Dim result As New Triangular
        result._mostlikely = _mostlikely
        result._min = _min
        result._max = _max
        result._sampleSize = _sampleSize
        result.Validate()
        Return result
  End Function

  Public Overloads Shared Operator =(left As Triangular, right As Triangular) As Boolean
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
  Public Overloads Shared Operator <>(left As Triangular, right As Triangular) As Boolean
    Return Not (left = right)
  End Operator
  Public Overrides Function Equals(obj As Object) As Boolean
    If TypeOf obj Is Triangular Then
      Return Me = CType(obj, Triangular)
    Else
      Return False
    End If
  End Function
  Public Overrides Function GetHashCode() As Integer
    Return Hash(_min, _max, _mostlikely)
  End Function
End Class

