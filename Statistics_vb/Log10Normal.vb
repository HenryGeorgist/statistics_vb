Public Class Log10Normal
    Inherits ContinuousDistribution
    Private _mean As Double 'the mean of the input data
    Private _StDev As Double 'the standard deviation of the input data
    Private _sampleSize As Int32 'the number of records in the input data
    ''' <summary>
    ''' the log normal distribution converts the data to log base 10 before fitting a normal distribution, do not include values that are negative.
    ''' </summary>
    ''' <param name="data">original data, which will be transformed to log base 10</param>
    ''' <remarks></remarks>
    Sub New(ByVal data As Double())
        SetParameters(data)
    End Sub
    ''' <summary>
    ''' This constructor is not intended for use, it is available for reflection purposes only
    ''' </summary>
    ''' <remarks></remarks>
    Public Sub New()
        'an empty constructor for reflection
    End Sub
    Sub New(ByVal mean As Double, ByVal stdev As Double)
        _sampleSize = 0
        _mean = mean
        _StDev = stdev
    End Sub
    Public Overrides Sub SetParameters(data() As Double)
        For i = 0 To data.Count - 1
            data(i) = Math.Log(data(i), 10)
        Next
        Dim s As New BasicProductMomentsStats(data)
        _sampleSize = CInt(s.GetSampleSize)
        _mean = s.GetMean
        _StDev = s.GetSampleStDev
    End Sub
    Public Overrides Function Validate() As String
        Return Nothing    'can the mean be negative?
    End Function

    ''' <summary>
    ''' Gets a variable distributed based on the log normal distribution
    ''' </summary>
    ''' <param name="probability">a value between zero and 1 exclusive</param>
    ''' <returns>a log base 10 variate.  (convert it back to linear space!)</returns>
    ''' <remarks></remarks>
    Public Overrides Function getDistributedVariable(probability As Double) As Double
        Dim z As New Normal()
        Return 10 ^ (_mean + ((_StDev) * (z.getDistributedVariable(probability))))
    End Function
    Public Overrides Function GetCDF(value As Double) As Double
        Dim z As New Normal(_mean, _StDev)
        Return z.GetCDF(Math.Log(value, 10))
    End Function
    Public Overrides Function GetPDF(Value As Double) As Double
        Dim z As New Normal(_mean, _StDev)
        Return z.GetPDF(Math.Log(Value, 10))
    End Function
    Public Overrides ReadOnly Property GetNumberOfParameters As Short
        Get
            Return 2
        End Get
    End Property
    Public ReadOnly Property GetStDev() As Double
        Get
            Return _StDev
        End Get
    End Property
    Public ReadOnly Property GetMean() As Double
        Get
            Return _mean
        End Get
    End Property
    Public Overrides ReadOnly Property GetCentralTendency As Double
        Get
            Return _mean
        End Get
    End Property
    Public Overrides ReadOnly Property GetSampleSize() As Int32
        Get
            Return _sampleSize
        End Get
    End Property
    Public Overrides Function Clone() As ContinuousDistribution
        Dim result As New Log10Normal
        result._mean = _mean
        result._StDev = _StDev
        result._sampleSize = _sampleSize
        Return result
    End Function

    ' NOTE: Not comparing _sampleSize
    Public Overloads Shared Operator =(left As Log10Normal, right As Log10Normal) As Boolean
        ' Check for null arguments. Keep in mind null == null
        If left Is Nothing AndAlso right Is Nothing Then
            Return True
        ElseIf left Is Nothing Then
            Return False
        ElseIf right Is Nothing Then
            Return False
        End If

        If left._mean = right._mean AndAlso left._StDev = right._StDev Then
            Return True
        Else
            Return False
        End If
    End Operator
    Public Overloads Shared Operator <>(left As Log10Normal, right As Log10Normal) As Boolean
        Return Not (left = right)
    End Operator
    Public Overrides Function Equals(obj As Object) As Boolean
        If TypeOf obj Is LogNormal Then
            Return Me = CType(obj, LogNormal)
        Else
            Return False
        End If
    End Function
    Public Overrides Function GetHashCode() As Integer
        Return Hash(_mean, _StDev)
    End Function
End Class
