''' <summary>
''' The exponential distribution is another extreme value distribution.
''' </summary>
''' <remarks></remarks> 
Public Class Exponential
    Inherits ContinuousDistribution
    Private _mean As Double 'the mean of the input data
    Private _StDev As Double 'the standard deviation of the input data
    Private _sampleSize As Int32 'the number of records in the input data
    Private _Lambda As Double
    ''' <summary>
    ''' a constructor that allows the user to create an exponential from a series of data
    ''' </summary>
    ''' <param name="data">data you wish to convert into an exponential distribution using method of moments</param>
    ''' <remarks></remarks>
    Sub New(ByRef data As Double())
        SetParameters(data)
    End Sub
    Public Overrides Sub SetParameters(data() As Double)
        Dim s As New BasicProductMomentsStats(data)
        _sampleSize = CInt(s.GetSampleSize)
        _mean = s.GetMean
        _Lambda = 1 / _mean
        _StDev = s.GetMean ' stdev = mean in the exponential... 
        Validate()
    End Sub
    ''' <summary>
    ''' a constructor to create an exponential distribution from its summary moments
    ''' </summary>
    ''' <param name="mean">the first moment</param>
    ''' <param name="stdev">the second moment</param>
    ''' <remarks></remarks>
    Sub New(ByVal mean As Double, ByVal stdev As Double)
        _sampleSize = 0
        _mean = mean
        _StDev = stdev
        _Lambda = 1 / _mean
        Validate()
    End Sub
    ''' <summary>
    ''' This constructor is not intended for use, it is available for reflection purposes only
    ''' </summary>
    ''' <remarks></remarks>
    Public Sub New()
        'an empty constructor for reflection
    End Sub
    Public Overrides Function Validate() As String
        If _Lambda = 0 Then Return "Exponential Distribution Error: Lambda cannot be equal to zero"
        Return Nothing
    End Function

    ''' <summary>
    ''' this function returns an exponentially distributed random variate
    ''' </summary>
    ''' <param name="probability">a random number between zero and 1 exclusive</param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Overrides Function getDistributedVariable(ByVal probability As Double) As Double
        Return (-Math.Log(probability) / _Lambda)
    End Function
    Public Overrides Function GetPDF(Value As Double) As Double
        If Value < 0 Then
            Return 0
        Else
            Return _Lambda * (Math.Exp(-_Lambda * Value))
        End If
    End Function
    Public Overrides Function GetCDF(value As Double) As Double
        Return 1 - Math.Exp(-_Lambda * value)
    End Function
    ''' <summary>
    ''' for an exponential distribution there is only one descriptive variable, the value Lambda
    ''' </summary>
    ''' <value></value>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Overrides ReadOnly Property GetNumberOfParameters As Short
        Get
            Return 1
        End Get
    End Property
    Public Overrides ReadOnly Property GetCentralTendency As Double
        Get
            Return _mean
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
    Public Overrides ReadOnly Property GetSampleSize() As Int32
        Get
            Return _sampleSize
        End Get
    End Property
    Public Overrides Function Clone() As ContinuousDistribution
        Dim result As New Exponential
        result._mean = _mean
        result._StDev = _StDev
        result._Lambda = _Lambda
        result._sampleSize = _sampleSize
        result.Validate()
        Return result
    End Function

    Public Overloads Shared Operator =(left As Exponential, right As Exponential) As Boolean
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
    Public Overloads Shared Operator <>(left As Exponential, right As Exponential) As Boolean
        Return Not (left = right)
    End Operator
    Public Overrides Function Equals(obj As Object) As Boolean
        If TypeOf obj Is Exponential Then
            Return Me = CType(obj, Exponential)
        Else
            Return False
        End If
    End Function
    Public Overrides Function GetHashCode() As Integer
        Return Hash(_mean, _StDev)
    End Function
End Class
