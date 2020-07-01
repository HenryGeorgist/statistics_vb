''' <summary>
''' The Gumbel Distribution is an extreme value distribution which uses mu and beta (shape and scale) to describe the transform of uniformly distributed random numbers.
''' </summary>
''' <remarks></remarks>
Public Class Gumbel
    Inherits ContinuousDistribution
    Private _mean As Double 'the mean of the input data
    Private _StDev As Double 'the standard deviation of the input data
    Private _sampleSize As Int32 'the number of records in the input data
    Private _mu As Double
    Private _beta As Double
    ''' <summary>
    ''' The basic constructor for a Gumbel Distribution which utilizes data as an input
    ''' </summary>
    ''' <param name="data">input of data that will be fitted using method of moments to determine mean, stdev, mu, and beta</param>
    ''' <remarks></remarks>
    Sub New(ByRef data As Double())
        SetParameters(data)
    End Sub
    Sub New(ByVal mean As Double, ByVal stdev As Double, ByVal mu As Double, ByVal beta As Double)
        _sampleSize = 0
        _mean = mean
        _StDev = stdev
        _mu = mu
        _beta = beta
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
        If _StDev = 0 Then Return "Gumbel Distribution Error: Stdev value was equal to zero."
        If _beta = 0 Then Return "Gumbel Distribution Error: Beta value was equal to zero."
        Return Nothing
    End Function

    Public ReadOnly Property Getmu As Double
        Get
            Return _mu
        End Get
    End Property
    Public ReadOnly Property Getbeta As Double
        Get
            Return _beta
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
    Public Overrides Sub SetParameters(data() As Double)
        Dim s As New BasicProductMomentsStats(data)
        _sampleSize = CInt(s.GetSampleSize)
        _mean = s.GetMean
        _StDev = s.GetSampleStDev
        _beta = Math.PI / (_StDev * Math.Sqrt(6))
        _mu = _mean - _beta * 0.57721566490153287
        Validate()
    End Sub
    ''' <summary>
    ''' Returns a Gumbel distribted variable.
    ''' </summary>
    ''' <param name="probability">a random number between zero and 1 exclusive</param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Overrides Function getDistributedVariable(ByVal probability As Double) As Double
        Return (_mu - (_beta * (Math.Log(-Math.Log(probability)))))
    End Function
    Public Overrides Function GetCDF(value As Double) As Double
        Return Math.Exp(-Math.Exp(-(value - _mu) / _beta))
    End Function
    Public Overrides Function GetPDF(Value As Double) As Double
        Dim z As Double = (Value - _mu) / _beta
        Return (1 / _beta) * Math.Exp(-(z + Math.Exp(-z)))
    End Function
    Public Overrides ReadOnly Property GetNumberOfParameters As Short
        Get
            Return 2
        End Get
    End Property
    Public Overrides Function Clone() As ContinuousDistribution
        Dim result As New Gumbel
        result._mean = _mean
        result._StDev = _StDev
        result._mu = _mu
        result._beta = _beta
        result._sampleSize = _sampleSize
        Return result
    End Function

    Public Overloads Shared Operator =(left As Gumbel, right As Gumbel) As Boolean
        ' Check for null arguments. Keep in mind null == null
        If left Is Nothing AndAlso right Is Nothing Then
            Return True
        ElseIf left Is Nothing Then
            Return False
        ElseIf right Is Nothing Then
            Return False
        End If

        If left._mean = right._mean AndAlso left._StDev = right._StDev AndAlso
          left._mu = right._mu AndAlso left._beta = right._beta Then
            Return True
        Else
            Return False
        End If
    End Operator
    Public Overloads Shared Operator <>(left As Gumbel, right As Gumbel) As Boolean
        Return Not (left = right)
    End Operator
    Public Overrides Function Equals(obj As Object) As Boolean
        If TypeOf obj Is Gumbel Then
            Return Me = CType(obj, Gumbel)
        Else
            Return False
        End If
    End Function
    Public Overrides Function GetHashCode() As Integer
        Return Hash(_mean, _StDev, _mu, _beta)
    End Function
End Class
