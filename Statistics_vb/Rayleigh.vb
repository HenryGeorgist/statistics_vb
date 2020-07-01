Public Class Rayleigh
    Inherits ContinuousDistribution
    Private _Sigma As Double
    Private _sampleSize As Int32
    Sub New()

    End Sub
    Sub New(ByVal Sigma As Double)
        _Sigma = Sigma
        Validate()
        _samplesize = 1
    End Sub
    Sub New(ByVal data As Double())
        SetParameters(data)
    End Sub
    Public Overrides Sub SetParameters(data() As Double)
        Dim s As New BasicProductMomentsStats(data)
        _Sigma = s.GetSampleStDev
        Validate()
        _samplesize = CInt(s.GetSampleSize)
    End Sub
    Public Overrides Function Validate() As String
        If _Sigma <= 0 Then Return "Rayleigh Distribution Error: Sigma value was less than or equal to zero."
        Return Nothing
    End Function

    Public Overrides ReadOnly Property GetCentralTendency As Double
        Get
            Return _Sigma * Math.Sqrt(Math.PI / 2)
        End Get
    End Property
    Public Overrides ReadOnly Property GetNumberOfParameters As Short
        Get
            Return 1
        End Get
    End Property
    Public Overrides ReadOnly Property GetSampleSize As Integer
        Get
            Return _sampleSize
        End Get
    End Property
    Public Overrides Function Clone() As ContinuousDistribution
        Return New Rayleigh(Me._Sigma)
    End Function
    Public Overrides Function GetCDF(value As Double) As Double
        Return 1 - (Math.Exp(-(value ^ 2) / (2 * (_Sigma ^ 2))))
    End Function
    Public Overrides Function getDistributedVariable(probability As Double) As Double
        Return _Sigma * Math.Sqrt(-2 * Math.Log(probability))
    End Function
    Public Overrides Function GetPDF(Value As Double) As Double
        Return (Value / (_Sigma ^ 2)) * Math.Exp(-(Value ^ 2) / (2 * (_Sigma ^ 2)))
    End Function
End Class
