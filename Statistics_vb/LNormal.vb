Public Class LNormal
    Inherits ContinuousDistribution
    Private _mu As Double
    Private _sigma As Double
    Private _L1 As Double
    Private _L2 As Double
    Private _sampleSize As Integer
    Sub New() '
        'empty for reflection
    End Sub
    Sub New(ByVal Data() As Double)
        SetParameters(Data)
    End Sub
    Public Overrides Sub SetParameters(data() As Double)
        Dim L As New LinearMomentsStats(data)
        _samplesize = data.Count
        _L1 = L.GetL1
        _L2 = L.GetL2
        _mu = _L1
        _sigma = ((Math.PI) ^ 0.5) * _L2
        Validate()
    End Sub
    Public Overrides Function Clone() As ContinuousDistribution
        Dim result As New LNormal
        With result
            ._mu = _mu
            ._sigma = _sigma
            ._samplesize = _samplesize
            ._L1 = _L1
            ._L2 = ._L2
        End With
        Return result
    End Function
    Public Overrides Function Validate() As String
        If _sigma < 0 Then Return "Linear Moments Normal Distribution Error: The Standard Deviation was less than 0"
        Return Nothing
    End Function

    Public Overrides Function GetCDF(value As Double) As Double
        Throw New NotImplementedException
    End Function
    Public Overrides ReadOnly Property GetCentralTendency As Double
        Get
            Return _mu
        End Get
    End Property
    Public Overrides Function getDistributedVariable(probability As Double) As Double
        Throw New NotImplementedException
    End Function

    Public Overrides ReadOnly Property GetNumberOfParameters As Short
        Get
            Return 2
        End Get
    End Property

    Public Overrides Function GetPDF(Value As Double) As Double
        Return (_sigma ^ -1) * (Math.Sqrt(2 * Math.PI) * Math.Exp(-0.5 * (((Value - _mu) / _sigma) ^ 2)))
    End Function

    Public Overrides ReadOnly Property GetSampleSize As Integer
        Get
            Return _sampleSize
        End Get
    End Property
End Class
