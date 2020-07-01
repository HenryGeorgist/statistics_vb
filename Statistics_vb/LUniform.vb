Public Class LUniform
    Inherits ContinuousDistribution
    Private _alpha As Double
    Private _beta As Double
    Private _sampleSize As Int32
    Sub New()

    End Sub
    Sub New(ByVal data() As Double)
        SetParameters(data)
    End Sub
    Sub New(ByVal min As Double, ByVal max As Double)
        _alpha = min
        _beta = max
        _sampleSize = 0
        Validate()
    End Sub
    Public Overrides Sub SetParameters(data() As Double)
        Dim L As New LinearMomentsStats(data)
        _alpha = L.GetL1 - (3 * L.GetL2)
        _beta = L.GetL1 + (3 * L.GetL2)
        _sampleSize = data.Count
        Validate()
    End Sub
    Public Overrides Function Clone() As ContinuousDistribution
        Dim ret As New LUniform
        ret._alpha = _alpha
        ret._beta = _beta
        ret._sampleSize = _sampleSize
        ret.Validate()
        Return ret
    End Function

    Public Overrides Function GetCDF(value As Double) As Double
        Return (value - _alpha) / (_beta - _alpha)
    End Function

    Public Overrides ReadOnly Property GetCentralTendency As Double
        Get
            Return 0.5 * (_alpha - _beta)
        End Get
    End Property
    Public Overrides Function Validate() As String
        If _alpha > _beta Then Return "Linear Moments Uniform distribution error: The min value is greater than the max value"
        Return Nothing
    End Function

    Public Overrides Function getDistributedVariable(probability As Double) As Double
        Return _alpha + (_beta - _alpha) * probability
    End Function

    Public Overrides ReadOnly Property GetNumberOfParameters As Short
        Get
            Return 2
        End Get
    End Property

    Public Overrides Function GetPDF(Value As Double) As Double
        Return 1 / (_beta - _alpha)
    End Function

    Public Overrides ReadOnly Property GetSampleSize As Integer
        Get
            Return _sampleSize
        End Get
    End Property
End Class
