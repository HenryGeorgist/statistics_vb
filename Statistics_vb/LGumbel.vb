Public Class LGumbel
    Inherits ContinuousDistribution
    Private _Alpha As Double 'scale
    Private _Xi As Double 'location
    Private _L1 As Double
    Private _L2 As Double
    Private _sampleSize As Int32
    Sub New()
        'reflection
    End Sub
    ''' <summary>
    ''' Takes data as an input and computes the lmoments, and describes fit parameters based on the data
    ''' </summary>
    ''' <param name="data"></param>
    ''' <remarks></remarks>
    Sub New(ByVal data() As Double)
        SetParameters(data)
    End Sub
    Public Overrides Sub SetParameters(data() As Double)
        Dim L As New LinearMomentsStats(data)
        _samplesize = L.GetSampleSize
        _L1 = L.GetL1
        _L2 = L.GetL2
        _Alpha = _L2 / Math.Log(2)
        _Xi = _L1 - 0.57721566490153287 * _Alpha
        Validate()
    End Sub
    ''' <summary>
    ''' returns a Gumbel distribution using the L moments methodology to define alhpa and xi
    ''' </summary>
    ''' <param name="probability"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Overrides Function getDistributedVariable(probability As Double) As Double
        Return _Xi - _Alpha * Math.Log(-Math.Log(probability))
    End Function
    Public Overrides Function Validate() As String
        If _Alpha = 0 Then Return "Linear Moments Gumbel Distribution Error: Alpha parameter canont equal zero."
        Return Nothing
    End Function

    Public Overrides Function GetCDF(value As Double) As Double
        Return Math.Exp(-Math.Exp(-(value - _Xi) / _Alpha))
    End Function
    Public Overrides Function GetPDF(Value As Double) As Double
        Return (_Alpha ^ -1) * Math.Exp(-(Value - _Xi) / _Alpha) * Math.Exp(-Math.Exp(-(Value - _Xi) / _Alpha))
    End Function
    Public Overrides ReadOnly Property GetNumberOfParameters As Short
        Get
            Return 2
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
    Public ReadOnly Property GetAlpha As Double
        Get
            Return _Alpha
        End Get
    End Property
    Public ReadOnly Property GetXi As Double
        Get
            Return _Xi
        End Get
    End Property
    Public Overrides Function Clone() As ContinuousDistribution
        Dim result As New LGumbel
        With result
            ._Alpha = _Alpha
            ._Xi = _Xi
            ._L1 = _L1
            ._sampleSize = _sampleSize
            .Validate()
        End With
        Return result
    End Function

    Public Overloads Shared Operator =(left As LGumbel, right As LGumbel) As Boolean
        ' Check for null arguments. Keep in mind null == null
        If left Is Nothing AndAlso right Is Nothing Then
            Return True
        ElseIf left Is Nothing Then
            Return False
        ElseIf right Is Nothing Then
            Return False
        End If

        If left._Alpha = right._Alpha AndAlso left._Xi = right._Xi AndAlso left._L1 = right._L1 AndAlso left._sampleSize = right._sampleSize Then
            Return True
        Else
            Return False
        End If
    End Operator
    Public Overloads Shared Operator <>(left As LGumbel, right As LGumbel) As Boolean
        Return Not (left = right)
    End Operator
    Public Overrides Function Equals(obj As Object) As Boolean
        If TypeOf obj Is LGumbel Then
            Return Me = CType(obj, LGumbel)
        Else
            Return False
        End If
    End Function
    Public Overrides Function GetHashCode() As Integer
        Return Hash(_Alpha, _Xi, _L1, _sampleSize)
    End Function
End Class
