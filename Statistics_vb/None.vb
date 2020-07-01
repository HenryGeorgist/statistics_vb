Public Class None
    Inherits ContinuousDistribution
    Private _value As Double
    Sub New()
        _value = 0
    End Sub
    Sub New(ByVal value As Double)
        _value = value
    End Sub
    Sub New(ByVal data() As Double)
        SetParameters(data)
    End Sub
    Public Overrides Sub SetParameters(data() As Double)
        _value = Mean(data, data.Count - 1, 0)
    End Sub
    Public Overrides Function Validate() As String
        Return Nothing ' no validation known
    End Function

    Public Overrides Function GetPDF(Value As Double) As Double
        If Value = _value Then
            Return 1
        Else
            Return 0
        End If
    End Function
    Public Overrides Function GetCDF(value As Double) As Double
        If value > _value Then
            Return 0
        Else
            Return 1
        End If
    End Function
    Public Overrides Function getDistributedVariable(probability As Double) As Double
        Return _value
    End Function
    Public Overrides ReadOnly Property GetNumberOfParameters As Short
        Get
            Return 1
        End Get
    End Property
    Public Overrides ReadOnly Property GetSampleSize As Integer
        Get
            Return 1
        End Get
    End Property
    Public Overrides ReadOnly Property GetCentralTendency As Double
        Get
            Return _value
        End Get
    End Property
    Public Overrides Function Clone() As ContinuousDistribution
        Return New None(_value)
    End Function

    Public Overloads Shared Operator =(left As None, right As None) As Boolean
        ' Check for null arguments. Keep in mind null == null
        If left Is Nothing AndAlso right Is Nothing Then
            Return True
        ElseIf left Is Nothing Then
            Return False
        ElseIf right Is Nothing Then
            Return False
        End If

        Return left._value = right._value
    End Operator
    Public Overloads Shared Operator <>(left As None, right As None) As Boolean
        Return Not (left = right)
    End Operator
    Public Overrides Function Equals(obj As Object) As Boolean
        If TypeOf obj Is None Then
            Return Me = CType(obj, None)
        Else
            Return False
        End If
    End Function
    Public Overrides Function GetHashCode() As Integer
        Return Hash(_value)
    End Function
End Class
