Public Class TruncatedLogNormal
    Inherits LogNormal
    Private _minvalue As Double
    Private _maxvalue As Double
    Sub New()
        'empty for reflection
        MyBase.New()
    End Sub
    Sub New(data() As Double, ByVal min As Double, ByVal max As Double)
        MyBase.New(data)
        SetScaleFactors(min, max)
    End Sub
    Sub New(ByVal mean As Double, ByVal stdev As Double, ByVal min As Double, ByVal max As Double)
        MyBase.New(mean, stdev)
        SetScaleFactors(min, max)
    End Sub
    Private Sub SetScaleFactors(ByVal min As Double, ByVal max As Double)
        _minvalue = MyBase.GetCDF(min)
        _maxvalue = MyBase.GetCDF(max)
    End Sub
    Public Overrides Function GetCDF(value As Double) As Double
        Throw New NotImplementedException()
        Return MyBase.GetCDF(value)
    End Function
    Public Overrides Function getDistributedVariable(probability As Double) As Double
        Dim p As Double = _minvalue + ((_maxvalue - _minvalue) * probability)
        Return MyBase.getDistributedVariable(p)
  End Function
    Public Overrides Function Validate() As String
        Return MyBase.Validate() 'max and min greater than mean? mean less than zero?
    End Function
    Public Overloads Shared Operator =(left As TruncatedLogNormal, right As TruncatedLogNormal) As Boolean
        ' Check for null arguments. Keep in mind null == null
        If left Is Nothing AndAlso right Is Nothing Then
            Return True
        ElseIf left Is Nothing Then
            Return False
        ElseIf right Is Nothing Then
            Return False
        End If

        If CType(left, LogNormal) = CType(right, LogNormal) AndAlso
          left._minvalue = right._minvalue AndAlso
          left._maxvalue = right._maxvalue Then
            Return True
        Else
            Return False
        End If
    End Operator
    Public Overloads Shared Operator <>(left As TruncatedLogNormal, right As TruncatedLogNormal) As Boolean
        Return Not (left = right)
    End Operator
    Public Overrides Function Equals(obj As Object) As Boolean
        If TypeOf obj Is TruncatedLogNormal Then
            Return Me = CType(obj, TruncatedLogNormal)
        Else
            Return False
        End If
    End Function
    Public Overrides Function GetHashCode() As Integer
        Return Hash(_minvalue, _maxvalue, MyBase.GetHashCode())
    End Function
End Class
