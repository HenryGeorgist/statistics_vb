Public Class TruncatedNormal
    Inherits Normal
    Private _minvalue As Double = 0
    Private _maxvalue As Double = 1
    Sub New()
        'empty for reflection
        MyBase.New()
    End Sub
    Sub New(data() As Double, ByVal min As Double, ByVal max As Double)
        MyBase.New(data)
        SetScaleFactors(min, max)
    End Sub
    ''' <summary>
    ''' creates a specified truncated normal
    ''' </summary>
    ''' <param name="mean"></param>
    ''' <param name="stdev"></param>
    ''' <param name="min">if nothing then we assume probability of 0</param>
    ''' <param name="max">if nothing then we assume probability of 1</param>
    ''' <remarks></remarks>
    Sub New(ByVal mean As Double, ByVal stdev As Double, Optional ByVal min As Double = Double.MinValue, Optional ByVal max As Double = Double.MaxValue)
        MyBase.New(mean, stdev)
        SetScaleFactors(min, max)
    End Sub
    Public Overrides Function Validate() As String
        Return MyBase.Validate() 'should i check if the min or max are on the same side of the mean, wouldnt that be a weird case?
    End Function
    Private Sub SetScaleFactors(ByVal min As Double, ByVal max As Double)
        Dim tmpmin As Double = MyBase.GetCDF(min)
        _minvalue = tmpmin
        Dim tmpmax As Double = MyBase.GetCDF(max)
        _maxvalue = tmpmax
    End Sub
    Public Overrides Function GetCDF(value As Double) As Double
        Throw New NotImplementedException()
        Return MyBase.GetCDF(value)
    End Function
    Public Overrides Function getDistributedVariable(probability As Double) As Double
        Dim p As Double = _minvalue + ((_maxvalue - _minvalue) * probability)
        Return MyBase.getDistributedVariable(p)
    End Function

    Public Overloads Shared Operator =(left As TruncatedNormal, right As TruncatedNormal) As Boolean
        ' Check for null arguments. Keep in mind null == null
        If left Is Nothing AndAlso right Is Nothing Then
            Return True
        ElseIf left Is Nothing Then
            Return False
        ElseIf right Is Nothing Then
            Return False
        End If

        If CType(left, Normal) = CType(right, Normal) AndAlso
          left._minvalue = right._minvalue AndAlso
          left._maxvalue = right._maxvalue Then
            Return True
        Else
            Return False
        End If
    End Operator
    Public Overloads Shared Operator <>(left As TruncatedNormal, right As TruncatedNormal) As Boolean
        Return Not (left = right)
    End Operator
    Public Overrides Function Equals(obj As Object) As Boolean
        If TypeOf obj Is TruncatedNormal Then
            Return Me = CType(obj, TruncatedNormal)
        Else
            Return False
        End If
    End Function
    Public Overrides Function GetHashCode() As Integer
        Return Hash(_minvalue, _maxvalue, MyBase.GetHashCode())
    End Function
End Class
