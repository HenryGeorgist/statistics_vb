Public Class GenericCurveSingle
    Inherits PairedData
    Public Property X As List(Of Single)
    Public Property Y As List(Of Single)
    Sub New()
        X = New List(Of Single)
        Y = New List(Of Single)
    End Sub
    Sub New(ele As XElement)
        ReadFromXelement(ele)
    End Sub
    Sub New(ByVal xvalues As Single(), ByVal yvalues As Single())
        X = xvalues.ToList
        Y = yvalues.ToList
    End Sub
    Public Sub New(ByVal XValues As List(Of Single), ByVal YValues As List(Of Single))
        X = XValues.ToList
        Y = YValues.ToList
    End Sub
    Public Overrides Function Clone() As PairedData
        Return New GenericCurveSingle(X, Y)
    End Function
    Public Function ExportToTypedCurve(ByVal type As Type) As PairedData
        Select Case type
            Case GetType(MonotonicCurveIncreasing)
                Dim c As New MonotonicCurveIncreasing(_X.ToArray, _Y.ToArray)
                c.Verify()
                If c.Verify.Errors.Count > 1 Then Throw New Exception("Cannot be exported to type monotoniccurveincreasing")
                Return c
            Case GetType(MonotonicCurveDecreasing)
                Dim c As New MonotonicCurveDecreasing(_X.ToArray, _Y.ToArray)
                c.Verify()
                If c.Verify.Errors.Count > 1 Then Throw New Exception("Cannot be exported to type monotoniccurvedecreasing")
                Return c
            Case Else
                Throw New Exception("This data cannot be converted to " & type.GetType.Name)
        End Select
    End Function
    Public Overrides Function Verify() As ErrorReport
        Return New ErrorReport()
    End Function

    Public Overrides Sub ReadFromXelement(ele As XElement)
        Throw New NotImplementedException
    End Sub

    Public Overrides Function WriteToXElement() As XElement
        Throw New NotImplementedException
    End Function
End Class
