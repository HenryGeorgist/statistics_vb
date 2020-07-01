Public Class GenericCurveSingleUncertain
    Inherits PairedData
    Protected _X As List(Of Single)
    Protected _Y As List(Of ContinuousDistribution)
    ''' <summary>
    ''' provides access to the x values which are represented as an array of single
    ''' </summary>
    ''' <value></value>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Property X As List(Of Single)
        Get
            Return _X
        End Get
        Set(value As List(Of Single))
            _X = value
        End Set
    End Property
    ''' <summary>
    ''' provides access to the y values which are represented as an array of continuous distributions
    ''' </summary>
    ''' <value></value>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Property Y As List(Of ContinuousDistribution)
        Get
            Return _Y
        End Get
        Set(value As List(Of ContinuousDistribution))
            _Y = value
        End Set
    End Property
    Sub New()
        _X = New List(Of Single)
        _Y = New List(Of ContinuousDistribution)
    End Sub
    Sub New(ByVal xvalues As List(Of Single), ByVal yvalues As List(Of ContinuousDistribution))
        _X = xvalues
        _Y = yvalues
    End Sub
    Public Overrides Function Clone() As PairedData
        Dim x As New List(Of Single)
        Dim y As New List(Of ContinuousDistribution)
        For i = 0 To _X.Count - 1
            x.Add(_X(i))
            y.Add(_Y(i))
        Next
        Dim NewCurve As New GenericCurveSingleUncertain(x, y)
        Return NewCurve
    End Function
    Public Function ExportToTypedCurve(ByVal type As Type) As PairedData
        Select Case type
            Case GetType(MonotonicCurveUSingle)
                Dim c As New MonotonicCurveUSingle(_X.ToArray, _Y.ToArray)
                c.Verify()
                If c.Verify.Errors.Count > 1 Then Throw New Exception("Cannot be exported to type monotoniccurveusingle")
                Return c
            Case Else
                Return Nothing
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
