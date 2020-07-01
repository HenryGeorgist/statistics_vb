Public Class MonotonicCurveUException
    Inherits Exception
    Private _Row As Integer
    Private _Column As Integer
    Sub New(ByVal row As Integer, ByVal column As Integer)
        MyBase.New()
        _Row = row
        _Column = column
    End Sub
    Sub New(ByVal message As String, ByVal row As Integer, ByVal column As Integer)
        MyBase.New(message)
        _Row = row
        _Column = column
    End Sub
    Public ReadOnly Property GetRow As Integer
        Get
            Return _Row
        End Get
    End Property
    Public ReadOnly Property GetColumn As Integer
        Get
            Return _Column
        End Get
    End Property
End Class
