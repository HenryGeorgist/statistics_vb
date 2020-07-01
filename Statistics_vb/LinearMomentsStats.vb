Public Class LinearMomentsStats
    Private _L1 As Double
    Private _L2 As Double
    Private _L3 As Double
    Private _L4 As Double
    Private _N As Int32
    Private _Max As Double
    Private _Min As Double
    Sub New(ByVal data() As Double)
        Array.Sort(data)
        _N = data.Count
        _Min = data(0)
        _Max = data(_N - 1)
        Dim cl1 As Long
        Dim cl2 As Long
        Dim cl3 As Long
        Dim cl4 As Long
        Dim cr1 As Long
        Dim cr2 As Long
        Dim cr3 As Long
        'Dim cr4 As Long
        Dim sl1 As Double = 0
        Dim sl2 As Double = 0
        Dim sl3 As Double = 0
        Dim sl4 As Double = 0
        For i = 0 To _N - 1
            cl1 = i
            cl2 = CLng((cl1 * (i - 1)) / 2)
            cl3 = CLng((cl2 * (i - 2)) / 3)
            'cl4 = cl3 * (i - 3) / 4
            cr1 = (_N - (i + 1))
            cr2 = CLng((cr1 * ((_N - (i + 2))) / 2))
            cr3 = CLng((cr2 * ((_N - (i + 3))) / 3))
            'cr4 = cr3 * ((_N - (i + 4)) / 4)
            sl1 += data(i)
            sl2 += data(i) * (cl1 - cr1)
            sl3 += data(i) * (cl2 - 2 * cl1 * cr1 + cr2)
            sl4 += data(i) * (cl3 - 3 * cl2 * cr1 + 3 * cl1 * cr2 - cr3)
        Next
        cl1 = _N
        cl2 = CLng(cl1 * (_N - 1) / 2)
        cl3 = CLng(cl2 * (_N - 2) / 3)
        cl4 = CLng(cl3 * (_N - 3) / 4)
        _L1 = sl1 / cl1
        _L2 = sl2 / cl2 / 2
        _L3 = sl3 / cl3 / 3
        _L4 = sl4 / cl4 / 4
    End Sub
    Public ReadOnly Property GetL1 As Double
        Get
            Return _L1
        End Get
    End Property
    Public ReadOnly Property GetL2 As Double
        Get
            Return _L2
        End Get
    End Property
    Public ReadOnly Property GetL3 As Double
        Get
            Return _L3
        End Get
    End Property
    Public ReadOnly Property GetL4 As Double
        Get
            Return _L4
        End Get
    End Property
    Public ReadOnly Property GetT As Double
        Get
            Return _L2 / _L1
        End Get
    End Property
    Public ReadOnly Property GetT3 As Double
        Get
            Return _L3 / _L2
        End Get
    End Property
    Public ReadOnly Property GetT4 As Double
        Get
            Return _L4 / _L2
        End Get
    End Property
    Public ReadOnly Property GetSampleSize As Int32
        Get
            Return _N
        End Get
    End Property
    Public ReadOnly Property GetMax As Double
        Get
            Return _Max
        End Get
    End Property
    Public ReadOnly Property GetMin As Double
        Get
            Return _Min
        End Get
    End Property
    'other derivative descriptors 
End Class
