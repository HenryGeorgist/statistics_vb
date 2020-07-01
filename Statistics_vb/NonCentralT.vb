Public Class NonCentralT
    '    Inherits ContinuousDistribution
    '    Private _Mean As Double
    '    Private _Stdev As Double
    '    Private _SampleSize As Int32
    '    Private _Skew As Double
    '    Sub New(ByVal data() As Double)
    '        Dim recordlength As Int32 = _SampleSize
    '        Dim logflows(recordlength - 1) As Double
    '        For i = 0 To recordlength - 1
    '            logflows(i) = Math.Log(data(i), 10)
    '        Next
    '        _Skew = skew(logflows, recordlength - 1, 0)
    '    End Sub
    '    Public Overrides Function getDistributedVariable(probability As Double) As Double 'this needs work since it needs to random variables.
    '        Dim n As New Normal
    '        Dim K As Double = (2 / _Skew) * (((n.getDistributedVariable(probability) - _Skew / 6) * _Skew / 6 + 1) ^ 3 - 1) 'kof skew and exceedance probability(natural)
    '        Dim z As Double = (n.getDistributedVariable(knowledge)) 'a standard normal distribution for the location in distance from the mean for the natural probablity based on the knowledge uncertianty probability
    '        Dim Avalue As Double = (1 - ((z ^ 2) / (2 * (_SampleSize)))) 'the parameter A in the bulletin 17b document
    '        Dim bvalue As Double = (K ^ 2) - ((z ^ 2) / (_SampleSize)) 'the parameter b in the bulletin 17b document
    '        Dim rootvalue As Double = ((K ^ 2) - (Avalue * bvalue)) ^ (1 / 2) 'the information under the square root in the bulletin 17b document
    '        Dim result As Double = 0
    '        If knowledge > 0.5 Then
    '            'upper confidence interval if the value is greater than 50% for the knowledge uncertianty
    '            result = _Mean + (((K + rootvalue) / Avalue) * _Stdev)
    '        Else
    '            'lower confidence interval if the value is lesser than 50% for the knowledge uncertianty
    '            result = _Mean + (((K - rootvalue) / Avalue) * _Stdev)
    '        End If
    '        Return result
    '    End Function

    '    Public Overrides ReadOnly Property GetMean As Double
    '        Get
    '            Return _Mean
    '        End Get
    '    End Property

    '    Public Overrides ReadOnly Property GetSampleSize As Integer
    '        Get
    '            Return _SampleSize
    '        End Get
    '    End Property

    '    Public Overrides ReadOnly Property GetStDev As Double
    '        Get
    '            Return _Stdev
    '        End Get
    '    End Property
End Class
