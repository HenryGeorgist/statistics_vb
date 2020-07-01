Public Class BasicProductMomentsStatsHistogram
    Inherits BasicProductMomentsStats
    Private _bins() As Int32
    Private _ExpectedMin As Double
    Private _ExpectedMax As Double
    Private _BinWidth As Double
    Private _IsFixed As Boolean
    Private _LessThanBinMin As Int32 = 0
    Private _GreaterThanBinMax As Int32 = 0
    Public ReadOnly Property Histogram As Int32()
        Get
            Return _bins
        End Get
    End Property
    ''' <summary>
    ''' The number of instances where an observation was less than the histogram minimum.
    ''' </summary>
    ''' <value></value>
    ''' <returns>Number of observations less than the histogram minimum.</returns>
    ''' <remarks></remarks>
    Public ReadOnly Property LessThanHistogramMinimum As Int32
        Get
            Return _LessThanBinMin
        End Get
    End Property
    ''' <summary>
    ''' The number of instances where an observation was greater than the histogram maximum.
    ''' </summary>
    ''' <value></value>
    ''' <returns>Number of observations greater than the histogram maximum.</returns>
    ''' <remarks></remarks>
    Public ReadOnly Property GreaterThanHistogramMaximum As Int32
        Get
            Return _GreaterThanBinMax
        End Get
    End Property
    ''' <summary>
    ''' Create a basic product moment stats object that also includes a histogram. The histogram can dynamically update the number of bins or be fixed.
    ''' </summary>
    ''' <param name="numberofBins">Number of bins desired for the histogram.</param>
    ''' <param name="ExpectedMin">The expected minimum value.</param>
    ''' <param name="ExpectedMax">The expected maximum value.</param>
    ''' <param name="FixedHistogram">Boolean value determining whether the histogram size is fixed or will dynamically increase to contain all values added.</param>
    ''' <remarks></remarks>
    Sub New(ByVal numberofBins As Int32, ByVal ExpectedMin As Double, ByVal ExpectedMax As Double, ByVal FixedHistogram As Boolean)
        MyBase.New()
        _IsFixed = FixedHistogram
        If numberofBins = 0 Then numberofBins = 2
        If ExpectedMin = ExpectedMax Then
            ExpectedMax = ExpectedMin + 8
        End If
        ReDim _bins(numberofBins - 1)
        _ExpectedMin = ExpectedMin
        _ExpectedMax = ExpectedMax
        _BinWidth = (_ExpectedMax - _ExpectedMin) / _bins.Count
    End Sub
    Public Shadows Sub AddObservation(ByVal observation As Double)
        MyBase.AddObservation(observation)
        'bin the value.
        If observation > _ExpectedMin AndAlso observation < _ExpectedMax Then
            Dim index As Integer = CInt(Math.Floor(_bins.Count * (observation - _ExpectedMin) / (_ExpectedMax - _ExpectedMin)))
            _bins(index) += 1
        ElseIf observation > _ExpectedMax Then
            If _IsFixed = False Then
                'determine the number of whole bins to add to include the new point.
                Dim overdist As Integer = CInt(Math.Ceiling((observation - _ExpectedMax) / _BinWidth))
                'set the new max
                _ExpectedMax = _ExpectedMax + overdist * _BinWidth
                'redim preserve
                Dim NewBins(_bins.Count + overdist - 1) As Int32
                Buffer.BlockCopy(_bins, 0, NewBins, 0, _bins.Count)
                _bins = NewBins
                'ReDim Preserve _bins(_bins.Count + overdist - 1)
                Dim index As Integer = CInt(Math.Floor(_bins.Count * (observation - _ExpectedMin) / (_ExpectedMax - _ExpectedMin)))
                If index = _bins.Count Then index -= 1 'must handle the edge case where the new expected maximum could still equal the observed
                _bins(index) += 1
            Else
                _GreaterThanBinMax += 1
            End If
        ElseIf observation < _ExpectedMin Then
            If _IsFixed = False Then
                'determine the number of whole bins to insert to include the new point.
                Dim overdist As Integer = CInt(Math.Ceiling(-(observation - _ExpectedMin) / _BinWidth))
                'set the new min
                _ExpectedMin = _ExpectedMin - overdist * _BinWidth
                'shuffle to new array.
                Dim NewBins(_bins.Count + overdist - 1) As Int32
                Buffer.BlockCopy(_bins, 0, NewBins, overdist - 1, _bins.Count)
                _bins = NewBins
                Dim index As Integer = CInt(Math.Floor(_bins.Count * (observation - _ExpectedMin) / (_ExpectedMax - _ExpectedMin)))
                _bins(index) += 1
            Else
                _LessThanBinMin += 1
            End If
        ElseIf observation = _ExpectedMax Then
            'This is not 100% accurate based on the indexing logic. Really a new bin should be added to the end.
            _bins(_bins.Count - 1) += 1
        ElseIf observation = _ExpectedMin Then
            _bins(0) += 1
        End If

        'If observation <= _ExpectedMin Then
        '    ' increase the lower bound by the proper amount and reset the min.
        '    Dim binwidth As Double = (_ExpectedMax - _ExpectedMin) / _bins.Count
        '    'determine the number of whole bins to add to include the new point.
        '    Dim overdist As Integer = CInt(Math.Ceiling(-(observation - _ExpectedMin) / binwidth))
        '    'set the new max
        '    _ExpectedMin = _ExpectedMin - overdist * binwidth
        '    'shuffle to new array.
        '    Dim tmparray(_bins.Count + overdist - 1) As Int32
        '    For i As Integer = overdist To tmparray.Count - 1
        '        tmparray(i) = _bins(i - overdist)
        '    Next
        '    _bins = tmparray
        'End If
        'If observation >= _ExpectedMax Then
        '    'determine the bin width.
        '    Dim binwidth As Double = (_ExpectedMax - _ExpectedMin) / _bins.Count
        '    'determine the number of whole bins to add to include the new point.
        '    Dim overdist As Integer = CInt(Math.Ceiling((observation - _ExpectedMax) / binwidth))
        '    'set the new max
        '    _ExpectedMax = _ExpectedMax + overdist * binwidth
        '    'redim preserve
        '    ReDim Preserve _bins(_bins.Count + overdist - 1)
        'End If

    End Sub
    Public Shadows Sub AddObservations(ByVal Observations() As Double)
        For i As Int32 = 0 To Observations.Count - 1
            AddObservation(Observations(i))
        Next
    End Sub
    Public Function HistogramBinMinimum(ByVal BinIndex As Int32) As Double
        Return _ExpectedMin + _BinWidth * BinIndex
    End Function
    Public Function HistogramBinMaximum(ByVal BinIndex As Int32) As Double
        Return _ExpectedMin + _BinWidth * (BinIndex + 1)
    End Function
    Public Function ExceedanceValue(ByVal Probability As Double) As Double
        Dim incrementalprob As Double = 0
        Dim cumulativeProb As Double = 0
        Dim index As Integer = 0
        Do Until cumulativeProb >= Probability
            incrementalprob = _bins(index) / MyBase.GetSampleSize
            cumulativeProb += incrementalprob
            index += 1
        Loop
        'index -= 1 'because 0 is a bin, but also my calculation below is adding the increment to whole bins we subtract

        Dim binwidth As Double = (_ExpectedMax - _ExpectedMin) / _bins.Count
        'interpolate?
        Return _ExpectedMin + (index * binwidth) - (binwidth * ((cumulativeProb - Probability) / incrementalprob))
    End Function
    Public Function ExceedanceProbability(ByVal Value As Double) As Double
        Dim incrementalprob As Double = 0
        Dim cumulativeProb As Double = 0
        Dim index As Integer = 0
        Do Until index > _bins.Count - 1 OrElse index * _BinWidth >= Value
            incrementalprob = _bins(index) / MyBase.GetSampleSize
            cumulativeProb += incrementalprob
            index += 1
        Loop
        'index -= 1 'because 0 is a bin, but also my calculation below is adding the increment to whole bins we subtract
        If index = _bins.Count - 1 Then Return cumulativeProb
        'interpolate?
        Dim percent As Double = (Value - (_ExpectedMin + (index * _BinWidth)) / _BinWidth)
        Return cumulativeProb + (percent * _bins(index + 1) / MyBase.GetSampleSize)
    End Function
End Class
