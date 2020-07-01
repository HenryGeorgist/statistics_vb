''' <summary>
''' The Lindell continuous distribution is not a true distribution. It has been included as a continuous distribution to make it
''' easy to use in conjunction with other continuous distributions to define uncertainty around warning issuance delay. The formula was developed to describe
''' diffusion and mobilization and has also been applied to warning issuance delay. The A and B parameters are used to fit the curve to the desired shape. 
''' The mathematical formulation describing diffusion and mobilization developed by Lindell et al. (2002) and Lindell and Perry (2004).
''' Exercise caution when using this distribution as it was developed for a very specific use case.
''' </summary>
''' <remarks></remarks>
Public Class Lindell
    Inherits ContinuousDistribution
    Private _A As Double
    Private _B As Double
    Public ReadOnly Property A_Parameter As Double
        Get
            Return _A
        End Get
    End Property
    Public ReadOnly Property B_Parameter As Double
        Get
            Return _B
        End Get
    End Property
    Sub New()

    End Sub
    Sub New(ByVal A As Double, ByVal B As Double)
        _A = A
        _B = B
        Validate()
    End Sub
    Public Overrides Function Clone() As ContinuousDistribution
        Return New Lindell(_A, _B)
    End Function
    ''' <summary>
    ''' Gets the CDF of the Lindell relationship for a given value.
    ''' </summary>
    ''' <param name="value">Minutes</param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Overrides Function GetCDF(value As Double) As Double
        If value < 0 Then Return 0
        Return 1 - (Math.Exp(-1 * _A * (value / 60) ^ _B))
    End Function
    ''' <summary>
    ''' Not available and will throw an exception.
    ''' </summary>
    ''' <value></value>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Overrides ReadOnly Property GetCentralTendency As Double
        Get
            Throw New NotImplementedException
        End Get
    End Property
    ''' <summary>
    ''' Calculates the distributed variable using the inverse CDF based on the A and B variables. The returned value will not be greater than 360 (6 hours).
    ''' </summary>
    ''' <param name="probability">a random number between zero and 1 exclusive</param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Overrides Function getDistributedVariable(probability As Double) As Double
        If probability <= 0 Then Return 0
        If probability > 1 Then probability = 1
        Dim SampledMinutes As Double = Math.Min(((Math.Log(1 - probability) * -1 * 60 ^ _B) / _A) ^ (1 / _B), CDbl(360))
        Return SampledMinutes
    End Function

    Public Overrides ReadOnly Property GetNumberOfParameters As Short
        Get
            Return 2
        End Get
    End Property
    ''' <summary>
    ''' This function gets an approximation of the PDF based on 15 minute intervals to better align with the Lindell approximations.
    ''' </summary>
    ''' <param name="Value">Minutes</param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Overrides Function GetPDF(Value As Double) As Double
        If Value <= 0 Then Return 0
        Dim LowValue As Int32 = CInt(Math.Floor(Value / 15)) * 15
        Dim HighValue As Int32 = CInt(Math.Ceiling(Value / 15)) * 15
        If LowValue = HighValue Then Return (GetCDF(HighValue) - GetCDF(HighValue - 15))
        Dim LowData As Double = GetCDF(LowValue)
        Dim HighData As Double = GetCDF(HighValue)

        'Dim PreviousHighValue As Int32 = LowValue
        Dim PreviousLowValue As Int32 = LowValue - 15
        Dim PreviousLowData As Double
        If PreviousLowValue <= 0 Then
            PreviousLowValue = 0
            PreviousLowData = 0
        Else
            PreviousLowData = GetCDF(PreviousLowValue)
        End If
        Dim PreviousHighData As Double = LowData

        Dim Y1 As Double = PreviousHighData - PreviousLowData
        Dim X1 As Double = LowValue
        Dim Y2 As Double = HighData - LowData
        Dim X2 As Double = HighValue

        'Y1 + (x - X1) / (X2 - X1) * (Y2 - Y1)
        Return Y1 + (Value - X1) / (X2 - X1) * (Y2 - Y1)
    End Function

    Public Overrides ReadOnly Property GetSampleSize As Integer
        Get
            Return 1
        End Get
    End Property
    ''' <summary>
    ''' Currently not available and will throw and exception.
    ''' </summary>
    ''' <param name="data"></param>
    ''' <remarks></remarks>
    Public Overrides Sub SetParameters(data() As Double)
        Throw New NotImplementedException
    End Sub

    Public Overrides Function Validate() As String
        If _A < 0.1 Then Return "Lindell Distribution Error: A value must be greater than or equal to 0.1."
        If _A > 1.8 Then Return "Lindell Distribution Error: A value must be less than or equal to 1.8."
        If _B < 0.5 Then Return "Lindell Distribution Error: B value must be greater than or equal to 0.5."
        If _B > 2.1 Then Return "Lindell Distribution Error: B value must be less than or equal to 2.1."
        Return Nothing
    End Function
    Public Overloads Shared Operator =(left As Lindell, right As Lindell) As Boolean
        ' Check for null arguments. Keep in mind null == null
        If left Is Nothing AndAlso right Is Nothing Then
            Return True
        ElseIf left Is Nothing Then
            Return False
        ElseIf right Is Nothing Then
            Return False
        End If

        If left._A = right._A AndAlso left._B = right._B Then
            Return True
        Else
            Return False
        End If
    End Operator
    Public Overloads Shared Operator <>(left As Lindell, right As Lindell) As Boolean
        Return Not (left = right)
    End Operator
    Public Overrides Function Equals(obj As Object) As Boolean
        If TypeOf obj Is Lindell Then
            Return Me = CType(obj, Lindell)
        Else
            Return False
        End If
    End Function
    Public Overrides Function GetHashCode() As Integer
        Return Hash(_A, _B)
    End Function
End Class
