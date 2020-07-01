Public Module LinearAlgebra
    Public Function OLS(ByVal X As Double(,), ByVal Y As Double(,)) As Double(,)
        Dim xt As Double(,) = transpose(X)
        Dim xtx As Double(,) = Multiply(xt, X)
        Dim xtxin As Double(,)
        Try
            xtxin = Inverse(xtx)
        Catch ex As Exception
            Return Nothing
        End Try

        Dim xtxinxt As Double(,) = Multiply(xtxin, xt)
        Dim betahat As Double(,) = Multiply(xtxinxt, Y)
        Return betahat
    End Function
    Public Function OLS_SSR(ByVal Y As Double(,), ByVal X As Double(,), ByVal BetaHat As Double(,)) As Double
        Dim XB As Double(,) = Multiply(X, BetaHat)
        Dim yminusxb As Double(,) = subtraction(Y, XB)
        Dim SSR As Double(,) = Multiply(transpose(yminusxb), yminusxb)
        Dim ssrd As Double = converttodouble(SSR)
        Return ssrd
    End Function
    Public Function OLS_ESS(ByVal Y As Double(,), ByVal x As Double(,)) As Double
        Dim b As Double(,) = OLS(x, Y)
        Dim xb As Double(,) = Multiply(x, b)
        Dim bt As Double(,) = transpose(b)
        Dim btx As Double(,) = Multiply(bt, transpose(x))
        Dim ess As Double(,) = Multiply(btx, xb)
        Dim essd As Double = converttodouble(ess)
        Return essd
    End Function
    Public Function OLS_TSS(ByVal Y As Double(,), ByVal x As Double(,)) As Double
        Dim tss As Double(,) = Multiply(transpose(Y), Y)
        Dim tssd As Double = converttodouble(tss)
        Return tssd
    End Function
    Public Function converttodouble(ByVal matrix As Double(,)) As Double
        Dim m As Integer = matrix.GetUpperBound(0)
        Dim n As Integer = matrix.GetUpperBound(1)
        Dim result As Double
        If m = 0 And n = 0 Then
            result = matrix(m, n)
        Else
            result = 0
        End If
        Return result

    End Function

    Public Function Rsquared(ByVal y As Double(,), ByVal x As Double(,)) As Double
        Dim ess As Double = OLS_ESS(y, x)
        Dim TSS As Double = OLS_TSS(y, x)
        Return ess / TSS
    End Function
    Public Function OLS_Px(ByVal X As Double(,)) As Double(,)
        Dim xt As Double(,) = transpose(X)
        Dim xtx As Double(,) = Multiply(xt, X)
        Dim xtxin As Double(,)
        Try
            xtxin = Inverse(xtx)
        Catch ex As Exception
            Return Nothing
        End Try
        Dim xxtxin As Double(,) = Multiply(X, xtxin)
        Dim xxtxinxt As Double(,) = Multiply(xxtxin, xt)
        Return xxtxinxt
    End Function
    Function identity(ByVal N As Integer) As Double(,)
        Dim result(N, N) As Double
        For i = 0 To N
            result(i, i) = 1
        Next
        Return result
    End Function
    Public Function OLS_Mx(ByVal x As Double(,)) As Double(,)
        Dim px As Double(,) = OLS_Px(x)
        Dim i As Double(,) = identity(x.GetUpperBound(0))

        Return subtraction(i, px)
    End Function
    Public Function transpose(ByVal a(,) As Double) As Double(,)
        'Get bounds from a
        Dim m As Integer = a.GetUpperBound(0)
        Dim n As Integer = a.GetUpperBound(1)
        Dim at(n, m) As Double
        For I As Integer = 0 To m
            For j = 0 To n
                at(j, I) = a(I, j)
            Next j
        Next I
        Return at
    End Function
    Function choleskydecomp(ByVal a(,) As Double) As Double(,)
        'http://vbadeveloper.net/numericalmethodsvbacholeskydecomposition.pdf
        Dim element As Double

        Dim m As Integer = a.GetUpperBound(0)
        Dim n As Integer = a.GetUpperBound(1)
        If m <> n Then
            Return a 'i should complain loudly here, since it is not a square matrix
        Else
            For i = 0 To m
                If a(m, m) < 0 Then
                    Return a 'negatives in the diagonal are bad news bears?
                End If
            Next
        End If
        Dim L_Lower(m, n) As Double
        For i = 0 To n
            For j = 0 To n
                element = a(i, j)
                For k = 0 To i - 1
                    element = element - L_Lower(i, k) * L_Lower(j, k)
                Next k
                If i = j Then
                    L_Lower(i, i) = Math.Sqrt(element)
                ElseIf i < j Then
                    L_Lower(j, i) = element / L_Lower(i, i)
                End If
            Next j
        Next i
        Return L_Lower
    End Function
    Public Function Inverse(ByVal sourceMatrix(,) As Double) As Double(,)
        ' ----- Build a new matrix that is the mathematical inverse
        '       of the supplied matrix. Multiplying a matrix and its
        '       inverse together will give the identity matrix.
        Dim eachCol As Integer
        Dim eachRow As Integer
        Dim rowsAndCols As Integer

        ' ----- Determine the size of each dimension of the matrix.
        '       Only square matrices can be inverted.
        If (UBound(sourceMatrix, 1) <> UBound(sourceMatrix, 2)) Then
            Throw New Exception("Matrix must be square.")
        End If
        Dim rank As Integer = UBound(sourceMatrix, 1)

        ' ----- Clone a copy of the matrix (not just a new reference).
        Dim workMatrix(,) As Double = _
            CType(sourceMatrix.Clone, Double(,))

        ' ----- Variables used for backsolving.
        Dim destMatrix(rank, rank) As Double
        Dim rightHandSide(rank) As Double
        Dim solutions(rank) As Double
        Dim rowPivots(rank) As Integer
        Dim colPivots(rank) As Integer

        ' ----- Use LU decomposition to form a triangular matrix.
        workMatrix = FormLU(workMatrix, rowPivots, colPivots, rowsAndCols)

        ' ----- Backsolve the triangular matrix to get the inverted
        '       value for each position in the final matrix.
        For eachCol = 0 To rank
            rightHandSide(eachCol) = 1
            BackSolve(workMatrix, rightHandSide, solutions, rowPivots, colPivots)
            For eachRow = 0 To rank
                destMatrix(eachRow, eachCol) = solutions(eachRow)
                rightHandSide(eachRow) = 0
            Next eachRow
        Next eachCol

        ' ----- Return the inverted matrix result.
        Return destMatrix
    End Function

    Public Function Determinant(ByVal sourceMatrix(,) As Double) As Double
        ' ----- Calculate the determinant of a matrix.
        Dim result As Double
        Dim pivots As Integer
        Dim count As Integer

        ' ----- Only calculate the determinants of square matrices.
        If (UBound(sourceMatrix, 1) <> UBound(sourceMatrix, 2)) Then
            Throw New Exception( _
                "Determinant only calculated for square matrices.")
        End If
        Dim rank As Integer = UBound(sourceMatrix, 1)

        ' ----- Make a copy of the matrix so we can work inside of it.
        Dim workMatrix(rank, rank) As Double
        Array.Copy(sourceMatrix, workMatrix, sourceMatrix.Length)

        ' ----- Use LU decomposition to form a triangular matrix.
        Dim rowPivots(rank) As Integer
        Dim colPivots(rank) As Integer
        workMatrix = FormLU(workMatrix, rowPivots, colPivots, count)

        ' ----- Get the product at each of the pivot points.
        result = 1
        For pivots = 0 To rank
            result *= workMatrix(rowPivots(pivots), colPivots(pivots))
        Next pivots

        ' ----- Determine the sign of the result using LaPlace's formula.
        result = (-1) ^ count * result
        Return result
    End Function

    Public Function SimultEq(ByVal sourceEquations(,) As Double, ByVal sourceRHS() As Double) As Double()
        ' ----- Use matrices to solve simultaneous equations.
        Dim rowsAndCols As Integer

        ' ----- The matrix must be square and the array size must match.
        Dim rank As Integer = UBound(sourceEquations, 1)
        If (UBound(sourceEquations, 2) <> rank) Or (UBound(sourceRHS, 1) <> rank) Then
            Throw New Exception("Size problem for simultaneous equations.")
        End If

        ' ----- Create some arrays for doing all of the work.
        Dim coefficientMatrix(rank, rank) As Double
        Dim rightHandSide(rank) As Double
        Dim solutions(rank) As Double
        Dim rowPivots(rank) As Integer
        Dim colPivots(rank) As Integer

        ' ----- Make copies of the original matrices so we don't
        '       mess them up.
        Array.Copy(sourceEquations, coefficientMatrix, sourceEquations.Length)
        Array.Copy(sourceRHS, rightHandSide, sourceRHS.Length)

        ' ----- Use LU decomposition to form a triangular matrix.
        coefficientMatrix = FormLU(coefficientMatrix, rowPivots, colPivots, rowsAndCols)

        ' ----- Find the unique solution for the upper-triangle.
        BackSolve(coefficientMatrix, rightHandSide, solutions, rowPivots, colPivots)

        ' ----- Return the simultaneous equations result in an array.
        Return solutions
    End Function

    Public Function FormLU(ByVal sourceMatrix(,) As Double, ByRef rowPivots() As Integer, ByRef colPivots() As Integer, ByRef rowsAndCols As Integer) As Double(,)
        ' ----- Perform an LU (lower and upper) decomposition of a matrix,
        '       a modified form of Gaussian elimination.
        Dim eachRow As Integer
        Dim eachCol As Integer
        Dim pivot As Integer
        Dim rowIndex As Integer
        Dim colIndex As Integer
        Dim bestRow As Integer
        Dim bestCol As Integer
        Dim rowToPivot As Integer
        Dim colToPivot As Integer
        Dim maxValue As Double
        Dim testValue As Double
        Dim oldMax As Double
        Const Deps As Double = 0.0000000000000001

        ' ----- Determine the size of the array.
        Dim rank As Integer = UBound(sourceMatrix, 1)
        Dim destMatrix(rank, rank) As Double
        Dim rowNorm(rank) As Double
        ReDim rowPivots(rank)
        ReDim colPivots(rank)

        ' ----- Make a copy of the array so we don't mess it up.
        Array.Copy(sourceMatrix, destMatrix, sourceMatrix.Length)

        ' ----- Initialize row and column pivot arrays.
        For eachRow = 0 To rank
            rowPivots(eachRow) = eachRow
            colPivots(eachRow) = eachRow
            For eachCol = 0 To rank
                rowNorm(eachRow) += Math.Abs(destMatrix(eachRow, eachCol))
            Next eachCol
            If (rowNorm(eachRow) = 0) Then
                Throw New Exception("Cannot invert a singular matrix.")
            End If
        Next eachRow

        ' ----- Use Gauss-Jordan elimination on the matrix rows.
        For pivot = 0 To rank - 1
            maxValue = 0
            For eachRow = pivot To rank
                rowIndex = rowPivots(eachRow)
                For eachCol = pivot To rank
                    colIndex = colPivots(eachCol)
                    testValue = Math.Abs(destMatrix(rowIndex, colIndex)) _
                        / rowNorm(rowIndex)
                    If (testValue > maxValue) Then
                        maxValue = testValue
                        bestRow = eachRow
                        bestCol = eachCol
                    End If
                Next eachCol
            Next eachRow

            ' ----- Detect a singular, or very nearly singular, matrix.
            If (maxValue = 0) Then
                Throw New Exception("Singular matrix used for LU.")
            ElseIf (pivot > 1) Then
                If (maxValue < (Deps * oldMax)) Then
                    Throw New Exception("Non-invertible matrix used for LU.")
                End If
            End If
            oldMax = maxValue

            ' ----- Swap row pivot values for the best row.
            If (rowPivots(pivot) <> rowPivots(bestRow)) Then
                rowsAndCols += 1
                Swap(rowPivots(pivot), rowPivots(bestRow))
            End If

            ' ----- Swap column pivot values for the best column.
            If (colPivots(pivot) <> colPivots(bestCol)) Then
                rowsAndCols += 1
                Swap(colPivots(pivot), colPivots(bestCol))
            End If

            ' ----- Work with the current pivot points.
            rowToPivot = rowPivots(pivot)
            colToPivot = colPivots(pivot)

            ' ----- Modify the remaining rows from the pivot points.
            For eachRow = (pivot + 1) To rank
                rowIndex = rowPivots(eachRow)
                destMatrix(rowIndex, colToPivot) = _
                    -destMatrix(rowIndex, colToPivot) / _
                    destMatrix(rowToPivot, colToPivot)
                For eachCol = (pivot + 1) To rank
                    colIndex = colPivots(eachCol)
                    destMatrix(rowIndex, colIndex) += _
                        destMatrix(rowIndex, colToPivot) * _
                        destMatrix(rowToPivot, colIndex)
                Next eachCol
            Next eachRow
        Next pivot

        ' ----- Detect a non-invertible matrix.
        If (destMatrix(rowPivots(rank), colPivots(rank)) = 0) Then
            Throw New Exception("Non-invertible matrix used for LU.")
        ElseIf (Math.Abs(destMatrix(rowPivots(rank), colPivots(rank))) / _
                rowNorm(rowPivots(rank))) < (Deps * oldMax) Then
            Throw New Exception("Non-invertible matrix used for LU.")
        End If

        ' ----- Success. Return the LU triangular matrix.
        Return destMatrix
    End Function

    Public Sub Swap(ByRef firstValue As Integer, ByRef secondValue As Integer)
        ' ----- Reverse the values of two reference integers.
        Dim holdValue As Integer
        holdValue = firstValue
        firstValue = secondValue
        secondValue = holdValue
    End Sub

    Public Sub BackSolve(ByVal sourceMatrix(,) As Double, ByVal rightHandSide() As Double, ByVal solutions() As Double, ByRef rowPivots() As Integer, ByRef colPivots() As Integer)
        ' ----- Solve an upper-right-triangle matrix.
        Dim pivot As Integer
        Dim rowToPivot As Integer
        Dim colToPivot As Integer
        Dim eachRow As Integer
        Dim eachCol As Integer
        Dim rank As Integer = UBound(sourceMatrix, 1)

        ' ----- Work through all pivot points. This section builds
        '       the "B" in the AX=B formula.
        For pivot = 0 To (rank - 1)
            colToPivot = colPivots(pivot)
            For eachRow = (pivot + 1) To rank
                rowToPivot = rowPivots(eachRow)
                rightHandSide(rowToPivot) += _
                    sourceMatrix(rowToPivot, colToPivot) _
                    * rightHandSide(rowPivots(pivot))
            Next eachRow
        Next pivot

        ' ----- Now solve for each X using the general formula
        '       x(i) = (b(i) - summation(a(i,j)x(j)))/a(i,i)
        For eachRow = rank To 0 Step -1
            colToPivot = colPivots(eachRow)
            rowToPivot = rowPivots(eachRow)
            solutions(colToPivot) = rightHandSide(rowToPivot)
            For eachCol = (eachRow + 1) To rank
                solutions(colToPivot) -= _
                    sourceMatrix(rowToPivot, colPivots(eachCol)) _
                    * solutions(colPivots(eachCol))
            Next eachCol
            solutions(colToPivot) /= sourceMatrix(rowToPivot, colToPivot)
        Next eachRow
    End Sub
    Public Function Multiply(matrix1 As Double(,), matrix2 As Double(,)) As Double(,)
        Dim row1 As Integer = matrix1.GetUpperBound(0)
        Dim col2 As Integer = matrix2.GetUpperBound(1)
        Dim row2 As Integer = matrix2.GetUpperBound(0)
        Dim col1 As Integer = matrix1.GetUpperBound(1)
        If col1 = row2 Then
            Dim ret(row1, col2) As Double

            For i As Integer = 0 To row1
                For j As Integer = 0 To col2
                    Dim temp As Double = 0
                    For k As Integer = 0 To row2
                        temp = temp + (matrix1(i, k) * matrix2(k, j))
                    Next
                    ret(i, j) = temp
                Next
            Next
            Return ret
        Else
            Throw New RankException("Number of columns in matrix1 is not equal to the number of rows in matrix2.")
        End If
    End Function
    Function addition(ByVal a As Double(,), ByVal b As Double(,)) As Double(,)
        Dim row1 As Integer = a.GetUpperBound(0)
        Dim col2 As Integer = b.GetUpperBound(1)
        Dim row2 As Integer = b.GetUpperBound(0)
        Dim col1 As Integer = a.GetUpperBound(1)
        If col1 = col2 And row1 = row2 Then
            Dim ret(row1, col1) As Double

            For i As Integer = 0 To row1
                For j As Integer = 0 To col2
                    ret(i, j) = (a(i, j) + b(i, j))
                Next
            Next
            Return ret
        Else
            Throw New RankException("Matrix1 and Matrix2 do not have the same dimensions.")
        End If
    End Function
    Function subtraction(ByVal a As Double(,), ByVal b As Double(,)) As Double(,)
        Dim row1 As Integer = a.GetUpperBound(0)
        Dim col2 As Integer = b.GetUpperBound(1)
        Dim row2 As Integer = b.GetUpperBound(0)
        Dim col1 As Integer = a.GetUpperBound(1)
        If col1 = col2 And row1 = row2 Then
            Dim ret(row1, col1) As Double

            For i As Integer = 0 To row1
                For j As Integer = 0 To col2
                    ret(i, j) = (a(i, j) - b(i, j))
                Next
            Next
            Return ret
        Else
            Throw New RankException("Matrix1 and Matrix2 do not have the same dimensions.")
        End If
    End Function
End Module
