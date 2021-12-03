Attribute VB_Name = "x00_Tabulate"
Option Explicit
Public wsf As WorksheetFunction

Public Sub Bible_Tabulate()
  Dim arr0() As String, ref1() As String, ref2() As String, vrs0() As String, vrs1() As String
  Dim istr As String, cel As Range, i As Long, j As Long: j = j + 1
  Dim stdin As Worksheet: Set stdin = ThisWorkbook.Sheets("Raw")
  
  Call NewStdout
  Dim stdout As Worksheet: Set stdout = ThisWorkbook.Sheets("Tabulate")

  For Each cel In stdin.Range("A1:A47")
    istr = cel.Value
    arr0() = Split(istr, " KJV - " & Chr(34)) ' split ref and text
    ref1() = Split(arr0(0), Chr(32))          ' split at space
    ref2() = Split(ref1(1), Chr(58))          ' split at colon
    vrs0() = Split(arr0(1), "] ")             ' split as strongs & terms
    
    For i = LBound(vrs0) To UBound(vrs0)
      j = j + 1
      vrs1() = Split(vrs0(i), Chr(91))
      
      vrs1(0) = cleanStr(vrs1(0))
      vrs1(1) = cleanStr(vrs1(1))
      
      Call TabString(stdout, j, ref1(0), ref2(0), ref2(1), vrs1(1), vrs1(0))
    Next i
  Next cel
End Sub

Private Sub NewStdout()
    Sheets.Add After:=ActiveSheet
    ActiveSheet.Name = "Tabulate"
End Sub

Private Function cleanStr(strg As String) As String
  Dim i As Long
  For i = 1 To Len(strg)
    Select Case Asc(Mid(strg, i, 1))
      Case 32, 48 To 57, 65 To 90, 97 To 122
      Case Else
        Mid(strg, i, 1) = " "
    End Select
  Next i
  cleanStr = Application.Trim(strg)
End Function

Private Sub TabString(ws As Worksheet, rw As Long, bk As String, ch As String, vs As String, strg As String, term As String)
  Set wsf = Application.WorksheetFunction
  Dim arr() As Variant: arr = Array("BK", "CH", "VS", "STRG", "TERM")
  ws.Range("A1:E1").Value = arr
  
  ws.Cells(rw, 1).Value = bk
  ws.Cells(rw, 2).Value = ch
  ws.Cells(rw, 3).Value = vs
  
  If Len(term) = 0 Then
    ws.Cells(rw, 5).Value = strg
  Else
    ws.Cells(rw, 4).Value = strg
    ws.Cells(rw, 5).Value = term
  End If
  ws.Columns("A:E").EntireColumn.AutoFit
End Sub
