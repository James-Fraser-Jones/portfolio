Option Explicit 'forces all variables to be declared explicitly

Public CalcState As Long 'storing settings which are disabled during optimization
Public EventState As Boolean
Public PageBreakState As Boolean

'constants for error types
Const ERROR_URL_INVALID As Long = vbObjectError + 513
Const ERROR_GET_NOTFOUND As Long = vbObjectError + 514
Const ERROR_GET_FAILED As Long = vbObjectError + 515
Const ERROR_FIND_FAILED As Long = vbObjectError + 516

'other useful constants
Const SHEET_NAME_EXCLUDE As String = "Summary"
Const VALID_URL_START As String = "https://www.justgiving.com/fundraising/"
Const VALID_URL_LENGTH As Integer = 40
Const CLASS_NAME_1 As String = "raised-so-far"
Const CLASS_NAME_2 As String = "statistics-amount-raised theme-highlight-text-font"
Const URL_COLUMN As Long = 12
Const TOTAL_COLUMN As Long = 13
Const START_ROW As Long = 2

Sub OptimizeCode_Begin() 'begin running optimized code
    EventState = Application.EnableEvents 'store current settings so we can revert them once code has completed
    CalcState = Application.Calculation
    PageBreakState = ActiveSheet.DisplayPageBreaks

    Application.EnableEvents = False 'settings for optimization during the running of the code
    Application.Calculation = xlCalculationManual
    ActiveSheet.DisplayPageBreaks = False
    Application.ScreenUpdating = False
End Sub

Sub OptimizeCode_End() 'end running optimized code
    Application.EnableEvents = EventState 'reset all changed settings
    Application.Calculation = CalcState
    ActiveSheet.DisplayPageBreaks = PageBreakState
    Application.ScreenUpdating = True
End Sub

Sub UpdateAllButton(control As IRibbonControl)
    Call OptimizeCode_Begin
    Call UpdateAll
    Call OptimizeCode_End
End Sub

Sub UpdateSheetButton(control As IRibbonControl)
    Call OptimizeCode_Begin
    Call UpdateSheet
    Call OptimizeCode_End
End Sub

Sub UpdateFromRowButton(control As IRibbonControl)
    Call OptimizeCode_Begin
    Call UpdateFromRow
    Call OptimizeCode_End
End Sub

Private Sub UpdateAll()
    Dim CurrentSheet As Worksheet
    Dim Sheet As Worksheet
    Set CurrentSheet = ActiveWorkbook.ActiveSheet
    For Each Sheet In ActiveWorkbook.Worksheets
        If (Sheet.Name <> SHEET_NAME_EXCLUDE) Then
            Sheet.Activate 'activate sheet so that update subroutine may activate erroneous cell on that sheet if necessary
            Call Update(Sheet, START_ROW)
        End If
    Next
    CurrentSheet.Activate 're-activate original sheet at the end of the process
End Sub

Private Sub UpdateSheet()
    Dim Sheet As Worksheet
    Set Sheet = ActiveWorkbook.ActiveSheet
    If (Sheet.Name <> SHEET_NAME_EXCLUDE) Then
        Call Update(Sheet, START_ROW)
    Else
        MsgBox ("Macro disabled on the " & Chr(34) & SHEET_NAME_EXCLUDE & Chr(34) & " sheet")
    End If
End Sub

Private Sub UpdateFromRow()
    Dim Row As Long
    Row = ActiveCell.Row

    Dim Sheet As Worksheet
    Set Sheet = ActiveWorkbook.ActiveSheet
    If (Sheet.Name <> SHEET_NAME_EXCLUDE) Then
        Call Update(Sheet, Row)
    Else
        MsgBox ("Macro disabled on the " & Chr(34) & SHEET_NAME_EXCLUDE & Chr(34) & " sheet")
    End If
End Sub

Private Sub Update(Sheet As Worksheet, StartRow As Long)
    On Error GoTo Er 'error handling

    Dim XMLPage As New MSXML2.XMLHTTP60 'new xml page
    Dim HTMLDoc As New MSHTML.HTMLDocument 'new HTML document

    Dim Raised As Double 'for coercing value to double type
    Dim Row As Long 'for looping over all rows

    Dim Elems As Variant 'array to store HTML elements
    Dim URL As String 'store URL

    For Row = StartRow To Sheet.Rows.Count
        If IsEmpty(Sheet.Cells(Row, URL_COLUMN)) Then
            Exit For 'only loop until first empty URL cell
        Else
            URL = Sheet.Cells(Row, URL_COLUMN) 'get URL from column 12

            If (Len(URL) < VALID_URL_LENGTH) Or (Left(URL, VALID_URL_LENGTH - 1) <> VALID_URL_START) Then 'throw error if URL isn't valid
                Err.Raise 513, "Update Function", "Invalid URL" & vbCrLf & Chr(34) & URL & Chr(34) & vbCrLf & vbCrLf & "Valid URLs have the following format" & vbCrLf & _
                Chr(34) & VALID_URL_START & "..." & Chr(34)
            End If

            XMLPage.Open "GET", URL, False 'prepare GET request
            XMLPage.send 'send GET request to URL

            If (XMLPage.statusText = "Not Found") Then 'throw error if GET request is "Not Found"
                Err.Raise 514, "Update Function", "Page Not Found" & vbCrLf & vbCrLf & "End of URL could be incorrect " & vbCrLf & Chr(34) & Split(URL, "/")(4) & Chr(34)
            ElseIf (XMLPage.statusText <> "OK") Then 'throw error if GET request is otherwise unsuccessful
                Err.Raise 515, "Update Function", "GET Request Failed" & vbCrLf & Chr(34) & XMLPage.statusText & Chr(34)
            End If

            HTMLDoc.body.innerHTML = XMLPage.responseText 'put XML response into HTML document

            Set Elems = HTMLDoc.getElementsByClassName(CLASS_NAME_1) 'try for this className first
            If (Elems.Length < 1) Then
                Set Elems = HTMLDoc.getElementsByClassName(CLASS_NAME_2) 'if it fails, try this one instead
                If (Elems.Length < 1) Then 'throw error if there are none of either className
                    WriteError (XMLPage.responseText) 'write XML response out to a seperate file for debugging
                    Err.Raise 516, "Update Function", "HTML element was not found"
                End If
            End If

            Raised = Elems.Item(0).innerText 'coerce the text from the first element from the list into a Double (ERROR COULD HAPPEN HERE BUT VERY UNLIKELY)
            Sheet.Cells(Row, TOTAL_COLUMN).Value = Raised 'write this back to the cell in column 13
        End If
    Next Row

Done: 'exit function if successful
    Exit Sub
Er: 'print sheet and row number along with error message if unsuccessful
    Sheet.Cells(Row, URL_COLUMN).Activate 'activate cell containing URL at row where error occoured
    Sheet.Cells(Row, TOTAL_COLUMN).ClearContents 'clear first cell which wasn't correctly updated
    MsgBox "Error: " & Err.Description & vbCrLf & vbCrLf & "Sheet: " & Sheet.Name & vbCrLf & "Row: " & Row
End Sub

Private Sub WriteError(XMLResponse As String)
    'initialisation
    Dim fso As Object
    Set fso = CreateObject("Scripting.FileSystemObject")
    Dim Fileout As Object

    'write the response text as an html file
    Set Fileout = fso.CreateTextFile(ActiveWorkbook.Path & "\errorPage.html", True, True)
    Fileout.Write XMLResponse
    Fileout.Close
End Sub
