Private Sub ComboBox1_Enter()
ComboBox1.ColumnCount = 2
ComboBox1.RowSource = "Plan1!A2:B25"


End Sub

Private Sub CommandButton1_Click()

Dim DATA As Date
Dim OutlookApp As Object
Dim OutlookMail As Object


Sheets("Planilha1").Range("C1").Value = ComboBox1.Value
Sheets("Planilha1").Range("B1").Value = CCur(TextBox1)

dia = Sheets("Planilha1").Range("B2")


Sheets("hist").Range("A1048576").End(xlUp).Offset(1, 0).Value = Sheets("Planilha1").Range("B2")
Sheets("hist").Range("B1048576").End(xlUp).Offset(1, 0).Value = Sheets("Planilha1").Range("C1")
Sheets("hist").Range("C1048576").End(xlUp).Offset(1, 0).Value = Sheets("Planilha1").Range("D1")
Sheets("hist").Range("D1048576").End(xlUp).Offset(1, 0).Value = Sheets("Planilha1").Range("B1")
Sheets("hist").Range("E1048576").End(xlUp).Offset(1, 0).Value = Sheets("Planilha1").Range("F1")


' ### Trecho do boletador anterior: Enviar texto da cell A1 no e-mail

'Set OutlookApp = CreateObject("Outlook.Application")
'Set OutlookMail = OutlookApp.CreateItem(0)
'With OutlookMail
'    .to = Sheets("Planilha1").Range("E1")
'    .CC = "backoffice@vgrasset.com.br"
'    '.BCC = ""
'    .Subject = "Movimentação [VGR]"
'    .Body = Sheets("Planilha1").Range("A1") & vbNewLine & vbNewLine & Sheets("Planilha1").Range("A2")
'    .Display
'End With
'    Set OutlookMail = Nothing
'    Set OutlookApp = Nothing
    

End Sub

Private Sub CommandButton2_Click()
Dim DATA As Date
Dim OutlookApp As Object
Dim OutlookMail As Object
Dim rng As Range

Set rng = Nothing
On Error Resume Next
    
With Sheets("hist").Range("A1:E99999")
    .AutoFilter Field:=1, Operator:=xlAnd, Criteria1:="=" & Format(CDbl(Date), "dd/mm/yyyy")
End With

Sheets("hist").Select
Range("A1").Select
Range(Selection, Selection.End(xlDown)).Select
Range(Selection, Selection.End(xlToRight)).Select

Set rng = Selection.SpecialCells(xlCellTypeVisible)
On Error GoTo 0

If rng Is Nothing Then
        MsgBox "The selection is not a range or the sheet is protected" & _
               vbNewLine & "please correct and try again.", vbOKOnly
        Exit Sub
End If


With Application
        .EnableEvents = False
        .ScreenUpdating = False
End With

Set OutApp = CreateObject("Outlook.Application")
Set OutMail = OutApp.CreateItem(0)

On Error Resume Next
With OutMail
    .To = "backoffice@vgrasset.com.br"
    .CC = ""
    .BCC = ""
    .Subject = "Movimentações" & " - " & Sheets("Planilha1").Range("B2")
    .HTMLBody = RangetoHTML(rng)
    .Display   'or use .Send
End With
On Error GoTo 0

With Application
    .EnableEvents = True
    .ScreenUpdating = True
End With

Set OutMail = Nothing
Set OutApp = Nothing


End Sub

Function RangetoHTML(rng As Range)


Dim fso As Object
Dim ts As Object
Private Sub ComboBox1_Enter()
ComboBox1.ColumnCount = 2
ComboBox1.RowSource = "Plan1!A2:B25"


End Sub

Private Sub ComboBox2_Enter()
ComboBox2.ColumnCount = 1
ComboBox2.RowSource = "Planilha1!G1:G4"

End Sub

Private Sub CommandButton1_Click()

Dim DATA As Date
Dim OutlookApp As Object
Dim OutlookMail As Object


Sheets("Planilha1").Range("C1").Value = ComboBox1.Value
Sheets("Planilha1").Range("B1").Value = CCur(TextBox1)
Sheets("Planilha1").Range("H1").Value = ComboBox2.Value

dia = Sheets("Planilha1").Range("B2")


Sheets("hist").Range("A1048576").End(xlUp).Offset(1, 0).Value = Sheets("Planilha1").Range("B2")
Sheets("hist").Range("B1048576").End(xlUp).Offset(1, 0).Value = Sheets("Planilha1").Range("H1")
Sheets("hist").Range("C1048576").End(xlUp).Offset(1, 0).Value = Sheets("Planilha1").Range("C1")
Sheets("hist").Range("D1048576").End(xlUp).Offset(1, 0).Value = Sheets("Planilha1").Range("D1")
Sheets("hist").Range("E1048576").End(xlUp).Offset(1, 0).Value = Sheets("Planilha1").Range("B1")
Sheets("hist").Range("F1048576").End(xlUp).Offset(1, 0).Value = Sheets("Planilha1").Range("F1")



'Set OutlookApp = CreateObject("Outlook.Application")
'Set OutlookMail = OutlookApp.CreateItem(0)
'With OutlookMail
'    .to = Sheets("Planilha1").Range("E1")
'    .CC = "backoffice@vgrasset.com.br"
'    '.BCC = ""
'    .Subject = "Movimentação [VGR]"
'    .Body = Sheets("Planilha1").Range("A1") & vbNewLine & vbNewLine & Sheets("Planilha1").Range("A2")
'    .Display
'End With
'    Set OutlookMail = Nothing
'    Set OutlookApp = Nothing
    

End Sub

Private Sub CommandButton2_Click()
Dim DATA As Date
Dim OutlookApp As Object
Dim OutlookMail As Object
Dim rng As Range

Set rng = Nothing
On Error Resume Next
    
With Sheets("hist").Range("A1:E99999")
    .AutoFilter Field:=1, Operator:=xlAnd, Criteria1:="=" & Format(CDbl(Date), "dd/mm/yyyy")
End With

Sheets("hist").Select
Range("A1").Select
Range(Selection, Selection.End(xlDown)).Select
Range(Selection, Selection.End(xlToRight)).Select

Set rng = Selection.SpecialCells(xlCellTypeVisible)
On Error GoTo 0

If rng Is Nothing Then
        MsgBox "The selection is not a range or the sheet is protected" & _
               vbNewLine & "please correct and try again.", vbOKOnly
        Exit Sub
End If


With Application
        .EnableEvents = False
        .ScreenUpdating = False
End With

Set OutApp = CreateObject("Outlook.Application")
Set OutMail = OutApp.CreateItem(0)

On Error Resume Next
With OutMail
    .To = "backoffice@vgrasset.com.br"
    .CC = ""
    .BCC = ""
    .Subject = "Movimentações" & " - " & Sheets("Planilha1").Range("B2")
    .HTMLBody = RangetoHTML(rng)
    .Display   'or use .Send
End With
On Error GoTo 0

With Application
    .EnableEvents = True
    .ScreenUpdating = True
End With

Set OutMail = Nothing
Set OutApp = Nothing


End Sub

Function RangetoHTML(rng As Range)


Dim fso As Object
Dim ts As Object
Dim TempFile As String
Dim TempWB As Workbook

TempFile = Environ$("temp") & "\" & Format(Now, "dd-mm-yy h-mm-ss") & ".htm"

'Copy the range and create a new workbook to past the data in
rng.Copy
Set TempWB = Workbooks.Add(1)
With TempWB.Sheets(1)
    .Cells(1).PasteSpecial Paste:=8
    .Cells(1).PasteSpecial xlPasteValues, , False, False
    .Cells(1).PasteSpecial xlPasteFormats, , False, False
    .Cells(1).Select
    Application.CutCopyMode = False
    On Error Resume Next
    .DrawingObjects.Visible = True
    .DrawingObjects.Delete
    On Error GoTo 0
End With

'Publish the sheet to a htm file
With TempWB.PublishObjects.Add( _
        SourceType:=xlSourceRange, _
        Filename:=TempFile, _
        Sheet:=TempWB.Sheets(1).Name, _
        Source:=TempWB.Sheets(1).UsedRange.Address, _
        HtmlType:=xlHtmlStatic)
    .Publish (True)
End With

'Read all data from the htm file into RangetoHTML
Set fso = CreateObject("Scripting.FileSystemObject")
Set ts = fso.GetFile(TempFile).OpenAsTextStream(1, -2)
RangetoHTML = ts.readall
ts.Close
RangetoHTML = Replace(RangetoHTML, "align=center x:publishsource=", _
                          "align=left x:publishsource=")

    'Close TempWB
TempWB.Close savechanges:=False

    'Delete the htm file we used in this function
Kill TempFile

Set ts = Nothing
Set fso = Nothing
Set TempWB = Nothing
End Function



Private Sub OptionButton1_Click()
Sheets("Planilha1").Range("F1").Value = "Aplicação"
End Sub

Private Sub OptionButton2_Click()
Sheets("Planilha1").Range("F1").Value = "Resgate"
End Sub
