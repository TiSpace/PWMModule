VERSION 5.00
Object = "{648A5603-2C6E-101B-82B6-000000000014}#1.1#0"; "MSCOMM32.OCX"
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Begin VB.Form Form1 
   Caption         =   "PWM Modul"
   ClientHeight    =   3945
   ClientLeft      =   225
   ClientTop       =   855
   ClientWidth     =   8085
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   3945
   ScaleWidth      =   8085
   StartUpPosition =   3  'Windows-Standard
   Begin VB.Timer Timer1 
      Enabled         =   0   'False
      Interval        =   250
      Left            =   3120
      Top             =   4320
   End
   Begin VB.CheckBox Check1 
      Caption         =   "Check1"
      Height          =   375
      Left            =   120
      TabIndex        =   20
      Top             =   4560
      Width           =   1935
   End
   Begin VB.CheckBox chkServo 
      Caption         =   "Servobetrieb"
      Height          =   255
      Left            =   240
      TabIndex        =   19
      Top             =   3480
      Width           =   2535
   End
   Begin VB.CommandButton cmdRead 
      Caption         =   "read"
      Height          =   1335
      Left            =   2880
      TabIndex        =   18
      Top             =   360
      Width           =   975
   End
   Begin VB.TextBox txtCom 
      BeginProperty Font 
         Name            =   "Terminal"
         Size            =   6
         Charset         =   255
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   1695
      Left            =   8640
      MultiLine       =   -1  'True
      TabIndex        =   17
      Top             =   840
      Width           =   1935
   End
   Begin VB.Frame Frame3 
      Caption         =   "PWM Frequenz"
      Height          =   1455
      Left            =   4200
      TabIndex        =   12
      Top             =   240
      Width           =   3615
      Begin VB.ListBox List1 
         Height          =   1035
         ItemData        =   "PWM Modul.frx":0000
         Left            =   2160
         List            =   "PWM Modul.frx":0013
         TabIndex        =   13
         ToolTipText     =   "Doppelklick zur Auswahl"
         Top             =   240
         Width           =   1095
      End
      Begin VB.Label lblFrequenz 
         Caption         =   "resultierende Frequenz"
         Height          =   375
         Left            =   120
         TabIndex        =   14
         Top             =   480
         Width           =   1095
      End
   End
   Begin VB.Frame Frame2 
      Caption         =   "PWM Tastverhältnis"
      Height          =   1455
      Left            =   240
      TabIndex        =   3
      Top             =   1920
      Width           =   7575
      Begin VB.TextBox txtSlide 
         Height          =   285
         Index           =   0
         Left            =   5520
         TabIndex        =   7
         Text            =   "0"
         Top             =   480
         Width           =   615
      End
      Begin VB.TextBox txtSlide 
         Height          =   285
         Index           =   1
         Left            =   5520
         TabIndex        =   6
         Text            =   "0"
         Top             =   840
         Width           =   615
      End
      Begin VB.CommandButton cmdSenden 
         Caption         =   "senden"
         Height          =   375
         Index           =   0
         Left            =   6240
         TabIndex        =   5
         Top             =   360
         Width           =   1095
      End
      Begin VB.CommandButton cmdSenden 
         Caption         =   "senden"
         Height          =   375
         Index           =   1
         Left            =   6240
         TabIndex        =   4
         Top             =   840
         Width           =   1095
      End
      Begin MSComctlLib.Slider Slider 
         Height          =   255
         Index           =   0
         Left            =   1440
         TabIndex        =   8
         Top             =   480
         Width           =   3735
         _ExtentX        =   6588
         _ExtentY        =   450
         _Version        =   393216
         Max             =   255
         TickStyle       =   3
      End
      Begin MSComctlLib.Slider Slider 
         Height          =   255
         Index           =   1
         Left            =   1440
         TabIndex        =   9
         Top             =   960
         Width           =   3735
         _ExtentX        =   6588
         _ExtentY        =   450
         _Version        =   393216
         Max             =   255
         TickStyle       =   3
      End
      Begin VB.Label lblKanal 
         Caption         =   "Kanal 1"
         Height          =   375
         Index           =   0
         Left            =   240
         TabIndex        =   11
         Top             =   480
         Width           =   975
      End
      Begin VB.Label lblKanal 
         Caption         =   "Kanal 2"
         Height          =   255
         Index           =   1
         Left            =   240
         TabIndex        =   10
         Top             =   960
         Width           =   975
      End
   End
   Begin VB.Frame Frame1 
      Caption         =   "COM Port"
      Height          =   1455
      Left            =   240
      TabIndex        =   0
      Top             =   240
      Width           =   2295
      Begin VB.TextBox Text1 
         Alignment       =   1  'Rechts
         Enabled         =   0   'False
         Height          =   285
         Left            =   1200
         TabIndex        =   16
         Text            =   "9600"
         Top             =   840
         Width           =   735
      End
      Begin VB.TextBox txtCOMPort 
         Alignment       =   1  'Rechts
         Height          =   285
         Left            =   1200
         TabIndex        =   1
         Text            =   "4"
         Top             =   360
         Width           =   735
      End
      Begin VB.Label Label1 
         Caption         =   "Baudrate"
         Height          =   255
         Left            =   240
         TabIndex        =   15
         Top             =   840
         Width           =   855
      End
      Begin VB.Label lblPort 
         Caption         =   "Port"
         Height          =   255
         Left            =   240
         TabIndex        =   2
         Top             =   360
         Width           =   615
      End
   End
   Begin MSCommLib.MSComm MSComm 
      Left            =   4560
      Top             =   2760
      _ExtentX        =   1005
      _ExtentY        =   1005
      _Version        =   393216
      CommPort        =   4
      DTREnable       =   -1  'True
      RThreshold      =   1
      SThreshold      =   1
   End
   Begin VB.Shape ShapeError 
      FillColor       =   &H000000FF&
      Height          =   375
      Left            =   7440
      Shape           =   2  'Oval
      Top             =   3480
      Width           =   375
   End
   Begin VB.Menu mnuEnde 
      Caption         =   "&Ende"
   End
   Begin VB.Menu mnuInfo 
      Caption         =   "&Info"
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim strKommunikation As String
Dim Flag_NoChange As Boolean
    
Private Sub Check1_Click()
If Check1.Value = 1 Then
    Timer1.Enabled = True
Else
    Timer1.Enabled = False
End If
End Sub

Private Sub chkServo_Click()
Dim i As Integer

Select Case chkServo.Value
    Case 1
        For i = 0 To 1
            Slider(i).Min = 215
            Slider(i).Max = 254
        Next
        Frame2.Caption = "Servobetrieb"
        Frame3.Enabled = False
        Call DataSend("3", 4) ' auf 61Hz
        List1.ListIndex = 3
        
    Case 0
        For i = 0 To 1
            Slider(i).Min = 0
            Slider(i).Max = 255
        Next
        Frame2.Caption = "PWM Tastverhältnis"
        Frame3.Enabled = True
End Select

End Sub

Private Sub cmdRead_Click()

    Call ReadSetting
End Sub

Private Sub cmdSenden_Click(Index As Integer)

    Call DataSend(Chr(Index + 49), Chr(Val(txtSlide(Index).Text)))
End Sub
Private Sub DataSend(CommandByte As String, DataByte As String)

On Error GoTo errhdl 'Resume Next ' kommt vor, wenn zu schnell Port geöffnet und geschlossen wird

    strKommunikation = ""
    With MSComm
        .CommPort = Val(txtCOMPort.Text)
        .PortOpen = True
        .Output = "*"
        warte 0.1
        .Output = CommandByte ' nur "1" oder "2" sind gültig
        warte 0.1
        .Output = DataByte
        .PortOpen = False
        
    End With
    txtCom.Text = strKommunikation

    ShapeError.FillStyle = 1
Exit Sub

errhdl:
    ShapeError.FillStyle = 0
    DoEvents
    Resume Next
    
End Sub



Private Sub Form_Activate()

Call ReadSetting 'lese aktuelles Setting

End Sub

Private Sub Form_Load()
' hole gespeicherte Parameter aus Registry
txtCOMPort.Text = Val(GetSetting("PWM-Modul", "ComPort", "Port", "1"))



End Sub

Private Sub List1_DblClick()
    Call DataSend("3", List1.ListIndex + 1)
End Sub

Private Sub mnuEnde_Click()
End
End Sub

Private Sub mnuInfo_Click()
Dim sText As String

sText = "GUI für PWM Modul" & vbCrLf & _
        "HW: tr090-2016" & vbCrLf & _
        "Version: " & App.Major & "." & App.Minor
        
        
MsgBox sText, vbInformation, "Info"
End Sub

Private Sub ReadSetting()
       Dim Datensätze() As String
    Me.MousePointer = 11
    strKommunikation = ""
    
    With MSComm
        .CommPort = Val(txtCOMPort.Text)
        .PortOpen = True
        .Output = "*"
        warte 0.1
        .Output = "4"
        warte 0.1
        .PortOpen = False
        
    End With
    
    txtCom.Text = strKommunikation
    Datensätze = Split(strKommunikation, vbCrLf)
            
    If UBound(Datensätze) = 5 Then
    
        Flag_NoChange = True ' abblocken, dass Slider aktualisiert
        Slider(0).Value = Datensätze(2)
        Slider(1).Value = Datensätze(3)
        Flag_NoChange = False
        List1.Selected((Datensätze(4) - 1) And &H7) = True

    End If
    Me.MousePointer = 0
End Sub
Private Sub MSComm_OnComm()
Dim byteRec As Byte
    
    Select Case MSComm.CommEvent
        Case comOverrun

        Case comRxOver
    
        Case comEvReceive
            'txtCom.Text = txtCom.Text & MSComm.Input
            strKommunikation = strKommunikation & MSComm.Input
    End Select
End Sub

Private Sub Slider_Change(Index As Integer)
    txtSlide(Index).Text = Slider(Index).Value
    If Not (Flag_NoChange) Then
        Call DataSend(Chr(Index + 49), Chr(Val(txtSlide(Index).Text)))
    End If
End Sub

Private Sub Timer1_Timer()
If txtSlide(0).Text < Slider(0).Max Then
    txtSlide(0).Text = txtSlide(0).Text + 1
Else
   txtSlide(0).Text = Slider(0).Min
End If
Call cmdSenden_Click(0)
End Sub

Private Sub txtCom_DblClick()
txtCom = ""
End Sub

Private Sub txtCOMPort_Change()
SaveSetting "PWM-Modul", "ComPort", "Port", txtCOMPort.Text
End Sub

Private Sub txtSlide_Change(Index As Integer)
Slider(Index).Value = txtSlide(Index).Text
End Sub
