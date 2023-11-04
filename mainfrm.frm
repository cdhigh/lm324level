VERSION 5.00
Begin VB.Form mainfrm 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "LM324电量指示设计软件v1.2"
   ClientHeight    =   8385
   ClientLeft      =   4080
   ClientTop       =   1260
   ClientWidth     =   12315
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   8385
   ScaleWidth      =   12315
   Begin VB.OptionButton optSch2 
      Caption         =   "电路图 2"
      Height          =   375
      Left            =   3120
      TabIndex        =   30
      Top             =   7800
      Width           =   1575
   End
   Begin VB.OptionButton optSch1 
      Caption         =   "电路图 1"
      Height          =   375
      Left            =   960
      TabIndex        =   29
      Top             =   7800
      Value           =   -1  'True
      Width           =   1575
   End
   Begin VB.CommandButton cmdInverseCal 
      Caption         =   "通过电阻值计算门限电压"
      BeginProperty Font 
         Name            =   "宋体"
         Size            =   10.5
         Charset         =   134
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   495
      Left            =   8400
      TabIndex        =   24
      Top             =   7680
      Width           =   3615
   End
   Begin VB.CommandButton cmdCalculate 
      Caption         =   "计算各电阻值"
      BeginProperty Font 
         Name            =   "宋体"
         Size            =   10.5
         Charset         =   134
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   495
      Left            =   5400
      TabIndex        =   23
      Top             =   7680
      Width           =   2535
   End
   Begin VB.Frame frmOut 
      Caption         =   "输出结果"
      BeginProperty Font 
         Name            =   "宋体"
         Size            =   10.5
         Charset         =   134
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   3255
      Left            =   5400
      TabIndex        =   9
      Top             =   4200
      Width           =   6615
      Begin VB.CommandButton cmdMore 
         Caption         =   "其他组合..."
         Height          =   315
         Left            =   3600
         TabIndex        =   42
         Top             =   2400
         Width           =   1455
      End
      Begin VB.TextBox txtR3 
         BeginProperty Font 
            Name            =   "宋体"
            Size            =   10.5
            Charset         =   134
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   315
         Left            =   1560
         TabIndex        =   40
         Top             =   802
         Width           =   1215
      End
      Begin VB.TextBox txtR8 
         BeginProperty Font 
            Name            =   "宋体"
            Size            =   10.5
            Charset         =   134
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   315
         Left            =   1560
         TabIndex        =   36
         Top             =   2750
         Width           =   1215
      End
      Begin VB.TextBox txtR2 
         BeginProperty Font 
            Name            =   "宋体"
            Size            =   10.5
            Charset         =   134
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   315
         Left            =   1560
         TabIndex        =   35
         Top             =   360
         Width           =   1215
      End
      Begin VB.TextBox txtR4 
         BeginProperty Font 
            Name            =   "宋体"
            Size            =   10.5
            Charset         =   134
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   315
         Left            =   1560
         TabIndex        =   34
         Top             =   1289
         Width           =   1215
      End
      Begin VB.TextBox txtR5 
         BeginProperty Font 
            Name            =   "宋体"
            Size            =   10.5
            Charset         =   134
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   315
         Left            =   1560
         TabIndex        =   33
         Top             =   1776
         Width           =   1215
      End
      Begin VB.TextBox txtR7 
         BeginProperty Font 
            Name            =   "宋体"
            Size            =   10.5
            Charset         =   134
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   315
         Left            =   1560
         TabIndex        =   32
         Top             =   2263
         Width           =   1215
      End
      Begin VB.Label lblHideR7R8 
         Height          =   495
         Left            =   120
         TabIndex        =   41
         Top             =   2640
         Width           =   3375
      End
      Begin VB.Label lblR3 
         Alignment       =   1  'Right Justify
         Caption         =   "R3"
         BeginProperty Font 
            Name            =   "宋体"
            Size            =   10.5
            Charset         =   134
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   120
         TabIndex        =   39
         Top             =   840
         Width           =   1215
      End
      Begin VB.Label Label14 
         Alignment       =   1  'Right Justify
         Caption         =   "实际门限电压2"
         BeginProperty Font 
            Name            =   "宋体"
            Size            =   10.5
            Charset         =   134
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   3240
         TabIndex        =   38
         Top             =   840
         Width           =   1815
      End
      Begin VB.Label lblThresholdVol2 
         BorderStyle     =   1  'Fixed Single
         BeginProperty Font 
            Name            =   "宋体"
            Size            =   10.5
            Charset         =   134
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   5280
         TabIndex        =   37
         Top             =   840
         Width           =   975
      End
      Begin VB.Label lblR8 
         Alignment       =   1  'Right Justify
         Caption         =   "R8"
         BeginProperty Font 
            Name            =   "宋体"
            Size            =   10.5
            Charset         =   134
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   120
         TabIndex        =   31
         Top             =   2760
         Width           =   1215
      End
      Begin VB.Label lblThresholdVol4 
         BorderStyle     =   1  'Fixed Single
         BeginProperty Font 
            Name            =   "宋体"
            Size            =   10.5
            Charset         =   134
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   5280
         TabIndex        =   22
         Top             =   1800
         Width           =   975
      End
      Begin VB.Label lblThresholdVol3 
         BorderStyle     =   1  'Fixed Single
         BeginProperty Font 
            Name            =   "宋体"
            Size            =   10.5
            Charset         =   134
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   5280
         TabIndex        =   21
         Top             =   1320
         Width           =   975
      End
      Begin VB.Label lblThresholdVol1 
         BorderStyle     =   1  'Fixed Single
         BeginProperty Font 
            Name            =   "宋体"
            Size            =   10.5
            Charset         =   134
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   5280
         TabIndex        =   20
         Top             =   360
         Width           =   975
      End
      Begin VB.Label Label16 
         Alignment       =   1  'Right Justify
         Caption         =   "实际门限电压4"
         BeginProperty Font 
            Name            =   "宋体"
            Size            =   10.5
            Charset         =   134
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   3240
         TabIndex        =   27
         Top             =   1800
         Width           =   1815
      End
      Begin VB.Label Label15 
         Alignment       =   1  'Right Justify
         Caption         =   "实际门限电压3"
         BeginProperty Font 
            Name            =   "宋体"
            Size            =   10.5
            Charset         =   134
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   3240
         TabIndex        =   26
         Top             =   1320
         Width           =   1815
      End
      Begin VB.Label Label13 
         Alignment       =   1  'Right Justify
         Caption         =   "实际门限电压1"
         BeginProperty Font 
            Name            =   "宋体"
            Size            =   10.5
            Charset         =   134
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   3240
         TabIndex        =   25
         Top             =   360
         Width           =   1815
      End
      Begin VB.Label lblR7 
         Alignment       =   1  'Right Justify
         Caption         =   "R6 + R7"
         BeginProperty Font 
            Name            =   "宋体"
            Size            =   10.5
            Charset         =   134
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   120
         TabIndex        =   13
         Top             =   2280
         Width           =   1215
      End
      Begin VB.Label lblR5 
         Alignment       =   1  'Right Justify
         Caption         =   "R5"
         BeginProperty Font 
            Name            =   "宋体"
            Size            =   10.5
            Charset         =   134
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   120
         TabIndex        =   12
         Top             =   1800
         Width           =   1215
      End
      Begin VB.Label lblR4 
         Alignment       =   1  'Right Justify
         Caption         =   "R4"
         BeginProperty Font 
            Name            =   "宋体"
            Size            =   10.5
            Charset         =   134
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   120
         TabIndex        =   11
         Top             =   1320
         Width           =   1215
      End
      Begin VB.Label lblR2 
         Alignment       =   1  'Right Justify
         Caption         =   "R2"
         BeginProperty Font 
            Name            =   "宋体"
            Size            =   10.5
            Charset         =   134
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   120
         TabIndex        =   10
         Top             =   360
         Width           =   1215
      End
   End
   Begin VB.Frame frmIn 
      Caption         =   "输入参数"
      BeginProperty Font 
         Name            =   "宋体"
         Size            =   10.5
         Charset         =   134
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   3375
      Left            =   5400
      TabIndex        =   3
      Top             =   720
      Width           =   6615
      Begin VB.ComboBox cmbResistorSerial 
         Height          =   300
         ItemData        =   "mainfrm.frx":0000
         Left            =   3480
         List            =   "mainfrm.frx":000A
         Style           =   2  'Dropdown List
         TabIndex        =   19
         Top             =   2880
         Width           =   1215
      End
      Begin VB.TextBox txtReferenceVol 
         BeginProperty Font 
            Name            =   "宋体"
            Size            =   10.5
            Charset         =   134
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   270
         Left            =   3480
         TabIndex        =   18
         Text            =   "2.5"
         Top             =   2400
         Width           =   1215
      End
      Begin VB.TextBox txtThresholdVol4 
         BeginProperty Font 
            Name            =   "宋体"
            Size            =   10.5
            Charset         =   134
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   270
         Left            =   3480
         TabIndex        =   17
         Text            =   "13.5"
         Top             =   1920
         Width           =   1215
      End
      Begin VB.TextBox txtThresholdVol3 
         BeginProperty Font 
            Name            =   "宋体"
            Size            =   10.5
            Charset         =   134
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   270
         Left            =   3480
         TabIndex        =   16
         Text            =   "12.5"
         Top             =   1440
         Width           =   1215
      End
      Begin VB.TextBox txtThresholdVol2 
         BeginProperty Font 
            Name            =   "宋体"
            Size            =   10.5
            Charset         =   134
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   270
         Left            =   3480
         TabIndex        =   15
         Text            =   "12"
         Top             =   960
         Width           =   1215
      End
      Begin VB.TextBox txtThresholdVol1 
         BeginProperty Font 
            Name            =   "宋体"
            Size            =   10.5
            Charset         =   134
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   270
         Left            =   3480
         TabIndex        =   14
         Text            =   "11.5"
         Top             =   480
         Width           =   1215
      End
      Begin VB.Label Label1 
         Alignment       =   1  'Right Justify
         Caption         =   "电阻系列"
         BeginProperty Font 
            Name            =   "宋体"
            Size            =   10.5
            Charset         =   134
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   960
         TabIndex        =   28
         Top             =   2880
         Width           =   2175
      End
      Begin VB.Label Label7 
         Alignment       =   1  'Right Justify
         Caption         =   "参考电压"
         BeginProperty Font 
            Name            =   "宋体"
            Size            =   10.5
            Charset         =   134
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   960
         TabIndex        =   8
         Top             =   2400
         Width           =   2175
      End
      Begin VB.Label Label6 
         Alignment       =   1  'Right Justify
         Caption         =   "门限电压4 (LED4点亮)"
         BeginProperty Font 
            Name            =   "宋体"
            Size            =   10.5
            Charset         =   134
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   960
         TabIndex        =   7
         Top             =   1920
         Width           =   2175
      End
      Begin VB.Label Label5 
         Alignment       =   1  'Right Justify
         Caption         =   "门限电压3 (LED3点亮)"
         BeginProperty Font 
            Name            =   "宋体"
            Size            =   10.5
            Charset         =   134
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   960
         TabIndex        =   6
         Top             =   1440
         Width           =   2175
      End
      Begin VB.Label Label4 
         Alignment       =   1  'Right Justify
         Caption         =   "门限电压2 (LED2点亮)"
         BeginProperty Font 
            Name            =   "宋体"
            Size            =   10.5
            Charset         =   134
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   960
         TabIndex        =   5
         Top             =   960
         Width           =   2175
      End
      Begin VB.Label Label3 
         Alignment       =   1  'Right Justify
         Caption         =   "门限电压1 (LED1点亮)"
         BeginProperty Font 
            Name            =   "宋体"
            Size            =   10.5
            Charset         =   134
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   960
         TabIndex        =   4
         Top             =   480
         Width           =   2175
      End
   End
   Begin VB.PictureBox cavSch 
      Height          =   6615
      Left            =   240
      ScaleHeight     =   6555
      ScaleWidth      =   4695
      TabIndex        =   0
      Top             =   840
      Width           =   4755
   End
   Begin VB.Label Label2 
      Caption         =   "一乐论坛3AG1老师的LM324电池电量电路设计"
      BeginProperty Font 
         Name            =   "宋体"
         Size            =   12
         Charset         =   134
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   375
      Left            =   240
      TabIndex        =   2
      Top             =   240
      Width           =   5295
   End
   Begin VB.Label lblLink 
      Alignment       =   1  'Right Justify
      Caption         =   "原帖地址：https://www.yleee.com.cn/thread-84439-1-1.html"
      BeginProperty Font 
         Name            =   "宋体"
         Size            =   9
         Charset         =   134
         Weight          =   400
         Underline       =   -1  'True
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FF0000&
      Height          =   375
      Left            =   6000
      TabIndex        =   1
      Top             =   240
      Width           =   5415
   End
End
Attribute VB_Name = "mainfrm"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub lblLink_Click()

End Sub

Private Sub optSch1_Click()

End Sub

Private Sub optSch2_Click()

End Sub
