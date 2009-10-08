object Form4: TForm4
  Left = 0
  Top = 0
  Width = 306
  Height = 527
  Caption = 'TNPipe_Server'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lbl_Timeout: TLabel
    Left = 8
    Top = 56
    Width = 42
    Height = 13
    Caption = 'Timeout:'
  end
  object cb_LocalAccess: TCheckBox
    Left = 8
    Top = 8
    Width = 109
    Height = 17
    Caption = 'Local access only'
    TabOrder = 0
    OnClick = cb_LocalAccessClick
  end
  object cb_Active: TCheckBox
    Left = 8
    Top = 28
    Width = 97
    Height = 17
    Caption = 'Server active'
    TabOrder = 1
    OnClick = cb_ActiveClick
  end
  object ed_PipeName: TLabeledEdit
    Left = 124
    Top = 24
    Width = 169
    Height = 21
    EditLabel.Width = 53
    EditLabel.Height = 13
    EditLabel.Caption = 'Pipe name:'
    TabOrder = 2
    OnChange = ed_PipeNameChange
  end
  object tb_Timeout: TTrackBar
    Left = 8
    Top = 72
    Width = 289
    Height = 45
    Max = 10000
    Min = 1
    Frequency = 50
    Position = 1
    TabOrder = 3
    OnChange = tb_TimeoutChange
  end
  object gb_PB: TGroupBox
    Left = 4
    Top = 112
    Width = 289
    Height = 41
    Caption = 'Progress'
    TabOrder = 4
    object PB: TProgressBar
      Left = 8
      Top = 16
      Width = 273
      Height = 16
      TabOrder = 0
    end
  end
  object gb_IncomingData: TGroupBox
    Left = 4
    Top = 156
    Width = 289
    Height = 257
    Caption = 'Incoming data'
    TabOrder = 5
    object rb_SaveToFile: TRadioButton
      Left = 8
      Top = 24
      Width = 81
      Height = 17
      Caption = 'Save to file'
      TabOrder = 0
    end
    object ed_FileName: TEdit
      Left = 8
      Top = 44
      Width = 273
      Height = 21
      TabOrder = 1
    end
    object rb_ShowMemo: TRadioButton
      Left = 8
      Top = 72
      Width = 97
      Height = 17
      Caption = 'Show in memo'
      Checked = True
      TabOrder = 2
      TabStop = True
    end
    object mem_Data: TMemo
      Left = 8
      Top = 92
      Width = 273
      Height = 157
      HideSelection = False
      ParentShowHint = False
      ScrollBars = ssBoth
      ShowHint = True
      TabOrder = 3
    end
  end
  object gb_Reply: TGroupBox
    Left = 4
    Top = 416
    Width = 289
    Height = 81
    Caption = 'Reply data'
    TabOrder = 6
    object rb_ReplySize: TRadioButton
      Left = 12
      Top = 20
      Width = 77
      Height = 17
      Caption = 'Reply size'
      TabOrder = 0
    end
    object rb_ReplyData: TRadioButton
      Left = 12
      Top = 40
      Width = 113
      Height = 17
      Caption = 'Reply whole data'
      TabOrder = 1
    end
    object rb_NoReply: TRadioButton
      Left = 12
      Top = 60
      Width = 85
      Height = 17
      Caption = 'No reply'
      Checked = True
      TabOrder = 2
      TabStop = True
    end
  end
  object NPipeServer1: TNPipeServer
    PipeName = 'MyPipe'
    OnIncomingData = NPipeServer1IncomingData
    OnError = NPipeServer1Error
    OnProgress = NPipeServer1Progress
    OnStartWork = NPipeServer1StartWork
    OnEndWork = NPipeServer1EndWork
    Left = 232
    Top = 4
  end
end
