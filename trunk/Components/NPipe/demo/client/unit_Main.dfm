object Form4: TForm4
  Left = 0
  Top = 0
  Width = 339
  Height = 606
  Caption = 'TNPipe_Client'
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
  object lbl_SendTimeout: TLabel
    Left = 4
    Top = 52
    Width = 67
    Height = 13
    Caption = 'Send timeout:'
  end
  object ed_Pipename: TLabeledEdit
    Left = 8
    Top = 20
    Width = 153
    Height = 21
    EditLabel.Width = 53
    EditLabel.Height = 13
    EditLabel.Caption = 'Pipe name:'
    TabOrder = 0
    OnChange = ed_PipenameChange
  end
  object ed_Pipeserver: TLabeledEdit
    Left = 176
    Top = 20
    Width = 153
    Height = 21
    EditLabel.Width = 58
    EditLabel.Height = 13
    EditLabel.Caption = 'Pipe server:'
    TabOrder = 1
    OnChange = ed_PipeserverChange
  end
  object tb_SendTimeout: TTrackBar
    Left = 2
    Top = 72
    Width = 325
    Height = 45
    Max = 5000
    TabOrder = 2
    OnChange = tb_SendTimeoutChange
  end
  object btn_SendFile: TButton
    Left = 244
    Top = 160
    Width = 75
    Height = 25
    Caption = 'Send file'
    TabOrder = 3
    OnClick = btn_SendFileClick
  end
  object ed_FileName: TLabeledEdit
    Left = 8
    Top = 132
    Width = 313
    Height = 21
    EditLabel.Width = 46
    EditLabel.Height = 13
    EditLabel.Caption = 'Filename:'
    TabOrder = 4
  end
  object mem_Data: TMemo
    Left = 8
    Top = 192
    Width = 309
    Height = 121
    HideSelection = False
    ParentShowHint = False
    ScrollBars = ssBoth
    ShowHint = True
    TabOrder = 5
  end
  object btn_SendText: TButton
    Left = 244
    Top = 320
    Width = 75
    Height = 25
    Caption = 'Send text'
    TabOrder = 6
    OnClick = btn_SendTextClick
  end
  object gb_Progress: TGroupBox
    Left = 8
    Top = 348
    Width = 313
    Height = 41
    Caption = 'Progress'
    TabOrder = 7
    object PB: TProgressBar
      Left = 8
      Top = 16
      Width = 297
      Height = 16
      TabOrder = 0
    end
  end
  object gb_Reply: TGroupBox
    Left = 8
    Top = 396
    Width = 313
    Height = 181
    Caption = 'Server reply'
    TabOrder = 8
    object rb_ReplyToFile: TRadioButton
      Left = 8
      Top = 20
      Width = 85
      Height = 17
      Caption = 'Save to file'
      TabOrder = 0
    end
    object ed_SaveFileName: TEdit
      Left = 12
      Top = 40
      Width = 293
      Height = 21
      TabOrder = 1
    end
    object rb_ShowText: TRadioButton
      Left = 8
      Top = 68
      Width = 89
      Height = 17
      Caption = 'Show as text'
      Checked = True
      TabOrder = 2
      TabStop = True
    end
    object mem_Text: TMemo
      Left = 8
      Top = 88
      Width = 297
      Height = 85
      HideSelection = False
      Lines.Strings = (
        'mem_Text')
      ParentShowHint = False
      ScrollBars = ssBoth
      ShowHint = True
      TabOrder = 3
    end
  end
  object NPipeClient1: TNPipeClient
    PipeName = 'MyPipe'
    PipeServer = '.'
    OnError = NPipeClient1Error
    OnServerReply = NPipeClient1ServerReply
    OnProgress = NPipeClient1Progress
    OnStartWork = NPipeClient1StartWork
    OnEndWork = NPipeClient1EndWork
    Left = 232
    Top = 44
  end
end
