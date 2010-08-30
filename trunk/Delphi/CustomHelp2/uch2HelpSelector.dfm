object ch2formHelpSelector: Tch2formHelpSelector
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'CustomHelp2 - Select help ...'
  ClientHeight = 494
  ClientWidth = 499
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 493
    Height = 166
    Align = alTop
    Caption = 'Keywords'
    TabOrder = 0
    object lst_Keywords: TListBox
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 483
      Height = 143
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
      OnClick = lst_KeywordsClick
    end
  end
  object GroupBox2: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 191
    Width = 493
    Height = 300
    Align = alClient
    Caption = 'Help'
    TabOrder = 1
    object tv_Help: TTreeView
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 483
      Height = 277
      Align = alClient
      Indent = 19
      TabOrder = 0
      OnDblClick = tv_HelpDblClick
    end
  end
  object Progress: TProgressBar
    AlignWithMargins = True
    Left = 3
    Top = 175
    Width = 493
    Height = 10
    Align = alTop
    Smooth = True
    TabOrder = 2
  end
  object tm_DelayedFirstSearch: TTimer
    Enabled = False
    Interval = 200
    OnTimer = tm_DelayedFirstSearchTimer
    Left = 240
    Top = 248
  end
end
