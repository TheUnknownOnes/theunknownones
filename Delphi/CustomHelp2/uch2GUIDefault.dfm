object ch2FormGUIDefault: Tch2FormGUIDefault
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  ClientHeight = 398
  ClientWidth = 531
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnClose = FormClose
  OnKeyUp = FormKeyUp
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  inline Tree: Tch2FrameHelpTree
    Left = 0
    Top = 0
    Width = 531
    Height = 398
    Align = alClient
    TabOrder = 0
    TabStop = True
    ExplicitWidth = 531
    ExplicitHeight = 398
    inherited TreeView1: TTreeView
      Width = 525
      Height = 362
      ExplicitWidth = 525
      ExplicitHeight = 362
    end
    inherited Panel1: TPanel
      Width = 531
      ExplicitWidth = 531
      inherited cbKeywords: TComboBox
        Width = 465
        ExplicitWidth = 465
      end
    end
  end
  object tmDoSearch: TTimer
    Enabled = False
    Interval = 200
    OnTimer = tmDoSearchTimer
    Left = 256
    Top = 200
  end
end
