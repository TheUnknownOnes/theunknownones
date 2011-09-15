object ch2FormGUIDockableTree: Tch2FormGUIDockableTree
  Left = 0
  Top = 0
  Caption = 'Custom Help 2'
  ClientHeight = 253
  ClientWidth = 452
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inline Frame: Tch2FrameHelpTree
    Left = 0
    Top = 0
    Width = 452
    Height = 253
    Align = alClient
    TabOrder = 0
    TabStop = True
    ExplicitWidth = 452
    ExplicitHeight = 253
    inherited TreeView1: TTreeView
      Width = 446
      Height = 217
      ParentFont = False
      ExplicitWidth = 446
      ExplicitHeight = 217
    end
    inherited Panel1: TPanel
      Width = 452
      ExplicitWidth = 452
      inherited cbKeywords: TComboBox
        Width = 386
        ExplicitWidth = 386
      end
    end
  end
end
