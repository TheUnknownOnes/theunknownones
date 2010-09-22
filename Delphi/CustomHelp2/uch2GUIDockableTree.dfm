object ch2FormGUIDockableTree: Tch2FormGUIDockableTree
  Left = 0
  Top = 0
  Caption = 'Custom Help 2'
  ClientHeight = 253
  ClientWidth = 452
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 12
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
      ExplicitWidth = 446
      ExplicitHeight = 217
    end
    inherited Panel1: TPanel
      Width = 452
      ExplicitWidth = 452
      inherited Label1: TLabel
        Width = 51
        ExplicitWidth = 51
        ExplicitHeight = 12
      end
      inherited cbKeywords: TComboBox
        Left = 60
        Width = 389
        Height = 20
        ItemHeight = 12
        ExplicitLeft = 60
        ExplicitWidth = 389
        ExplicitHeight = 20
      end
    end
  end
end
