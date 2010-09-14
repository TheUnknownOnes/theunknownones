object ch2FormGUIDockableTree: Tch2FormGUIDockableTree
  Left = 0
  Top = 0
  Caption = 'Custom Help 2'
  ClientHeight = 283
  ClientWidth = 552
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  inline Frame: Tch2FrameHelpTree
    Left = 0
    Top = 0
    Width = 552
    Height = 283
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 552
    ExplicitHeight = 283
    inherited TreeView1: TTreeView
      Top = 34
      Width = 546
      Height = 246
      ExplicitTop = 34
      ExplicitWidth = 546
      ExplicitHeight = 246
    end
    inherited Panel1: TPanel
      Width = 552
      Height = 31
      ExplicitWidth = 552
      ExplicitHeight = 31
      inherited Label1: TLabel
        Height = 25
        ParentFont = False
      end
      inherited cbKeywords: TComboBox
        Width = 475
        ParentFont = False
        ExplicitWidth = 475
      end
    end
  end
end
