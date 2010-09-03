object ch2FormProviderMsHelp: Tch2FormProviderMsHelp
  Left = 0
  Top = 0
  Caption = 'MS Help 2.x'
  ClientHeight = 549
  ClientWidth = 807
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  PixelsPerInch = 120
  TextHeight = 16
  object GroupBox2: TGroupBox
    AlignWithMargins = True
    Left = 4
    Top = 83
    Width = 799
    Height = 462
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alClient
    Caption = 'MS Help Namespaces'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -14
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object lvNamespaces: TListView
      AlignWithMargins = True
      Left = 6
      Top = 23
      Width = 787
      Height = 293
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alClient
      Checkboxes = True
      Columns = <
        item
          Caption = 'Name'
          Width = 250
        end
        item
          AutoSize = True
          Caption = 'Description'
        end>
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
    end
    object Panel2: TPanel
      Left = 2
      Top = 320
      Width = 795
      Height = 140
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
    end
  end
  object GroupBox1: TGroupBox
    AlignWithMargins = True
    Left = 4
    Top = 4
    Width = 799
    Height = 71
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alTop
    Caption = 'Priority'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -14
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object ed_Prio: TSpinEdit
      Left = 14
      Top = 27
      Width = 92
      Height = 27
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      MaxValue = 0
      MinValue = 0
      TabOrder = 0
      Value = 0
    end
  end
end
