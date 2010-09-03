object ch2FormConfigure: Tch2FormConfigure
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Configure CustomHelp'
  ClientHeight = 479
  ClientWidth = 724
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
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 718
    Height = 263
    Align = alClient
    Caption = 'Helpprovider (doubleclick to configure)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    object lv_Provider: TListView
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 708
      Height = 240
      Align = alClient
      Columns = <
        item
          Caption = 'Name'
          Width = 150
        end
        item
          AutoSize = True
          Caption = 'Description'
        end>
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ReadOnly = True
      RowSelect = True
      ParentFont = False
      SortType = stData
      TabOrder = 0
      ViewStyle = vsReport
      OnCompare = lv_ProviderCompare
      OnDblClick = lv_ProviderDblClick
    end
  end
  object GroupBox2: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 272
    Width = 718
    Height = 172
    Align = alBottom
    Caption = 'GUI'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    object lv_GUI: TListView
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 708
      Height = 149
      Align = alClient
      Checkboxes = True
      Columns = <
        item
          Caption = 'Name'
          Width = 150
        end
        item
          AutoSize = True
          Caption = 'Description'
        end>
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ReadOnly = True
      RowSelect = True
      ParentFont = False
      TabOrder = 0
      ViewStyle = vsReport
      OnItemChecked = lv_GUIItemChecked
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 447
    Width = 724
    Height = 32
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object btn_Cancel: TButton
      AlignWithMargins = True
      Left = 646
      Top = 3
      Width = 75
      Height = 26
      Align = alRight
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object btn_OK: TButton
      AlignWithMargins = True
      Left = 565
      Top = 3
      Width = 75
      Height = 26
      Align = alRight
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = btn_OKClick
    end
  end
end
