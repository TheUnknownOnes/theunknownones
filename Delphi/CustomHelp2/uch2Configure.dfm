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
    Left = 2
    Top = 2
    Width = 720
    Height = 270
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Align = alClient
    Caption = 'Helpprovider (doubleclick to configure)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    ExplicitHeight = 267
    object lv_Provider: TListView
      AlignWithMargins = True
      Left = 4
      Top = 17
      Width = 712
      Height = 249
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
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
      DragMode = dmAutomatic
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
      OnDblClick = lv_ProviderDblClick
      OnDragDrop = lv_ProviderDragDrop
      OnDragOver = lv_ProviderDragOver
      ExplicitHeight = 246
    end
  end
  object GroupBox2: TGroupBox
    AlignWithMargins = True
    Left = 2
    Top = 276
    Width = 720
    Height = 172
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Align = alBottom
    Caption = 'GUI'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    ExplicitTop = 273
    object lv_GUI: TListView
      AlignWithMargins = True
      Left = 4
      Top = 17
      Width = 712
      Height = 151
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
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
      OnChange = lv_GUIChange
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 450
    Width = 724
    Height = 29
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object btn_Cancel: TButton
      AlignWithMargins = True
      Left = 647
      Top = 2
      Width = 75
      Height = 25
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alRight
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      ExplicitHeight = 28
    end
    object btn_OK: TButton
      AlignWithMargins = True
      Left = 568
      Top = 2
      Width = 75
      Height = 25
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alRight
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = btn_OKClick
      ExplicitHeight = 28
    end
  end
end
