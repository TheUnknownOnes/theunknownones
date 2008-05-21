object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 494
  ClientWidth = 894
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 682
    Top = 0
    Height = 494
    Align = alRight
    ExplicitLeft = 368
    ExplicitTop = 160
    ExplicitHeight = 100
  end
  object btn_SetValue: TButton
    Left = 24
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Set Value'
    TabOrder = 0
    OnClick = btn_SetValueClick
  end
  object btn_ValuesExists: TButton
    Left = 24
    Top = 39
    Width = 75
    Height = 25
    Caption = 'Values Exists'
    TabOrder = 1
    OnClick = btn_ValuesExistsClick
  end
  object btn_GetValue: TButton
    Left = 24
    Top = 70
    Width = 75
    Height = 25
    Caption = 'Get Value'
    TabOrder = 2
    OnClick = btn_GetValueClick
  end
  object btn_DeleteValue: TButton
    Left = 24
    Top = 101
    Width = 75
    Height = 25
    Caption = 'Delete Value'
    TabOrder = 3
    OnClick = btn_DeleteValueClick
  end
  object btn_Save: TButton
    Left = 24
    Top = 132
    Width = 75
    Height = 25
    Caption = 'Save'
    TabOrder = 4
    OnClick = btn_SaveClick
  end
  object btn_Load: TButton
    Left = 24
    Top = 163
    Width = 75
    Height = 25
    Caption = 'Load'
    TabOrder = 5
    OnClick = btn_LoadClick
  end
  object pan_Right: TPanel
    Left = 685
    Top = 0
    Width = 209
    Height = 494
    Align = alRight
    BevelOuter = bvNone
    Color = clNavy
    TabOrder = 6
    object Splitter2: TSplitter
      Left = 0
      Top = 351
      Width = 209
      Height = 3
      Cursor = crVSplit
      Align = alBottom
      ExplicitTop = 0
      ExplicitWidth = 248
    end
    object Splitter3: TSplitter
      Left = 0
      Top = 215
      Width = 209
      Height = 3
      Cursor = crVSplit
      Align = alBottom
      ExplicitTop = 0
      ExplicitWidth = 112
    end
    object TabControl1: TTabControl
      Left = 0
      Top = 354
      Width = 209
      Height = 140
      Align = alBottom
      TabOrder = 0
      Tabs.Strings = (
        'fgdfg'
        'dfg'
        'dfgdfgdfg'
        'dfgdfg')
      TabIndex = 1
    end
    object PageControl1: TPageControl
      Left = 0
      Top = 218
      Width = 209
      Height = 133
      ActivePage = TabSheet1
      Align = alBottom
      TabOrder = 1
      object TabSheet1: TTabSheet
        Caption = 'TabSheet1'
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
      end
    end
  end
  object ListView1: TListView
    Left = 216
    Top = 132
    Width = 337
    Height = 150
    Columns = <
      item
        Caption = '0'
      end
      item
        Caption = '1'
      end
      item
        Caption = '2'
      end
      item
        Caption = '3'
      end>
    TabOrder = 7
    ViewStyle = vsReport
  end
  object VirtualStringTree1: TVirtualStringTree
    Left = 24
    Top = 328
    Width = 417
    Height = 113
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Options = [hoColumnResize, hoDrag, hoVisible]
    TabOrder = 8
    Columns = <
      item
        Position = 0
        WideText = '0'
      end
      item
        Position = 1
        WideText = '1'
      end
      item
        Position = 2
        WideText = '2'
      end
      item
        Position = 3
        WideText = '3'
      end>
  end
  object Settings2: TSettingsXMLFile
    FileName = 'c:\temp\test.xml'
    Left = 296
    Top = 80
  end
  object Settings1: TSettingsXMLFile
    ParentSettings = Settings2
    FileName = 'c:\temp\test1.xml'
    Left = 248
    Top = 80
  end
  object SettingsCompLinkControl1: TSettingsCompLinkControl
    Component = pan_Right
    Settings = Settings2
    RootSetting = '/Form1/pan_Right'
    SaveWidth = True
    Left = 696
    Top = 16
  end
  object SettingsCompLinkTabControl1: TSettingsCompLinkTabControl
    Component = TabControl1
    Settings = Settings2
    RootSetting = '/Form1/pan_Right/TabControl1'
    SaveHeight = True
    Left = 608
    Top = 312
  end
  object SettingsCompLinkForm1: TSettingsCompLinkForm
    Settings = Settings2
    RootSetting = '/Form1'
    SaveLeft = True
    SaveTop = True
    SaveWidth = True
    SaveHeight = True
    Left = 456
    Top = 32
  end
  object SettingsCompLinkPageControl1: TSettingsCompLinkPageControl
    Component = PageControl1
    Settings = Settings2
    RootSetting = '/Form1/pan_Right/PageControl1'
    SaveHeight = True
    Left = 584
    Top = 168
  end
  object SettingsCompLinkListView1: TSettingsCompLinkListView
    Component = ListView1
    Settings = Settings2
    RootSetting = '/Form1/ListView1'
    Left = 352
    Top = 208
  end
end
