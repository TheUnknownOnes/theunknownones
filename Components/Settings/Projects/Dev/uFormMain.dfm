object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 255
  ClientWidth = 502
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
  object Settings2: TSettingsXMLFile
    FileName = 'c:\temp\test.xml'
    Left = 296
    Top = 80
  end
  object Settings1: TSettingsXMLFile
    FileName = 'c:\temp\test1.xml'
    Left = 248
    Top = 80
  end
  object SettingsComponentLinkControl1: TSettingsComponentLinkControl
    Component = btn_SetValue
    Settings = Settings2
    RootSetting = '/Form1/btn_SetValue'
    Left = 72
  end
end
