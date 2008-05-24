object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 301
  ClientWidth = 562
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object SettingsLinkForm1: TSettingsLinkForm
    Settings = SettingsXMLFile1
    DefaultRootSetting = '/Form1'
    SaveProperties.Strings = (
      'Height'
      'Left'
      'Top'
      'Width')
    Left = 264
    Top = 64
  end
  object SettingsXMLFile1: TSettingsXMLFile
    Left = 224
    Top = 64
  end
end
