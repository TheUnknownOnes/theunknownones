object Form42: TForm42
  Left = 0
  Top = 0
  Caption = 'Form42'
  ClientHeight = 636
  ClientWidth = 1272
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 240
    Top = 56
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object RichViewEditor1: TRichViewEditor
    Left = 96
    Top = 232
    Width = 1081
    Height = 385
    MergeMenu = MainMenu1
  end
  object frxReport1: TfrxReport
    Version = '4.11.8'
    DotMatrixReport = False
    IniFile = '\Software\Fast Reports'
    PreviewOptions.Buttons = [pbPrint, pbLoad, pbSave, pbExport, pbZoom, pbFind, pbOutline, pbPageSetup, pbTools, pbEdit, pbNavigator, pbExportQuick]
    PreviewOptions.Zoom = 1.000000000000000000
    PrintOptions.Printer = 'Vorgabe'
    PrintOptions.PrintOnSheet = 0
    ReportOptions.CreateDate = 40753.289326875000000000
    ReportOptions.LastChange = 40753.289326875000000000
    ScriptLanguage = 'PascalScript'
    ScriptText.Strings = (
      'begin'
      ''
      'end.')
    Left = 312
    Top = 152
    Datasets = <>
    Variables = <>
    Style = <>
  end
  object frxDesigner1: TfrxDesigner
    DefaultScriptLanguage = 'PascalScript'
    DefaultFont.Charset = DEFAULT_CHARSET
    DefaultFont.Color = clWindowText
    DefaultFont.Height = -13
    DefaultFont.Name = 'Arial'
    DefaultFont.Style = []
    DefaultLeftMargin = 10.000000000000000000
    DefaultRightMargin = 10.000000000000000000
    DefaultTopMargin = 10.000000000000000000
    DefaultBottomMargin = 10.000000000000000000
    DefaultPaperSize = 9
    DefaultOrientation = poPortrait
    GradientEnd = 11982554
    GradientStart = clWindow
    TemplatesExt = 'fr3'
    Restrictions = []
    RTLLanguage = False
    MemoParentFont = False
    Left = 416
    Top = 152
  end
  object frxRichObject1: TfrxRichObject
    Left = 176
    Top = 136
  end
  object MainMenu1: TMainMenu
    Left = 616
    Top = 56
    object abc1: TMenuItem
      Caption = 'abc'
    end
  end
end
