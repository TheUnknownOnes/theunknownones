object ch2FormConfig: Tch2FormConfig
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Configure CustomHelp'
  ClientHeight = 414
  ClientWidth = 789
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object PC: TPageControl
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 783
    Height = 408
    ActivePage = ts_General
    Align = alClient
    TabOrder = 0
    ExplicitHeight = 378
    object ts_General: TTabSheet
      Caption = 'General'
      ExplicitHeight = 350
    end
  end
end
