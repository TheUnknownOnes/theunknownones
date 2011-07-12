object formConfig: TformConfig
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'HistoryRelocator'
  ClientHeight = 87
  ClientWidth = 362
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
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 60
    Height = 13
    Caption = 'Historypath:'
  end
  object Label2: TLabel
    Left = 74
    Top = 32
    Width = 215
    Height = 13
    Caption = '(leave it empty to get the default behaviour)'
  end
  object Panel1: TPanel
    Left = 0
    Top = 57
    Width = 362
    Height = 30
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = 102
    object Button1: TButton
      AlignWithMargins = True
      Left = 203
      Top = 3
      Width = 75
      Height = 24
      Align = alRight
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 0
      ExplicitLeft = 288
      ExplicitTop = 16
      ExplicitHeight = 25
    end
    object Button2: TButton
      AlignWithMargins = True
      Left = 284
      Top = 3
      Width = 75
      Height = 24
      Align = alRight
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      ExplicitLeft = 408
      ExplicitTop = 16
      ExplicitHeight = 25
    end
  end
  object edPath: TEdit
    Left = 74
    Top = 5
    Width = 280
    Height = 21
    TabOrder = 1
  end
end
