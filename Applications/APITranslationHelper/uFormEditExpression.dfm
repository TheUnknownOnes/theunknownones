object form_EditExpression: Tform_EditExpression
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Edit expression'
  ClientHeight = 189
  ClientWidth = 304
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
  object lbl_Expression: TLabel
    Left = 8
    Top = 8
    Width = 56
    Height = 13
    Caption = 'Expression:'
  end
  object lbl_ReplaceBy: TLabel
    Left = 8
    Top = 54
    Width = 57
    Height = 13
    Caption = 'Replace by:'
  end
  object ed_Expression: TEdit
    Left = 24
    Top = 27
    Width = 273
    Height = 21
    TabOrder = 0
  end
  object pan_Bottom: TPanel
    Left = 0
    Top = 159
    Width = 304
    Height = 30
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object btn_OK: TButton
      AlignWithMargins = True
      Left = 145
      Top = 3
      Width = 75
      Height = 24
      Align = alRight
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object btn_Cancel: TButton
      AlignWithMargins = True
      Left = 226
      Top = 3
      Width = 75
      Height = 24
      Align = alRight
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object ed_ReplaceBy: TEdit
    Left = 24
    Top = 73
    Width = 272
    Height = 21
    TabOrder = 1
  end
end
