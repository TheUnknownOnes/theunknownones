object FormCNAPropEdBase: TFormCNAPropEdBase
  Left = 247
  Top = 111
  Width = 432
  Height = 340
  BorderIcons = [biMinimize, biMaximize]
  BorderStyle = bsSizeToolWin
  BorderWidth = 5
  Caption = 'FormCNAPropEdBase'
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
  object panBottom: TPanel
    Left = 0
    Top = 248
    Width = 414
    Height = 55
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      414
      55)
    object btnCancel: TButton
      Left = 305
      Top = 26
      Width = 107
      Height = 26
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'btnCancel'
      ModalResult = 2
      TabOrder = 1
    end
    object btnOK: TButton
      Left = 188
      Top = 26
      Width = 107
      Height = 26
      Anchors = [akTop, akRight]
      Caption = 'btnOK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object btnClearValue: TButton
      Left = 7
      Top = 26
      Width = 107
      Height = 26
      Caption = 'btnClearValue'
      ModalResult = 5
      TabOrder = 3
    end
    object cbShowInputBox: TCheckBox
      Left = 8
      Top = 2
      Width = 409
      Height = 17
      Caption = 'cbShowInputBox'
      TabOrder = 2
    end
  end
  object panPropName: TPanel
    Left = 0
    Top = 0
    Width = 414
    Height = 33
    Align = alTop
    Alignment = taLeftJustify
    BevelOuter = bvNone
    Caption = 'panPropName'
    Color = clActiveCaption
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clCaptionText
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
  end
end
