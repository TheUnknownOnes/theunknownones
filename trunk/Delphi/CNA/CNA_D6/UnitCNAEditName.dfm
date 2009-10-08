object formName: TformName
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'CNA'
  ClientHeight = 83
  ClientWidth = 356
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblNewName: TLabel
    Left = 8
    Top = 8
    Width = 58
    Height = 13
    Caption = 'lblNewName'
  end
  object panBottom: TPanel
    Left = 0
    Top = 50
    Width = 356
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      356
      33)
    object btnCancel: TButton
      Left = 241
      Top = 5
      Width = 115
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'btnCancel'
      ModalResult = 2
      TabOrder = 1
    end
    object btnOK: TButton
      Left = 113
      Top = 5
      Width = 115
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'btnOK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
  end
  object edName: TEdit
    Left = 8
    Top = 24
    Width = 345
    Height = 21
    TabOrder = 0
    Text = 'edName'
  end
end
