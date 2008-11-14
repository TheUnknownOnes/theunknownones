object form_ConfigSetValue: Tform_ConfigSetValue
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Choose value'
  ClientHeight = 240
  ClientWidth = 372
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pan_Buttons: TPanel
    Left = 0
    Top = 206
    Width = 372
    Height = 34
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      372
      34)
    object btn_OK: TButton
      Left = 210
      Top = 4
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object btn_Cancel: TButton
      Left = 291
      Top = 4
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object cb_ShowDialog: TCheckBox
      Left = 4
      Top = 8
      Width = 196
      Height = 17
      Caption = 'Show dialog on component drop'
      TabOrder = 2
    end
  end
end
