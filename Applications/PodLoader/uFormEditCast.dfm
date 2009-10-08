object form_EditCast: Tform_EditCast
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'PodCast details'
  ClientHeight = 156
  ClientWidth = 476
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnShow = FormShow
  DesignSize = (
    476
    156)
  PixelsPerInch = 96
  TextHeight = 13
  object lbl_URL: TLabel
    Left = 13
    Top = 40
    Width = 98
    Height = 13
    Caption = 'URL of the PodCast:'
  end
  object lbl_SaveTo: TLabel
    Left = 48
    Top = 67
    Width = 63
    Height = 13
    Caption = 'Save files to:'
  end
  object lbl_CheckInterval: TLabel
    Left = 8
    Top = 94
    Width = 103
    Height = 13
    Caption = 'Check interval (min.):'
  end
  object lbl_Name: TLabel
    Left = 80
    Top = 9
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object pan_Bottom: TPanel
    Left = 0
    Top = 126
    Width = 476
    Height = 30
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 4
    object btn_OK: TButton
      AlignWithMargins = True
      Left = 317
      Top = 3
      Width = 75
      Height = 24
      Align = alRight
      Caption = 'OK'
      TabOrder = 0
      OnClick = btn_OKClick
    end
    object btn_Cancel: TButton
      AlignWithMargins = True
      Left = 398
      Top = 3
      Width = 75
      Height = 24
      Align = alRight
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object ed_URL: TEdit
    Left = 128
    Top = 35
    Width = 340
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object ed_SaveTo: TJvDirectoryEdit
    Left = 128
    Top = 64
    Width = 340
    Height = 21
    DialogKind = dkWin32
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object ed_Interval: TJvValidateEdit
    Left = 128
    Top = 91
    Width = 57
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    CriticalPoints.MaxValueIncluded = False
    CriticalPoints.MinValueIncluded = False
    EditText = '60'
    HasMinValue = True
    MaxValue = 1.000000000000000000
    MinValue = 1.000000000000000000
    TabOrder = 3
  end
  object ed_Name: TEdit
    Left = 128
    Top = 6
    Width = 340
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'PodCast'
  end
  object SettingsLinkForm: TSettingsLinkForm
    DefaultRootSetting = '/GUI/form_EditCast'
    SaveProperties.Strings = (
      'Height'
      'Left'
      'Top'
      'Width')
    Settings = Data.Settings
    Left = 296
    Top = 80
  end
end
